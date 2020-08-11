;;; dap-variables.el --- Launch configuration variables -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nikita Bloshchanevich

;; Author: Nikita Bloshchanevich <nikblos@outlook.com>
;; Keywords: languages

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'cl-lib)
(require 'lsp-mode)

;;; Commentary:
;; This module implements dap-mode's launch configuration variable support.

;;; Code:

(defun dap-variables--project-basename (&optional dir)
  "Return the name of the project root directory.
Starts the project-root search at DIR."
    (file-name-nondirectory (directory-file-name (lsp-workspace-root dir))))

(defun dap-variables--project-relative-file (&optional file dir)
  "Return the path to FILE relative to the project root.
The search for the project root starts at DIR. FILE defaults to
variable `buffer-file-name'."
    (file-relative-name (or file buffer-file-name) (lsp-workspace-root dir)))

(defun dap-variables--project-relative-dirname (&optional file dir)
  "Return the path to the directory of file relative to the project root.
The search for the project root starts at DIR. FILE defaults to
variable `buffer-file-name'"
  (dap-variables--project-relative-file (file-name-directory (or file buffer-file-name))
                                        dir))

(defun dap-variables--buffer-basename ()
  "Return the name of the current buffer's file without its directory."
  (file-name-nondirectory buffer-file-name))

(defun dap-variables--buffer-basename-sans-extension ()
  "Same as `dap-variables--buffer-basename', but without the extension."
  (file-name-sans-extension (dap-variables--buffer-basename)))

(defun dap-variables--buffer-extension ()
  "Return the extension of the buffer's file with a leading dot.
If there is either no file associated with the current buffer or
if that file has no extension, return the empty string."
  (if-let ((buffer-name buffer-file-name)
           (ext (file-name-extension buffer-name)))
      (concat "." ext)
    ""))

(defun dap-variables--buffer-dirname ()
  "Return the directory the buffer's file is in."
  (file-name-directory buffer-file-name))

(defun dap-variables--buffer-current-line ()
  "Return the line the cursor is on in the current buffer."
  (number-to-string (line-number-at-pos)))

(defun dap-variables--buffer-selected-text ()
  "Return the text selected in the current buffer.
If no text is selected, return the empty string."
  ;; Cannot fail, as if there is no mark, (mark) and (point) will be equal, and
  ;; (`buffer-substring-no-properties') will yield "", as it should.
  (buffer-substring-no-properties (mark) (point)))

(defun dap-variables--launch-configuration-var-getenv (var)
  "Return the environment variable in matched string VAR.
Only for use in `dap-launch-configuration-variables'."
  (let ((envvar (match-string 1 var)))
    (or (getenv envvar)
        (lsp-warn "launch.json: no such environment variable '%s' (in ${%s})"
                  envvar var)
         "")))

(defvar dap-variables-launch-configuration-variables
  ;; list taken from https://code.visualstudio.com/docs/editor/variables-reference
  '(("workspaceFolderBasename" . dap-variables--project-basename)
    ("workspaceFolder" . lsp-workspace-root)
    ("relativeFileDirname" . dap-variables--project-relative-dirname)
    ("relativeFile" . dap-variables--project-relative-file)
    ("fileBasenameNoExtension" . dap-variables--buffer-basename-sans-extension)
    ("fileBasename" . dap-variables--buffer-basename)
    ("fileDirname" . dap-variables--buffer-dirname)
    ("fileExtname" . dap-variables--buffer-extension)
    ("lineNumber" . dap-variables--buffer-current-line)
    ("selectedText" . dap-variables--buffer-selected-text)
    ("file" . buffer-file-name)
    ("env:\\(.*\\)" . dap-variables--launch-configuration-var-getenv)
    ;; technically not in VSCode, but I still wanted to add a way to escape $
    ("$" . "$")
    ;; the following variables are valid in VSCode, but have no meaning in
    ;; Emacs, and are as such unsupported.
    ;; ("cwd") ;; the task runner's current working directory,
    ;;         ;; not `default-directory'
    ;; ("execPath")
    ;; ("defaultBuildTask")
    )
  "Alist of (REGEX . VALUE) pairs listing variables usable in launch.json files.
This list is iterated from the top to bottom when expanding variables in the strings of the selected launch configuration
from launch.json or in `dap-variables-expand-variable'.

When a REGEX matches (`string-match'), its corresponding VALUE is
evaluated as follows: if it is a function (or a quoted lambda),
that function is called with `funcall', and its result, which
must be a string, is used in place of the variable. If you used
capture groups in REGEX, the function you specified in VALUE is
called with the variable as its only argument. This way, you can
use `string-match' to get the capture groups. If, however, REGEX
does not contain capture groups, your function is called without
any arguments. Otherwise, if it is a symbol, the symbol's value
is used the same way. Lastly, if it is a string, the string is
used as a replacement. If no regex matches, the empty string is
used as a replacement and a warning is issued.

See `dap-variables--launch-configuration-var-getenv' for an
example on how to use capture groups in REGEX.")

(defun dap-variables--eval-poly-type (value var)
  "Get the value from VALUE depending on its type.
If it is a function, and VAR is not nil, call VALUE and pass VAR as an argument.
If it is a symbol, return its value.
Otherwise, return VALUE"
  (cond ((and var (functionp value)) (funcall value var))
        ((functionp value) (funcall value))
        ((symbolp value) (symbol-value value))
        (t value)))

(defun dap-variables-expand-variable (var)
  "Expand variable VAR using `dap--launch-json-variables'."
  (catch 'ret
    (save-match-data
      (dolist (var-pair dap-variables-launch-configuration-variables)
        (when (string-match (car var-pair) var)
          (throw 'ret
                 (or
                  (dap-variables--eval-poly-type
                   (cdr var-pair)
                   (if (= (length (match-data)) 2) ;; no capture groups
                       nil
                     var))
                  (and (lsp-warn "launch.json: no such variable ${%s}" var) nil)
                  "")))))
    nil))

(defun dap-variables-expand-in-string (s)
  "Expand all launch.json variables of the from ${variable} in S.
Return the result."
  (let ((old-buffer (current-buffer)))
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))

      (save-match-data
        (while (re-search-forward "${\\([^}]*\\)}" nil t)
          (let ((var (match-string 1)))
            (replace-match
             (with-current-buffer old-buffer
               (dap-variables-expand-variable var))))))

      (buffer-string))))

(defun dap-variables-expand-in-launch-configuration (conf)
  "Non-destructively expand all variables in all strings of CONF.
CONF is regular dap-mode launch configuration. Return the result."
  (cond ((and (listp conf) (-all? #'consp conf))
         (-map (-lambda ((k . v))
                 (cons k (dap-variables-expand-in-launch-configuration v)))
               conf))
        ((listp conf)
         (apply #'nconc
                (cl-loop
                 for (k v) on conf by #'cddr collect
                 (list k (dap-variables-expand-in-launch-configuration v)))))
        ((stringp conf) (dap-variables-expand-in-string conf))
        (t conf)))

(provide 'dap-variables)
;;; dap-variables.el ends here
