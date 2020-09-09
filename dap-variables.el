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
        (progn
          (lsp-warn "launch.json: no such environment variable '%s' (in ${%s})"
                    envvar var)
          ""))))

(defface dap-variables-pid-face '((t :inherit font-lock-function-name-face))
  "Face processed ids are shown in for ${command:pickProcess}.
Each entry in that function consists of the process args followed
by the PID, in parentheses \(). Only the PID \(not the
parentheses) is shown with that face."
  :group 'dap-faces)

(defun dap-variables--pick-process ()
  "Prompt the user to select a system process and return it.
The return value is the process' PID, as an integer."
  (let* ((pids (list-system-processes))
         (pids-propertized (cl-loop for pid in pids collect
                                    (propertize (number-to-string pid) 'face
                                                'dap-variables-pid-face)))
         (proc-attrs (mapcar #'process-attributes pids))
         (proc-names (mapcar (apply-partially #'alist-get 'args) proc-attrs))
         (entries (cl-mapcar (apply-partially #'format "%s (%s)")
                             proc-names pids-propertized))
         (message-pid-alist (cl-mapcar #'cons entries pids))

         ;; History doesn't make sense here since PIDs are usually very unstable
         ;; (sometimes randomized for security and because processes spawn and
         ;; die). NOTE: the list does not update itself live, which is why we
         ;; don't require-match: the user may find out the PID after running
         ;; `dap-debug'.
         (chosen-entry (completing-read "Select a PID: " message-pid-alist)))
    (cdr (assoc chosen-entry message-pid-alist #'string=))))

(defvar dap-variables-launch-configuration-variables
  ;; list taken from https://code.visualstudio.com/docs/editor/variables-reference
  '(("\\`workspaceFolderBasename\\'" . dap-variables--project-basename)
    ("\\`workspaceFolder\\'" . lsp-workspace-root)
    ("\\`relativeFileDirname\\'" . dap-variables--project-relative-dirname)
    ("\\`relativeFile\\'" . dap-variables--project-relative-file)
    ("\\`fileBasenameNoExtension\\'" . dap-variables--buffer-basename-sans-extension)
    ("\\`fileBasename\\'" . dap-variables--buffer-basename)
    ("\\`fileDirname\\'" . dap-variables--buffer-dirname)
    ("\\`fileExtname\\'" . dap-variables--buffer-extension)
    ("\\`lineNumber\\'" . line-number-at-pos)
    ("\\`selectedText\\'" . dap-variables--buffer-selected-text)
    ("\\`file\\'" . buffer-file-name)
    ("\\`env:\\(.*\\)\\'" . dap-variables--launch-configuration-var-getenv)
    ;; technically not in VSCode, but I still wanted to add a way to escape $
    ("\\`\\$\\'" . "$")
    ;; the following variables are valid in VSCode, but have no meaning in
    ;; Emacs, and are as such unsupported.
    ;; ("cwd") ;; the task runner's current working directory,
    ;;         ;; not `default-directory'
    ;; ("execPath")
    ;; ("defaultBuildTask")
    ("\\`command:pickProcess\\'" . dap-variables--pick-process)
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

(defvar dap-variables-numbered-prompts '()
  "Mapping between numbered variables ${1} and their prompts.
List of lists (NUMBER QUESTION VAR) where NUMBER is the number of
the question (questions are asked in correct order), QUESTION is
a prompt to be displayed to the user and VAR is the variable
corresponding to the prompt. So if there is a variable
${1:hostname}, NUMBER would be 1, QUESTION would be hostname and
VAR would be 1:hostname.")

(defun dap-variables-count-unique-numbered-prompts (prompts)
  "Count the unique prompt numbers in PROMPTS.
PROMPTS must have the form of `dap-variables-numbered-prompts'
and it must be sorted by NUMBER."
  (let ((prev-n nil)
        (count 0))
    (dolist (prompt prompts)
      (unless (eq prev-n (nth 0 prompt))
        (setq count (1+ count))))
    count))

(defvar dap-variables-pre-expand-hook '()
  "List of functions to be run before a launch configuration is expanded.
They take one argument: the run configuration.")

(defvar dap-variables-post-expand-hook '()
  "List of functions to be run after a launch configuration has been expanded.
They take one argument: the run configuration, this time with all
variables expanded.")

(defun dap-variables--colon-prompt-var (var)
  "Implement a variable of the form ${num:message}.
VAR is the original variable encountered. For ${1:host?} it would
be \"1:host?\". Only for use in
`dap-variables-pre-expand-variables'."
  (push (list (string-to-number (match-string 1 var))
              (match-string 2 var) var) dap-variables-numbered-prompts))

(defvar dap-variables-prompt-histories (make-hash-table :test 'equal)
  "History of the user's answers to variable prompts (${1:host?}).
You may want to add this to `savehist-additional-variables'.")

(when (boundp 'savehist-additional-variables)
  (add-to-list 'savehist-additional-variables 'dap-variables-prompt-histories))

(defun dap-variables-reset-prompt-histories ()
  "Reset the histories of prompting variables."
  (interactive)
  (setq dap-variables-prompt-histories (make-hash-table :test 'equal)))

(defvar dap-variables--temp-hist nil
  "Temporarily the history list during expansion of a prompting variable.
Since read-string's history must be a symbol, I devised the
following trick to implement per prompt history: Look up the
corresponding history entry in `dap-variables--prompt-histories',
setq this variable to the result and pass this variable as the
history argument. puthash this variable under the prompt back
into `dap-variables--prompt-histories' and then finally setq this
back to nil.")

(defun dap-variables--do-prompts ()
  "Ask the questions in `dap-variables-numbered-prompts' in correct order."
  (let* ((prev-id nil)
         (prev-answer nil)
         (extra-vars '())
         (current-promptn 1)
         (numbered-prompts (--sort (< (car it) (car other))
                                   dap-variables-numbered-prompts))
         (unique-prompts (dap-variables-count-unique-numbered-prompts
                          numbered-prompts)))
    (mapc
     (-lambda ((id prompt var))
       (if (eq prev-id id) ;; prev-id can be nil, so eq and not =
           (progn
             (lsp-warn
              "launch.json: multiple prompts for variable number %d (in ${%s})"
              id var))
         (setq prev-id id)
         (setq dap-variables--temp-hist
               (gethash prompt dap-variables-prompt-histories))
         (setq prev-answer (read-string (format "\(%d/%d) %s: " current-promptn
                                                unique-prompts prompt)
                                        nil 'dap-variables--temp-hist))
         (puthash prompt dap-variables--temp-hist
                  dap-variables-prompt-histories)
         (setq dap-variables--temp-hist nil)
         (setq current-promptn (1+ current-promptn))
         ;; Doesn't appear to work. The intention was to have per-prompt
         ;; history.
         ;; (let ((history (gethash prompt dap-variables--prompt-history '())))
         ;;   (setq prev-answer (read-string (format "%s: " prompt) nil history))
         ;;   (push prev-answer history) ;; add the answer to history
         ;;   (puthash prompt history dap-variables--prompt-history))
         )
       ;; the variable with the prompt still has to be expanded
       (push (cons (concat "^" (regexp-quote var) "$") prev-answer) extra-vars)
       (push (cons (format "^%d$" id) prev-answer) extra-vars)) ;; ${n}
     numbered-prompts)
    extra-vars))

(defun dap-variables--do-prompts-reset (&optional _)
  "Ask and reset the questions in `dap-variables-numbered-prompts'.
For details, see `dap-variables--do-prompts'."
  (unwind-protect (dap-variables--do-prompts)
    (setq dap-variables-numbered-prompts nil)))

(defvar dap-variables-post-walk-hook
  '(dap-variables--do-prompts-reset)
  "Functions to be run after first walking the launch configuration.
When expanding a launch configuration, first
`dap-variables-pre-expand-hook' is called. Then, the launch
configuration is walked, visiting, but not expanding, all
variables in `dap-variables-pre-expand-variables'. Then all
functions in this list are called, with the launch configuration
as their only argument. They shall return a list of additional
variables of the form (REGEX . VALUE) (see
`dap-variables-launch-configuration-variables'). All lists are
concatenated and added to the list of variables used for
expansion.")

(defvar dap-variables-pre-expand-variables
  '(("\\(^[[:digit:]]+\\):\\(.*\\)$" . dap-variables--colon-prompt-var))
  "Alist of (REGEX . FUNCTION) pairs listing pre-expansion variables.
Before any expansion occurs, all variables matching REGEX have
their corresponding FUNCTION called, with the variable as
argument. Its result is ignored, and FUNCTION could be used to
initialize something for expansion.")

(defun dap-variables--eval-poly-type (value var)
  "Get the value from VALUE depending on its type.
VALUE is evaluated in two stages:

1. If it is a function, call it using `funcall', passing VAR as
argument if and only if VAR is not nil. If VALUE is a symbol and
that symbol does not have an associated function, yield that
symbol's value.

2. If the result of the first stage is a number, make it a
string, yielding stage 1's result otherwise."
  (let ((pre-res
         (pcase value
           ((pred functionp) (if var (funcall value var) (funcall value)))
           ((pred symbolp) (symbol-value value))
           (_ value))))
    ;; stage two expansion: VALUE as a function could have returned something,
    ;; or a number was stored in the symbol.
    (pcase pre-res
      ((pred numberp) (number-to-string pre-res))
      ;; by using a pcase here, this function becomes more extensible.
      (_ pre-res))))

(defun dap-variables-find-matching (var variable-alist)
  "Return the corresponding VALUE to the REGEX matching VAR.
Return nil if no matching VALUE is found. VARIABLE-ALIST is a
list of the form (REGEX . VARIABLE). This function modifies the
`match-data'. REGEX may contain captures, which may be accessed
with regular (`match-string' <n> VAR) or `match-data'."
  (cdr (cl-find-if (lambda (x) (string-match (car x) var)) variable-alist)))

(defun dap-variables-expand-alist-variable (var variable-alist)
  "Expand VAR by looking it up in VARIABLE-ALIST."
  (save-match-data
    (if-let ((value (dap-variables-find-matching var variable-alist)))
        (or (dap-variables--eval-poly-type
             value (if (= (length (match-data)) 2) nil var))
            (progn (lsp-warn "launch.json: variable ${%s} is nil here" var) ""))
      (lsp-warn "launch.json: variable ${%s} is unknown" var)
      "")))

(defun dap-variables-expand-escapes (s)
  "Expand all characters escaped with backslashes in S. Return the result.
S is not altered."
  (with-temp-buffer
    (insert s)
    (goto-char (point-min))

    (save-match-data
      (while (re-search-forward "\\\\\\(.\\)" nil t)
        (replace-match (match-string 1))))

    (buffer-string)))

(defun dap-variables-expand-in-string (s var-callback)
  "Expand all launch.json variables of the from ${variable} in S.
Return the result. This function does not modify S. To expand
each variable, VAR-CALLBACK is called, with the variable as
argument. If it returns nil, no expansion is performed."
  (let ((old-buffer (current-buffer)))
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))

      (save-match-data
        (while (re-search-forward
                ;; roughly corresponds to this rx expression
                ;; (minus unused groups):
                ;; (rx (or (group "\\$") ("${" (group (or (not (any "\\}"))
                ;;                                    (and "\\" any)))"}")))
                "\\(\\\\\\$\\)\\|${\\(\\([^}\\]\\|\\(\\\\.\\)\\)*\\)}" nil t)
          (if-let ((pre-unescaped (match-string 2))
                   (var (dap-variables-expand-escapes pre-unescaped)))
              (when-let ((replacement (with-current-buffer old-buffer
                                        (funcall var-callback var))))
                (replace-match replacement))
            (replace-match "$") ;; escaped \\$, since match-string 2 is nil
            )))

      ;; caused issues with ws-butler in combination with
      ;; ${command:pickProcess}. Even though `dap-variables--eval-poly-type'
      ;; returned a property-less string, the result of this function was a
      ;; strangely propertized string:
      ;; (dap-variables-expand-in-launch-configuration "${command:pickProcess}")
      ;; -> #("1" 0 1 (ws-butler-chg chg)).
      ;;
      ;; The different handling of narrowing in `buffer-substring-no-properties'
      ;; and `buffer-string' won't lead to any problems because this function
      ;; uses a temp-buffer and does not use narrowing.
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun dap-variables-walk-launch-configuration (conf var-callback)
  "Non-destructively expand all variables in all strings of CONF.
VAR-CALLBACK is called on each variable. Its result, if it is not
nil, is used as the replacement. Otherwise, nothing is replaced."
  ;; do not modify functions (lambdas, closures...)
  (cond ((functionp conf) conf)
        ;; dotted pair that is not a list (e.g.: ("JUNIT_CLASS_PATH" . "foo"))
        ((and (consp conf) (not (listp (cdr conf))))
         (cons (car conf) (dap-variables-walk-launch-configuration
                           (cdr conf) var-callback)))
        ((listp conf) (cl-loop for x in conf collect
                               (dap-variables-walk-launch-configuration
                                x var-callback)))
        ((vectorp conf) ;; also yield a vector
         (vconcat (cl-loop for x across conf collect
                           (dap-variables-walk-launch-configuration
                            x var-callback))))

        ((stringp conf) (dap-variables-expand-in-string conf var-callback))

        ;; base case: just yield tree. NOTE: also handles keyword arguments.
        (t conf)))

(defun dap-variables--call-pre-expand-variable (var)
  "Call the corresponding FUNCTION for VAR.
The function is looked up in
`dap-variables-pre-expand-variables'. Always returns nil."
  (when-let ((cb (dap-variables-find-matching
                  var dap-variables-pre-expand-variables)))
    (funcall cb var)
    nil))

(defun dap-variables-expand-in-launch-configuration (conf)
  "Non-destructively expand all variables in all strings of CONF.
CONF is regular dap-mode launch configuration. Return the result."
  (run-hook-with-args 'dap-variables-pre-expand-hook conf)

  (dap-variables-walk-launch-configuration
   conf #'dap-variables--call-pre-expand-variable)

  (let ((vars (nconc (-mapcat (lambda (f) (funcall f conf))
                              dap-variables-post-walk-hook)
                     dap-variables-launch-configuration-variables)))
    (prog1 (dap-variables-walk-launch-configuration
            conf (lambda (var) (dap-variables-expand-alist-variable var vars)))
      (run-hook-with-args 'dap-variables-post-expand-hook conf))))

;; Not used anywhere, but it wasn't private, so keep it as to not break
;; anything; also, I might make use of it in a future feature.
(defun dap-variables-expand-variable (var)
  "Expand VAR with `dap-variables-launch-configuration-variables'.
VAR is looked up in
`dap-variables-launch-configuration-variables' and the result is
returned, as a string. A warning is issued and the empty string
returned if VAR doesn't match any REGEX."
  (dap-variables-expand-alist-variable
   var dap-variables-launch-configuration-variables))

(provide 'dap-variables)
;;; dap-variables.el ends here
