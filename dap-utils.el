;;; dap-utils.el --- DAP UTILS -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kien Nguyen

;; Author: Kien Nguyen <kien.n.quang@gmail.com>
;; Keywords: languages, utils

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

;; URL: https://github.com/yyoncho/dap-mode
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.2

;;; Commentary:
;; Dap-mode utils

;;; Code:

(require 'dap-mode)
(require 'xml)
(require 'dom)


(defconst dap-utils--ext-unzip-script "bash -c 'mkdir -p %2$s && unzip -qq %1$s -d %2$s'"
  "Unzip script to unzip vscode extension package file.")

(defconst dap-utils--ext-pwsh-script "powershell -noprofile -noninteractive \
-nologo -ex bypass Expand-Archive -path '%s' -dest '%s'"
  "Powershell script to unzip vscode extension package file.")

(defcustom dap-utils-unzip-script (cond ((executable-find "powershell") dap-utils--ext-pwsh-script)
                                        ((executable-find "unzip") dap-utils--ext-unzip-script)
                                        (t nil))
  "The script to unzip vscode extension package file."
  :group 'dap-utils
  :type 'string)

(defun dap-utils--get-extension (url dest)
  "Get extension from URL and extract to DEST."
  (let ((temp-file (make-temp-file "ext" nil ".zip")))
    (url-copy-file url temp-file 'overwrite)
    (if (file-exists-p dest) (delete-directory dest 'recursive))
    (shell-command (format dap-utils-unzip-script temp-file dest))))

(defcustom dap-utils-vscode-ext-url
  "https://marketplace.visualstudio.com/_apis/public/gallery/publishers/%s/vsextensions/%s/%s/vspackage"
  "Vscode extension template url."
  :group 'dap-utils
  :type 'string)

(defcustom dap-utils-github-extension-url
  "https://github.com/%s/%s/archive/v%s.zip"
  "Github extension template url."
  :group 'dap-utils
  :type 'string)

(defcustom dap-utils-extension-path (expand-file-name ".extension" user-emacs-directory)
  "Directory to store vscode extension."
  :group 'dap-utils
  :type 'directory)

(defun dap-utils-get-vscode-extension (publisher name &optional version path)
  "Get vscode extension from PUBLISHER named NAME.
VERSION is the version of the extenssion, otherwise the latest.
PATH is the download destination dir."
  (let* ((version (or version "latest"))
         (url (format dap-utils-vscode-ext-url publisher name version))
         (dest (or path
                   (f-join dap-utils-extension-path "vscode" (concat publisher "." name)))))
    (dap-utils--get-extension url dest)))

(defun dap-utils-get-github-extension (owner repo version &optional path)
  "Get extension from github named OWNER/REPO with VERSION.
PATH is the download destination path."
  (let* ((url (format dap-utils-github-extension-url owner repo version))
         (dest (or path
                   (f-join dap-utils-extension-path "github" (concat owner "." repo)))))
    (dap-utils--get-extension url dest)))

(defun dap-utils-vscode-get-installed-extension-version (path)
  "Check the version of the vscode extension installed in PATH.
Returns nil if the extension is not installed."
  (require 'xml)
  (require 'dom)
  (let* ((extension-manifest (f-join path "extension.vsixmanifest")))
    (when (f-exists? extension-manifest)
      (let ((pkg-identity (dom-by-tag (xml-parse-file extension-manifest) 'Identity)))
        (dom-attr pkg-identity 'Version)))))

(defmacro dap-utils-vscode-setup-function (dapfile publisher name &optional path version callback)
  "Helper to create DAPFILE setup function for vscode debug extension.
PUBLISHER is the vscode extension publisher.
NAME is the vscode extension name.
PATH is the download destination dir.
if VERSION is nil, use latest version from vscode marketplace.
If CALLBACK is non nil, call it after download the extension."
  (let* ((extension-name (concat publisher "." name))
         (dest (or path
                   (f-join dap-utils-extension-path "vscode" extension-name)))
         (help-string (format "Downloading %s to path specified.
With prefix, FORCED to redownload the extension." extension-name)))
    `(progn
       (defun ,(intern (format "%s-setup" dapfile)) (&optional forced)
         ,help-string
         (interactive "P")
         (unless (and (not forced) (file-exists-p ,dest))
           (dap-utils-get-vscode-extension ,publisher ,name ,version ,dest)
           (message "%s: Downloading done!" ,dapfile)))
       (when ,callback
         (funcall ,callback))
       (unless (file-exists-p ,dest)
         (message "%s: %s debug extension are not set. You can download it with M-x %s-setup"
                  ,dapfile ,extension-name ,dapfile)))))

(defmacro dap-utils-github-extension-setup-function (dapfile owner repo version &optional path callback)
  "Helper to create DAPFILE setup function for debug extension from github.
OWNER is the github owner.
REPO is the github repository.
VERSION is the github extension version.
PATH is the download destination dir.
CALLBACK is the fn to be called after the download."
  (let* ((extension-name (concat owner "." repo))
         (dest (or path
                   (f-join dap-utils-extension-path "github" extension-name)))
         (help-string (format "Downloading %s to path specified.
With prefix, FORCED to redownload the extension." extension-name)))
    `(progn
       (defun ,(intern (format "%s-setup" dapfile)) (&optional forced)
         ,help-string
         (interactive "P")
         (unless (and (not forced) (file-exists-p ,dest))
           (dap-utils-get-github-extension ,owner ,repo ,version ,dest)
           (rename-file (concat ,dest "/" (concat ,repo "-" ,version))
                        (concat ,dest "/extension"))
           (message "%s: Downloading done!" ,dapfile)
           (when ,callback
             (funcall ,callback))))
       (unless (file-exists-p ,dest)
         (message "%s: %s debug extension are not set. You can download it with M-x %s-setup"
                  ,dapfile ,extension-name ,dapfile)))))

(defun dap-utils-sanitize-json ()
  "Remove all C-style comments and trailing commas in the current buffer.
Comments in strings are ignored. The buffer is modified in place.
Replacement starts at point, and strings before it are ignored,
so you may want to move point to `point-min' with `goto-char'
first. This function moves `point'. Both // and /**/ comments are
supported."
  (while (re-search-forward
          (rx
           (or (group
                (or (: "//" (* nonl) eol)
                    (: "/*" (* (or (not (any ?*))
                                   (: (+ ?*) (not (any ?/))))) (+ ?*) ?/)
                    (: "," (group (* (any blank space ?\v ?\u2028 ?\u2029))
                                  (any ?\} ?\])))))
               (: "\"" (* (or (not (any ?\\ ?\")) (: ?\\ nonl))) "\"")))
          nil t)
    ;; we matched a comment
    (when (match-beginning 1)
      (replace-match (or (match-string 2) "")))))

(defun dap-utils-get-os-key ()
  "Get an applicable OS name used for keys in launch/tasks configurations."
  (pcase system-type
    ('darwin "osx")
    ('gnu/linux "linux")
    (_ "windows")))

(defun dap-utils-string-to-keyword (str)
  "Convert a STR to a keyword symbol."
  (intern-soft (format ":%s" str)))

(provide 'dap-utils)
;;; dap-utils.el ends here
