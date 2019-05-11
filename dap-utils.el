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

(defconst dap-utils--ext-unzip-script "bash -c 'mkdir -p %2$s && unzip -qq %1$s -d %2$s'"
  "Unzip script to unzip vscode extension package file.")

(defconst dap-utils--ext-pwsh-script "powershell -noprofile -noninteractive \
-nologo -ex bypass Expand-Archive -path '%s' -dest '%s'"
  "Powershell script to unzip vscode extension package file.")

(defcustom dap-utils-unzip-script (cond ((executable-find "unzip") dap-utils--ext-unzip-script)
                                        ((executable-find "powershell") dap-utils--ext-pwsh-script)
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

(defcustom dap-utils-extension-path (expand-file-name ".extension" user-emacs-directory)
  "Directory to store vscode extension."
  :group 'dap-utils
  :type 'directory)

(defun dap-utils-get-vscode-extension (publisher name &optional version path)
  "Get vscode extension named NAME with VERSION."
  (let* ((version (or version "latest"))
         (url (format dap-utils-vscode-ext-url publisher name version))
         (dest (or path
                   (f-join dap-utils-extension-path "vscode" (concat publisher "." name)))))
    (dap-utils--get-extension url dest)))

(defmacro dap-utils-vscode-setup-function (dapfile publisher name &optional path)
  "Helper to create setup function for vscode debug extension."
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
           (dap-utils-get-vscode-extension ,publisher ,name nil ,dest)
           (message "%s: Downloading done!" ,dapfile)))
       (unless (file-exists-p ,dest)
         (message "%s: %s debug extension are not set. You can download it with M-x %s-setup"
                  ,dapfile ,extension-name ,dapfile)))))

(provide 'dap-utils)
;;; dap-utils.el ends here
