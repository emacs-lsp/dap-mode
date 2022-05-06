;;; dap-codelldb.el --- Debug Adapter Protocol mode for CodeLLDB      -*- lexical-binding: t; -*-

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

;;; Commentary:
;; Adapter for https://github.com/vadimcn/vscode-lldb

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-codelldb-extension-version "1.7.0"
  "The version of the codelldb vscode extension."
  :group 'dap-codelldb
  :type 'string)

(defcustom dap-codelldb-download-url
  (format "https://github.com/vadimcn/vscode-lldb/releases/download/v%s/codelldb-x86_64-%s.vsix"
          dap-codelldb-extension-version
          (alist-get system-type
                     '((windows-nt . "windows")
                       (darwin . "darwin")
                       (gnu/linux . "linux"))))
  "The download url."
  :group 'dap-codelldb
  :type 'string)

(defcustom dap-codelldb-debug-path (expand-file-name "vscode/codelldb" dap-utils-extension-path)
  "The path to go vscode extension."
  :group 'dap-codelldb
  :type 'string)

(defcustom dap-codelldb-debug-program
  (concat dap-codelldb-debug-path
          (if (eq system-type 'windows-nt)
              "/extension/adapter/codelldb.exe"
            "/extension/adapter/codelldb"))
  "The path to the codelldb debugger."
  :group 'dap-codelldb
  :type 'string)

(defun dap-codelldb-setup (&optional forced)
  "Download and install codelldb adapter.
With prefix, FORCED to redownload the extension."
  (interactive "P")
  (unless (and (not forced) (file-exists-p dap-codelldb-debug-path))
    (dap-utils--get-extension dap-codelldb-download-url dap-codelldb-debug-path)
    (message "%s: Downloading done!" "dap-codelldb")))

(dap-register-debug-provider
 "codelldb"
 (lambda (conf)
   (let ((debug-port (dap--find-available-port)))
     (plist-put conf :program-to-start (format "%s --port %s" dap-codelldb-debug-program debug-port))
     (plist-put conf :debugServer debug-port))
   (plist-put conf :host "localhost")
   (plist-put conf :type "lldb")
   (plist-put conf :cargo "")
   conf))

(provide 'dap-codelldb)
;;; dap-codelldb.el ends here
