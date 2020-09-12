;;; dap-cpptools.el --- Debug Adapter Protocol mode for cpptools      -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: language, tools

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

;; Adapter for https://github.com/microsoft/vscode-cpptools

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-cpptools-debug-path (expand-file-name "vscode/cpptools" dap-utils-extension-path)
  "The path to go vscode extension."
  :group 'dap-cpptools
  :type 'string)

(defcustom dap-cpptools-download-url
  (format "https://github.com/microsoft/vscode-cpptools/releases/download/0.29.0/cpptools-%s.vsix"
          (alist-get system-type
                     '((windows-nt . "win32")
                       (darwin . "osx")
                       (gnu/linux . "linux"))))
  "The download url."
  :group 'dap-cpptools
  :type 'string)

(defcustom dap-cpptools-debug-program
  `(,(concat dap-cpptools-debug-path
             (concat "/extension/debugAdapters/OpenDebugAD7"
                     (if (eq system-type 'win32) ".exe" ""))))
  "The path to the go debugger."
  :group 'dap-cpptools
  :type '(repeat string))

(defun dap-cpptools-setup (&optional forced)
  "Downloading ms-vscode.cpptools to path specified.
With prefix, FORCED to redownload the extension."
  (interactive "P")
  (unless (and (not forced) (file-exists-p dap-cpptools-debug-path))
    (dap-utils--get-extension dap-cpptools-download-url dap-cpptools-debug-path)
    (let* ((adapter-binary (cl-first dap-cpptools-debug-program))
           (mono (f-join (f-parent adapter-binary) "mono.linux-x86_64")))
      (set-file-modes adapter-binary #o0700)
      (when (f-exists? mono)
        (set-file-modes mono #o0700)))

    (message "%s: Downloading done!" "dap-cpptools")))

(defun dap-cpptools--populate-args (conf)
  "Populate auto arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-cpptools-debug-program)
      (dap--put-if-absent :request "launch")
      (dap--put-if-absent :type "cppdbg")
      (dap--put-if-absent :environment [])))

(dap-register-debug-provider "cppdbg" #'dap-cpptools--populate-args)

(dap-register-debug-template "cpptools::Run Configuration"
                             (list :type "cppdbg"
                                   :request "launch"
                                   :name "cpptools::Run Configuration"
                                   :MIMode "gdb"
                                   :program "${workspaceFolder}/ replace with your binary"
                                   :cwd "${workspaceFolder}"))

(provide 'dap-cpptools)
;;; dap-cpptools.el ends here
