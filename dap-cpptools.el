;;; dap-cpptools.el --- Debug Adapter Protocol mode for cpptools      -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Ivan Yonchovski

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
  "The path to cpptools vscode extension."
  :group 'dap-cpptools
  :type 'string)

(defcustom dap-cpptools-extension-version
  (let ((current-ver "1.13.8")
        (installed-ver (dap-utils-vscode-get-installed-extension-version dap-cpptools-debug-path)))
    (when (and installed-ver (version< installed-ver current-ver))
      (warn "You have an old cpptools v%s. Please run `C-u 1 M-x dap-cpptools-setup' \
to install the new v%s." installed-ver current-ver))
    current-ver)
  "The version of the cpptools vscode extension."
  :group 'dap-cpptools
  :type 'string)

(defcustom dap-cpptools-download-url
  (format "https://github.com/microsoft/vscode-cpptools/releases/download/v%s/cpptools-%s.vsix"
          dap-cpptools-extension-version
          (plist-get
           (list 'windows-nt
                 (cond ((string-match "\\(?:arm\\|aarch\\).?64" system-configuration) "win-arm64")
                       ((string-match "64" system-configuration) "win64")
                       (t "win32"))
                 'darwin
                 (cond ((string-match "^aarch64.*" system-configuration) "osx-arm64")
                       (t "osx"))
                 'gnu/linux
                 (cond ((string-match "^aarch64.*" system-configuration) "linux-aarch64")
                       ((string-match "^armhf.*" system-configuration) "linux-armhf")
                       (t "linux")))
           system-type))
  "The download url."
  :group 'dap-cpptools
  :type 'string)

(defcustom dap-cpptools-debug-program
  `(,(concat dap-cpptools-debug-path "/extension/debugAdapters/bin/OpenDebugAD7"
             (if (eq system-type 'windows-nt) ".exe" "")))
  "The path to the cpptools debug adapter."
  :group 'dap-cpptools
  :type '(repeat string))

(defun dap-cpptools-setup (&optional forced)
  "Downloading ms-vscode.cpptools to path specified.
With prefix, FORCED to redownload the extension."
  (interactive "P")
  (unless (and (not forced) (file-exists-p dap-cpptools-debug-path))
    (dap-utils--get-extension dap-cpptools-download-url dap-cpptools-debug-path)
    (let* ((adapter-binary (cl-first dap-cpptools-debug-program))
           (adapters-path (f-parent (f-parent adapter-binary)))
           (extension-bins-path (f-join (f-parent adapters-path) "bin"))
           (bins
            (append (mapcar (lambda (path) (f-join extension-bins-path path))
                            '("cpptools" "cpptools-srv"))
                    (mapcar (lambda (path) (f-join adapters-path path))
                            '("bin/createdump" ;; In Linux and OSX versions
                              ;; Exists in OSX version
                              "lldb-mi/bin/lldb-mi"
                              "lldb/bin/lldb-mi"
                              "lldb/bin/debugserver"
                              "lldb/bin/lldb-argdumper"
                              "lldb/bin/lldb-launcher")))))
      (set-file-modes adapter-binary #o0700)
      (dolist (bin bins)
        (when (f-exists? bin)
          (set-file-modes bin #o700))))

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
