;;; dap-unity.el --- Debug Adapter Protocol mode for Unity      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Owen Robertson

;; Author: Owen Robertson
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

;;; Commentary:
;; Adapter for https://marketplace.visualstudio.com/items?itemName=Unity.unity-debug

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-unity-debug-path (expand-file-name "vscode/Unity.unity-debug"
                                                  dap-utils-extension-path)
  "The path to unity-debug vscode extension."
  :group 'dap-unity
  :type 'string)

(defcustom dap-unity-debug-program (expand-file-name "extension/bin/UnityDebug.exe"
                                                     dap-unity-debug-path)
  "The path to the unity debugger."
  :group 'dap-unity
  :type 'string)

(dap-utils-vscode-setup-function "dap-unity" "Unity" "unity-debug"
                                 dap-unity-debug-path
                                 nil
                                 (lambda () ;; After adapter is downloaded, flag the debugger as executable
                                   (unless (eq system-type 'windows-nt)
                                     (shell-command
                                      (concat "chmod u+x " dap-unity-debug-program)))))

(defun dap-unity--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (setq conf (-> conf
                 (plist-put :type "unity")
                 (plist-put :dap-server-path (list dap-unity-debug-program)))))

(dap-register-debug-provider "unity" #'dap-unity--populate-start-file-args)

(dap-register-debug-template "Unity Editor"
                             (list :type "unity"
                                   :request "launch"
                                   :name "Unity Editor"))

(provide 'dap-unity)
;;; dap-unity.el ends here
