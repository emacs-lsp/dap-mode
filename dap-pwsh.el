;;; dap-pwsh.el --- Debug Adapter Protocol mode for Pwsh      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kien Nguyen

;; Author: Kien Nguyen <kien.n.quang@gmail.com>
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

;; URL: https://github.com/yyoncho/dap-mode
;; Package-Requires: ((emacs "25.1") (lsp-mode "4.0") (dap-mode "0.2"))
;; Version: 0.2

;;; Commentary:

;;; Code:

(require 'dap-mode)
(require 'lsp-pwsh)
(require 'f)
(require 'dash)

(defgroup dap-pwsh nil
  "Debugger support for PowerShell."
  :group 'dap-mode
  :package-version '(dap-mode . "0.2"))

(defcustom dap-pwsh-program
  `(,lsp-pwsh-exe "-NoProfile" "-NonInteractive" "-NoLogo"
                  ,@(if (eq system-type 'windows-nt) '("-ExecutionPolicy" "Bypass"))
                  "-OutputFormat" "Text"
                  "-File"
                  ,(f-join lsp-pwsh-dir "PowerShellEditorServices/Start-EditorServices.ps1")
                  "-HostName" "'Emacs Host'"
                  "-HostProfileId" "'Emacs.LSP'"
                  "-HostVersion" "0.1"
                  "-LogPath" ,(f-join lsp-pwsh-log-path "emacs-powershell-debug.log")
                  "-LogLevel" ,lsp-pwsh-developer-editor-services-log-level
                  "-SessionDetailsPath"
                  ,(format "%s/PSES-VSCode-%d-Debug" lsp-pwsh-log-path lsp-pwsh--sess-id)
                  ;; "-AdditionalModules" "@('PowerShellEditorServices.VSCode')"
                  "-Stdio"
                  "-DebugServiceOnly"
                  "-BundledModulesPath" ,lsp-pwsh-dir
                  "-FeatureFlags" "@()")
  "The command to run the pwsh debugger."
  :group 'dap-pwsh
  :type '(repeat string))

(defun dap-pwsh--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-pwsh-program)
      (dap--put-if-absent :type "PowerShell")
      (dap--put-if-absent :cwd (lsp-find-session-folder (lsp-session) (buffer-file-name)))
      (dap--put-if-absent :script (read-file-name "Select the file to run:" nil (buffer-file-name) t))
      (dap--put-if-absent :name "PowerShell: Debug")
      (dap--put-if-absent :args [])))

(dap-register-debug-provider "PowerShell" #'dap-pwsh--populate-start-file-args)

(dap-register-debug-template "PowerShell: Launch Script"
                             (list :type "PowerShell"
                                   :cwd nil
                                   :request "launch"
                                   :script nil
                                   :args nil
                                   :name "PowerShell: Launch Script"))

(provide 'dap-pwsh)
;;; dap-pwsh.el ends here
