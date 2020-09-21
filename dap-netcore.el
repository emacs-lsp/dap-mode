;;; dap-netcore.el --- Debug Adapter Protocol mode for .NET Core -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Reed Mullanix

;; Author: Reed Mullanix <reedmullanix@gmail.com>
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
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "4.0"))
;; Version: 0.2

;;; Commentary:
;; Adapter for https://github.com/Samsung/netcoredbg .
;;; Code:

(require 'dap-mode)

(defcustom dap-netcore-install-dir user-emacs-directory
  "Install directory for netcoredbg.
The slash is expected at the end."
  :group 'dap-netcore
  :risky t
  :type 'directory
  )

(defcustom dap-netcore-download-url
  (pcase system-type
    (`gnu/linux "https://github.com/Samsung/netcoredbg/releases/download/latest/netcoredbg-linux-master.tar.gz")
    (`darwin "https://github.com/Samsung/netcoredbg/releases/download/latest/netcoredbg-osx-master.tar.gz")
    (`windows-nt "https://github.com/Samsung/netcoredbg/releases/download/latest/netcoredbg-win64-master.zip"))
  "Netcoredbg download url."
  :group 'dap-netcore
  :risky t
  :type 'string)

(defun dap-netcore--debugger-install ()
  "Download the latest version of netcoredbg and extract it to `dap-netcore-install-dir'."
  (let* ((temp-file (make-temp-file "netcoredbg" nil
                                    (if (eq system-type 'windows-nt)
                                        ".zip"
                                      ".tar.gz")))
         (install-dir-full (expand-file-name dap-netcore-install-dir))
         (unzip-script (pcase system-type
                         (`windows-nt (format "powershell -noprofile -noninteractive -nologo -ex bypass Expand-Archive -path '%s' -dest '%s'" temp-file install-dir-full))
                         ((or `gnu/linux `darwin) (format "mkdir -p %s && tar xvzf %s -C %s" dap-netcore-install-dir temp-file dap-netcore-install-dir))
                         (_ (user-error (format "Unable to extract server - file %s cannot be extracted, please extract it manually" temp-file))))))
    (url-copy-file dap-netcore-download-url temp-file t)
    (shell-command unzip-script)))

(defun dap-netcore--debugger-cmd ()
  "The location of the netcoredbg executable."
  (let ((file-ext (pcase system-type
                    (`windows-nt ".exe")
                    (_ ""))))
    (expand-file-name (concat "netcoredbg" file-ext) (concat dap-netcore-install-dir "netcoredbg"))))

(defun dap-netcore--debugger-locate-or-install ()
  "Return the location of netcoredbg."
  (let ((dbg (dap-netcore--debugger-cmd)))
    (unless (file-exists-p dbg)
      (if (yes-or-no-p "Netcoredbg is not installed. Do you want to install it?")
          (dap-netcore--debugger-install)
        (error "Cannot start debugger configuration without netcoredbg")))
    dbg))

(defun dap-netcore--populate-args (conf)
  "Populate CONF with arguments to launch or attach netcoredbg."
  (dap--put-if-absent conf :dap-server-path (list (dap-netcore--debugger-locate-or-install) "--interpreter=vscode"))
  (pcase (plist-get conf :mode)
    ("launch"
     (dap--put-if-absent conf :program (expand-file-name (read-file-name "Select an executable:"))))
    ("attach"
     (dap--put-if-absent conf :processId (string-to-number (read-string "Enter PID: " "2345"))))))


(dap-register-debug-provider
 "coreclr"
 'dap-netcore--populate-args)

(dap-register-debug-template ".Net Core Attach (Console)"
                             (list :type "coreclr"
                                   :request "attach"
                                   :mode "attach"
                                   :name "NetCoreDbg::Attach"))

(dap-register-debug-template ".Net Core Launch (Console)"
                             (list :type "coreclr"
                                   :request "launch"
                                   :mode "launch"
                                   :name "NetCoreDbg::Launch"))

(provide 'dap-netcore)
;;; dap-netcore.el ends here
