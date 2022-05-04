;;; dap-go.el --- Debug Adapter Protocol mode for Go      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Aya Igarashi

;; Author: Aya Igarashi <ladiclexxx@gmail.com>
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
;; Adapter for https://github.com/golang/vscode-go

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-go-debug-path (expand-file-name "vscode/golang.go" dap-utils-extension-path)
  "The path to go vscode extension."
  :group 'dap-go
  :type 'string)

(defcustom dap-go-debug-program
  `("node"
    ,(let ((old (f-join dap-go-debug-path "extension/out/src/debugAdapter/goDebug.js"))
           (new (f-join dap-go-debug-path "extension/dist/debugAdapter.js")))
       (if (f-exists? old)
           (progn
             (lsp--warn "Go debug adapter is outdated; some features will not work properly (map debugging).\n\
Update `dap-go' using `C-u M-x dap-go-setup'")
             old)
         new)))
  "The path to the go debugger."
  :group 'dap-go
  :type '(repeat string))

(defcustom dap-go-delve-path (or (executable-find "dlv")
                                 (expand-file-name "dlv" (expand-file-name "bin" (getenv "GOPATH"))))
  "The path to the delve command."
  :group 'dap-go
  :type 'string)

(dap-utils-vscode-setup-function "dap-go" "golang" "go" dap-go-debug-path "0.32.0")

(defun dap-go--populate-default-args (conf)
  "Populate CONF with the default arguments."
  (setq conf (pcase (plist-get conf :mode)
               ("auto" (dap-go--populate-auto-args conf))
               ("debug" (dap--put-if-absent conf :program (f-dirname (buffer-file-name))))
               ("exec" (dap--put-if-absent conf :program (read-file-name "enter full path to executable without tilde:")))
               ("remote" (dap--put-if-absent conf :program (f-dirname (buffer-file-name)))
		(dap--put-if-absent conf :host (read-string "enter host:" "127.0.0.1"))
		(dap--put-if-absent conf :port (string-to-number (read-string "Enter port: " "2345"))))
               ("local"
                (dap--put-if-absent conf :cwd (f-dirname (buffer-file-name)))
                (dap--put-if-absent conf :processId (string-to-number (read-string "Enter pid: " "2345"))))
	       ))


  (if (stringp (plist-get conf :args)) (plist-put conf :args (split-string (plist-get conf :args))) ())
  
  (-> conf
      (dap--put-if-absent :dap-server-path dap-go-debug-program)
      (dap--put-if-absent :dlvToolPath dap-go-delve-path)
      (dap--put-if-absent :packagePathToGoModPathMap
                          (ht<-alist `((,(f-dirname (buffer-file-name))  . ,(lsp-find-session-folder (lsp-session) (buffer-file-name))))))
      (dap--put-if-absent :type "go")
      (dap--put-if-absent :name "Go Debug")))

(defun dap-go--populate-auto-args (conf)
  "Populate auto arguments."
  (dap--put-if-absent conf :program (buffer-file-name))

  (if (string-suffix-p "_test.go" (buffer-file-name))
      (plist-put conf :mode "test")
    (plist-put conf :mode "debug")))

(dap-register-debug-provider "go" 'dap-go--populate-default-args)
(dap-register-debug-template "Go Launch File Configuration"
                             (list :type "go"
                                   :request "launch"
                                   :name "Launch File"
                                   :mode "auto"
                                   :program nil
                                   :buildFlags nil
                                   :args nil
                                   :env nil
                                   :envFile nil))
(dap-register-debug-template "Go Launch Debug Package Configuration"
                             (list :type "go"
                                   :request "launch"
                                   :name "Launch Debug Package"
                                   :mode "debug"
                                   :program nil
                                   :buildFlags nil
                                   :args nil
                                   :env nil
                                   :envFile nil))
(dap-register-debug-template "Go Launch Unoptimized Debug Package Configuration"
                             (list :type "go"
                                   :request "launch"
                                   :name "Launch Unoptimized Debug Package"
                                   :mode "debug"
                                   :program nil
                                   :buildFlags "-gcflags '-N -l'"
                                   :args nil
                                   :env nil
                                   :envFile nil))
(dap-register-debug-template "Go Launch Executable Configuration"
                             (list :type "go"
                                   :request "launch"
                                   :name "Launch Executable"
                                   :mode "exec"
                                   :program nil
                                   :args nil
                                   :env nil
                                   :envFile nil))
(dap-register-debug-template "Go Attach Executable Configuration"
                             (list :type "go"
                                   :request "attach"
                                   :name "Attach to Executable"
                                   :mode "local"
                                   :program nil
                                   :args nil
                                   :env nil
                                   :envFile nil))
(dap-register-debug-template "Go Connect Remote dlv Configuration"
                             (list :type "go"
                                   :request "launch"
                                   :name "Connect to Remote dlv"
                                   :mode "remote"
                                   :program nil
                                   :args nil
                                   :env nil
                                   :envFile nil))

(provide 'dap-go)
;;; dap-go.el ends here
