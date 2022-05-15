;;; dap-dlv-go.el --- Debug Adapter Protocol mode for Go      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Sergey Kostyaev

;; Author: Sergey Kostyaev <feo.me@ya.ru>
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
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "4.0") (f "0.20.0"))
;; Version: 0.1

;;; Commentary:
;; Adapter for https://github.com/go-delve/delve

;;; Code:

(require 'dap-mode)
(require 'dap-utils)
(require 'f)
(require 'lsp-mode)
(require 'dash)

(defcustom dap-dlv-go-delve-path
  (or (executable-find "dlv")
      (expand-file-name
       "dlv" (expand-file-name "bin" (or (getenv "GOPATH")
					 (f-join (getenv "HOME") "go")))))
  "The path to the delve command."
  :group 'dap-dlv-go
  :type 'string)

(defcustom dap-dlv-go-extra-args ""
  "Extra arguments passed to the delve command."
  :group 'dap-dlv-go
  :type 'string)

(defun dap-dlv-go--populate-default-args (conf)
  "Populate CONF with the default arguments."
  (setq conf (pcase (plist-get conf :mode)
               ("auto"
		(dap-dlv-go--populate-auto-args conf))
	       ("test"
		(dap-dlv-go--populate-test-args conf))
               ("debug"
		(dap--put-if-absent
		 conf :program (f-dirname (buffer-file-name))))
               ("exec"
		(dap--put-if-absent
		 conf :program
		 (f-expand (read-file-name "enter path to executable: "))))
               ("remote"
		(dap--put-if-absent conf :host (read-string "enter host: " "127.0.0.1"))
		(dap--put-if-absent conf :debugPort
				    (string-to-number (read-string "enter port: " "2345"))))
               ("local"
                (dap--put-if-absent conf :cwd (f-dirname (buffer-file-name)))
                (dap--put-if-absent
		 conf :processId (string-to-number (read-string "enter pid: " "2345"))))))

  (let ((debug-port (if (string= (plist-get conf :mode)
				 "remote")
			(plist-get conf :debugPort)
		      (dap--find-available-port))))
    (dap--put-if-absent conf :host "localhost")
    (when (not (string= "remote" (plist-get conf :mode)))
      (plist-put
       conf :program-to-start
       (format "%s dap --listen 127.0.0.1:%s %s" dap-dlv-go-delve-path debug-port dap-dlv-go-extra-args)))
    (plist-put conf :debugServer debug-port))

  (if (stringp (plist-get conf :args)) (plist-put conf :args (split-string (plist-get conf :args))) ())

  (when (string= (plist-get conf :name) "Test function")
    (-when-let (name (dap-dlv-go--extract-current--method-or-function-name t))
      (dap--put-if-absent conf :args (list (format "-test.run=^%s$" name)))))
  
  (-> conf
      (dap--put-if-absent :dlvToolPath dap-dlv-go-delve-path)
      
      (dap--put-if-absent :type "go")
      (dap--put-if-absent :name "Go Dlv Debug")))

(defun dap-dlv-go--populate-auto-args (conf)
  "Populate auto arguments according to CONF."
  (dap--put-if-absent conf :program (buffer-file-name))

  (if (string-suffix-p "_test.go" (buffer-file-name))
      (plist-put conf :mode "test")
    (plist-put conf :mode "debug")))

(defun dap-dlv-go--populate-test-args (conf)
  "Populate auto arguments according to CONF."
  (dap--put-if-absent conf :program "."))

(defun dap-dlv-go--extract-current--method-or-function-name (&optional no-signal?)
  "Extract current method or function name."
  (let ((symbols (lsp--get-document-symbols)))
    (or (->> symbols
             (-keep
              (-lambda ((&DocumentSymbol :kind :range :selection-range))
                (-let (((beg . end) (lsp--range-to-region range)))
                  (and (or (= lsp/symbol-kind-method kind)
			   (= lsp/symbol-kind-function kind))
		       (<= beg (point) end)
		       (lsp-region-text selection-range)))))
             (car))
	(unless no-signal?
	  (user-error "No method or function at point")))))

(dap-register-debug-provider "go" 'dap-dlv-go--populate-default-args)

(dap-register-debug-template "Go Dlv Launch File Configuration"
                             (list :type "go"
                                   :request "launch"
                                   :name "Launch File"
                                   :mode "auto"
                                   :program nil
				   :buildFlags nil
                                   :args nil
                                   :env nil))

(dap-register-debug-template "Go Dlv Attach Configuration"
                             (list :type "go"
                                   :request "attach"
                                   :name "Attach to running process"
                                   :mode "auto"))

(dap-register-debug-template "Go Dlv Launch Executable Configuration"
                             (list :type "go"
                                   :request "launch"
                                   :name "Launch Executable"
                                   :mode "exec"
                                   :program nil
                                   :args nil
                                   :env nil))

(dap-register-debug-template "Go Dlv Remote Debug"
                             (list :type "go"
                                   :request "attach"
                                   :name "Dlv Remote Debug"
                                   :mode "remote"))

(dap-register-debug-template "Go Dlv Test Current Function Configuration"
                             (list :type "go"
                                   :request "launch"
                                   :name "Test function"
                                   :mode "test"
                                   :program nil
                                   :args nil
                                   :env nil))
(provide 'dap-dlv-go)
;;; dap-dlv-go.el ends here
