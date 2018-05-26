;;; dap-java-mode.el --- DAP Adapter for Java        -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan

;; Author: Ivan <kyoncho@myoncho>
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

;; DAP Adapter for java

;;; Code:


(require 'lsp-mode)

(defvar tcp-proc)



(with-current-buffer "App.java"
  (let* ((classpath (lsp-send-execute-command "vscode.java.resolveClasspath" (list "some.App" nil)))
         (debug-port (lsp-send-execute-command "vscode.java.startDebugSession" ))
         )
    (setq          tcp-proc (open-network-stream "Java TCP connection" nil "localhost" debug-port :type 'plain))
    (set-process-filter tcp-proc (lambda (&args m)
                                   (debug)
                                   (message "%s"  m)))
    (process-send-string
     tcp-proc
     (dap--make-message
      (dap--initialize-message)))))

(provide 'dap-java-mode)
;;; dap-java-mode.el ends here
