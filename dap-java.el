;;; dap-java.el --- DAP Adapter for Java        -*- lexical-binding: t; -*-

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
(require 'dap-mode)

(defun dap--create-session (host port session-name)
  "HOST PORT SESSION-NAME ."
  (let* ((proc (open-network-stream session-name nil host port :type 'plain))
         (debug-session (make-dap--debug-session
                         :proc proc)))
    (set-process-filter proc (dap--create-filter-function debug-session))
    debug-session))

(defun dap-java-create-session ()
  "DD."
  (let* (;;(classpath (lsp-send-execute-command "vscode.java.resolveClasspath" (list "some.App" nil)))
         (debug-port (lsp-send-execute-command "vscode.java.startDebugSession" )))

    (dap--create-session "localhost" debug-port "name")))

(defun dap-java-debug ()
  "XX."
  (interactive)
  (dap-start-debugging "java" 'dap-java-create-session))
;; (setq dap-print-io t)
;; (with-current-buffer "App.java"
;;   (dap-java-debug ))

(provide 'dap-java)
;;; dap-java.el ends here
