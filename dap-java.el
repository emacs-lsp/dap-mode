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
(require 'lsp-java)
(require 'dap-mode)
(require 'tree-mode)

(defun dap-java-create-session ()
  "DD."
  (let* ((debug-port (lsp-send-execute-command "vscode.java.startDebugSession")))
    (dap--create-session "localhost" debug-port "Default Debug")))

(defmacro dap--put-if-absent (config key form)
  `(plist-put ,config ,key (or (plist-get ,config ,key) ,form)))


(defun dap-java--populate-default-args (conf)
  "Populates all of the fields that are not present in the configuration."
  (-let* ((main-class (let ((main-classes (lsp-send-execute-command "vscode.java.resolveMainClass")))
                        (case (length main-classes)
                          (0 (error "Unable to find main class"))
                          (1 (car main-classes))
                          (t (dap--completing-read "Select main class to run: " main-classes
                                                   (lambda (it)
                                                     (list (format "%s (%s)"(gethash "mainClass" it)
                                                                   (gethash "projectName" it)) it))
                                                   nil
                                                   t))))))
    (dap--put-if-absent conf :args "")
    (dap--put-if-absent conf :name (format "%s (%s) - Debug Launch"
                                           (gethash "mainClass" main-class)
                                           (gethash "projectName" main-class)))
    (dap--put-if-absent conf :cwd (lsp-java--get-root))
    (dap--put-if-absent conf :stopOnEntry :json-false)
    (dap--put-if-absent conf :request "launch")
    (dap--put-if-absent conf :modulePaths (vector))
    (dap--put-if-absent conf
                        :classPaths
                        (classpath (second
                                    (lsp-send-execute-command "vscode.java.resolveClasspath"
                                                              (list (gethash "mainClass" main-class)
                                                                    (gethash "projectName" main-class))))))
    (dap--put-if-absent conf :mainClass (gethash "mainClass" main-class))

    (plist-put conf :debugServer (lsp-send-execute-command "vscode.java.startDebugSession"))
    (plist-put conf :__sessionId (number-to-string (float-time)))
    (plist-put conf :type "java")

    conf))

(defun dap-java-debug (debug-args)
  "Start debug session with DEBUG-ARGS."
  (interactive (list (dap-java--calculate-default-args nil)))
  ;; (dap-start-debugging "java" 'dap-java-create-session ( debug-args))
  (dap-start-debugging 'dap-java-create-session ( debug-args)))


(provide 'dap-java)
;;; dap-java.el ends here
