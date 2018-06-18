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

(defun dap-java--select-main-class ()
  "Select main class from the current workspace."
  (let ((main-classes (lsp-send-execute-command "vscode.java.resolveMainClass")))
    (case (length main-classes)
      (0 (error "Unable to find main class"))
      (1 (car main-classes))
      (t (dap--completing-read "Select main class to run: "
                               main-classes
                               (lambda (it)
                                 (format "%s(%s)"
                                         (gethash "mainClass" it)
                                         (gethash "projectName" it)))
                               nil
                               t)))))

(defun dap-java--populate-default-args (conf)
  "Populate all of the fields that are not present in CONF."
  (when (not (and (plist-get conf :mainClass)
                  (plist-get conf :projectName)))
    (-if-let ((&hash "mainClass" main-class "projectName" project-name) (dap-java--select-main-class))
        (progn
          (setq conf (plist-put conf :mainClass main-class))
          (plist-put conf :projectName project-name))
      (error "Select main class")))

  (-let [(&plist :mainClass main-class :projectName project-name) conf]
    (dap--put-if-absent conf :args "")
    (dap--put-if-absent conf :cwd (lsp-java--get-root))
    (dap--put-if-absent conf :stopOnEntry :json-false)
    (dap--put-if-absent conf :host "localhost")
    (dap--put-if-absent conf :request "launch")
    (dap--put-if-absent conf :modulePaths (vector))
    (dap--put-if-absent conf
                        :classPaths
                        (second
                         (lsp-send-execute-command "vscode.java.resolveClasspath"
                                                   (list main-class project-name))))
    (dap--put-if-absent conf :name (format "%s (%s)"
                                           (plist-get conf :mainClass)
                                           (plist-get conf :projectName)))

    (plist-put conf :debugServer (lsp-send-execute-command "vscode.java.startDebugSession"))
    (plist-put conf :__sessionId (number-to-string (float-time)))
    (plist-put conf :type "java")

    conf))

(defun dap-java-debug (debug-args)
  "Start debug session with DEBUG-ARGS."
  (interactive (list (dap-java--populate-default-args  nil)))
  (dap-start-debugging debug-args))

(provide 'dap-java)
;;; dap-java.el ends here
