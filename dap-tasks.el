;;; dap-tasks.el --- support tasks.json -*- lexical-binding: t -*-

;; Copyright (C) 2022 Ellis Kenyo

;; Author: Ellis Kenyo <me@elken.dev>
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
;; Extend dap-mode with support for tasks.json files

;;; Code:

(require 'lsp-mode)
(require 'json)

(declare-function dap-variables-find-vscode-config "dap-variables" (f root))
(defun dap-tasks-find-tasks-json ()
  "Return the path to current project's launch.json file.
Yields nil if it cannot be found or there is no project."
  (when-let ((root (lsp-workspace-root)))
    (require 'dap-variables)
    (dap-variables-find-vscode-config "tasks.json" root)))

(defun dap-tasks-get-tasks-json ()
  "Parse the project's launch.json as json data and return the result."
  (when-let ((tasks-json (dap-tasks-find-tasks-json))
             (json-object-type 'plist)
             ;; Use 'vector instead of 'list. With 'list for array type,
             ;; json-encode-list interpreted a list with one plist element as
             ;; an alist. Using 'list, it turned the following value of
             ;; pathMappings:
             ;;
             ;;     "pathMappings": [
             ;;         {
             ;;             "localRoot": "${workspaceFolder}",
             ;;             "remoteRoot": "."
             ;;         }
             ;;     ]
             ;;
             ;; into:
             ;;
             ;;     ((:localRoot "${workspaceFolder}" :remoteRoot "."))
             ;;
             ;; and then into:
             ;;
             ;;     "pathMappings": {
             ;;         "localRoot": [
             ;;             "${workspaceFolder}",
             ;;             "remoteRoot",
             ;;             "."
             ;;         ]
             ;;     }
             (json-array-type 'vector))
    (require 'dap-utils)
    (with-temp-buffer
      ;; NOTE: insert-file-contents does not move point
      (insert-file-contents tasks-json)
      (dap-utils-sanitize-json)
      ;; dap-tasks-remove-comments does move point
      (goto-char (point-min))

      (json-read))))

(defun dap-tasks--get-key (key conf)
  "Given a KEY, attempt to get a value from a debug CONF.
The order of presedence within vscode is:
- OS properties
- Global properties
- Local properties"
  (or (plist-get (plist-get conf (dap-utils-string-to-keyword (dap-utils-get-os-key))) key)
      (plist-get (dap-tasks-configuration-get-all) key)
      (plist-get conf key)))

(defun dap-tasks-configuration-get-name (conf)
  "Return the name of launch configuration CONF."
  (plist-get conf :label))

(defun dap-tasks-configuration-get-command (conf)
  "Get the command to be run for the task configuration."
  (if (string= "npm" (plist-get conf :type))
      (concat "npm run " (plist-get conf :script))
    (concat
     (dap-tasks--get-key :command conf)
     " "
     (mapconcat #'identity (dap-tasks--get-key :args conf) " "))))

(defun dap-tasks-get-configuration-by-label (label)
  "Given a LABEL, return a task or nil if no task was found in TASKS."
  (-first (lambda (task)
            (string= label (dap-tasks-configuration-get-name task))) (dap-tasks-configuration-get-all)))

(defun dap-tasks-configuration-get-depends (conf)
  "Given a debug CONF, get an ordered list of all the dependant tasks."
  (cl-labels ((loop-fn (confs tasks)
                "Loop through TASKS to find all dependants."
                (-when-let* ((deps (-mapcat (lambda (task)
                                              (-if-let* (((&plist :dependsOn) task))
                                                  (if (stringp dependsOn)
                                                      (loop-fn
                                                       (list (dap-tasks-get-configuration-by-label dependsOn))
                                                       (append (list (dap-tasks-get-configuration-by-label dependsOn)) tasks))
                                                    (loop-fn
                                                     (cl-map 'list #'dap-tasks-get-configuration-by-label (append dependsOn nil))
                                                     (append (cl-map 'list #'dap-tasks-get-configuration-by-label (append dependsOn nil)) tasks)))
                                                task))
                                            confs)))
                  (cl-remove-duplicates
                   (append deps tasks)
                   :test (lambda (lhs rhs)
                           (string= (plist-get lhs :label) (plist-get rhs :label)))))))
    (-filter #'listp (loop-fn `(,conf) `(,conf)))))

(defun dap-tasks-configuration-prepend-name (conf)
  "Prepend the name of CONF to it as a string.
Extract the name from the :name property."
  (push (dap-tasks-configuration-get-name conf) conf))

(defun dap-tasks-parse-tasks-json (json)
  "Return a list of all task configurations in JSON.
JSON must have been acquired with `dap-tasks--get-tasks-json'."
  (plist-get json :tasks))

(defun dap-tasks-find-parse-tasks-json ()
  "Return a list of all task configurations for the current project.
Usable as a dap-tasks-configuration-providers backend."
  (when-let ((tasks-json (dap-tasks-get-tasks-json)))
    (dap-tasks-parse-tasks-json tasks-json)))

(defun dap-tasks-configuration-get-all ()
  "Get all applicable tasks from `dap-tasks-configuration-providers'."
  (cl-map 'list #'dap-variables-expand-in-launch-configuration (-mapcat #'funcall dap-tasks-configuration-providers)))

(provide 'dap-tasks)
;;; dap-tasks.el ends here
