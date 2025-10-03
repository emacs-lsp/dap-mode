;;; dap-launch.el --- support launch.json -*- lexical-binding: t -*-

;; Copyright (C) 2020 Nikita Bloshchanevich

;; Author: Nikita Bloshchanevich <nikblos@outlook.com>
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
;; Extend dap-mode with support for launch.json files

;;; Code:

(require 'lsp-mode)
(require 'json)

(declare-function dap-variables-find-vscode-config "dap-variables" (f root))
(defun dap-launch-find-launch-json ()
  "Return the path to current project's launch.json file.
Yields nil if it cannot be found or there is no project."
  (when-let ((root (lsp-workspace-root)))
	(require 'dap-variables)
	(dap-variables-find-vscode-config "launch.json" root)))


(declare-function dap-utils-sanitize-json "dap-utils")
(defun dap-launch-get-launch-json ()
  "Parse the project's launch.json as json data and return the result."
  (require 'dap-utils)
  (when-let ((launch-json (dap-launch-find-launch-json))
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
    (with-temp-buffer
      ;; NOTE: insert-file-contents does not move point
      (insert-file-contents launch-json)
      (dap-utils-sanitize-json)
      ;; dap-launch-remove-comments does move point
      (goto-char (point-min))

      (json-read))))

(defun dap-launch-configuration-get-name (conf)
  "Return the name of launch configuration CONF."
  (plist-get conf :name))

(defun dap-launch-configuration-prepend-name (conf)
  "Prepend the name of CONF to it as a string.
Extract the name from the :name property."
  (push (dap-launch-configuration-get-name conf) conf))

(defun dap--launch-extract-environment (conf)
  "Transform environment config into dap-mode format.
This handles a single configuration plist."
  (if (not (plist-get conf :environment))
	  ;; No environment specified, just return the old configuration
	  conf
	;; Standard format for the "environment" key is
	;;   {"name": "foo", "value": "bar"},
	;; which results in a (:name "foo" :value "bar) plist.
	;; We need to transform this into a ("foo" . "bar") cons cell.
	(let ((environ-spec (mapcar
						 (lambda (env-plist)
						   (cons (plist-get env-plist :name)
								 (plist-get env-plist :value)))
						 (plist-get conf :environment))))
	  (plist-put conf :environment-variables environ-spec))))

(defun dap--launch-extract-environments (conflist)
  "Transform environment config into dap-mode format.
This is intended to be run on a list of configurations."
  (mapcar #'dap--launch-extract-environment conflist))

(defun dap-launch-parse-launch-json (json)
  "Return a list of all launch configurations in JSON.
JSON must have been acquired with `dap-launch--get-launch-json'."
  (mapcar #'dap-launch-configuration-prepend-name
		  (dap--launch-extract-environments
		   (or (plist-get json :configurations) (list json)))))

(defun dap-launch-find-parse-launch-json ()
  "Return a list of all launch configurations for the current project.
Usable as a dap-launch-configuration-providers backend."
  (when-let ((launch-json (dap-launch-get-launch-json)))
	(dap-launch-parse-launch-json launch-json)))

(provide 'dap-launch)
;;; dap-launch.el ends here
