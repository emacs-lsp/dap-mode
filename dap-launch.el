;; dap-launch.el --- support launch.json -*- lexical-binding: t -*-

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

(require 'lsp-mode)
(require 'cl-lib)
(require 'json)

;;; Commentary:
;; Extend dap-mode with support for launch.json files

;;; Code:

(defun dap-launch-find-launch-json ()
  "Return the location of the launch.json file in the current project."
  (when-let ((project (lsp-workspace-root)))
    (concat project "/launch.json")))

(defun dap-launch-get-launch-json ()
  "Parse the project's launch.json as json data and return the result."
  (when-let ((launch-json (dap-launch-find-launch-json))
             (json-object-type 'plist)
             (json-array-type 'list))
    (json-read-file launch-json)))

(defun dap-launch-configuration-get-name (conf)
  "Return the name of launch configuration CONF."
  (plist-get conf :name))

(defun dap-launch-configuration-prepend-name (conf)
  "Prepend the name of CONF to it as a string.
Extract the name from the :name property."
  (push (dap-launch-configuration-get-name conf) conf))

(defun dap-launch-parse-launch-json (json)
  "Return a list of all launch configurations in JSON.
JSON must have been acquired with `dap-launch--get-launch-json'."
  (mapcar #'dap-launch-configuration-prepend-name
          (or (plist-get json :configurations) (list json))))

(defun dap-launch-find-parse-launch-json ()
  "Return a list of all launch configurations for the current project.
Usable as a dap-launch-configuration-providers backend."
  (when-let ((launch-json (dap-launch-get-launch-json)))
    (dap-launch-parse-launch-json launch-json)))

(provide 'dap-launch)
;;; dap-launch.el ends here
