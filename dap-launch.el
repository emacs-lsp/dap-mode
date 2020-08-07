;; dap-mode.el --- support launch.json -*- lexical-binding: t -*-

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

;;; Commentary:
;; Extend dap-mode with support for launch.json files

;;; Code:

(defun dap--project-find-launch-json ()
  "Return the location of the launch.json file in the current project."
  (when-let ((project (lsp-workspace-root)))
    (concat project "/launch.json")))

(defun dap--project-get-launch-json ()
  "Parse the project's launch.json as json data and return the result."
  (when-let ((launch-json (dap--project-find-launch-json))
             (json-object-type 'plist))
    (json-read-file launch-json)))

(defun dap--parse-launch-json (json)
  "Return a list of all launch configurations in JSON.
JSON must have been acquired with `dap--project-get-launch-json'."
  (or (plist-get json :configurations) (list json)))

(defun dap--project-parse-launch-json ()
  "Return a list of all launch configurations for the current project.
Usable as a dap-launch-configuration-providers backend."
  (when-let ((launch-json (dap--project-get-launch-json)))
    (dap--parse-launch-json launch-json)))

(defun dap--configuration-get-name (conf)
"Return the name of launch configuration CONF."
  (plist-get conf :name))

(provide 'dap-launch)
;;; dap-launch.el ends here
