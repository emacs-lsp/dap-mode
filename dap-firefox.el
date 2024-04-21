;;; dap-firefox.el --- Debug Adapter Protocol mode for Firefox      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
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
;; Adapter for https://github.com/firefoxide/vscode-firefox

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-firefox-debug-path (expand-file-name "vscode/firefox-devtools.vscode-firefox-debug"
                                                    dap-utils-extension-path)
  "The path to firefox vscode extension."
  :group 'dap-firefox
  :type 'string)

(defcustom dap-firefox-debug-program `("node"
                                       ,(f-join dap-firefox-debug-path
                                                "extension/out/firefoxDebugAdapter.js"))
  "The path to the firefox debugger."
  :group 'dap-firefox
  :type '(repeat string))

(dap-utils-vscode-setup-function "dap-firefox" "firefox-devtools" "vscode-firefox-debug"
                                 dap-firefox-debug-path)

(defun dap-firefox--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-firefox-debug-program)
      (dap--put-if-absent :type "Firefox")
      (dap--put-if-absent :cwd default-directory)
      (dap--put-if-absent :file (expand-file-name (read-file-name "Select the file to open in the browser:" nil (buffer-file-name) t)))
      (dap--put-if-absent :name "Firefox Debug")))

(dap-register-debug-provider "firefox" 'dap-firefox--populate-start-file-args)

(dap-register-debug-template "Firefox Run Configuration"
                             (list :type "firefox"
                                   :cwd nil
                                   :request "launch"
                                   :file nil
                                   :reAttach t
                                   :program nil
                                   :name "Firefox::Run"))

(provide 'dap-firefox)
;;; dap-firefox.el ends here
