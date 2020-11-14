;;; dap-chrome.el --- Debug Adapter Protocol mode for Chrome      -*- lexical-binding: t; -*-

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

;; URL: https://github.com/yyoncho/dap-mode
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "4.0"))
;; Version: 0.2

;;; Commentary:
;; Adapter for https://github.com/chromeide/vscode-chrome

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-chrome-debug-path (expand-file-name "vscode/msjsdiag.debugger-for-chrome"
                                                   dap-utils-extension-path)
  "The path to chrome vscode extension."
  :group 'dap-chrome
  :type 'string)

(defcustom dap-chrome-debug-program `("node"
                                      ,(f-join dap-chrome-debug-path "extension/out/src/chromeDebug.js"))
  "The path to the chrome debugger."
  :group 'dap-chrome
  :type '(repeat string))

(dap-utils-vscode-setup-function "dap-chrome" "msjsdiag" "debugger-for-chrome"
                                 dap-chrome-debug-path)

(defun dap-chrome--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (setq conf (-> conf
                 (plist-put :type "chrome")
                 (plist-put :dap-server-path dap-chrome-debug-program)
                 (dap--put-if-absent :cwd (expand-file-name default-directory))))
  (dap--plist-delete
   (pcase (plist-get conf :mode)
     ("url" (-> conf
                (dap--put-if-absent :url (read-string
                                          "Browse url: "
                                          "http://localhost:4200" t))
                (dap--put-if-absent :webRoot (lsp-workspace-root))))
     ("file" (dap--put-if-absent conf :file
                                 (read-file-name "Select the file to open in the browser:"
                                                 nil (buffer-file-name) t)))
     (_ conf))
   :mode))

(dap-register-debug-provider "chrome" #'dap-chrome--populate-start-file-args)

(dap-register-debug-template "Chrome Browse File"
                             (list :type "chrome"
                                   :mode "file"
                                   :cwd nil
                                   :request "launch"
                                   :file nil
                                   :reAttach t
                                   :name "Chrome Browse File"))

(dap-register-debug-template "Chrome Browse URL"
                             (list :type "chrome"
                                   :cwd nil
                                   :mode "url"
                                   :request "launch"
                                   :webRoot nil
                                   :url nil
                                   :name "Chrome Browse URL"))

(provide 'dap-chrome)
;;; dap-chrome.el ends here
