;;; dap-js.el --- Debug Adapter Protocol mode for Node      -*- lexical-binding: t; -*-

;; Copyright (C) Ivan Yonchovski

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

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-js-path (expand-file-name "js-debug" dap-utils-extension-path)
  "The path to the place at which the webfreak.debug extension.
Link: https://marketplace.visualstudio.com/items?itemName=webfreak.debug ."
  :group 'dap-js
  :type 'string)

(defcustom dap-js-debug-program `("node" ,(f-join dap-js-path "src" "dapDebugServer.js"))
  "The path to the JS debugger."
  :group 'dap-js
  :type '(repeat string))

(defun dap-js-setup (&optional forced)
  "Downloading webfreak.debug to path specified.
With prefix, FORCED to redownload the extension."
  (interactive "P")
  (unless (and (not forced) (file-exists-p dap-js-path))
    (lsp-download-install
     (lambda (&rest _) (lsp--info "Downloaded extension!"))
     (lambda (error)  (lsp--error "Failed Downloaded extension %s!" error))
     :url (lsp--find-latest-gh-release-url
           "https://api.github.com/repos/microsoft/vscode-js-debug/releases/latest"
           "js-debug-dap")
     :store-path dap-js-path
     :decompress :targz)))

(defun dap-js--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (let ((port (dap--find-available-port)))
    (-> conf
        (append
         (list :debugServer port
               :host "localhost"
               :type "pwa-node"
               :program-to-start (concat (s-join " " dap-js-debug-program)
                                         " "
                                         (number-to-string port))))
        (dap--put-if-absent :cwd default-directory)
        (dap--put-if-absent :name "Node Debug"))))

(dap-register-debug-provider "pwa-node" #'dap-js--populate-start-file-args)

(dap-register-debug-template
 "Node Run Configuration (new)"
 (list :type "pwa-node"
       :cwd nil
       :request "launch"
       :program nil
       :name "Node::Run"))

(provide 'dap-js)
;;; dap-js.el ends here
