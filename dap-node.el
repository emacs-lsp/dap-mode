;;; dap-node.el --- Debug Adapter Protocol mode for Node      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Kien Nguyen

;; Author: Kien Nguyen <kien.n.quang@gmail.com>
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

(defcustom dap-node-debug-path (expand-file-name "vscode/ms-vscode.node-debug2"
                                                 dap-utils-extension-path)
  "The path to node vscode extension."
  :group 'dap-node
  :type 'string)

(defcustom dap-node-debug-program `("node"
                                    ,(f-join dap-node-debug-path "extension/out/src/nodeDebug.js"))
  "The path to the node debugger."
  :group 'dap-node
  :type '(repeat string))

(dap-utils-openvsx-setup-function "dap-node" "ms-vscode" "node-debug2"
                                  dap-node-debug-path)

(defun dap-node--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (let ((conf (-> conf
                  (dap--put-if-absent :dap-server-path dap-node-debug-program)
                  (dap--put-if-absent :type "node")
                  (dap--put-if-absent :cwd default-directory)
                  (dap--put-if-absent :name "Node Debug"))))
    (if (plist-get conf :args)
        conf
      (dap--put-if-absent
       conf :program (read-file-name "Select the file to run:" nil (buffer-file-name) t)))))

(dap-register-debug-provider "node" #'dap-node--populate-start-file-args)

(dap-register-debug-template "Node Run Configuration"
                             (list :type "node"
                                   :cwd nil
                                   :request "launch"
                                   :program nil
                                   :name "Node::Run"))

(provide 'dap-node)
;;; dap-node.el ends here
