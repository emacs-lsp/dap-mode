;;; dap-edge.el --- Debug Adapter Protocol mode for Edge      -*- lexical-binding: t; -*-

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

;; URL: https://github.com/yyoncho/dap-mode
;; Package-Requires: ((emacs "25.1") (dash "2.14.1") (lsp-mode "4.0"))
;; Version: 0.2

;;; Code:

(require 'dap-mode)
(require 'dap-utils)

(defcustom dap-edge-debug-path (expand-file-name "vscode/msjsdiag.debugger-for-edge"
                                                   dap-utils-extension-path)
  "The path to edge vscode extension."
  :group 'dap-edge
  :type 'string)

(defcustom dap-edge-debug-program `("node"
                                    ,(f-join dap-edge-debug-path "extension/out/src/chromeDebug.js"))
  "The path to the edge debugger."
  :group 'dap-edge
  :type '(repeat string))

(dap-utils-vscode-setup-function "dap-edge" "msjsdiag" "debugger-for-edge"
                                 dap-edge-debug-path)

(defun dap-edge--populate-start-file-args (conf)
  "Populate CONF with the required arguments."
  (-> conf
      (dap--put-if-absent :dap-server-path dap-edge-debug-program)
      (dap--put-if-absent :type "edge")
      (dap--put-if-absent :cwd default-directory)
      (dap--put-if-absent :file (read-file-name "Select the file to open in the browser:" nil (buffer-file-name) t))
      (dap--put-if-absent :name "Edge Debug")))

(dap-register-debug-provider "edge" #'dap-edge--populate-start-file-args)

(dap-register-debug-template "Edge Run Configuration"
                             (list :type "edge"
                                   :cwd nil
                                   :request "launch"
                                   :file nil
                                   :reAttach t
                                   :program nil
                                   :version "dev"
                                   :name "Edge::Run"))

(provide 'dap-edge)
;;; dap-edge.el ends here
