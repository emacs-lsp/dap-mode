;;; dap-mode.el --- Debug Adapter Protocol mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan

;; Author: Ivan <kyoncho@myoncho>
;; Keywords:

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

;; Debug Adapter Protocol

;;; Code:

(require 'lsp-mode)
(require 'json)

(defun dap--make-message (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (let* ((json-encoding-pretty-print lsp-print-io)
         (json-false :json-false)
         (body (json-encode params)))
    (format "Content-Length: %d\r\n\r\n%s" (string-bytes body) body)))


(cl-defstruct dap--debug-session
  (proc nil :read-only t)
  ;; ‘response-handlers’ is a hash table mapping integral JSON-RPC request
  ;; identifiers for pending asynchronous requests to functions handling the
  ;; respective responses.  Upon receiving a response from the language server,
  ;; ‘lsp-mode’ will call the associated response handler function with a
  ;; single argument, the deserialized response parameters.
  (response-handlers (make-hash-table :test 'eql) :read-only t))

(defun dap--initialize-message (adapter-id)
  "Create initialize message.
ADAPTER-ID the id of the adapter."
  (list :command "initialize"
        :arguments (list :clientID "vscode"
                         :clientName "Visual Studio Code"
                         :adapterID adapter-id
                         :pathFormat "path"
                         :linesStartAt1 t
                         :columnsStartAt1 t
                         :supportsVariableType t
                         :supportsVariablePaging t
                         :supportsRunInTerminalRequest t
                         :locale "en-us")
        :type "request"))

(defun dap--create-session (host port session-name)
  ""
  (make-dap--debug-session
   :proc (open-network-stream session-name nil host port :type 'plain)))

(defun dap-start-debugging (adapter-id create-connection)
  ""
  (let ((session (funcall create-connection))))
  (dap--send-message (dap--initialize-message adapter-id)))

(provide 'dap-mode)
;;; dap-mode.el ends here
