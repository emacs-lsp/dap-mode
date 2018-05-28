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
;; Author: Ivan <kyoncho@myoncho>
;; URL: https://github.com/yyoncho/dap-mode
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.1

;; Debug Adapter Protocol

;;; Code:

(require 'lsp-mode)
(require 'json)

(defcustom dap-print-io t
  "If non-nil, print all messages to and from the DAP to messages."
  :group 'dap-mode
  :type 'boolean)

(defcustom dap-terminated-hook nil
  "List of functions to be called after a debug session has been terminated.

The functions will received the debug dession that
has been terminated."
  :type 'hook
  :group 'dap-mode)

(defun dap--json-encode (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (let* ((json-encoding-pretty-print dap-print-io)
         (json-false :json-false))
    (json-encode params)))

(defun dap--make-message (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (let* ((body (dap--json-encode params)))
    (format "Content-Length: %d\r\n\r\n%s" (string-bytes body) body)))

(cl-defstruct dap--debug-session
  (name)
  ;; ‘last-id’ is the last JSON-RPC identifier used.
  (last-id 0)

  (proc nil :read-only t)
  ;; ‘response-handlers’ is a hash table mapping integral JSON-RPC request
  ;; identifiers for pending asynchronous requests to functions handling the
  ;; respective responses.  Upon receiving a response from the language server,
  ;; ‘dap-mode’ will call the associated response handler function with a
  ;; single argument, the deserialized response parameters.
  (response-handlers (make-hash-table :test 'eql) :read-only t)

  ;; DAP parser.
  (parser (make-dap--parser) :read-only t)

  (output-buffer (generate-new-buffer "*out*")))

(cl-defstruct dap--parser
  (waiting-for-response nil)
  (response-result nil)

  ;; alist of headers
  (headers '())

  ;; message body
  (body nil)

  ;; If non-nil, reading body
  (reading-body nil)

  ;; length of current message body
  (body-length nil)

  ;; amount of current message body currently stored in 'body'
  (body-received 0)

  ;; Leftover data from previous chunk; to be processed
  (leftovers nil))

(defun dap--parse-header (s)
  "Parse string S as a DAP (KEY . VAL) header."
  (let ((pos (string-match "\:" s))
        key val)
    (unless pos
      (signal 'lsp-invalid-header-name (list s)))
    (setq key (substring s 0 pos)
          val (substring s (+ 2 pos)))
    (when (string-equal key "Content-Length")
      (cl-assert (cl-loop for c being the elements of val
                          when (or (> c ?9) (< c ?0)) return nil
                          finally return t)
                 nil (format "Invalid Content-Length value: %s" val)))
    (cons key val)))

(defun dap-toggle-breakpoint ()
  "TODO ."
  (interactive)

  (let ((breakpoints (or (lsp-workspace-get-metadata "Breakpoints")
                         (let (($ (make-hash-table :test "equal")))
                           (lsp-workspace-set-metadata "Breakpoints" $)
                           $))))
    )


  )

(defun dap--get-body-length (headers)
  "Get body length from HEADERS."
  (let ((content-length (cdr (assoc "Content-Length" headers))))
    (if content-length
        (string-to-number content-length)

      ;; This usually means either the server our our parser is
      ;; screwed up with a previous Content-Length
      (error "No Content-Length header"))))

(defun dap--parser-reset (p)
  "Reset `dap--parser' P."
  (setf
   (dap--parser-leftovers p) ""
   (dap--parser-body-length p) nil
   (dap--parser-body-received p) nil
   (dap--parser-headers p) '()
   (dap--parser-body p) nil
   (dap--parser-reading-body p) nil))

(defun dap--parser-read (p output)
  "Parser OUTPUT using parser P."
  (let ((messages '())
        (chunk (concat (dap--parser-leftovers p) output)))
    (while (not (string-empty-p chunk))
      (if (not (dap--parser-reading-body p))
          ;; Read headers
          (let* ((body-sep-pos (string-match-p "\r\n\r\n" chunk)))
            (if body-sep-pos
                ;; We've got all the headers, handle them all at once:
                (let* ((header-raw (substring chunk 0 body-sep-pos))
                       (content (substring chunk (+ body-sep-pos 4)))
                       (headers
                        (mapcar 'dap--parse-header
                                (split-string header-raw "\r\n")))
                       (body-length (dap--get-body-length headers)))
                  (setf
                   (dap--parser-headers p) headers
                   (dap--parser-reading-body p) t
                   (dap--parser-body-length p) body-length
                   (dap--parser-body-received p) 0
                   (dap--parser-body p) (make-string body-length ?\0)
                   (dap--parser-leftovers p) nil)
                  (setq chunk content))

              ;; Haven't found the end of the headers yet. Save everything
              ;; for when the next chunk arrives and await further input.
              (setf (dap--parser-leftovers p) chunk)
              (setq chunk "")))

        ;; Read body
        (let* ((total-body-length (dap--parser-body-length p))
               (received-body-length (dap--parser-body-received p))
               (chunk-length (string-bytes chunk))
               (left-to-receive (- total-body-length received-body-length))
               (this-body (substring chunk 0 (min left-to-receive chunk-length)))
               (leftovers (substring chunk (string-bytes this-body))))
          (store-substring (dap--parser-body p) received-body-length this-body)
          (setf (dap--parser-body-received p) (+ (dap--parser-body-received p)
                                                 (string-bytes this-body)))
          (when (>= chunk-length left-to-receive)
            ;; TODO: keep track of the Content-Type header, if
            ;; present, and use its value instead of just defaulting
            ;; to utf-8
            (push (decode-coding-string (dap--parser-body p) 'utf-8) messages)
            (dap--parser-reset p))

          (setq chunk leftovers))))
    (nreverse messages)))

(defun dap--read-json (str)
  "Read the JSON object contained in STR and return it."
  (let* ((json-array-type 'list)
         (json-object-type 'hash-table)
         (json-false nil))
    (json-read-from-string str)))

(defun dap--on-event (debug-session event)
  (let ((event-type (gethash "event" event)))
    (pcase event-type
      ("output" (with-current-buffer (dap--debug-session-output-buffer debug-session)
                  (insert (gethash "output" (gethash "body" event)))))
      ("exited" (with-current-buffer (dap--debug-session-output-buffer debug-session)
                  ;; (insert (gethash "body" (gethash "body" event)))
                  ))
      ("terminated"
       (run-hook-with-args 'dap-terminated-hook debug-session))
      (_ (message (format "No messages handler for %s" event-type))))))

(defun dap--create-filter-function (debug-session)
  "Create filter function for DEBUG-SESSION."
  (let ((parser (dap--debug-session-parser debug-session))
        (handlers (dap--debug-session-response-handlers debug-session)))
    (lambda (_ msg)
      (mapc (lambda (m)
              (let* ((parsed-msg (dap--read-json m))
                     (key (gethash "request_seq" parsed-msg nil)))
                (when dap-print-io
                  (message "Received:\n%s"
                           (dap--json-encode parsed-msg)))
                (pcase (gethash "type" parsed-msg)
                  ("event" (dap--on-event debug-session parsed-msg))
                  ("response" (if-let (callback (gethash key handlers nil))
                                  (progn
                                    (funcall callback m)
                                    (remhash key handlers))
                                (message "Unable to find handler for %s." (pp parsed-msg)))))))
            (dap--parser-read parser msg)))))

(defun dap--make-request (command &optional args)
  "Make request for COMMAND with arguments ARGS."
  (if args
      (list :command command
            :arguments args
            :type "request")
    (list :command command
          :type "request")))

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

(defun dap--json-pretty-print (msg)
  "Convert json MSG string to pretty printed json string."
  (let ((json-encoding-pretty-print t))
    (json-encode (json-read-from-string msg))))

(defun dap--send-message (message callback debug-session)
  "MESSAGE DEBUG-SESSION CALLBACK."
  (let* ((request-id (cl-incf (dap--debug-session-last-id debug-session)))
         (message (plist-put message :seq request-id)))
    (puthash request-id callback (dap--debug-session-response-handlers debug-session))
    (when dap-print-io
      (message "Sending: \n%s" (dap--json-encode message)))
    (process-send-string (dap--debug-session-proc debug-session)
                         (dap--make-message message))))

(defun dap--create-session (host port session-name)
  "HOST PORT SESSION-NAME ."
  (let* ((proc (open-network-stream session-name nil host port :type 'plain))
         (debug-session (make-dap--debug-session
                         :proc proc)))
    (set-process-filter proc (dap--create-filter-function debug-session))
    debug-session))

(defun dap-start-debugging (adapter-id create-session launch-args)
  "ADAPTER-ID CREATE-SESSION LAUNCH-ARGS."
  (let ((debug-session (funcall create-session)))
    (dap--send-message
     (dap--initialize-message adapter-id)
     (lambda (_initialize-result)
       (dap--send-message (dap--make-request "launch" launch-args)
                          (lambda (_launc-result)
                            (dap--send-message (dap--make-request "configurationDone")
                                               (lambda (configuration-done)
                                                 (message "%s" configuration-done))
                                               debug-session))
                          debug-session))
     debug-session)))

(provide 'dap-mode)
;;; dap-mode.el ends here
