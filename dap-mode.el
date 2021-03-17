;;; dap-mode.el --- Debug Adapter Protocol mode      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

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

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: languages, debug
;; URL: https://github.com/emacs-lsp/dap-mode
;; Package-Requires: ((emacs "26.1") (dash "2.18.0") (lsp-mode "6.0") (bui "1.1.0") (f "0.20.0") (s "1.12.0") (lsp-treemacs "0.1") (posframe "0.7.0") (ht "2.3"))
;; Version: 0.6

;;; Commentary:
;; Debug Adapter Protocol client for Emacs.

;;; Code:

(require 'lsp-mode)
(require 'json)
(require 'f)
(require 'dash)
(require 'dap-overlays)
(require 'cl-lib)
(require 'ansi-color)
(require 'posframe)
(require 'ht)

(require 'dap-launch)

(defcustom dap-breakpoints-file (expand-file-name (locate-user-emacs-file ".dap-breakpoints"))
  "Where to persist breakpoints"
  :group 'dap-mode
  :type 'file)

(defcustom dap-print-io nil
  "If non-nil, print all messages to and from the DAP to messages."
  :group 'dap-mode
  :type 'boolean)

(defcustom dap-external-terminal '("xterm" "-T" "{title}"
                                   "-hold" "-e" "sh" "-c" "exec {command}")
  "Command to launch the external terminal for debugging.
When specifying that the program be run in an external terminal,
this command is used to launch it. It shall be a list of strings,
the first of which is the program to run and the rest of which
are arguments to pass to it. Special expansion is performed:

{command} will be replaced with the program to execute.
{display} will be replaced with window title.

See also `dap-default-terminal-kind'."
  :group 'dap-mode
  :type '(repeat string))

(defun dap--make-terminal-buffer (title debug-session)
  "Generate an internal terminal buffer.
The name is derived from TITLE and DEBUG-SESSION. This function
should be used in `dap-internal-terminal-*'."
  (generate-new-buffer
   (format "*%s %s*"
           (dap--debug-session-name debug-session)
           (if title (concat "- " title) "console"))))

(declare-function vterm-mode "ext:vterm" (&optional arg))
(defvar vterm-shell)
(defvar vterm-kill-buffer-on-exit)
(defun dap-internal-terminal-vterm (command title debug-session)
  (with-current-buffer (dap--make-terminal-buffer title debug-session)
    (require 'vterm)
    (let ((vterm-shell command)
          (vterm-kill-buffer-on-exit nil))
      (vterm-mode)
      ;; TODO: integrate into dap-ui
      (display-buffer (current-buffer)))))

(defun dap-internal-terminal-shell (command title debug-session)
  (let ((buf (dap--make-terminal-buffer title debug-session)))
    (async-shell-command command buf buf)))

(defun dap-internal-terminal-auto (command title debug-session)
  "Run COMMAND with an auto-detected terminal.
If `vterm' is loaded or auto-loaded, use vterm. Otherwise, use
`async-shell-command'."
  ;; NOTE: 'vterm is autoloaded. This means that (fboundp 'vterm) will yield t
  ;; even before vterm is loaded.
  (if (fboundp 'vterm)
      (dap-internal-terminal-vterm command title debug-session)
    (dap-internal-terminal-shell command title debug-session)))

(defcustom dap-internal-terminal #'dap-internal-terminal-auto
  "Terminal used with :console \"integratedTerminal\".
It is a function that shall take three arguments: the command to
run, as a string, the title from the debug adapter (may be nil)
and the debug session and it should execute COMMAND. Aside from
that, it can do anything and is itself responsible for displaying
any buffers, ....

If you are looking at implementing your own such function, see
also `dap--make-terminal-buffer'."
  :group 'dap-mode
  :type '(radio
          (const :tag "auto-detect" :value dap-internal-terminal-auto)
          (const :tag "vterm" :value dap-internal-terminal-vterm)
          (const :tag "asnyc-shell" :value dap-internal-terminal-shell)
          (function :tag "Custom function")))

(defcustom dap-output-buffer-filter '("stdout" "stderr")
  "If non-nil, a list of output types to display in the debug output buffer."
  :group 'dap-mode
  :type 'list)

(defcustom dap-label-output-buffer-category nil
  "If non-nil, content that is printed to the output buffer will be labelled
based on DAP protocol category."
  :group 'dap-mode
  :type 'boolean)

(defcustom dap-auto-show-output t
  "If non-nil, the output buffer will be shown automatically."
  :group 'dap-mode
  :type 'boolean)

(defcustom dap-output-window-min-height 10
  "The minimum height of the output window."
  :group 'dap-mode
  :type 'number)

(defcustom dap-output-window-max-height 20
  "The maximum height of the output window."
  :group 'dap-mode
  :type 'number)

(defcustom dap-inhibit-io t
  "If non-nil, the messages will be inhibited."
  :group 'dap-mode
  :type 'boolean)

(defcustom dap-terminated-hook nil
  "List of functions to be called after a debug session has been terminated.

The functions will received the debug dession that
has been terminated."
  :type 'hook
  :group 'dap-mode)

(defcustom dap-stopped-hook nil
  "List of functions to be called after a breakpoint has been hit."
  :type 'hook
  :group 'dap-mode)

(defcustom dap-session-changed-hook nil
  "List of functions to be called after sessions have changed."
  :type 'hook
  :group 'dap-mode)

(defcustom dap-loaded-sources-changed-hook nil
  "List of functions to be called after loaded sources have changed for
the session."
  :type 'hook
  :group 'dap-mode)

(defcustom dap-session-created-hook nil
  "List of functions to be called after session have been created.
It will be called with one argument - the created session."
  :type 'hook
  :group 'dap-mode)

(defcustom dap-continue-hook nil
  "List of functions to be called after application started.

The hook is called after application has been stopped/started(e.
g. after calling `dap-continue')"
  :type 'hook
  :group 'dap-mode)

(defcustom dap-executed-hook nil
  "List of functions that will be called after execution and processing request."
  :type 'hook
  :group 'dap-mode)

(defcustom dap-breakpoints-changed-hook nil
  "List of functions that will be called after breakpoints have changed.
The hook will be called with the session file and the new set of breakpoint
locations."
  :type 'hook
  :group 'dap-mode)

(defcustom dap-position-changed-hook nil
  "List of functions that will be called after cursor position has changed."
  :type 'hook
  :group 'dap-mode)

(defcustom dap-stack-frame-changed-hook nil
  "List of functions that will be called after active stack frame has changed."
  :type 'hook
  :group 'dap-mode)

(defvar dap--debug-providers (make-hash-table :test 'equal))

(defcustom dap-debug-template-configurations nil
  "Plist Template configurations for DEBUG/RUN."
  :safe #'listp
  :group 'dap-mode
  :type '(plist))

(defcustom dap-auto-configure-features '(sessions locals breakpoints expressions controls tooltip)
  "Windows to auto show on debugging when in dap-ui-auto-configure-mode."
  :group 'dap-mode
  :type '(set (const :tag "Show sessions popup window when debugging" sessions)
              (const :tag "Show locals popup window when debugging" locals)
              (const :tag "Show breakpoints popup window when debugging" breakpoints)
              (const :tag "Show expressions popup window when debugging" expressions)
              (const :tag "Show REPL popup window when debugging" repl)
              (const :tag "Enable `dap-ui-controls-mode` with controls to manage the debug session when debugging" controls)
              (const :tag "Enable `dap-tooltip-mode` that enables mouse hover support when debugging" tooltip)))

(defconst dap-features->windows
  '((sessions . (dap-ui-sessions . dap-ui--sessions-buffer))
    (locals . (dap-ui-locals . dap-ui--locals-buffer))
    (breakpoints . (dap-ui-breakpoints . dap-ui--breakpoints-buffer))
    (expressions . (dap-ui-expressions . dap-ui--expressions-buffer))
    (repl . (dap-ui-repl . dap-ui--repl-buffer))))

(defconst dap-features->modes
  '((controls . (dap-ui-controls-mode . posframe))
    (tooltip . dap-tooltip-mode)))

(defvar dap--debug-configuration nil
  "List of the previous configuration that have been executed.")

(defvar dap-connect-retry-count 1000
  "Retry count for dap connect.")

(defvar dap-connect-retry-interval 0.02
  "Retry interval for dap connect.")

(eval-and-compile
  (defun dash-expand:&dap-session (key source)
    `(,(intern-soft (format "dap--debug-session-%s" (eval key))) ,source)))

(cl-defstruct dap--debug-session
  (name nil)
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
  (output-buffer nil)
  (thread-id nil)
  ;; reference to the workspace that holds the information about the lsp workspace.
  (workspace nil)
  (threads nil)
  (thread-states (make-hash-table :test 'eql) :read-only t)
  (active-frame-id nil)
  (active-frame nil)
  (cursor-marker nil)
  ;; one of 'pending 'running 'terminated 'failed
  (state 'pending)
  ;; hash table containing mapping file -> active breakpoints.
  (breakpoints (make-hash-table :test 'equal) :read-only t)
  ;; hash table tread-id -> stack frame
  (thread-stack-frames (make-hash-table :test 'eql) :read-only t)
  ;; the arguments that were used to start the debug session.
  (launch-args nil)
  ;; Currently-available server capabilities
  (current-capabilities (make-hash-table :test 'equal))
  (error-message nil)
  (loaded-sources nil)
  (program-proc)
  ;; Optional metadata to set and get it.
  (metadata (make-hash-table :test 'eql))
  ;; when t the output already has been displayed for this buffer.
  (output-displayed))

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

(defun dap--get-sessions ()
  "Get sessions for WORKSPACE."
  (lsp-workspace-get-metadata "debug-sessions"))

(defun dap--cur-session ()
  "Get currently active `dap--debug-session'."
  (lsp-workspace-get-metadata "default-session"))

(defun dap--resp-handler (&optional success-callback error-callback)
  "Generate a response handler, for use in `dap--send-message'.

If the request is successful, call SUCCESS-CALLBACK with the
entire resulting messsage.

The handler will call ERROR-CALLBACK with the message or `error'
on failure."
  (-lambda ((result &as &hash "success" "message"))
    (if success
        (when success-callback (funcall success-callback result))
      (if error-callback (funcall error-callback message) (error message)))))

(defun dap--session-init-resp-handler (debug-session &optional success-callback)
  "Returned handler will mark the DEBUG-SESSION as failed if call return error.

SUCCESS-CALLBACK will be called if it is provided and if the call
has succeeded."
  (-lambda ((result &as &hash "success" "message"))
    (if success
        (when success-callback (funcall success-callback result))
      (warn "Initialize request failed: %s" message)
      (delete-process (dap--debug-session-proc debug-session))

      (setf (dap--debug-session-state debug-session) 'failed
            (dap--debug-session-error-message debug-session) message)

      (dap--refresh-breakpoints)
      (run-hook-with-args 'dap-terminated-hook debug-session)
      (run-hooks 'dap-session-changed-hook))))

(defun dap--cur-session-or-die ()
  "Get currently selection `dap--debug-session' or die."
  (or (dap--cur-session) (error "No active current session")))

(defun dap--session-running (debug-session)
  "Check whether DEBUG-SESSION still running."
  (and debug-session
       (not (memq (dap--debug-session-state debug-session) '(terminated failed)))))

(defun dap--cur-active-session-or-die ()
  "Get currently non-terminated  `dap--debug-session' or die."
  (-let ((debug-session (dap--cur-session-or-die)))
    (if (dap--session-running debug-session)
        debug-session
      (error "Session %s is terminated" (dap--debug-session-name debug-session)))))

(defun dap-breakpoint-get-point (breakpoint)
  "Get position of BREAKPOINT."
  (or (-some-> breakpoint (plist-get :marker) marker-position)
      (plist-get breakpoint :point)))

(defun dap--set-cur-session (debug-session)
  "Change the active debug session to DEBUG-SESSION."
  (lsp-workspace-set-metadata "default-session" debug-session))

(defmacro dap--put-if-absent (config key form)
  "Update KEY to FORM if KEY does not exist in plist CONFIG."
  `(plist-put ,config ,key (or (plist-get ,config ,key) ,form)))

(defun dap--completing-read (prompt collection transform-fn &optional predicate
                                    require-match initial-input
                                    hist def inherit-input-method)
  "Wrap `completing-read' to provide tranformation function.

TRANSFORM-FN will be used to transform each of the items before displaying.

PROMPT COLLECTION PREDICATE REQUIRE-MATCH INITIAL-INPUT HIST DEF
INHERIT-INPUT-METHOD will be proxied to `completing-read' without changes."
  (let* ((result (--map (cons (funcall transform-fn it) it) collection))
         (completion (completing-read prompt (-map 'cl-first result)
                                      predicate require-match initial-input hist
                                      def inherit-input-method)))
    (cdr (assoc completion result))))

(defun dap--plist-delete (plist property)
  "Delete PROPERTY from PLIST.
This is in contrast to merely setting it to 0."
  (let (p)
    (while plist
      (if (not (eq property (cl-first plist)))
          (setq p (plist-put p (cl-first plist) (nth 1 plist))))
      (setq plist (cddr plist)))
    p))

(defun dap--json-encode (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (let* ((json-encoding-pretty-print dap-print-io)
         (json-false :json-false))
    (json-encode params)))

(defun dap--make-message (params)
  "Create a LSP message from PARAMS, after encoding it to a JSON string."
  (let* ((body (dap--json-encode params)))
    (format "Content-Length: %d\r\n\r\n%s" (string-bytes body) body)))

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

(defun dap--get-breakpoints ()
  "Get breakpoints in WORKSPACE."
  (or (lsp-workspace-get-metadata "Breakpoints")
      (let ((breakpoints (make-hash-table :test 'equal)))
        (lsp-workspace-set-metadata "Breakpoints" breakpoints)
        breakpoints)))

(defun dap--persist (file to-persist)
  "Serialize TO-PERSIST to FILE."
  (with-demoted-errors
      "Failed to persist file: %S"
    (make-directory (file-name-directory file) t)
    (with-temp-file file
      (erase-buffer)
      (insert (prin1-to-string to-persist)))))

(defun dap--set-sessions (debug-sessions)
  "Update list of debug sessions for WORKSPACE to DEBUG-SESSIONS."
  (lsp-workspace-set-metadata "debug-sessions" debug-sessions)
  (run-hook-with-args 'dap-session-changed-hook))

(defun dap--persist-breakpoints (breakpoints)
  "Persist BREAKPOINTS."
  ;; filter markers before persisting the breakpoints (markers are not
  ;; writeable) and update the point based on the marker.
  (let ((filtered-breakpoints (make-hash-table :test 'equal)))
    (maphash (lambda (k v)
               (puthash k (-map (-lambda ((bkp &as &plist :marker :point))
                                  (-> bkp
                                      (dap--plist-delete :point)
                                      (dap--plist-delete :marker)
                                      (plist-put :point (if marker
                                                            (marker-position marker)
                                                          point))))
                                v)
                        filtered-breakpoints))
             breakpoints)
    (dap--persist dap-breakpoints-file filtered-breakpoints)))

(defun dap--breakpoints-changed (updated-file-breakpoints &optional file-name)
  "Common logic breakpoints related methods UPDATED-FILE-BREAKPOINTS.
FILE-NAME is the filename in which the breakpoints have been udpated."
  (let* ((file-name (or file-name buffer-file-name (error "No file name")))
         (breakpoints (dap--get-breakpoints)))
    ;; update the list
    (if updated-file-breakpoints
        (puthash file-name updated-file-breakpoints breakpoints)
      (remhash file-name breakpoints))

    ;; do not update the breakpoints represenations if there is active session.
    (when (not (and (dap--cur-session) (dap--session-running (dap--cur-session))))
      (--when-let (find-buffer-visiting file-name)
        (with-current-buffer it
          (run-hooks 'dap-breakpoints-changed-hook))))

    ;; Update all of the active sessions with the list of breakpoints.
    (let ((set-breakpoints-req (dap--set-breakpoints-request
                                file-name
                                updated-file-breakpoints)))
      (-as-> (dap--get-sessions) $
             (-filter 'dap--session-running $)
             (--each $
               (dap--send-message set-breakpoints-req
                                  (dap--resp-handler
                                   (lambda (resp)
                                     (dap--update-breakpoints it
                                                              resp
                                                              file-name)))
                                  it))))
    (dap--persist-breakpoints breakpoints)))

(defun dap-breakpoint-toggle ()
  "Toggle breakpoint on the current line."
  (interactive)
  (let ((file-breakpoints (gethash buffer-file-name (dap--get-breakpoints))))
    (dap--breakpoints-changed
     (if-let (existing-breakpoint (dap--get-breakpoint-at-point file-breakpoints))
         ;; delete if already exists
         (progn
           (-some-> existing-breakpoint
             (plist-get :marker)
             (set-marker nil))
           (cl-remove existing-breakpoint file-breakpoints))
       ;; add if does not exist
       (push (list :marker (point-marker)
                   :point (point))
             file-breakpoints)))))

(defun dap--get-breakpoint-at-point (&optional file-breakpoints)
  "Get breakpoint on the current point.
FILE-BREAKPOINTS is the list of breakpoints in the current file."
  (let ((current-line (line-number-at-pos (point))))
    (-first
     (-lambda (bp)
       (= current-line (line-number-at-pos (dap-breakpoint-get-point bp))))
     (or file-breakpoints (gethash buffer-file-name (dap--get-breakpoints))))))

(defun dap-breakpoint-delete (breakpoint file-name)
  "Delete breakpoint on the current line."
  (interactive (list (-some->> (dap--get-breakpoints)
                       (gethash buffer-file-name)
                       dap--get-breakpoint-at-point)
                     buffer-file-name))
  (when breakpoint
    (-some-> breakpoint (plist-get :marker) (set-marker nil))
    (dap--breakpoints-changed (-remove
                               (-lambda (bp)
                                 (eq (dap-breakpoint-get-point bp)
                                     (dap-breakpoint-get-point breakpoint)))
                               (gethash file-name (dap--get-breakpoints)))
                              file-name)))

(defun dap--breakpoint-update (property message file-name existing-breakpoint)
  "Common code for updating breakpoint.
MESSAGE to be displayed to the user.
PROPERTY is the breakpoint property that will be udpated."
  (let ((file-breakpoints (gethash file-name (dap--get-breakpoints))))
    (if existing-breakpoint
        (let ((value (read-string message
                                  (plist-get existing-breakpoint property))))
          (if (s-blank? value)
              (setq file-breakpoints (cons (-> existing-breakpoint
                                               (dap--plist-delete :hit-condition)
                                               (dap--plist-delete :condition)
                                               (dap--plist-delete :log-message))
                                           (delete existing-breakpoint file-breakpoints)))
            (plist-put existing-breakpoint property value))
          (dap--breakpoints-changed file-breakpoints file-name))
      (error "No breakpoint found."))))

(defun dap-breakpoint-condition (file-name breakpoint)
  "Set breakpoint condition for the breakpoint at point."
  (interactive (list buffer-file-name (dap--get-breakpoint-at-point)))
  (dap--breakpoint-update :condition "Enter breakpoint condition: " file-name breakpoint))

(defun dap-breakpoint-hit-condition (file-name breakpoint)
  "Set breakpoint hit condition for the breakpoint at point."
  (interactive (list buffer-file-name (dap--get-breakpoint-at-point)))
  (dap--breakpoint-update :hit-condition "Enter hit condition: " file-name breakpoint))

(defun dap-breakpoint-log-message (file-name breakpoint)
  "Set breakpoint log message for the breakpoint at point.

If log message for the breakpoint is specified it won't stop
thread exection but the server will log message."
  (interactive (list buffer-file-name (dap--get-breakpoint-at-point)))
  (dap--breakpoint-update :log-message "Enter log message: " file-name breakpoint))

(defun dap-breakpoint-add ()
  "Add breakpoint on the current line."
  (interactive)
  (let ((file-breakpoints (gethash buffer-file-name (dap--get-breakpoints))))
    (unless (dap--get-breakpoint-at-point file-breakpoints)
      (dap--breakpoints-changed (push (list :marker (point-marker)
                                            :point (point))
                                      file-breakpoints)))))

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
  (let* ((messages '())
         (output (encode-coding-string output 'utf-8 'nocopy))
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

(defun dap--resume-application (debug-session)
  "Resume DEBUG-SESSION."
  (-let [thread-id (dap--debug-session-thread-id debug-session)]
    (puthash thread-id "running" (dap--debug-session-thread-states debug-session))
    (remhash thread-id (dap--debug-session-thread-stack-frames debug-session)))
  (setf (dap--debug-session-active-frame debug-session) nil
        (dap--debug-session-thread-id debug-session) nil)
  (run-hook-with-args 'dap-continue-hook debug-session))

(defun dap-continue (debug-session thread-id)
  "Call continue for the currently active session and thread."
  (interactive (list (dap--cur-active-session-or-die)
                     (dap--debug-session-thread-id (dap--cur-active-session-or-die))))
  (if thread-id
      (progn
        (dap--send-message (dap--make-request "continue"
                                              (list :threadId thread-id))
                           (dap--resp-handler)
                           debug-session)
        (dap--resume-application debug-session))
    (lsp--error "Currently active thread is not stopped. Use `dap-switch-thread' or select stopped thread from sessions view.")))

(defun dap-disconnect (session)
  "Disconnect from the currently active session."
  (interactive (list (dap--cur-active-session-or-die)))
  (dap--send-message (dap--make-request "disconnect"
                                        (list :restart :json-false))
                     (lambda (_result)
                       (when-let (proc (dap--debug-session-program-proc session))
                         (lsp--info "Killing process %s" proc)
                         (kill-process proc)))
                     session)
  (dap--resume-application session))

(defun dap--step (cmd debug-session)
  "Send a request for CMD, a step command.
DEBUG-SESSION is the debug session in which the stepping is to be
executed."
  (if-let (thread-id (dap--debug-session-thread-id debug-session))
      (progn
        (dap--send-message (dap--make-request
                            cmd
                            (list :threadId thread-id))
                           (dap--resp-handler)
                           debug-session)
        (dap--resume-application debug-session))
    (lsp--error "Currently active thread is not stopped. Use `dap-switch-thread' or select stopped thread from sessions view.")))

(defun dap-next (debug-session)
  "Step over statements."
  (interactive (list (dap--cur-session-or-die)))
  (dap--step "next" debug-session))

(defun dap-step-in (debug-session)
  "Like `dap-next', but step into function calls."
  (interactive (list (dap--cur-session-or-die)))
  (dap--step "stepIn" debug-session))

(defun dap-step-out (debug-session)
  "Debug step out."
  (interactive (list (dap--cur-session-or-die)))
  (dap--step "stepOut" debug-session))

(defun dap-restart-frame (debug-session frame-id)
  "Restarts current frame."
  (interactive (let ((debug-session (dap--cur-active-session-or-die)))
                 (list debug-session
                       (-some->> debug-session dap--debug-session-active-frame (gethash "id")))))
  (dap--send-message (dap--make-request "restartFrame"
                                        (list :frameId frame-id))
                     (dap--resp-handler)
                     debug-session)
  (dap--resume-application debug-session))

(defcustom dap-debug-restart-keep-session t
  "Set if `dap-debug-restart' should use a new session.
When running `dap-debug-restart' with this variable set, the old
session will still be visible and accessible after restarting. If
not, the old session will be deleted, `dap-debug-restart'
behaving like an in-place restart instead. Note that you can
override this variable by calling `dap-debug-restart' with a
prefix argument (the effect will be reversed)."
  :group 'dap-mode
  :type 'boolean)

(defun dap-debug-restart (&optional toggle-keep-session)
  "Restarts current frame.
If TOGGLE-KEEP-SESSION is set (in interactive mode this is the
prefix argument), the effect of `dap-debug-restart-keep-session'
will be reversed."
  (interactive "P")
  (if-let ((debug-session (dap--cur-session)))
      (progn
        (when (dap--session-running debug-session)
          (message "Disconnecting from %s" (dap--debug-session-name debug-session))
          (if (eq toggle-keep-session dap-debug-restart-keep-session)
              (dap-delete-session debug-session)
            (dap-disconnect debug-session)))
        (dap-debug (dap--debug-session-launch-args debug-session)))
    (user-error "There is session to restart")))

(defun dap--get-path-for-frame (stack-frame)
  "Get file path for a STACK-FRAME."
  (-when-let* ((source (gethash "source" stack-frame))
               (path (gethash "path" source)))
    (if (-> path url-unhex-string url-generic-parse-url url-type)
        (lsp--uri-to-path path)
      path)))

(defun dap--go-to-stack-frame (debug-session stack-frame)
  "Make STACK-FRAME the active STACK-FRAME of DEBUG-SESSION."
  (with-lsp-workspace (dap--debug-session-workspace debug-session)
    (when stack-frame
      (-let* (((&hash "line" line "column" column "name" name) stack-frame)
              (path (dap--get-path-for-frame stack-frame)))
        (setf (dap--debug-session-active-frame debug-session) stack-frame)
        ;; If we have a source file with path attached, open it and
        ;; position the point in the line/column referenced in the
        ;; stack trace.
        (if (and path (file-exists-p path))
            (progn
              (select-window (get-mru-window (selected-frame) nil))
              (find-file path)
              (goto-char (point-min))
              (forward-line (1- line))
              (forward-char column))
          (message "No source code for %s. Cursor at %s:%s." name line column))))
    (run-hook-with-args 'dap-stack-frame-changed-hook debug-session)))

(defun dap--select-thread-id (debug-session thread-id &optional force)
  "Make the thread with id=THREAD-ID the active thread for DEBUG-SESSION."
  ;; make the thread the active session only if there is no active debug
  ;; session.
  (when (or force (not (dap--debug-session-thread-id debug-session)))
    (setf (dap--debug-session-thread-id debug-session) thread-id)
    (run-hook-with-args 'dap-stopped-hook debug-session))

  (dap--send-message
   (dap--make-request "stackTrace" (list :threadId thread-id))
   (dap--resp-handler
    (-lambda ((&hash "body" (&hash "stackFrames" stack-frames)))
      (puthash thread-id
               stack-frames
               (dap--debug-session-thread-stack-frames debug-session))
      ;; select stackframe only when session matches the active session and when
      ;; thread-id is the same as the active one
      (when (and (eq debug-session (dap--cur-session))
                 (eq thread-id (dap--debug-session-thread-id (dap--cur-session))))
        (dap--go-to-stack-frame debug-session (cl-first stack-frames)))))
   debug-session))

(defun dap--buffer-list ()
  "Get all file backed buffers."
  (-filter 'buffer-file-name (buffer-list)))

(defun dap--refresh-breakpoints ()
  "Refresh breakpoints for DEBUG-SESSION."
  (--each (dap--buffer-list)
    (when (buffer-live-p it)
      (with-current-buffer it
        (dap--set-breakpoints-in-file
         buffer-file-name
         (gethash buffer-file-name (dap--get-breakpoints)))))))

(defun dap--mark-session-as-terminated (debug-session)
  "Mark DEBUG-SESSION as terminated."
  (setf (dap--debug-session-state debug-session) 'terminated
        (dap--debug-session-active-frame debug-session) nil)

  (with-demoted-errors "Process cleanup failed with %s"
    (delete-process (dap--debug-session-proc debug-session)))
  (clrhash (dap--debug-session-breakpoints debug-session))

  (run-hook-with-args 'dap-stack-frame-changed-hook debug-session)
  (run-hook-with-args 'dap-terminated-hook debug-session)
  (dap--refresh-breakpoints)

  (when-let ((cleanup-fn (plist-get
                          (dap--debug-session-launch-args debug-session)
                          :cleanup-function)))
    (funcall cleanup-fn debug-session)))

(defun dap--output-buffer-format-with-category (category output)
  "Formats a string suitable for printing to the output buffer using CATEGORY and OUTPUT."
  (let ((message (format "%s: %s" category output)))
    (if (string= (substring message -1) "\n")
        message
      (concat message "\n"))))

(defun dap--output-buffer-format (output-body)
  "Formats a string suitable for printing to the output buffer using an OUTPUT-BODY."
  (if dap-label-output-buffer-category
      (dap--output-buffer-format-with-category (gethash "category" output-body)
                                               (gethash "output" output-body))
    (gethash "output" output-body)))

(defun dap--insert-at-point-max (str)
  "Inserts STR at point-max of the buffer."
  (goto-char (point-max))
  (insert (ansi-color-apply str)))

(defun dap--print-to-output-buffer (debug-session str)
  "Insert content from STR into the output buffer associated with DEBUG-SESSION."
  (with-current-buffer (get-buffer-create (dap--debug-session-output-buffer debug-session))
    (font-lock-mode t)
    (setq-local buffer-read-only nil)
    (if (and (eq (current-buffer) (window-buffer (selected-window)))
             (not (= (point) (point-max))))
        (save-excursion
          (dap--insert-at-point-max str))
      (dap--insert-at-point-max str))
    (setq-local buffer-read-only t))
  (when (and dap-auto-show-output
             (not (dap--debug-session-output-displayed debug-session)))
    (setf (dap--debug-session-output-displayed debug-session) t)
    (save-excursion (dap-go-to-output-buffer t))))

(cl-defgeneric dap-handle-event (event-type session params)
  "Extension point for handling custom events.
EVENT-TYPE is the event to handle.
SESSION is the session that has triggered the event.
PARAMS are the event params.")

(cl-defmethod dap-handle-event (event-type _session _params)
  (message "No message handler for %s" event-type))

(defun dap--on-event (debug-session event)
  "Dispatch EVENT for DEBUG-SESSION."
  (-let [(&hash "body" "event" event-type) event]
    (pcase event-type
      ("output" (-when-let* ((formatted-output (dap--output-buffer-format body))
                             (formatted-output (if-let ((output-filter-fn (-> debug-session
                                                                              (dap--debug-session-launch-args)
                                                                              (plist-get :output-filter-function))))
                                                   (funcall output-filter-fn formatted-output)
                                                 formatted-output)))
                  (when (or (not dap-output-buffer-filter) (member (gethash "category" body)
                                                                   dap-output-buffer-filter))
                    (dap--print-to-output-buffer debug-session formatted-output))))
      ("breakpoint" (-when-let* (((breakpoint &as &hash "id") (when body
                                                                (gethash "breakpoint" body)))
                                 (file-name (->> debug-session
                                                 (dap--debug-session-breakpoints)
                                                 (ht-find
                                                  (lambda (_ breakpoints)
                                                    (-first (-lambda ((bkp &as &hash "id" bkp-id))
                                                              (when (eq bkp-id id)
                                                                (ht-clear bkp)
                                                                (ht-aeach (ht-set bkp key value) breakpoint)
                                                                t))
                                                            breakpoints)))
                                                 (cl-first))))
                      (when (eq debug-session (dap--cur-session))
                        (with-current-buffer (find-file file-name)
                          (run-hooks 'dap-breakpoints-changed-hook)))))
      ("thread" (-let [(&hash "threadId" id "reason") body]
                  (puthash id reason (dap--debug-session-thread-states debug-session))
                  (run-hooks 'dap-session-changed-hook)
                  (dap--send-message
                   (dap--make-request "threads")
                   (-lambda ((&hash "body"))
                     (setf (dap--debug-session-threads debug-session)
                           (when body (gethash "threads" body)))
                     (run-hooks 'dap-session-changed-hook))
                   debug-session)))
      ("exited" (dap--mark-session-as-terminated debug-session))
      ("continued"
       (-let [(&hash "threadId" thread-id) body]
         (remhash thread-id (dap--debug-session-thread-states debug-session))
         (if (and (equal thread-id (dap--debug-session-thread-id debug-session))
                  (equal debug-session (dap--cur-session)))
             (dap--resume-application debug-session)
           (run-hooks 'dap-session-changed-hook))))
      ("stopped"
       (-let [(&hash "threadId" thread-id "type" "reason") body]
         (puthash thread-id (or type reason)
                  (dap--debug-session-thread-states debug-session))
         (dap--select-thread-id debug-session thread-id)
         (when (string= "exception" reason)
           (dap--send-message
            (dap--make-request "exceptionInfo" (list :threadId thread-id))
            (-lambda ((&hash "body" (&hash? "description" "exceptionId" exception-id)))
              (lsp--error "Exception has occurred: %s\n%s"
                          exception-id description))
            debug-session))
         (run-hooks 'dap-session-changed-hook)))
      ("terminated"
       (dap--mark-session-as-terminated debug-session))
      ("usernotification"
       (-let [(&hash "notificationType" notification-type "message") body]
         (warn  (format "[%s] %s" notification-type message))))
      ("initialized"
       (dap--configure-breakpoints
        debug-session
        (dap--get-breakpoints)
        (apply-partially #'dap--send-configuration-done debug-session)))
      ("loadedSource"
       (-let [(&hash "body" (&hash "source")) event]
         (cl-pushnew source (dap--debug-session-loaded-sources debug-session))
         (run-hook-with-args 'dap-loaded-sources-changed-hook debug-session)))
      ("capabilities"
       (-let [(&hash "body" (&hash "capabilities")) event]
         (ht-update! (dap--debug-session-current-capabilities debug-session) capabilities)))
      (_ (dap-handle-event (intern event-type) debug-session body)))))

(defcustom dap-default-terminal-kind "integrated"
  "Default terminal type used for :console."
  :type '(radio
          (const :tag "Terminal within Emacs" :value "integrated")
          (const :tag "External terminal program (`dap-external-terminal')"
                 :value "external"))
  :group 'dap-mode)

(defun dap--start-process (debug-session parsed-msg)
  (-let* (((&hash "arguments" (&hash? "args" "cwd" "title" "kind") "seq")
           parsed-msg)
          (default-directory cwd)
          (command-to-run (string-join args " "))
          (kind (or kind dap-default-terminal-kind)))
    (or
     (when (string= kind "external")
       (let* ((name (or title (concat (dap--debug-session-name debug-session)
                                      "- terminal")))
              (terminal-argv
               (cl-loop for part in dap-external-terminal collect
                        (->> part (s-replace "{display}" name)
                             (s-replace "{command}" command-to-run)))))
         (when
             (condition-case-unless-debug err
                 (progn (apply #'start-process name name terminal-argv) t)
               (error (lsp--warn
                       "dap-debug: failed to start external
terminal: %S (launch command was: \"%s\"). Set
`dap-external-terminal' to the correct value or install the
terminal configured (probably xterm)."
                       (error-message-string err)
                       (mapconcat #'shell-quote-argument terminal-argv " "))
                      ;; we did *not* succeed; use the integrated terminal
                      ;; instead
                      nil))
           ;; NOTE: we cannot know the process id of the started
           ;; application.
           (dap--send-message (dap--make-success-response
                               seq "runInTerminal")
                              ;; NOTE: assuming that the terminal starts the
                              ;; application without another subshell
                              (dap--resp-handler) debug-session)
           ;; success; don't use the integrated terminal
           t)))
     ;; integrated terminal *or* the external terminal could not be executed
     ;; (file error).
     (when (or (string= kind "integrated") (string= kind "external"))
       (funcall dap-internal-terminal command-to-run title debug-session)
       ;; NOTE: we don't know the PID of the shell that ran the process and we
       ;; don't know the PID of the started process.
       (dap--send-message (dap--make-success-response seq "runInTerminal")
                          (dap--resp-handler) debug-session)
       ;; success
       t)
     (dap--send-message (dap--make-error-response
                         seq "runInTerminal" nil
                         (format "unknown terminal kind %s" kind))
                        (dap--resp-handler) debug-session))))

(defun dap--create-filter-function (debug-session)
  "Create filter function for DEBUG-SESSION."
  (let ((parser (dap--debug-session-parser debug-session))
        (handlers (dap--debug-session-response-handlers debug-session)))
    (lambda (_ msg)
      (mapc (lambda (m)
              (let* ((parsed-msg (dap--read-json m))
                     (key (gethash "request_seq" parsed-msg nil)))
                (when dap-print-io
                  (let ((inhibit-message dap-inhibit-io))
                    (message "Received:\n%s" (dap--json-encode parsed-msg))))
                (pcase (gethash "type" parsed-msg)
                  ("event" (dap--on-event debug-session parsed-msg))
                  ("response" (if-let (callback (gethash key handlers nil))
                                  (progn
                                    (funcall callback parsed-msg)
                                    (remhash key handlers)
                                    (run-hook-with-args 'dap-executed-hook
                                                        debug-session
                                                        (gethash "command" parsed-msg)))
                                (message "Unable to find handler for %s." (pp parsed-msg))))
                  ("request" (dap--start-process debug-session parsed-msg)))))
            (dap--parser-read parser msg)))))

(defun dap--create-output-buffer (session-name)
  "Creates an output buffer with with name SESSION-NAME."
  (with-current-buffer (get-buffer-create (concat "*" session-name " out*"))
    (special-mode)
    (set (make-local-variable 'window-point-insertion-type) t)
    (current-buffer)))

(defun dap--make-request (command &optional args)
  "Make request for COMMAND with arguments ARGS."
  (if args
      (list :command command
            :arguments args
            :type "request")
    (list :command command
          :type "request")))

(defun dap--make-response (id command success &optional body message)
  (nconc (list :type "response" :request_seq id :success success
               :command command)
         (when message (list :message message))
         (when body (list :body body))))

(defun dap--make-success-response (id command &optional body message)
  "Make a successful DAP response.
The result is aplist usable with `json-encode'. ID, COMMAND, BODY
and MESSAGE correspond to \"request_seq\", \"command\", \"body\"
and \"message\". Consult the DAP protocol specification for
details."
  (dap--make-response id command t body message))

(defun dap--make-error-response (id command &optional body message)
  "Like `dap--make-success-response', but for failure.
In the future, this function may do logging, call `user-error',
etc...."
  (dap--make-response id command nil body message))

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
  (if (dap--session-running debug-session)
      (let* ((request-id (cl-incf (dap--debug-session-last-id debug-session)))
             (message (plist-put message :seq request-id)))
        (puthash request-id callback (dap--debug-session-response-handlers debug-session))
        (when dap-print-io
          (let ((inhibit-message dap-inhibit-io))
            (message "Sending: \n%s" (dap--json-encode message))))
        (process-send-string (dap--debug-session-proc debug-session)
                             (dap--make-message message)))
    (error "Session %s is already terminated" (dap--debug-session-name debug-session))))

(defun dap-request (session method &rest args)
  "Sync request."
  (let (result)
    (dap--send-message
     (dap--make-request method args)
     (lambda (res) (setf result (or res :finished)))
     session)

    (while (not result)
      (accept-process-output nil 0.001))

    (cond
     ((eq result :finished) nil)
     ((and (ht? result) (not (gethash "success" result))) (error (gethash "message" result)))
     (t (gethash "body" result)))))

(defun dap--open-network-stream (session-name host port)
  (let ((retries 0)
        result)
    (while (and (not result)
                (< retries dap-connect-retry-count))
      (condition-case err
          (prog1 (setq result (open-network-stream session-name nil
                                                   host port :type 'plain
                                                   :coding 'no-conversion))
            (set-process-coding-system result 'no-conversion 'no-conversion))
        (file-error
         (let ((inhibit-message t))
           (message "Failed to connect to %s:%s with error message %s"
                    host
                    port
                    (error-message-string err))
           (sit-for dap-connect-retry-interval)
           (setq retries (1+ retries))))))
    (or result (error "Failed to connect to port %s" port))))

(defun dap--create-session (launch-args)
  "Create debug session from LAUNCH-ARGS."
  (-let* (((&plist :host :dap-server-path :name session-name :debugServer port) launch-args)
          (proc (if dap-server-path
                    (make-process
                     :name session-name
                     :connection-type 'pipe
                     :coding 'no-conversion
                     :command dap-server-path
                     :stderr (concat "*" session-name " stderr*")
                     :noquery t)
                  (dap--open-network-stream session-name host port)))
          (debug-session (make-dap--debug-session
                          :launch-args launch-args
                          :proc proc
                          :name session-name
                          :output-buffer (dap--create-output-buffer session-name))))
    (set-process-sentinel proc
                          (lambda (_process exit-str)
                            (message "Debug session process exited with status: %s" exit-str)
                            (dap--mark-session-as-terminated debug-session)))
    (set-process-filter proc (dap--create-filter-function debug-session))
    debug-session))

(defun dap--send-configuration-done (debug-session)
  "Send 'configurationDone' message for DEBUG-SESSION."
  (dap--send-message (dap--make-request "configurationDone")
                     (dap--resp-handler
                      (lambda (_)
                        (when (eq 'pending (dap--debug-session-state debug-session))
                          (setf (dap--debug-session-state debug-session) 'running)
                          (run-hook-with-args 'dap-session-changed-hook))))
                     debug-session))

(defun dap--set-breakpoints-request (file-name file-breakpoints)
  "Make `setBreakpoints' request for FILE-NAME.
FILE-BREAKPOINTS is a list of the breakpoints to set for FILE-NAME."
  (with-temp-buffer
    (insert-file-contents file-name)
    (dap--make-request
     "setBreakpoints"
     (list :source (list :name (f-filename file-name)
                         :path (if (eq system-type 'windows-nt)
                                   (s-replace "/" "\\" file-name)
                                 file-name))
           :breakpoints (->> file-breakpoints
                             (-map (-lambda ((it &as &plist :condition :hit-condition :log-message))
                                     (let ((result (->> it dap-breakpoint-get-point line-number-at-pos (list :line))))
                                       (when condition (plist-put result :condition condition))
                                       (when log-message (plist-put result :logMessage log-message))
                                       (when hit-condition (plist-put result :hitCondition hit-condition))
                                       result)))
                             (apply 'vector))
           :sourceModified :json-false
           :lines (->> file-breakpoints
                       (--map (-> it dap-breakpoint-get-point line-number-at-pos))
                       (apply 'vector))))))

(defun dap--update-breakpoints (debug-session resp file-name)
  "Update breakpoints in FILE-NAME.

RESP is the result from the `setBreakpoints' request.
DEBUG-SESSION is the active debug session."
  (--if-let (-some->> resp (gethash "body") (gethash "breakpoints"))
      (->> debug-session dap--debug-session-breakpoints (puthash file-name it))
    (->> debug-session dap--debug-session-breakpoints (remhash file-name)))

  (when (eq debug-session (dap--cur-session))
    (-when-let (buffer (find-buffer-visiting file-name))
      (with-current-buffer buffer
        (run-hooks 'dap-breakpoints-changed-hook)))))

(defun dap--breakpoint-filter-enabled (filter type default)
  (defvar dap-exception-breakpoints) ;; NOTE: always called with dap-ui loaded
  (alist-get filter
             (alist-get type dap-exception-breakpoints nil nil #'equal)
             default
             nil
             #'equal))

(defun dap--set-exception-breakpoints (debug-session callback)
  (-let [(&dap-session 'current-capabilities 'launch-args (&plist :type)) debug-session]
    (dap--send-message
     (dap--make-request "setExceptionBreakpoints"
                        (list :filters
                              (or (-some->> current-capabilities
                                    (gethash "exceptionBreakpointFilters")
                                    (-keep (-lambda ((&hash "default" "filter"))
                                             (when (dap--breakpoint-filter-enabled filter type default)
                                               filter))))
                                  [])))
     (lambda (_result)
       (funcall callback))
     debug-session)))

(defun dap--configure-breakpoints (debug-session breakpoints callback)
  "Configure breakpoints for DEBUG-SESSION.
BREAKPOINTS is the list of breakpoints to set.
CALLBACK will be called once configure is finished.
RESULT to use for the callback."
  (let ((breakpoint-count (hash-table-count breakpoints))
        (finished 0))
    (if (zerop breakpoint-count)
        ;; no breakpoints to set
        (funcall callback)
      (maphash
       (lambda (file-name file-breakpoints)
         (condition-case _err
             (dap--send-message
              (dap--set-breakpoints-request file-name file-breakpoints)
              (dap--resp-handler
               (lambda (resp)
                 (setf finished (1+ finished))
                 (dap--update-breakpoints debug-session resp file-name)
                 (when (= finished breakpoint-count)
                   (dap--set-exception-breakpoints debug-session callback))))
              debug-session)
           (file-missing
            (setf finished (1+ finished))
            (remhash file-name breakpoints)
            (when (= finished breakpoint-count) (funcall callback))
            (dap--persist-breakpoints breakpoints))))
       breakpoints))))

(defun dap-eval (expression)
  "Eval and print EXPRESSION."
  (interactive "sEval: ")
  (let ((debug-session (dap--cur-active-session-or-die)))
    (if-let ((active-frame-id (-some->> debug-session
                                dap--debug-session-active-frame
                                (gethash "id"))))
        (dap--send-message
         (dap--make-request "evaluate"
                            (list :expression expression
                                  :frameId active-frame-id
                                  :context "hover"))
         (-lambda ((&hash "success" "message" "body"))
           (dap-overlays--display-interactive-eval-result
            (if success (gethash "result" body) message)
            (point)))
         debug-session)
      (error "There is no stopped debug session"))))

(defun dap-eval-thing-at-point ()
  "Eval and print EXPRESSION."
  (interactive)
  (dap-eval (thing-at-point 'symbol)))

(defun dap-eval-region (start end)
  "Evaluate the region between START and END."
  (interactive "r")
  (dap-eval (buffer-substring-no-properties start end)))

(defun dap-switch-stack-frame ()
  "Switch stackframe by selecting another stackframe stackframes from current thread."
  (interactive)
  (when (not (dap--cur-session))
    (error "There is no active session"))

  (-if-let (thread-id (dap--debug-session-thread-id (dap--cur-session)))
      (-if-let (stack-frames (gethash thread-id
                                      (dap--debug-session-thread-stack-frames (dap--cur-session))))
          (let* ((index 0)
                 (new-stack-frame (dap--completing-read "Select active frame: "
                                                        stack-frames
                                                        (-lambda ((frame &as &hash "name"))
                                                          (if-let (frame-path (dap--get-path-for-frame frame))
                                                              (format "%s: %s (in %s)"
                                                                      (cl-incf index) name frame-path)
                                                            (format "%s: %s" (cl-incf index) name)))
                                                        nil
                                                        t)))
            (dap--go-to-stack-frame (dap--cur-session) new-stack-frame))
        (->> (dap--cur-session)
             dap--debug-session-name
             (format "Current session %s is not stopped")
             error))
    (error "No thread is currently active %s" (dap--debug-session-name (dap--cur-session)))))

(defun dap--calculate-unique-name (debug-session-name debug-sessions)
  "Calculate unique name with prefix DEBUG-SESSION-NAME.
DEBUG-SESSIONS - list of the currently active sessions."
  (let* ((base-name (or (cl-second (s-match "\\(.*\\)<.*>" debug-session-name))
                        debug-session-name))
         (counter 1)
         (session-name base-name))
    (while (--first (string= session-name (dap--debug-session-name it)) debug-sessions)
      (setq session-name (format "%s<%s>" base-name counter))
      (setq counter (1+ counter)))
    session-name))

(define-derived-mode dap-server-log-mode compilation-mode "Debug Adapter"
  (read-only-mode 1)
  (setq-local window-point-insertion-type t)
  ;; we need to move window point to the end of the buffer once because
  ;; `compilation-start' inserts initial message before displaying the buffer.
  (run-with-idle-timer 0 nil
                       (lambda (buf)
                         (with-current-buffer buf
                           (mapc (lambda (w)
                                   (set-window-point w (point-max)))
                                 (get-buffer-window-list))))
                       (current-buffer)))

(declare-function dap-variables-expand "dap-variables" (plist))
(defvar dap-variables-project-root-function)
(defun dap-variables-expand-in-launch-configuration (conf)
  "Expand dap-mode launch configuration CONF."
  (require 'dap-variables)
  (let ((dap-variables-project-root-function #'lsp-workspace-root))
    (dap-variables-expand conf)))

(defun dap--set-breakpoints-in-file (file file-breakpoints)
  "Establish markers for FILE-BREAKPOINTS in FILE."
  (-when-let (buffer (get-file-buffer file))
    (with-current-buffer buffer
      (mapc (lambda (bkp)
              (-let [marker (or (plist-get bkp :marker) (make-marker))]
                (set-marker marker (dap-breakpoint-get-point bkp))
                (plist-put bkp :marker marker)))
            file-breakpoints)
      (run-hooks 'dap-breakpoints-changed-hook))))

(defun dap--read-from-file (file)
  "Read a lisp expression from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (cl-first (read-from-string
               (buffer-substring-no-properties (point-min) (point-max))))))

(defun dap--after-initialize ()
  "After initialize handler."
  (with-demoted-errors
      "Failed to load breakpoints for the current workspace with error: %S"
    (let ((breakpoints-file dap-breakpoints-file))
      (when (f-exists? breakpoints-file)
        (-let [breakpoints (dap--read-from-file breakpoints-file)]
          (maphash (lambda (file file-breakpoints)
                     (dap--set-breakpoints-in-file file file-breakpoints))
                   breakpoints)
          (lsp-workspace-set-metadata "Breakpoints" breakpoints))))))

(defun dap-mode-line ()
  "Calculate DAP modeline."
  (when lsp-mode
    (-when-let* ((debug-session (dap--cur-session))
                 (state (dap--debug-session-state debug-session)))
      (unless (member state '(failed terminated))
        (format " %s - %s"
                (dap--debug-session-name debug-session)
                (dap--debug-session-state debug-session))))))

(defun dap--thread-label (debug-session thread)
  "Calculate thread name for THREAD from DEBUG-SESSION."
  (let ((thread-id (gethash "id" thread))
        (name (gethash "name" thread)))
    (-if-let (status (gethash
                      thread-id
                      (dap--debug-session-thread-states debug-session)))
        (format "%s (%s)" name status)
      name)))

(defun dap-switch-thread ()
  "Switch current thread."
  (interactive)
  (let ((debug-session (dap--cur-active-session-or-die)))
    (dap--send-message
     (dap--make-request "threads")
     (-lambda ((&hash "body" (&hash "threads" threads)))
       (setf (dap--debug-session-threads debug-session) threads)
       (-let [(&hash "id" thread-id) (dap--completing-read
                                      "Select active thread: "
                                      threads
                                      (apply-partially 'dap--thread-label debug-session))]
         (dap--select-thread-id debug-session thread-id t)))
     debug-session)))

(defun dap-stop-thread-1 (debug-session thread-id)
  (dap--send-message
   (dap--make-request
    "pause"
    (list :threadId thread-id))
   (dap--resp-handler)
   debug-session))

(defun dap-stop-thread ()
  "Stop selected thread."
  (interactive)
  (let ((debug-session (dap--cur-active-session-or-die)))
    (dap--send-message
     (dap--make-request "threads")
     (-lambda ((&hash "body" (&hash "threads" threads)))
       (setf (dap--debug-session-threads debug-session) threads)
       (-let [(&hash "id" thread-id) (dap--completing-read
                                      "Select active thread: "
                                      threads
                                      (apply-partially 'gethash "name"))]
         (dap-stop-thread-1 debug-session thread-id)))
     debug-session)))

(defun dap--switch-to-session (new-session)
  "Make NEW-SESSION the active debug session."
  (dap--set-cur-session new-session)

  (when new-session
    (let ((breakpoints (dap--get-breakpoints)))
      (--each (dap--buffer-list) (with-current-buffer it
                                   (->> breakpoints
                                        (gethash buffer-file-name)
                                        (dap--set-breakpoints-in-file buffer-file-name))))))

  (run-hook-with-args 'dap-session-changed-hook lsp--cur-workspace)

  (-some->> new-session
    dap--debug-session-active-frame
    (dap--go-to-stack-frame new-session)))

(defun dap-switch-session ()
  "Switch current session interactively."
  (interactive)
  (lsp--cur-workspace-check)
  (pcase (reverse
          (--remove
           (or (not (dap--session-running it))
               (eq it (dap--cur-session)))
           (dap--get-sessions)))
    ('() (error "No active session to switch to"))
    (`(,debug-session) (dap--switch-to-session debug-session))
    (target-debug-sessions (dap--switch-to-session
                            (dap--completing-read "Select session: "
                                                  target-debug-sessions
                                                  #'dap--debug-session-name)))))

(defun dap-register-debug-provider (language-id provide-configuration-fn)
  "Register debug configuration provider for LANGUAGE-ID.

PROVIDE-CONFIGURATION-FN is a function which will be called when
function `dap-mode' has received a request to start debug session which
has language id = LANGUAGE-ID. The function must return debug
arguments which contain the debug port to use for opening TCP connection."
  (puthash language-id provide-configuration-fn dap--debug-providers))

(defun dap-register-debug-template (configuration-name configuration-settings)
  "Register configuration template CONFIGURATION-NAME.

CONFIGURATION-SETTINGS is a plist containing the preset settings
for the configuration. If its :name is omitted, it defaults to
CONFIGURATION-NAME."
  (setq dap-debug-template-configurations
        (delq (assoc configuration-name dap-debug-template-configurations)
              dap-debug-template-configurations))
  ;; Use `plist-member' instead of just `plist-get', because :name would
  ;; possibly not be properly removed in `dap-start-debugging-noexpand' due to
  ;; being specified twice (the inferred name, followed by nil).
  (unless (plist-member configuration-settings :name)
    ;; We mustn't modify CONFIGURATION-SETTINGS; `push' is non-destructive.
    (push configuration-name configuration-settings)
    (push :name configuration-settings))
  (add-to-list
   'dap-debug-template-configurations
   (cons configuration-name configuration-settings)))

(defun dap--find-available-port ()
  "Find available port on HOST starting from STARTING-PORT."
  (let ((process (make-network-process :name " *dap test-connection*"
                                       :family 'ipv4
                                       :service 0
                                       :server 't)))
    (prog1 (process-contact process :service)
      (delete-process process))))

(defun dap--wait-for-port (host port &optional retry-count sleep-interval)
  "Wait for PORT to be open on HOST.

RETRY-COUNT is the number of the retries.
SLEEP-INTERVAL is the sleep interval between each retry."
  (let ((success nil)
        (retries 0))
    (while (and (not success) (< retries (or retry-count 100)))
      (condition-case err
          (progn
            (delete-process (open-network-stream "*connection-test*" nil host port
                                                 :type 'plain
                                                 :coding 'no-conversion))
            (setq success t))
        (file-error
         (let ((inhibit-message t))
           (message "Failed to connect to %s:%s with error message %s"
                    host
                    port
                    (error-message-string err))
           (sit-for (or sleep-interval 0.02))
           (setq retries (1+ retries))))))
    success))

(defun dap--select-template (&optional origin)
  "Select the configuration to launch.
If ORIGIN is t, return the original configuration without prepopulation"
  (let ((debug-args (-> (dap--completing-read "Select configuration template: "
                                              dap-debug-template-configurations
                                              'cl-first nil t)
                        cl-rest
                        copy-tree)))
    (if origin debug-args
      (or (-some-> (plist-get debug-args :type)
            (gethash dap--debug-providers)
            (funcall debug-args))
          (error "There is no debug provider for language %s"
                 (or (plist-get debug-args :type) "'Not specified'"))))))

(defun dap-debug-template-configurations-provider ()
  dap-debug-template-configurations)

(defconst dap-launch-configuration-providers
  '(dap-launch-find-parse-launch-json
    dap-debug-template-configurations-provider)
  "List of functions that can contribute launch configurations to dap-debug.
When the user invokes dap-debug, all of the functions in this
list are called and their results (which must be lists) are
concatenated. The user can then choose one of them from the
resulting list.")

(defun dap-start-debugging (conf)
  "Like `dap-start-debugging-noexpand', but expand variables.
CONF's variables are expanded before being passed to
`dap-start-debugging'."
  (dap-start-debugging-noexpand
   (dap-variables-expand-in-launch-configuration conf)))

(defun dap-start-debugging-noexpand (launch-args)
  "Start debug session with LAUNCH-ARGS.
Special arguments:

:wait-for-port - boolean defines whether the debug configuration
should be started after the :port argument is taken.

:program-to-start - when set it will be started using `compilation-start'
before starting the debug process."
  (-let* (((&plist :name :skip-debug-session :cwd :program-to-start
                   :wait-for-port :type :request :port
                   :startup-function :environment-variables :hostName host) launch-args)
          (session-name (dap--calculate-unique-name name (dap--get-sessions)))
          (default-directory (or cwd default-directory))
          (process-environment (if environment-variables
                                   (cl-copy-list process-environment)
                                 process-environment))
          program-process)
    (mapc (-lambda ((env . value)) (setenv env value t)) environment-variables)
    (plist-put launch-args :name session-name)

    (when program-to-start
      (setf program-process
            (get-buffer-process
             (compilation-start program-to-start 'dap-server-log-mode
                                (lambda (_) (concat "*" session-name " server log*"))))))
    (when wait-for-port
      (dap--wait-for-port host port dap-connect-retry-count dap-connect-retry-interval))

    (when startup-function (funcall startup-function launch-args))

    (unless skip-debug-session
      (let ((debug-session (dap--create-session launch-args)))
        (setf (dap--debug-session-program-proc debug-session) program-process)
        (dap--send-message
         (dap--initialize-message type)
         (dap--session-init-resp-handler
          debug-session
          (-lambda ((&hash "body" capabilities))
            (-let [debug-sessions (dap--get-sessions)]

              (ht-update! (dap--debug-session-current-capabilities debug-session) capabilities)

              (dap--set-sessions (cons debug-session debug-sessions)))
            (dap--send-message
             (dap--make-request request (-> launch-args
                                            (cl-copy-list)
                                            (dap--plist-delete :cleanup-function)
                                            (dap--plist-delete :startup-function)
                                            (dap--plist-delete :dap-server-path)
                                            (dap--plist-delete :environment-variables)
                                            (dap--plist-delete :wait-for-port)
                                            (dap--plist-delete :skip-debug-session)
                                            (dap--plist-delete :program-to-start)))
             (dap--session-init-resp-handler debug-session)
             debug-session)))
         debug-session)

        (dap--set-cur-session debug-session)
        (push (cons session-name launch-args) dap--debug-configuration)
        (run-hook-with-args 'dap-session-created-hook debug-session)))))

;;;###autoload
(defun dap-debug (debug-args)
  "Run debug configuration DEBUG-ARGS.

If DEBUG-ARGS is not specified the configuration is generated
after selecting configuration template."
  (interactive (list (-> (dap--completing-read "Select configuration template: "
                                               (-mapcat #'funcall dap-launch-configuration-providers)
                                               'cl-first nil t)
                         cl-rest
                         copy-tree)))
  ;; NOTE: the launch configuration must be expanded *before* being passed to a
  ;; debug provider. This is because some debug providers (e.g. dap-python) pass
  ;; some fields of DEBUG-ARGS as shell arguments in :program-to-launch and try
  ;; very hard to quote them. Because of this, `dap-start-debugging' cannot
  ;; expand them properly. Any python configuration that uses variables in :args
  ;; will fail.
  (let* ((debug-args (dap-variables-expand-in-launch-configuration debug-args))
         (launch-args (or (-some-> (plist-get debug-args :type)
                            (gethash dap--debug-providers)
                            (funcall debug-args))
                          (user-error "Have you loaded the `%s' specific dap package?"
                                      (or (plist-get debug-args :type)
                                          (user-error "%s does not specify :type" debug-args))))))
    (if (functionp launch-args)
        (funcall launch-args #'dap-start-debugging-noexpand)
      (dap-start-debugging-noexpand launch-args))))

(defun dap-debug-edit-template (&optional debug-args)
  "Edit registered template DEBUG-ARGS.
When being invoked with prefix argument, poping up the prepopulated version of the template.
Otherwise, return its original version.  After registration, the new template can be used
normally with `dap-debug'"
  (interactive)
  (unless debug-args
    (setq debug-args (dap--select-template (not current-prefix-arg))))
  (with-current-buffer (or (get-buffer "*DAP Templates*")
                           (with-current-buffer (get-buffer-create "*DAP Templates*")
                             (emacs-lisp-mode)
                             (current-buffer)))
    (goto-char (point-max))
    (when (s-blank? (buffer-string))
      (insert ";; Eval Buffer with `M-x eval-buffer' to register the newly created template."))
    (insert
     (format "\n\n(dap-register-debug-template\n  \"%s%s\"\n"
             (plist-get debug-args :name)
             (if current-prefix-arg " - Copy" "")))
    (insert "  (list ")
    (-let ((column (current-column))
           ((fst snd . rst) debug-args))
      (insert (format "%s %s" fst (prin1-to-string snd)))
      (cl-loop for (k v) on rst by #'cddr
         do (if (not (equal k :program-to-start))
                (progn
                  (insert "\n")
                  (--dotimes column (insert " "))
                  (insert (format "%s %s" k (prin1-to-string v)))))))
    (insert "))"))
  (pop-to-buffer "*DAP Templates*")
  (goto-char (point-max)))

(defun dap-debug-last ()
  "Debug last configuration."
  (interactive)
  (if-let (configuration (cdr (cl-first dap--debug-configuration)))
      (dap-debug configuration)
    (call-interactively 'dap-debug)))

(defun dap-debug-recent ()
  "Debug last configuration."
  (interactive)
  (->> (dap--completing-read "Select configuration: "
                             dap--debug-configuration
                             'cl-first nil t)
       cl-rest
       dap-debug))

(defun dap-go-to-output-buffer (&optional no-select)
  "Go to output buffer."
  (interactive)
  (let ((win (display-buffer-in-side-window
              (dap--debug-session-output-buffer (dap--cur-session-or-die))
              `((side . bottom) (slot . 5) (window-width . 0.20)))))
    (set-window-dedicated-p win t)
    (unless no-select (select-window win))
    (fit-window-to-buffer win dap-output-window-max-height dap-output-window-min-height)))

(defun dap-delete-session (debug-session)
  "Remove DEBUG-SESSION.
If the current session it will be terminated."
  (interactive (list (dap--cur-session-or-die)))
  (let* ((cleanup-fn (lambda ()
                       (->> (dap--get-sessions)
                            (-remove-item debug-session)
                            (dap--set-sessions))
                       (when (eq (dap--cur-session) debug-session)
                         (dap--switch-to-session nil))
                       (when-let (buffer (dap--debug-session-output-buffer debug-session))
                         (kill-buffer buffer))
                       (dap--refresh-breakpoints))))
    (if (not (dap--session-running debug-session))
        (funcall cleanup-fn)
      (dap--send-message (dap--make-request "disconnect"
                                            (list :restart :json-false))
                         (dap--resp-handler
                          (lambda (_resp)
                            ;; its still alive, so kill its debugger off,
                            ;; causing the process sentinel from
                            ;; `dap--create-session' to do its thing. NOTE that
                            ;; this need not be done if the process isn't alive
                            ;; (case above), so this was moved here, as a minor
                            ;; optimization.
                            (when-let (proc (dap--debug-session-program-proc debug-session))
                              ;; ensure that `dap-terminated-hook' runs; must be
                              ;; done before CLEANUP-FN, as otherwise the
                              ;; process' buffer will be killed before it,
                              ;; potentially causing weirdness.
                              (when (process-live-p proc)
                                ;; The server might have already died due to the
                                ;; disconnect.
                                (kill-process proc)))
                            (funcall cleanup-fn)))
                         debug-session))))

(defun dap-delete-all-sessions ()
  "Terminate/remove all sessions."
  (interactive)
  ;; NOTE: this will call `dap-terminated-hook' for each live session; this is
  ;; correct because `dap-terminated-hook' must be called after each session
  ;; terminates.
  (mapc #'dap-delete-session (dap--get-sessions)))

(defun dap-breakpoint-delete-all ()
  "Delete all breakpoints."
  (interactive)
  (maphash (lambda (file-name _)
             (dap--breakpoints-changed nil file-name))
           (dap--get-breakpoints)))

(defun dap--buffer-killed ()
  "Buffer killed handler."
  ;; make sure that the breakpoints are updated on close of the file since the
  ;; file might have been edited so we need to recalculate the :point based on the marker.
  (let* ((breakpoints (dap--get-breakpoints))
         (file-breakpoints (gethash buffer-file-name breakpoints))
         (updated-breakpoints (-map (-lambda ((bkp &as &plist :marker :point))
                                      (-> bkp
                                          (dap--plist-delete :point)
                                          (dap--plist-delete :marker)
                                          (plist-put :point (if marker
                                                                (marker-position marker)
                                                              point))))
                                    file-breakpoints)))
    (if updated-breakpoints
        (puthash buffer-file-name updated-breakpoints breakpoints)
      (remhash buffer-file-name breakpoints))
    (dap--persist-breakpoints breakpoints)))

(defun dap--after-open ()
  "Handler of after open hook."
  (when (buffer-file-name)
    (->> (dap--get-breakpoints)
         (gethash buffer-file-name)
         (dap--set-breakpoints-in-file buffer-file-name))
    (add-hook 'kill-buffer-hook 'dap--buffer-killed nil t)))

(defun dap-mode-mouse-set-clear-breakpoint (event)
  "Set or remove a breakpoint at the position represented by an
`event' mouse click. If `dap-mode' is not enabled, then only the
point is set."
  (interactive "e")
  (mouse-minibuffer-check event)
  (let ((posn (event-end event)))
    (with-selected-window (posn-window posn)
      (if (and (buffer-file-name) (bound-and-true-p dap-mode))
          (if (numberp (posn-point posn))
              (save-excursion
                (goto-char (posn-point posn))
                (dap-breakpoint-toggle))))
      (posn-set-point posn))))

(defvar dap-mode-map
  (let ((dap-mode-map (make-sparse-keymap)))
    (define-key dap-mode-map [left-margin mouse-1]
      'dap-mode-mouse-set-clear-breakpoint)
    (define-key dap-mode-map [left-fringe mouse-1]
      'dap-mode-mouse-set-clear-breakpoint)
    dap-mode-map)
  "Keymap for `dap-mode'.")

;;;###autoload
(define-minor-mode dap-mode
  "Global minor mode for DAP mode."
  :init-value nil
  :group 'dap-mode
  :global t
  :require 'dap-mode
  :lighter (:eval (dap-mode-line))
  (dap--after-initialize)
  (add-hook 'lsp-after-open-hook 'dap--after-open))

(defun dap-turn-on-dap-mode ()
  "Turn on function `dap-mode'."
  (interactive)
  (dap-mode t))


;; Auto configure

(declare-function dap-ui-mode "dap-ui" (&optional arg))
(declare-function dap-ui-many-windows-mode "dap-ui" (&optional arg))
;;;###autoload
(define-minor-mode dap-auto-configure-mode
  "Auto configure dap minor mode."
  :init-value nil
  :global t
  :group 'dap-mode
  (cond
   (dap-auto-configure-mode
    (dap-mode 1)
    (dap-ui-mode 1) ;; NOTE: `dap-ui-mode' is auto-loaded
    (seq-doseq (feature dap-auto-configure-features)
      (when-let (mode (alist-get feature dap-features->modes))
        (if (consp mode)
            (when (require (cdr mode) nil t)
              (funcall (car mode) 1))
          (funcall mode 1))))
    (dap-ui-many-windows-mode 1))
   (t
    (dap-mode -1)
    (dap-ui-mode -1)
    (seq-doseq (feature dap-auto-configure-features)
      (when-let (mode (alist-get feature dap-features->modes))
        (if (consp mode)
            (funcall (car mode) -1)
          (funcall mode -1))))
    (dap-ui-many-windows-mode -1))))

(provide 'dap-mode)
;;; dap-mode.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
