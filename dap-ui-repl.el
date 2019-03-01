;;; dap-ui-repl.el --- DAP REPL -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; This code is heavily based on `skewer-repl'. Run `dap-ui-repl' to switch to
;; the REPL buffer and evaluate code.

;;; Code:

(require 'comint)
(require 'compile)
(require 'dash)
(require 'dap-mode)
(require 'lsp)

(defcustom dap-ui-repl-prompt ">> "
  "Prompt string for DAP REPL."
  :type 'string
  :group 'dap-ui)

(defvar dap-ui-repl-welcome
  (propertize "*** Welcome to Dap-Ui ***\n"
              'font-lock-face 'font-lock-comment-face)
  "Header line to show at the top of the REPL buffer.
Hack notice: this allows log messages to appear before anything is
evaluated because it provides insertable space at the top of the
buffer.")

(defun dap-ui-repl-process ()
  "Return the process for the dap-ui REPL."
  (get-buffer-process (current-buffer)))

(define-derived-mode dap-ui-repl-mode comint-mode "DAP-REPL"
  "Provide a REPL for the active debug session."
  :group 'dap-ui
  :syntax-table emacs-lisp-mode-syntax-table
  (setq comint-prompt-regexp (concat "^" (regexp-quote dap-ui-repl-prompt))
        comint-input-sender 'dap-ui-input-sender
        comint-process-echoes nil)
  ;; Make opportunistic use of company-mode, but don't require it.
  ;; This means company-backends may be undeclared, so don't emit a
  ;; warning about it.
  (with-no-warnings
    (setq-local company-backends '(company-dap-ui-repl)))
  (unless (comint-check-proc (current-buffer))
    (insert dap-ui-repl-welcome)
    (start-process "dap-ui-repl" (current-buffer) nil)
    (set-process-query-on-exit-flag (dap-ui-repl-process) nil)
    (goto-char (point-max))
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)
    (comint-output-filter (dap-ui-repl-process) dap-ui-repl-prompt)
    (set-process-filter (dap-ui-repl-process) 'comint-output-filter)))

(defun dap-ui-input-sender (_ input)
  "REPL comint handler.
INPUT is the current input."
  (let ((debug-session (dap--cur-active-session-or-die)))
    (if-let ((active-frame-id (-some->> debug-session
                                        dap--debug-session-active-frame
                                        (gethash "id"))))
        (dap--send-message
         (dap--make-request "evaluate"
                            (list :expression input
                                  :frameId active-frame-id))
         (-lambda ((&hash "success" "message" "body"))
           (-when-let (buffer (get-buffer "*dap-ui-repl*"))
             (with-current-buffer buffer
               (comint-output-filter (dap-ui-repl-process)
                                     (concat (if success (gethash "result" body) message)
                                             "\n"
                                             dap-ui-repl-prompt)))))
         debug-session)
      (error "There is no stopped debug session"))))

;;;###autoload
(defun dap-ui-repl ()
  "Start a JavaScript REPL to be evaluated in the visiting browser."
  (interactive)
  (let ((workspaces (lsp-workspaces)))
    (unless (get-buffer "*dap-ui-repl*")
      (with-current-buffer (get-buffer-create "*dap-ui-repl*")
        (dap-ui-repl-mode)
        (when (functionp 'company-mode)
          (company-mode 1))
        (setq-local lsp--buffer-workspaces workspaces))))
  (pop-to-buffer (get-buffer "*dap-ui-repl*")))

(defun dap-ui-repl--calculate-candidates ()
  "Calculate candidates.
TEXT is the current input."
  (let ((text (comint-get-old-input-default))
        (debug-session (dap--cur-active-session-or-die)))
    (if-let (frame-id (-some->> debug-session
                                dap--debug-session-active-frame
                                (gethash "id")))
        (cons :async
              (lambda (callback)
                (dap--send-message
                 (dap--make-request "completions"
                                    (list :frameId frame-id
                                          :text text
                                          :column (- (length text) (- (point-at-eol) (point)))))
                 (dap--resp-handler
                  (lambda (result)
                    (-if-let (targets (-some->> result (gethash "body") (gethash "targets")))
                        (funcall callback (-map (-lambda ((item &as &hash "label" "text" "type"))
                                                  (propertize label :text text :type type :dap-completion-item item))
                                                targets))
                      (funcall callback ()))))
                 debug-session))))))

(defun dap-ui-repl--post-completion (candidate)
  "Post completion handling for CANDIDATE."
  (let ((to-insert (plist-get (text-properties-at 0 candidate) :text)))
    (when to-insert
      (delete-char (- (length candidate)))
      (insert to-insert))))

(defun dap-ui-repl--annotate (candidate)
  "Get annotation for CANDIDATE."
  (concat " " (plist-get (text-properties-at 0 candidate) :type)))

(defun company-dap-ui-repl (command &optional candidate &rest _args)
  "Dap-Ui REPL backend for company-mode.
See `company-backends' for more info about COMMAND and CANDIDATE."
  (interactive (list 'interactive))
  (cl-case command
    (interactive
     (with-no-warnings ;; opportunistic use of company-mode
       (company-begin-backend 'company-dap-ui-repl)))
    (prefix (dap-ui-repl-company-prefix))
    (ignore-case t)
    (sorted t)
    (match (length candidate))
    (annotation (dap-ui-repl--annotate candidate))
    (candidates (dap-ui-repl--calculate-candidates))
    (post-completion (dap-ui-repl--post-completion candidate))))

(defun dap-ui-repl-company-prefix ()
  "Prefix for company."
  (and (eq major-mode 'dap-ui-repl-mode)
       (or (with-no-warnings ;; opportunistic use of company-mode
             (company-grab-word))
           'stop)))

(provide 'dap-ui-repl)
;;; dap-ui-repl.el ends here
