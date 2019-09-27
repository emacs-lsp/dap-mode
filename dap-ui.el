;;; dap-ui.el --- Debug Adapter Protocol UI -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: languages, debug

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
;; Package-Requires: ((emacs "25.1") (tree-mode "1.1.1.1") (bui "1.1.0"))
;; Version: 0.2

;;; Commentary:
;; DAP Windows/overlays

;;; Code:
(require 'dap-mode)
(require 'tree-widget)
(require 'tree-mode)
(require 'wid-edit)
(require 'dash)
(require 'bui)
(require 'comint)
(require 'compile)
(require 'gdb-mi)

(defcustom dap-ui-stack-frames-loaded nil
  "Stack frames loaded."
  :type 'hook
  :group 'dap-ui)

(defcustom dap-ui-session-refresh-delay 0.5
  "Delay before the session view is updated."
  :type 'hook
  :group 'dap-ui)

(defcustom dap-ui-themes-directory (f-join (f-dirname (or load-file-name buffer-file-name)) "icons")
  "Directory containing themes."
  :type 'directory
  :group 'dap-ui)

(defcustom dap-ui-theme "eclipse"
  "Theme to use."
  :type 'string
  :group 'dap-ui)

(defcustom dap-ui-breakpoints-ui-list-displayed-hook nil
  "List of functions to run when breakpoints list is displayed."
  :type 'hook
  :group 'dap-ui)

(defface dap-ui-compile-errline
  '((t (:inherit compilation-error)))
  "Face used for marking the line on which an error occurs."
  :group 'dap-ui)

(defface dap-ui-sessions-active-session-face
  '((t :inherit font-lock-function-name-face  :underline t))
  "Face used for marking current session in sessions list."
  :group 'dap-ui)

(defface dap-ui-sessions-terminated-face
  '((t :inherit italic :underline t :weight bold))
  "Face used for marking terminated session."
  :group 'dap-ui)

(defface dap-ui-sessions-running-face
  '((t :inherit font-lock-function-name-face :underline t :weight bold))
  "Face used for marking terminated session."
  :group 'dap-ui)

(defface dap-ui-locals-scope-face
  '((t :inherit font-lock-function-name-face :weight bold :underline t))
  "Face used for scopes in locals view."
  :group 'dap-ui)

(defface dap-ui-inspect-face
  '((t :inherit font-lock-function-name-face :weight bold :underline t))
  "Face used for scopes in locals view."
  :group 'dap-ui)

(defface dap-ui-locals-variable-leaf-face
  '((t :inherit font-lock-builtin-face :italic t))
  "Face used for variables that does not have nested items."
  :group 'dap-ui)

(defface dap-ui-locals-variable-face
  '((t :inherit  font-lock-builtin-face :weight bold))
  "Face used for marking terminated session."
  :group 'dap-ui)

(defface dap-ui-sessions-thread-face
  '((t :inherit font-lock-keyword-face))
  "Face used for threads in sessions view."
  :group 'dap-ui)

(defface dap-ui-sessions-stack-frame-face
  '((t :inherit font-lock-builtin-face))
  "Face used for threads in sessions view."
  :group 'dap-ui)

(defface dap-ui-pending-breakpoint-face
  '((t ()))
  "Face used for marking lines with a pending breakpoints."
  :group 'dap-ui)

(defface dap-ui-verified-breakpoint-face
  '((t ()))
  "Face used for marking lines with a verified breakpoints."
  :group 'dap-ui)

(defface dap-ui-marker-face
  '((t :inherit highlight))
  "Face used for marking the current point of execution."
  :group 'dap-ui)

(defface dap-ui-compile-errline
  '((t (:inherit compilation-error)))
  "Face used for marking the line on which an error occurs."
  :group 'dap-ui)

(defface dap-ui-breakpoint-verified-fringe
  '((t
     :foreground "dark green"
     :weight bold))
  "Face for enabled breakpoint icon in fringe."
  :group 'dap-ui)

(defcustom  dap-ui-default-fetch-count 30
  "Default number of variables to load in inspect variables view for array variables."
  :group 'dap-ui
  :type 'number)

(define-widget 'dap-ui-widget-guide 'item
  "Vertical guide line."
  :tag       " "
  :format    "%t")

(define-widget 'dap-ui-widget-end-guide 'item
  "End of a vertical guide line."
  :tag       " "
  :format    "%t")

(define-widget 'dap-ui-widget-handle 'item
  "Horizontal guide line that joins a vertical guide line to a node."
  :tag       " "
  :format    "%t")

(defmacro dap-ui-define-widget (name &optional image-open image-closed image-empty)
  "Helper for defining widget icons."
  (let ((open-icon (make-symbol (format "dap-ui-%s-open" name)))
        (close-icon (make-symbol (format "dap-ui-%s-close" name)))
        (empty-icon (make-symbol (format "dap-ui-%s-empty" name)))
        (leaf-icon (make-symbol (format "dap-ui-%s-leaf" name))))
    `(progn
       (define-widget (quote ,open-icon) 'tree-widget-icon
         "Icon for a open tree-widget node."
         :tag        "[+]"
         :glyph-name ,(or image-open name))
       (define-widget (quote ,close-icon) 'tree-widget-icon
         "Icon for a closed tree-widget node."
         :tag        "[-]"
         :glyph-name ,(or image-closed image-open name))
       (define-widget (quote ,empty-icon) 'tree-widget-icon
         "Icon for a closed tree-widget node."
         :tag        "[.]"
         :glyph-name ,(or image-empty image-open name))
       (list :open-icon (quote ,open-icon)
             :close-icon (quote ,close-icon)
             :empty-icon (quote ,empty-icon)
             :leaf-icon (quote ,leaf-icon)
             :handle 'dap-ui-widget-handle
             :end-guide 'dap-ui-widget-end-guide
             :guide 'dap-ui-widget-guide))))

(defvar dap-ui-icons-project-running (dap-ui-define-widget "project-running"))
(defvar dap-ui-icons-project-stopped (dap-ui-define-widget "project-stopped"))
(defvar dap-ui-icons-stack-frame-running (dap-ui-define-widget "stack-frame-running"))
(defvar dap-ui-icons-stack-frame-stopped (dap-ui-define-widget "stack-frame-stopped"))
(defvar dap-ui-icons-thread-running (dap-ui-define-widget "thread-running"))
(defvar dap-ui-icons-thread-stopped (dap-ui-define-widget "thread-stopped"))
(defvar dap-ui-icons-inspect (dap-ui-define-widget "inspect"))
(defvar dap-ui-icons-scope (dap-ui-define-widget "scope"))
(defvar dap-ui-icons-variable (dap-ui-define-widget "variable" "expanded" "collapsed"))

(defconst dap-ui--loading-tree-widget
  (list '(tree-widget :tag "Loading..." :format "%[%t%]\n")))

(defconst dap-ui--locals-buffer "*dap-ui-locals*")
(defconst dap-ui--sessions-buffer "*dap-ui-sessions*")
(defconst dap-ui--inspect-buffer "*dap-ui-inspect*")
(defconst dap-ui--debug-window-buffer "*debug-window*")

(defvar dap-ui-buffer-configurations
  `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.20)))
    (,dap-ui--inspect-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
    (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
    (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))))

(defvar dap-ui--sessions-refresh-timer nil)

(defvar-local dap-ui--locals-request-id 0
  "The locals request id that is currently active.")

(defun dap-ui-sessions--tree-under-cursor ()
  "Get tree under cursor."
  (-when-let (widget-under-cursor (dap-ui--nearest-widget))
    (if (tree-widget-p widget-under-cursor )
        widget-under-cursor
      (widget-get widget-under-cursor :parent))))

(defun dap-ui-sessions-select ()
  "Select the element under cursor."
  (interactive)
  (if-let (widget (dap-ui-sessions--tree-under-cursor))
      (-let ((session (widget-get widget :session))
             (dap-ui-session-refresh-delay nil))
        (cl-case (widget-get widget :element-type)
          (:session (dap--switch-to-session session))
          (:thread (-let ((thread  (widget-get widget :thread)))
                     (setf (dap--debug-session-thread-id session) (gethash "id" thread) )
                     (dap--switch-to-session session)
                     (dap--select-thread-id session (gethash "id" thread))))
          (:stack-frame (-let ((thread (widget-get widget :thread))
                               (stack-frame (widget-get widget :stack-frame)))
                          (setf (dap--debug-session-thread-id session) (gethash "id" thread)
                                (dap--debug-session-active-frame session) stack-frame)
                          (dap--switch-to-session session)))))
    (message "Nothing under cursor.")))

(defun dap-ui--stack-frames (thread-tree)
  "Method for expanding stackframe content.

THREAD-TREE will be widget element holding thread info."
  (let* ((session (widget-get thread-tree :session))
         (thread (widget-get thread-tree :thread))
         (thread-id (gethash "id" thread))
         (stack-frames (gethash thread-id (dap--debug-session-thread-stack-frames session))))
    (if stack-frames
        ;; aldready loaded
        (mapcar (-lambda ((stack-frame &as &hash "name" "line" "source"))
                  (let* ((tag (if source
                                  (format "%s (%s:%s)" name (or (gethash "name" source)
                                                                (gethash "path" source))
                                          line)
                                (format "%s (Unknown source)" name)))
                         (current-session (dap--cur-session))
                         (icons (if (and (equal session current-session)
                                         (= thread-id (dap--debug-session-thread-id current-session))
                                         (equal stack-frame (dap--debug-session-active-frame current-session)))
                                    dap-ui-icons-stack-frame-running
                                  dap-ui-icons-stack-frame-stopped)))
                    `(tree-widget :node (push-button :format "%[%t%]\n"
                                                     :tag ,tag
                                                     'action 'dap-ui-sessions-select)
                                  :stack-frame ,stack-frame
                                  :session ,session
                                  :element-type :stack-frame
                                  :thread ,thread
                                  :button-face dap-ui-sessions-stack-frame-face
                                  :open nil
                                  ,@icons)))
                stack-frames)
      (when (and (string= (gethash thread-id (dap--debug-session-thread-states session)) "stopped")
                 (widget-get thread-tree :loading))
        (widget-put thread-tree :loading t)
        (dap--send-message
         (dap--make-request "stackTrace"
                            (list :threadId thread-id))
         (dap--resp-handler
          (lambda (stack-frames-resp)
            (with-current-buffer dap-ui--sessions-buffer
              (let ((stack-frames (or (-some->> stack-frames-resp
                                                (gethash "body")
                                                (gethash "stackFrames"))
                                      (vector))))
                (puthash thread-id
                         stack-frames
                         (dap--debug-session-thread-stack-frames session))

                (tree-mode-reflesh-tree thread-tree)
                (run-hook-with-args 'dap-ui-stack-frames-loaded session stack-frames)))))
         session)))))

(defun dap-ui-sessions-delete-session ()
  "Delete session under cursor."
  (interactive)
  (-> (dap-ui-sessions--tree-under-cursor)
      (widget-get :session)
      dap-delete-session))

(defun dap-ui--load-threads (session-tree)
  "Method for expanding threads.

SESSION-TREE will be the root of the threads(session holder)."
  (let ((debug-session (widget-get session-tree :session)))
    (when (dap--session-running debug-session)
      (if-let (threads (dap--debug-session-threads debug-session))
          (mapcar (-lambda ((thread &as &hash "name" "id"))
                    (-let* ((status (gethash
                                     id
                                     (dap--debug-session-thread-states debug-session)))
                            (label (if status (format "%s (%s)" name status) name))
                            (icons (if (string= status "stopped")
                                       dap-ui-icons-thread-stopped
                                     dap-ui-icons-thread-running)))
                      `(tree-widget
                        :node (push-button :tag ,label
                                           :format "%[%t%]\n"
                                           'action 'dap-ui-sessions-select)
                        :thread ,thread
                        :session ,debug-session
                        :dynargs dap-ui--stack-frames
                        :element-type :thread
                        :button-face dap-ui-sessions-thread-face
                        :open t
                        ,@icons)))
                  threads)
        (dap--send-message
         (dap--make-request "threads")
         (dap--resp-handler
          (lambda (threads-resp)
            (let ((threads (gethash "threads" (gethash "body" threads-resp))))

              (setf (dap--debug-session-threads debug-session) threads)

              (tree-mode-reflesh-tree session-tree)
              (run-hook-with-args 'dap-ui-stack-frames-loaded
                                  debug-session
                                  threads))))
         debug-session)
        dap-ui--loading-tree-widget))))

(defvar dap-ui-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'tree-mode-toggle-expand)
    (define-key map (kbd "RET") #'dap-ui-sessions-select)
    map))

(defvar dap-ui-inspect-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'tree-mode-toggle-expand)
    map))

(defun dap-ui-sessions--cleanup-hooks ()
  "Remove UI sessions related hooks."
  (remove-hook 'dap-terminated-hook 'dap-ui-sessions--schedule-refresh)
  (remove-hook 'dap-session-changed-hook 'dap-ui-sessions--schedule-refresh)
  (remove-hook 'dap-continue-hook 'dap-ui-sessions--schedule-refresh)
  (remove-hook 'dap-stack-frame-changed-hook  'dap-ui-sessions--schedule-refresh))

(define-minor-mode dap-ui-sessions-mode
  "UI Session list minor mode."
  :init-value nil
  :group dap-ui
  :keymap dap-ui-session-mode-map


  (add-hook 'dap-terminated-hook 'dap-ui-sessions--schedule-refresh )
  (add-hook 'dap-session-changed-hook 'dap-ui-sessions--schedule-refresh)
  (add-hook 'dap-continue-hook 'dap-ui-sessions--schedule-refresh)
  (add-hook 'dap-stack-frame-changed-hook 'dap-ui-sessions--schedule-refresh)
  (add-hook 'kill-buffer-hook 'dap-ui-sessions--cleanup-hooks nil t)
  (setq buffer-read-only t))

(defun dap-ui-session--calculate-face (debug-session)
  "Calculate the face of DEBUG-SESSION based on its state."
  (cond
   ((eq debug-session (dap--cur-session)) 'dap-ui-sessions-active-session-face)
   ((not (dap--session-running debug-session)) 'dap-ui-sessions-terminated-face)
   (t 'dap-ui-sessions-running-face)))

(defun dap-ui--nearest-widget ()
  "Return widget at point or next nearest widget."
  (or (widget-at)
      (ignore-errors
        (let ((pos (point)))
          (widget-forward 1)
          (and (< pos (point))
               (widget-at))))))

(defun dap-ui-sessions--render-session-node (session)
  "Render SESSION node."
  `(push-button :format "%[%t%]\n"
                :tag ,(format "%s (%s)"
                              (dap--debug-session-name session)
                              (dap--debug-session-state session))
                :button-face ,(dap-ui-session--calculate-face session)
                'action 'dap-ui-sessions-select))

(defun dap-ui-sessions--render-session (session)
  "Render SESSION."
  (widget-create
   (if (dap--session-running session)
       `(tree-widget
         :node ,(dap-ui-sessions--render-session-node session)
         :open nil
         :session ,session
         :element-type :session
         :dynargs dap-ui--load-threads
         ,@dap-ui-icons-project-running)
     `(tree-widget
       :node ,(dap-ui-sessions--render-session-node session)
       :open t
       :element-type :session
       :session ,session
       ,@dap-ui-icons-project-stopped))))

(defun dap-ui-sessions--refresh (&rest _args)
  "Refresh ressions view."
  (when dap-ui--sessions-refresh-timer
    (cancel-timer dap-ui--sessions-refresh-timer)
    (setq dap-ui--sessions-refresh-timer nil))

  (with-current-buffer (get-buffer-create dap-ui--sessions-buffer)
    (let ((debug-sessions (dap--get-sessions))
          (inhibit-read-only t)
          present-sessions parent session present-widgets)
      ;; delete redundant sessions
      (save-excursion
        (goto-char (point-min))
        (let ((widget (dap-ui--nearest-widget)))
          (while widget
            (if (and (tree-widget-p (setq parent (widget-get widget :parent)))
                     (not (-contains? debug-sessions
                                      (setq session (widget-get parent :session)))))
                (widget-delete parent)
              (goto-char (widget-get (or parent widget) :to))
              (push session present-sessions)
              (push parent present-widgets))
            (setq widget (dap-ui--nearest-widget)))))

      ;; update present sessions
      (dolist (tree present-widgets)
        (let ((session (widget-get tree :session)))
          (widget-put tree :node (dap-ui-sessions--render-session-node session))
          (tree-mode-reflesh-tree tree)))

      ;; add missing sessions
      (save-excursion
        (goto-char (point-max))
        (-each (cl-set-difference debug-sessions present-sessions)
          'dap-ui-sessions--render-session)))))

(defun dap-ui-sessions--schedule-refresh (&rest _args)
  "Refresh ressions view."
  (if dap-ui-session-refresh-delay
      (when (not dap-ui--sessions-refresh-timer)
        (setq dap-ui--sessions-refresh-timer (run-at-time dap-ui-session-refresh-delay nil 'dap-ui-sessions--refresh)))
    (dap-ui-sessions--refresh)))

(defun dap-ui-sessions ()
  "Show currently active sessions."
  (interactive)
  (let ((sessions (reverse (lsp-workspace-get-metadata "debug-sessions")))
        (buf (get-buffer-create dap-ui--sessions-buffer))
        (inhibit-read-only t)
        (workspace lsp--cur-workspace))
    (with-current-buffer buf
      (erase-buffer)
      (setq mode-line-format "Sessions")
      (setq-local lsp--cur-workspace workspace)
      (setq-local tree-widget-themes-directory dap-ui-themes-directory)
      (tree-widget-set-theme dap-ui-theme)
      (mapc 'dap-ui-sessions--render-session sessions)
      (dap-ui-sessions-mode t))
    (dap-ui--show-buffer buf)))

(defun dap-ui--make-overlay (beg end tooltip-text visuals &optional mouse-face buf)
  "Allocate a DAP UI overlay in range BEG and END.
TOOLTIP-TEXT, VISUALS, MOUSE-FACE will be used for the overlay.
BUF is the active buffer."
  (let ((ov (make-overlay beg end buf t t)))
    (overlay-put ov 'face           (plist-get visuals :face))
    (overlay-put ov 'mouse-face     mouse-face)
    (overlay-put ov 'help-echo      tooltip-text)
    (overlay-put ov 'dap-ui-overlay  t)
    (overlay-put ov 'priority (plist-get visuals :priority))
    (let ((char (plist-get visuals :char)))
      (if (window-system)
          (when char
            (overlay-put ov 'before-string
                         (propertize char
                                     'display
                                     (list 'left-fringe
                                           (plist-get visuals :bitmap)
                                           (plist-get visuals :fringe)))))))
    ov))

(defun dap-ui--make-overlay-at (file point msg visuals)
  "Create an overlay highlighting the given POINT in FILE.
VISUALS and MSG will be used for the overlay."
  (-when-let (buf (find-buffer-visiting file))
    (with-current-buffer buf
      ;; If point is provided, use it to define region
      (when (integer-or-marker-p point)
        (save-excursion
          (goto-char point)
          (dap-ui--make-overlay (point-at-bol) (point-at-eol) msg visuals nil buf))))))

(defvar-local dap-ui--breakpoint-overlays '())

(defvar-local dap-ui--cursor-overlay '())

(defun dap-ui--clear-breakpoint-overlays ()
  "Remove all overlays that ensime-debug has created."
  (mapc #'delete-overlay dap-ui--breakpoint-overlays)
  (setq dap-ui--breakpoint-overlays '()))

(defun dap-ui--breakpoint-visuals (breakpoint breakpoint-dap)
  "Calculate visuals for a BREAKPOINT based on the data comming from DAP server.
BREAKPOINT-DAP - nil or the data comming from DAP."
  (list :face (if (and breakpoint-dap (gethash "verified" breakpoint-dap))
                  'dap-ui-verified-breakpoint-face
                'dap-ui-pending-breakpoint-face)
        :char "."
        :bitmap (cond
                 ((plist-get breakpoint :condition) 'filled-rectangle)
                 ((plist-get breakpoint :log-message) 'right-arrow)
                 ((plist-get breakpoint :hit-condition) 'hollow-rectangle)
                 (t 'breakpoint))
        :fringe (if (and breakpoint-dap (gethash "verified" breakpoint-dap))
                    'dap-ui-breakpoint-verified-fringe
                  'breakpoint-disabled)
        :priority 1))

(defun dap-ui--refresh-breakpoints ()
  "Refresh breakpoints in FILE-NAME.
DEBUG-SESSION the new breakpoints for FILE-NAME."
  (dap-ui--clear-breakpoint-overlays)
  (-map (-lambda ((bp . remote-bp))
          (push (dap-ui--make-overlay-at buffer-file-name
                                         (dap-breakpoint-get-point bp)
                                         "Breakpoint"
                                         (dap-ui--breakpoint-visuals bp remote-bp))
                dap-ui--breakpoint-overlays))
        (-zip-fill
         nil
         (gethash buffer-file-name (dap--get-breakpoints))
         (-some->> (dap--cur-session) dap--debug-session-breakpoints (gethash buffer-file-name))))
  (save-mark-and-excursion
    (dap-ui--stack-frame-changed (dap--cur-session))))

(defun dap-ui--clear-marker-overlay (&rest _rest)
  "Clear marker overlay for DEBUG-SESSION."
  (--map
   (with-current-buffer it
     (when dap-ui--cursor-overlay
       (delete-overlay dap-ui--cursor-overlay)
       (setq-local dap-ui--cursor-overlay nil)))
   (dap--buffer-list)))

(defun dap-ui--set-debug-marker (file point)
  "Set debug marker for DEBUG-SESSION in FILE at POINT."
  (dap-ui--clear-marker-overlay)
  (setq-local dap-ui--cursor-overlay
              (dap-ui--make-overlay-at
               file point
               "Debug Marker"
               (list :face 'dap-ui-marker-face
                     :char ">"
                     :bitmap 'right-triangle
                     :fringe 'dap-ui-compile-errline
                     :priority 2))))

(defun dap-ui--stack-frame-changed (debug-session)
  "Handler for `dap-stack-frame-changed-hook'.
DEBUG-SESSION is the debug session triggering the event."
  (when debug-session
    (-if-let* (((stack-frame &as &hash "source" "line" "column")
                (dap--debug-session-active-frame debug-session))
               (path (dap--get-path-for-frame stack-frame))
               (buffer (find-buffer-visiting path)))
        (with-current-buffer buffer
          (goto-char (point-min))
          (forward-line (1- line))
          (forward-char column)
          (dap-ui--set-debug-marker path (point)))
      (dap-ui--clear-marker-overlay))))

(defun dap-ui--after-open ()
  "Handler for `lsp-after-open-hook'."
  (dap-ui--refresh-breakpoints)
  (-when-let* ((debug-session (dap--cur-session))
               (path (-some->> debug-session
                               dap--debug-session-active-frame
                               (gethash "source")
                               (gethash "path"))))
    (when (string= buffer-file-name path)
      (dap-ui--stack-frame-changed debug-session))))

;;;###autoload
(define-minor-mode dap-ui-mode
  "Displaying DAP visuals."
  :init-value nil
  :global t
  :require 'dap-ui
  (cond
   (dap-ui-mode
    (add-hook 'dap-breakpoints-changed-hook 'dap-ui--refresh-breakpoints)
    (add-hook 'dap-continue-hook 'dap-ui--clear-marker-overlay)
    (add-hook 'dap-stack-frame-changed-hook 'dap-ui--stack-frame-changed)
    (add-hook 'lsp-after-open-hook 'dap-ui--after-open))
   (t
    (remove-hook 'dap-breakpoints-changed-hook 'dap-ui--refresh-breakpoints)
    (remove-hook 'dap-continue-hook 'dap-ui--clear-marker-overlay)
    (remove-hook 'dap-stack-frame-changed-hook 'dap-ui--stack-frame-changed)
    (remove-hook 'lsp-after-open-hook 'dap-ui--after-open))))

(defun dap-ui-inspect--invalidate (&rest _args)
  "Inspect window invalidated."
  (let ((inhibit-read-only t))
    (with-current-buffer dap-ui--inspect-buffer
      (erase-buffer))))

(defun dap-ui-inspect--cleanup-hooks ()
  "Cleanup after inspect buffer has been killed."
  (remove-hook 'dap-terminated-hook 'dap-ui-inspect--invalidate)
  (remove-hook 'dap-session-changed-hook 'dap-ui-inspect--invalidate)
  (remove-hook 'dap-continue-hook 'dap-ui-inspect--invalidate)
  (remove-hook 'dap-stack-frame-changed-hook 'dap-ui-inspect--invalidate))

(define-minor-mode dap-ui-inspect-mode
  "Inspect mode."
  :init-value nil
  :group dap-ui
  :keymap dap-ui-inspect-mode-map
  (add-hook 'dap-terminated-hook 'dap-ui-inspect--invalidate)
  (add-hook 'dap-session-changed-hook 'dap-ui-inspect--invalidate)
  (add-hook 'dap-continue-hook 'dap-ui-inspect--invalidate)
  (add-hook 'dap-stack-frame-changed-hook 'dap-ui-inspect--invalidate)
  (add-hook 'kill-buffer-hook 'dap-ui-inspect--cleanup-hooks nil t)
  (setq buffer-read-only t))

;; TODO - handle indexed-variables > dap-ui-default-fetch-count.
(defun dap-ui--load-variables (debug-session tree)
  "Method for expanding variables.

TREE will be the root of the threads(session holder).
DEBUG-SESSION is the active debug session."
  (let ((variables-reference (widget-get tree :variables-reference))
        (indexed-variables (widget-get tree :indexed-variables)))
    (when (dap--session-running debug-session)
      (or (widget-get tree :variables)
          (progn (dap--send-message
                  (dap--make-request "variables"
                                     (cl-list* :variablesReference variables-reference
                                               (when (and indexed-variables
                                                          (< dap-ui-default-fetch-count indexed-variables ))
                                                 (list :start 0
                                                       :count dap-ui-default-fetch-count))))
                  (dap--resp-handler
                   (-lambda ((&hash "body" (&hash "variables" variables)))
                     (widget-put tree :variables
                                 (or (-map (apply-partially 'dap-ui--render-variable debug-session)
                                           variables)
                                     (vector)))
                     (tree-mode-reflesh-tree tree)))
                  debug-session)
                 dap-ui--loading-tree-widget)))))

(defun dap-ui--render-eval-result (debug-session variable)
  "Render VARIABLE.
DEBUG-SESSION is the active debug session."
  (-let [(&hash "variablesReference" variables-reference
                "result" result
                "indexedVariables" indexed-variables) variable]
    `(tree-widget
      :node (push-button :format "%[%t%]\n"
                         :tag ,result
                         :button-face dap-ui-inspect-face)
      :open nil
      :variables-reference ,variables-reference
      :indexed-variables ,indexed-variables
      :dynargs ,(when (not (zerop variables-reference))
                  (apply-partially 'dap-ui--load-variables debug-session))
      ,@dap-ui-icons-inspect)))

(defun dap-ui--render-variable (debug-session variable)
  "Render VARIABLE for DEBUG-SESSION."
  (-let* (((&hash "variablesReference" variables-reference
                  "value" value
                  "name" name
                  "indexedVariables" indexed-variables) variable)
          (has-children? (and variables-reference (not (zerop variables-reference))))
          (face (if has-children? 'dap-ui-locals-variable-face 'dap-ui-locals-variable-leaf-face)))
    `(tree-widget
      :node (push-button :format "%[%t%]\n"
                         :tag ,(format "%s=%s" name value)
                         :button-face ,face)
      :open nil
      :indexed-variables ,indexed-variables
      :variables-reference ,variables-reference
      :dynargs ,(when has-children?
                  (apply-partially 'dap-ui--load-variables debug-session))
      ,@dap-ui-icons-variable)))


(defun dap-ui--show-buffer (buf)
  "Show BUF according to defined rules."
  (let ((win (display-buffer-in-side-window buf
                                            (or (-> buf
                                                    buffer-name
                                                    (assoc dap-ui-buffer-configurations)
                                                    cl-rest)
                                                '((side . right)
                                                  (slot . 1)
                                                  (window-width . 0.20))))))
    (set-window-dedicated-p win t)
    (select-window win)))

(defun dap-ui--inspect-value (debug-session value)
  "Inspect VALUE in DEBUG-SESSION."
  (-let ((buf (get-buffer-create dap-ui--inspect-buffer))
         (inhibit-read-only t)
         (workspace lsp--cur-workspace)
         (body (gethash "body" value)))
    (with-current-buffer buf
      (erase-buffer)
      (setq mode-line-format "Inspect"
            lsp--cur-workspace workspace)

      (setq-local tree-widget-themes-directory dap-ui-themes-directory)
      (tree-widget-set-theme dap-ui-theme)

      (widget-create
       (dap-ui--render-eval-result debug-session body))
      (dap-ui-inspect-mode t))
    (dap-ui--show-buffer buf)))

(defun dap-ui-inspect (expression)
  "Inspect EXPRESSION."
  (interactive "sInspect: ")
  (let ((debug-session (dap--cur-active-session-or-die)))
    (if-let ((active-frame-id (-some->> debug-session
                                        dap--debug-session-active-frame
                                        (gethash "id"))))
        (dap--send-message (dap--make-request
                            "evaluate"
                            (list :expression expression
                                  :frameId active-frame-id))
                           (dap--resp-handler (apply-partially 'dap-ui--inspect-value debug-session))
                           debug-session)
      (error "There is no stopped debug session"))))

(defun dap-ui-inspect-thing-at-point ()
  "Inspect thing at point."
  (interactive)
  (dap-ui-inspect (thing-at-point 'symbol)))

(defun dap-ui-inspect-region (start end)
  "Inspect the region between START and END."
  (interactive "r")
  (dap-ui-inspect (buffer-substring-no-properties start end)))

(defun dap-ui-locals--cleanup-hooks ()
  "Cleanup of locals subscriptions."
  (remove-hook 'dap-terminated-hook 'dap-ui-locals--refresh)
  (remove-hook 'dap-session-changed-hook 'dap-ui-locals--refresh)
  (remove-hook 'dap-continue-hook 'dap-ui-locals--refresh)
  (remove-hook 'dap-stack-frame-changed-hook 'dap-ui-locals--refresh))

(define-minor-mode dap-ui-locals-mode
  "Locals mode."
  :init-value nil
  :group dap-ui
  (add-hook 'dap-terminated-hook 'dap-ui-locals--refresh)
  (add-hook 'dap-session-changed-hook 'dap-ui-locals--refresh)
  (add-hook 'dap-continue-hook 'dap-ui-locals--refresh)
  (add-hook 'dap-stack-frame-changed-hook 'dap-ui-locals--refresh)
  (add-hook 'kill-buffer-hook 'dap-ui-locals--cleanup-hooks nil t)
  (setq buffer-read-only t))

(defun dap-ui--render-scope (debug-session request-id scope)
  "Render SCOPE for DEBUG-SESSION.
REQUEST-ID is the active request id. If it doesn't maches the
`dap-ui--locals-request-id' the rendering will be skipped."
  (-let [(&hash "name" name "variablesReference" variables-reference) scope]
    (dap--send-message
     (dap--make-request "variables"
                        (list :variablesReference variables-reference))
     (dap--resp-handler
      (-lambda ((&hash "body" (&hash "variables")))
        (with-current-buffer dap-ui--locals-buffer
          (when (= request-id dap-ui--locals-request-id)
            (widget-create
             `(tree-widget
               :node (push-button :format "%[%t%]\n"
                                  :tag ,name
                                  :button-face dap-ui-locals-scope-face)
               :open t
               :dynargs ,(lambda (_)
                           (or (-map (apply-partially 'dap-ui--render-variable debug-session)
                                     variables)
                               (vector)))
               ,@dap-ui-icons-scope))))))
     debug-session)))

(defun dap-ui-locals--refresh (&rest _)
  "Refresh locals buffer."
  (with-current-buffer dap-ui--locals-buffer
    (setq-local dap-ui--locals-request-id (1+ dap-ui--locals-request-id))
    (let ((inhibit-read-only t)
          (debug-session (dap--cur-session))
          (request-id dap-ui--locals-request-id))
      (erase-buffer)
      (setq mode-line-format "Locals")
      (if (dap--session-running debug-session)
          (if-let (frame-id (-some->> debug-session
                                      dap--debug-session-active-frame
                                      (gethash "id")))
              (dap--send-message (dap--make-request "scopes"
                                                    (list :frameId frame-id))
                                 (dap--resp-handler
                                  (-lambda ((&hash "body" (&hash "scopes")))
                                    (let ((inhibit-read-only t))
                                      (mapc (apply-partially 'dap-ui--render-scope debug-session request-id) scopes))))
                                 debug-session)
            (insert "Thread not stopped..."))
        (insert "Session is not running...")))))

(defun dap-ui-locals ()
  "Display locals view."
  (interactive)
  (-let ((buf (get-buffer-create dap-ui--locals-buffer))
         (inhibit-read-only t)
         (workspace lsp--cur-workspace))
    (with-current-buffer buf
      (setq-local lsp--cur-workspace workspace)
      (setq-local tree-widget-themes-directory dap-ui-themes-directory)
      (tree-widget-set-theme dap-ui-theme)
      (dap-ui-locals-mode t)
      (dap-ui-locals--refresh)
      (dap-ui--show-buffer buf))))

(defun dap-ui--breakpoints-entries ()
  "Get breakpoints entries."
  (let ((id 0)
        result)
    (apply 'append
           (maphash
            (lambda (file-name breakpoints)
              (let ((session-breakpoints (-some->> (dap--cur-session)
                                                   dap--debug-session-breakpoints
                                                   (gethash file-name))))
                (with-temp-buffer
                  (insert-file-contents file-name)
                  (mapc
                   (-lambda (((&plist :point :condition :hit-condition :log-message) . remote-bp))
                     (push `((id ,(setq id (1+ id)))
                             (file-name . (,file-name ,point))
                             (line . ,(line-number-at-pos point))
                             (verified . ,(if (and remote-bp (gethash "verified" remote-bp))
                                              "y"
                                            "n"))
                             (hit-condition . ,hit-condition)
                             (log-message . ,log-message)
                             (condition . ,condition))
                           result))
                   (-zip-fill nil breakpoints session-breakpoints)))))
            (dap--get-breakpoints)))
    (or result (vector))))

(define-button-type 'dap-ui-breakpoint-position
  :supertype 'bui
  'face 'bui-file-name
  'help-echo "Go to breakpoint"
  'action (lambda (btn)
            (find-file (button-get btn 'file))
            (goto-char (button-get btn 'point))))

(defun dap-ui--get-file-info (file-data &optional _)
  "Used to render FILE-DATA in breakpoints' list."
  (list (f-filename (cl-first file-data))
        :type 'dap-ui-breakpoint-position
        'file (cl-first file-data)
        'point (cl-second file-data)))

(bui-define-interface dap-ui-breakpoints-ui list
  :buffer-name "*Breakpoints*"
  :get-entries-function 'dap-ui--breakpoints-entries
  :format '((file-name dap-ui--get-file-info 30 t)
            (line nil 8 bui-list-sort-numerically-2)
            (verified  nil 8 t)
            (condition nil 25 t)
            (hit-condition nil 20 t)
            (log-message nil 15 t))
  :sort-key '(file-name))

(defun dap-ui-breakpoints-goto ()
  "Go to breakpoint under cursor."
  (interactive)
  (--when-let (bui-list-current-entry)
    (-let (((file point) (alist-get 'file-name it)))
      (find-file file)
      (goto-char point))))

(defun dap-ui-breakpoints-delete (breakpoint)
  "Delete BREAKPOINT on the current line."
  (interactive (list (bui-list-current-entry)))
  (-when-let* (((&alist 'file-name (file-name ui-list-point)) breakpoint)
               (file-breakpoints (gethash file-name (dap--get-breakpoints)))
               (existing-breakpoint (cl-find-if
                                     (-lambda ((&plist :point)) (= ui-list-point point))
                                     file-breakpoints)))
    (-some-> existing-breakpoint (plist-get :marker) (set-marker nil))
    (dap--breakpoints-changed (cl-remove existing-breakpoint file-breakpoints) file-name)))

(defun dap-ui-breakpoints-delete-selected ()
  "Delete breakpoint on the current line."
  (interactive)
  (->> (bui-list-get-marked 'general)
       (-map 'cl-first)
       (bui-entries-by-ids (bui-current-entries))
       (-map 'dap-ui-breakpoints-delete)))

(let ((map dap-ui-breakpoints-ui-list-mode-map))
  (define-key map (kbd "RET") 'dap-ui-breakpoints-goto)
  (define-key map (kbd "d") 'dap-ui-breakpoints-delete)
  (define-key map (kbd "D") 'dap-ui-breakpoints-delete-selected))

(defun dap-ui-refresh-breakpoints-list ()
  "Refresh breakpoints' list."
  (with-current-buffer "*Breakpoints*"
    (let ((workspace lsp--cur-workspace))
      (bui-revert nil t)
      (setq-local lsp--cur-workspace workspace)
      (run-hooks 'dap-ui-breakpoints-ui-list-displayed-hook))))

(defun dap-ui--brekapoints-list-cleanup ()
  "Cleanup when buffer list has been deleted."
  (remove-hook 'dap-breakpoints-changed-hook 'dap-ui-refresh-breakpoints-list))

(add-hook
 'dap-ui-breakpoints-ui-list-mode-hook
 (lambda ()
   (add-hook 'dap-breakpoints-changed-hook 'dap-ui-refresh-breakpoints-list)
   (add-hook 'kill-buffer-hook 'dap-ui--brekapoints-list-cleanup nil t)))

(defun dap-ui-breakpoints ()
  "List breakpoints."
  (interactive)
  (let ((workspaces lsp--buffer-workspaces))
    (bui-get-display-entries 'dap-ui-breakpoints-ui 'list)
    (setq-local lsp--buffer-workspaces workspaces)
    (add-hook 'bui-after-redisplay-hook
              (lambda () (setq-local lsp--buffer-workspaces workspaces)))
    (run-hooks 'dap-ui-breakpoints-ui-list-displayed-hook)))


;; dap-ui posframe stuff
(defvar dap-ui--control-images-root-dir (f-join (f-dirname (or load-file-name buffer-file-name)) "icons/vscode"))
(defvar dap-ui--control-buffer " *dap-ui*")

(defun dap-ui--create-command (image command hover-text)
  (propertize "    "
              'display `(image :type png
                               :file ,(f-join dap-ui--control-images-root-dir image)
                               :ascent center
                               :background ,(face-attribute 'mode-line :background nil t))
              'local-map (--doto (make-sparse-keymap)
                           (define-key it [mouse-1] command))
              'help-echo hover-text))

(declare-function posframe-show "posframe")
(declare-function posframe-hide "posframe")

(defun dap-ui--update-controls (&rest _)
  (let* ((session (dap--cur-session))
         (stopped? (and session (dap--debug-session-active-frame session)))
         (running? (and session (dap--session-running session))))
    (if running?
        (let ((content (s-concat
                        (dap-ui--create-command "continue.png" #'dap-continue "Continue")
                        (dap-ui--create-command (if stopped?
                                                          "step-over.png"
                                                        "step-over-disabled.png")
                                                      (when stopped? #'dap-next)
                                                      (if stopped? "Step over"
                                                        "Session not stopped?"))
                        (dap-ui--create-command (if stopped? "step-out.png"
                                                        "step-out-disabled.png")
                                                      (when stopped? #'dap-step-out)
                                                      (if stopped? "Step out"
                                                        "Session not stopped? "))
                        (dap-ui--create-command (if stopped? "step-into.png"
                                                        "step-into-disabled.png")
                                                      (when stopped? #'dap-step-in)
                                                      (if stopped? "Step in"
                                                        "Session not stopped?"))
                        (dap-ui--create-command "disconnect.png" #'dap-disconnect "Disconnect")
                        (dap-ui--create-command "restart.png" #'dap-debug-restart "Restart")))
              (posframe-mouse-banish nil)
              (pos-frame (-first
                          (lambda (frame)
                            (let ((buffer-info (frame-parameter frame 'posframe-buffer)))
                              (or (equal dap-ui--control-buffer (car buffer-info))
                                  (equal dap-ui--control-buffer (cdr buffer-info)))))
                          (frame-list))))
          (when (eq (selected-frame) pos-frame)
            (select-frame (frame-parent pos-frame)))
          (posframe-show dap-ui--control-buffer
                         :string content
                         :poshandler #'posframe-poshandler-frame-top-center))
      (posframe-hide dap-ui--control-buffer))))

;;;###autoload
(define-minor-mode dap-ui-controls-mode
  "Displaying DAP visuals."
  :init-value nil
  :global t
  :require 'dap-ui
  (cond
   (dap-ui-mode
    (add-hook 'dap-session-changed-hook 'dap-ui--update-controls)
    (add-hook 'dap-terminated-hook 'dap-ui--update-controls )
    (add-hook 'dap-session-changed-hook 'dap-ui--update-controls)
    (add-hook 'dap-continue-hook 'dap-ui--update-controls)
    (add-hook 'dap-stack-frame-changed-hook 'dap-ui--update-controls))
   (t
    (remove-hook 'dap-session-changed-hook 'dap-ui--update-controls)
    (remove-hook 'dap-terminated-hook 'dap-ui--update-controls )
    (remove-hook 'dap-session-changed-hook 'dap-ui--update-controls)
    (remove-hook 'dap-continue-hook 'dap-ui--update-controls)
    (remove-hook 'dap-stack-frame-changed-hook 'dap-ui--update-controls))))



(defun dap-ui-debug-sessions-send ()
  "Send current selection for evaluation to the DAP server."
  (interactive))

(provide 'dap-ui)
;;; dap-ui.el ends here
