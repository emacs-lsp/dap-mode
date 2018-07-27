;;; dap-ui.el --- Debug Adapter Protocol for Emacs UI  -*- lexical-binding: t; -*-


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

;;; Commentary:

;;

;;; Code:
(require 'dap-mode)
(require 'tree-widget)
(require 'wid-edit)
(require 'dash)
(require 'bui)

(defcustom dap-ui-stack-frames-loaded nil
  "Stack frames loaded."
  :type 'hook
  :group 'dap-ui)

(defface dap-ui-compile-errline
  '((t (:inherit compilation-error)))
  "Face used for marking the line on which an error occurs."
  :group 'dap-ui)

(defface dap-ui-sessions-active-session-face
  '((t :inherit bold))
  "Face used for marking current session in sessions list."
  :group 'dap-ui)

(defface dap-ui-sessions-terminated-face
  '((t :inherit italic))
  "Face used for marking terminated session."
  :group 'dap-ui)

(defface dap-ui-sessions-running-face
  '((t :inherit default))
  "Face used for marking terminated session."
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

(defcustom dap-left-margin-gutter t
  "If non-nil, DAP UI will show the compilation and warning icons
in the left margin, when in terminal mode. These icons can
interfere with other modes that use the left-margin. (git-gutter,
linum, etc..)"
  :type 'boolean
  :group 'dap-ui)

(defconst dap-ui--loading-tree-widget
  (list '(tree-widget :tag "Loading..." :format "%[%t%]\n")))

(defconst dap-ui--locals-buffer "*dap-ui-locals*")

(defconst dap-ui--sessions-buffer "*dap-ui-sessions*")

(defconst dap-ui--inspect-buffer "*dap-ui-inspect*")

(defconst dap-ui--brekapoint-priority 200)

;; define debug marker priority so it will be over the breakpoint marker if
;; there is such on the current line.
(defconst dap-ui--marker-priority 300)

(defvar dap-ui-buffer-configurations
  `((,dap-ui--locals-buffer . ((side . right)
                          (slot . 1)
                          (window-width . 0.20)))
    (,dap-ui--inspect-buffer . ((side . right)
                           (slot . 2)
                           (window-width . 0.20)))
    (,dap-ui--sessions-buffer . ((side . right)
                            (slot . 3)
                            (window-width . 0.20)))))

(defvar dap-ui--sessions-refresh-timer nil)

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
      (-let [session (widget-get widget :session)]
        (case (widget-get widget :element-type)
          (:session (dap--switch-to-session session))
          (:thread (-let ((thread  (widget-get widget :thread)))
                     (setf (dap--debug-session-thread-id session) (gethash "id" thread) )
                     (dap--switch-to-session session)
                     (dap--select-thread-id session (gethash "id" thread))))
          (:stack-frame (-let ((thread (widget-get widget :thread))
                               (stack-frame (widget-get widget :stack-frame)))

                          (setf (dap--debug-session-thread-id session) (gethash "id" thread) )
                          (setf (dap--debug-session-active-frame session) stack-frame)
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
        (mapcar (-lambda ((stack-frame &as
                                       &hash
                                       "name" name
                                       "line" line
                                       "source" source))
                  (let ((tag (if source
                                 (format "%s (%s:%s)" name (gethash "name" source) line)
                               (format "%s (Unknown source)" name))))
                    `(tree-widget :tag ,tag
                                  :format "%[%t%]\n"
                                  :stack-frame ,stack-frame
                                  :session ,session
                                  :element-type :stack-frame
                                  :thread ,thread
                                  :dynargs dap-ui--stack-frames
                                  :open nil)))
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

;;;###autoload
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
          (mapcar (-lambda ((thread &as &hash "name" name "id" thread-id))
                    (-let [label (-if-let (status (gethash
                                                   thread-id
                                                   (dap--debug-session-thread-states debug-session)))
                                     (format "%s (%s)" name status)
                                   name)]
                      `(tree-widget
                        :node (push-button :tag ,label
                                           :format "%[%t%]\n")
                        :thread ,thread
                        :session ,debug-session
                        :dynargs dap-ui--stack-frames
                        :element-type :thread
                        :open t)))
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
   (t 'dap-ui-pending-breakpoint-face)))

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
                :button-face ,(dap-ui-session--calculate-face session)))

(defun dap-ui-sessions--render-session (session)
  "Render SESSION."
  (widget-create
   (if (dap--session-running session)
       `(tree-widget
         :node ,(dap-ui-sessions--render-session-node session)
         :open nil
         :session ,session
         :element-type :session
         :dynargs dap-ui--load-threads)
     `(tree-widget
       :node ,(dap-ui-sessions--render-session-node session)
       :open t
       :open-icon tree-widget-leaf-icon
       :close-icon tree-widget-leaf-icon
       :empty-icon tree-widget-leaf-icon
       :element-type :session
       :session ,session))))

(defun dap-ui-sessions--refresh (&rest _args)
  "Refresh ressions view."
  (cancel-timer dap-ui--sessions-refresh-timer )
  (setq dap-ui--sessions-refresh-timer nil)
  (with-current-buffer (get-buffer-create dap-ui--sessions-buffer)
    (let ((debug-sessions (dap--get-sessions lsp--cur-workspace))
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
  (when (not dap-ui--sessions-refresh-timer)
    (setq dap-ui--sessions-refresh-timer (run-at-time 0.5 nil 'dap-ui-sessions--refresh))))

;;;###autoload
(defun dap-ui-sessions ()
  "Show currently active sessions."
  (interactive)
  (lsp--cur-workspace-check)
  (let ((sessions (reverse (lsp-workspace-get-metadata "debug-sessions")))
        (buf (get-buffer-create dap-ui--sessions-buffer))
        (inhibit-read-only t)
        (workspace lsp--cur-workspace))
    (with-current-buffer buf
      (erase-buffer)
      (setq mode-line-format "Sessions")
      (setq-local lsp--cur-workspace workspace)
      (mapc 'dap-ui-sessions--render-session sessions)
      (dap-ui-sessions-mode t))
    (dap-ui--show-buffer buf)))

(defun dap-ui--internalize-offset (offset)
  "Internalize OFFSET."
  (if (eq 1 (coding-system-eol-type buffer-file-coding-system))
      (save-excursion
        (save-restriction
          (widen)
          (block nil
            (when (<= offset 0) (return 1))
            (when (>= offset (dap--externalize-offset (point-max)))
              (return (point-max)))

            (goto-char offset)
            (while t
              (let* ((diff (- (dap--externalize-offset (point)) offset))
                     (step (/ (abs diff) 2)))
                (cond
                 ((eql diff 0) (return (point)))

                 ;; Treat -1 and +1 specially: if offset matches a CR character
                 ;; we want to avoid an infinite loop
                 ((eql diff -1) (if (eql (char-after (point)) ?\n)
                                    (return (point))
                                  (return (1+ (point)))))
                 ((eql diff 1)  (return (1- (point))))

                 ((> diff 0) (backward-char step))
                 ((< diff 0) (forward-char step))))))))
    (+ offset 1)))

(defun dap-ui--before-string (sign face)
  (propertize " "
              'display
              `((margin left-margin)
                ,(propertize sign 'face
                             (face-remap-add-relative face
                                                      :underline nil
                                                      :weight 'normal
                                                      :slant 'normal)))))

(defun dap--show-sign-overlay (sign face ov)
  (save-excursion
    (overlay-put ov 'before-string (dap-ui--before-string sign face))))

(defun dap-ui--make-overlay (beg end tooltip-text visuals &optional mouse-face buf)
  "Allocate a DAP UI overlay in range BEG and END."
  (let ((ov (make-overlay beg end buf t t)))
    (overlay-put ov 'face           (plist-get visuals :face))
    (overlay-put ov 'mouse-face     mouse-face)
    (overlay-put ov 'help-echo      tooltip-text)
    (overlay-put ov 'dap-ui-overlay  t)
    (overlay-put ov 'priority 100)
    (let ((char (plist-get visuals :char)))
      (if (window-system)
          (when char
            (overlay-put ov 'before-string
                         (propertize char
                                     'display
                                     (list 'left-fringe
                                           (plist-get visuals :bitmap)
                                           (plist-get visuals :fringe)))))
        (when (and char dap-left-margin-gutter)
          (dap--show-sign-overlay char (plist-get visuals :fringe) ov))))
    ov))

(defun dap-ui--make-overlay-at (file point b e msg visuals)
  "Create an overlay highlighting the given line in any buffer visiting the given file."
  (let ((beg b)
        (end e))
    (assert (and
             file
             (or (integerp point)
                 (and (integerp beg)
                      (integerp end)))))
    (-when-let (buf (find-buffer-visiting file))
      (with-current-buffer buf
        (if (and (integerp beg) (integerp end))
            (progn
              (setq beg (dap-ui--internalize-offset beg))
              (setq end (dap-ui--internalize-offset end)))
          ;; If line provided, use line to define region
          (save-excursion
            (goto-char point)
            (setq beg (point-at-bol))
            (setq end (point-at-eol)))))

      (dap-ui--make-overlay beg end msg visuals nil buf))))

(defvar-local dap-ui--breakpoint-overlays '())

(defvar-local dap-ui--cursor-overlay '())

(defun dap-ui--clear-breakpoint-overlays ()
  "Remove all overlays that ensime-debug has created."
  (mapc #'delete-overlay dap-ui--breakpoint-overlays)
  (setq dap-ui--breakpoint-overlays '()))

(defun dap-ui--breakpoint-visuals (breakpoint-dap)
  "Calculate visuals for a breakpoint based on the data comming from DAP server.
BREAKPOINT-DAP - nil or the data comming from DAP."
  (cond
   ((and breakpoint-dap (gethash "verified" breakpoint-dap))
    (list :face 'dap-ui-verified-breakpoint-face
          :char "."
          :bitmap 'breakpoint
          :fringe 'dap-ui-breakpoint-verified-fringe
          :priority 'dap-ui--brekapoint-priority))
   (t
    (list :face 'dap-ui-pending-breakpoint-face
          :char "."
          :bitmap 'breakpoint
          :fringe 'breakpoint-disabled
          :priority dap-ui--brekapoint-priority))))

(defun dap-ui--refresh-breakpoints ()
  "Refresh breakpoints in FILE-NAME.
DEBUG-SESSION the new breakpoints for FILE-NAME."
  (dap-ui--clear-breakpoint-overlays)
  (-map (-lambda ((bp . remote-bp))
          (push (dap-ui--make-overlay-at buffer-file-name
                                    (dap-breakpoint-get-point bp)
                                    nil nil
                                    "Breakpoint"
                                    (dap-ui--breakpoint-visuals remote-bp))
                dap-ui--breakpoint-overlays))
        (-zip-fill
         nil
         (->> lsp--cur-workspace dap--get-breakpoints (gethash buffer-file-name))
         (-some->> (dap--cur-session) dap--debug-session-breakpoints (gethash buffer-file-name)))))

(defun dap-ui--clear-marker-overlay (debug-session)
  "Clear marker overlay for DEBUG-SESSION."
  (--map
   (with-current-buffer it
     (when dap-ui--cursor-overlay
       (delete-overlay dap-ui--cursor-overlay)
       (setq-local dap-ui--cursor-overlay nil)))
   (lsp--workspace-buffers (dap--debug-session-workspace debug-session))))

(defun dap-ui--set-debug-marker (debug-session file point)
  "Set debug marker for DEBUG-SESSION in FILE at POINT."
  (dap-ui--clear-marker-overlay debug-session)

  (setq-local dap-ui--cursor-overlay
              (dap-ui--make-overlay-at
               file point nil nil
               "Debug Marker"
               (list :face 'dap-ui-marker-face
                     :char ">"
                     :bitmap 'right-triangle
                     :fringe 'dap-ui-compile-errline
                     :priority 'dap-ui--marker-priority))))

(defun dap-ui--stack-frame-changed (debug-session)
  "Handler for `dap-stack-frame-changed-hook'.
DEBUG-SESSION is the debug session triggering the event."
  (-when-let ((&hash "source" source
                     "line" line
                     "column" column) (dap--debug-session-active-frame debug-session))
    (when source
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char column)
      (dap-ui--set-debug-marker debug-session
                           (lsp--uri-to-path (gethash "path" source))
                           (point)))))

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
                                    (list* :variablesReference variables-reference
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
                         :tag ,result)
      :open nil
      :variables-reference ,variables-reference
      :indexed-variables ,indexed-variables
      :dynargs ,(when (not (zerop variables-reference))
                  (apply-partially 'dap-ui--load-variables debug-session)))))

(defun dap-ui--render-variable (debug-session variable)
  "Render VARIABLE for DEBUG-SESSION."
  (-let [(&hash "variablesReference" variables-reference
                "value" value
                "name" name
                "indexedVariables" indexed-variables) variable]
    `(tree-widget
      :node (push-button :format "%[%t%]\n"
                         :tag ,(format "%s=%s" name value))
      :open nil
      :indexed-variables ,indexed-variables
      :variables-reference ,variables-reference
      :dynargs ,(when (not (zerop variables-reference))
                  (apply-partially 'dap-ui--load-variables debug-session)))))


(defun dap-ui--show-buffer (buf)
  "Show BUF according to defined rules."
  (let ((win (display-buffer-in-side-window buf
                                            (or (-> buf
                                                    buffer-name
                                                    (assoc dap-ui-buffer-configurations)
                                                    rest)
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
      (setq mode-line-format "Inspect")
      (setq lsp--cur-workspace workspace)

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

(defun dap-ui--render-scope (debug-session scope)
  "Render SCOPE for DEBUG-SESSION."
  (-let [(&hash "name" name "variablesReference" variables-reference) scope]
    (dap--send-message
     (dap--make-request "variables"
                       (list :variablesReference variables-reference))
     (dap--resp-handler
      (-lambda ((&hash "body" (&hash "variables" variables)))
        (with-current-buffer dap-ui--locals-buffer
          (widget-create
           `(tree-widget
             :node (push-button :format "%[%t%]\n"
                                :tag ,name)
             :open t
             :dynargs ,(lambda (_)
                         (or (-map (apply-partially 'dap-ui--render-variable debug-session)
                                   variables)
                             (vector))))))))
     debug-session)))

(defun dap-ui-locals--refresh (&rest _)
  "Refresh locals buffer."
  (with-current-buffer dap-ui--locals-buffer
    (let ((inhibit-read-only t)
          (debug-session (dap--cur-session)))
      (erase-buffer)
      (setq mode-line-format "Locals")
      (if (dap--session-running debug-session)
          (if-let (frame-id (-some->> debug-session
                                      dap--debug-session-active-frame
                                      (gethash "id")))
              (dap--send-message (dap--make-request "scopes"
                                                  (list :frameId frame-id))
                                (dap--resp-handler
                                 (-lambda ((&hash "body" (&hash "scopes" scopes)))
                                   (mapc (apply-partially 'dap-ui--render-scope debug-session) scopes)))
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
      (dap-ui-locals-mode t)
      (dap-ui-locals--refresh)
      (dap-ui--show-buffer buf))))

(defun dap-ui--breakpoints-entries ()
  "Get breakpoints entries."
  (let (result)
    (apply 'append
           (maphash
            (lambda (file-name breakpoints)
              (mapc
               (-lambda ((bkp . remote-bp))
                 (let ((point (plist-get bkp :point)))
                   (push `((file-name . (,file-name ,point))
                           (line . ,(line-number-at-pos point))
                           (verified . ,(if (and remote-bp (gethash "verified" remote-bp))
                                            "y"
                                          "n")))
                         result)))
               (-zip-fill nil
                          breakpoints
                          (-some->> (dap--cur-session)
                                    dap--debug-session-breakpoints
                                    (gethash file-name)))))
            (dap--get-breakpoints lsp--cur-workspace)))
    result))

(define-button-type 'dap-ui-breakpoint-position
  :supertype 'bui
  'face 'bui-file-name
  'help-echo "Go to breakpoint"
  'action (lambda (btn)
            (find-file (button-get btn 'file))
            (goto-char (button-get btn 'point))))

(defun dap-ui--get-file-info (file-data &optional _)
  "TODO."
  (list (f-filename (first file-data))
        :type 'dap-ui-breakpoint-position
        'file (first file-data)
        'point (second file-data)))

(bui-define-interface dap-ui-breakpoints-ui-3 list
  :buffer-name "*Breakpoints*"
  :get-entries-function 'dap-ui--breakpoints-entries
  :format '((file-name dap-ui--get-file-info 30 t)
            (line nil 8 bui-list-sort-numerically-2)
            (verified  nil 8 t)
            (condition nil 25 t)
            (hit-count nil 8 bui-list-sort-numerically-2 :right-align t))
  :sort-key '(file-name))

(defun dap-ui-breakpoints ()
  "List breakpoints."
  (interactive)
  (lsp--cur-workspace-check)
  (let ((workspace lsp--cur-workspace))
    (bui-get-display-entries 'dap-ui-breakpoints-ui-3 'list)
    (setq-local lsp--cur-workspace workspace)))

(provide 'dap-ui)
;;; dap-ui.el ends here
