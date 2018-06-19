;;; dap-ui.el --- Debug Adapter Protocol for Emacs UI  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ivan

;; Author: Ivan <kyoncho@myoncho>
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

;;; Commentary:

;;

;;; Code:

(require 'dap-mode)
(require 'tree-widget)
(require 'wid-edit)
(require 'dash)

(defcustom dap-ui-stack-frames-loaded nil
  "Stack frames loaded."
  :type 'hook
  :group 'dap-ui)

(defface dap-ui-breakpoint-face
  '((t ()))
  "Face used for marking lines with breakpoints."
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
  '((t (:inherit hl-line-face)))
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

(defcustom dap-left-margin-gutter t
  "If non-nil, DAP UI will show the compilation and warning icons
in the left margin, when in terminal mode. These icons can
interfere with other modes that use the left-margin. (git-gutter,
linum, etc..)"
  :type 'boolean
  :group 'dap-ui)

(defconst dap-ui--loading-tree-widget
  (list '(tree-widget :tag "Loading..." :format "%[%t%]\n")))

(defun dap-ui-sessions-stack-frame ()
  "XX"
  (interactive)
  (let ((tree (get-char-property (point) 'button)))
    (dap--go-to-stack-frame
     (widget-get tree :stack-frame)
     (widget-get tree :session))))

(defun dap-ui-sessions-thread ()
  "XX"
  (interactive)
  (let* ((tree (get-char-property (point) 'button))
         (session (widget-get tree :session))
         (thread (widget-get tree :thread))
         (thread-id (gethash "id" thread)))
    (dap--select-thread-id session thread-id)))

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
                                       "source" (&hash "name" source-name)))
                  `(tree-widget :tag ,(format "%s (%s:%s)" name source-name line)
                                :format "%[%t%]\n"
                                :stack-frame ,stack-frame
                                :session ,session
                                :element-type :stack-frame
                                :thread ,thread
                                :dynargs dap-ui--stack-frames
                                :open nil))
                stack-frames)

      (dap--send-message (dap--make-request "stackTrace"
                                            (list :threadId thread-id))
                         (lambda (stack-frames-resp)
                           (let ((stack-frames (or (gethash "stackFrames"
                                                            (gethash "body" stack-frames-resp))
                                                   (vector))))

                             (puthash thread-id
                                      stack-frames
                                      (dap--debug-session-thread-stack-frames session))

                             (tree-mode-reflesh-tree thread-tree)
                             (run-hook-with-args 'dap-ui-stack-frames-loaded session stack-frames)))
                         session)
      dap-ui--loading-tree-widget)))

(defun dap-ui--load-threads (session-tree)
  "Method for expanding threads.

SESSION-TREE will be the root of the threads(session holder)."
  (let ((debug-session (widget-get session-tree :session)))
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
                      :open nil)))
                threads)
      (dap--send-message
       (dap--make-request "threads")
       (lambda (threads-resp)
         (let ((threads (gethash "threads" (gethash "body" threads-resp))))

           (setf (dap--debug-session-threads debug-session) threads)

           (tree-mode-reflesh-tree session-tree)
           (run-hook-with-args 'dap-ui-stack-frames-loaded
                               debug-session
                               threads)))
       debug-session)
      dap-ui--loading-tree-widget)))


(defvar dap-ui-session-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "TAB") #'tree-mode-toggle-expand)
    (define-key map (kbd "TAB") #'tree-mode-toggle-expand)
    map))

(define-minor-mode dap-ui-sessions-mode
  "UI Session list minor mode."
  :init-value nil
  :group dap-ui
  :keymap dap-ui-session-mode-map
  (cond
   (dap-ui-sessions-mode
    (read-only-mode t))
   (t)))

;;;###autoload
(defun dap-ui-list-sessions ()
  "Show currently active sessions and it's threads."
  (interactive)
  (lsp--cur-workspace-check)
  (let ((sessions (reverse (lsp-workspace-get-metadata "debug-sessions")))
        (buf (get-buffer-create "*sessions*")))
    (with-current-buffer buf
      (erase-buffer)
      (mapc
       (lambda (session)
         (widget-create
          `(tree-widget
            :node (push-button :format "%[%t%]\n"
                               :tag ,(process-name (dap--debug-session-proc session)))
            :open nil
            :session ,session
            :dynargs dap-ui--load-threads)))
       sessions)
      (dap-ui-sessions-mode t))
    (pop-to-buffer buf)))

(defun dap--internalize-offset (offset)
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

(defun dap--before-string (sign face)
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
    (overlay-put ov 'before-string (dap--before-string sign face))))

(defun dap--make-overlay (beg end tooltip-text visuals &optional mouse-face buf)
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
              (setq beg (dap--internalize-offset beg))
              (setq end (dap--internalize-offset end)))
          ;; If line provided, use line to define region
          (save-excursion
            (goto-char point)
            (setq beg (point-at-bol))
            (setq end (point-at-eol)))))

      (dap--make-overlay beg end msg visuals nil buf))))

(defvar-local dap-ui--breakpoint-overlays '())

(defvar-local dap-ui--cursor-overlay '())

(defun dap-ui--clear-breakpoint-overlays ()
  "Remove all overlays that ensime-debug has created."
  (mapc #'delete-overlay dap-ui--breakpoint-overlays)
  (setq dap-ui--breakpoint-overlays '()))

(defun dap-ui--breakpoints-changed (_debug-session file-name breakpoints)
  "Handler for breakpoints changed.

FILE-NAME the name in which the breakpoints has changed.
BREAKPOINTS list of the active breakpoints."

  (dap-ui--refresh-breakpoints file-name breakpoints))

(defun dap-ui--breakpoint-visuals (breakpoint)
  "Calculate visuals for BREAKPOINT."
  (cond
   ((plist-get breakpoint :verified)
    (list :face 'dap-ui-verified-breakpoint-face
          :char "."
          :bitmap 'breakpoint
          :fringe 'dap-ui-breakpoint-verified-fringe))
   (t
    (list :face 'dap-ui-pending-breakpoint-face
          :char "."
          :bitmap 'breakpoint
          :fringe 'breakpoint-disabled))))

(defun dap-ui--refresh-breakpoints (file bps)
  "Refresh all breakpoints in FILE.

BPS the new breakpoints for FILE."
  (when (string= file buffer-file-name)
    (dap-ui--clear-breakpoint-overlays)
    (dolist (bp bps)
      (-when-let (ov (dap-ui--make-overlay-at
                      file
                      (dap-breakpoint-get-point bp)
                      nil nil
                      "Breakpoint"
                      (dap-ui--breakpoint-visuals bp)))
        (push ov dap-ui--breakpoint-overlays)))))

(defun dap-ui--clear-marker-overlay (debug-session)
  "DEBUG-SESSION."
  (--map
   (with-current-buffer it
     (when dap-ui--cursor-overlay
       (delete-overlay dap-ui--cursor-overlay)
       (setq-local dap-ui--cursor-overlay nil)))
   (lsp--workspace-buffers (dap--debug-session-workspace debug-session))))

(defface dap-ui-compile-errline
  '((t (:inherit compilation-error)))
  "Face used for marking the line on which an error occurs."
  :group 'dap-ui)

(defun dap-ui--set-debug-marker (debug-session file point)
  (dap-ui--clear-marker-overlay debug-session)

  (setq-local dap-ui--cursor-overlay
              (dap-ui--make-overlay-at
               file point nil nil
               "Debug Marker"
               (list :face 'dap-ui-marker-face
                     :char ">"
                     :bitmap 'right-triangle
                     :fringe 'dap-ui-compile-errline))))

(defun dap-ui--terminated (debug-session)
  "DEBUG-SESSION."
  (maphash (lambda (file-name breakpoints)
             (dap-ui--breakpoints-changed debug-session file-name breakpoints))
           (dap--get-breakpoints (dap--debug-session-workspace debug-session))))

(defun dap-ui--stack-frame-changed (debug-session)
  "Handler for `dap-stack-frame-changed-hook'.
DEBUG-SESSION is the debug session triggering the event."
  (-when-let ((&hash "source" (&hash "path" path)
                     "line" line
                     "column" column) (dap--debug-session-active-frame debug-session))
    (goto-char (point-min))
    (forward-line (1- line))
    (forward-char column)
    (dap-ui--set-debug-marker debug-session
                              (lsp--uri-to-path path)
                              (point))))

;;;###autoload
(define-minor-mode dap-ui-mode
  "Displaying DAP visuals."
  :init-value nil
  :group dap-ui
  (cond
   (dap-ui-mode
    (add-hook 'dap-breakpoints-changed-hook 'dap-ui--breakpoints-changed)
    (add-hook 'dap-terminated-hook 'dap-ui--terminated)
    (add-hook 'dap-continue-hook 'dap-ui--clear-marker-overlay)
    (add-hook 'dap-stack-frame-changed-hook 'dap-ui--stack-frame-changed)
    (when-let (breakpoints (dap--active-get-breakpoints))
      (dap-ui--breakpoints-changed (dap--cur-session)
                                   buffer-file-name
                                   breakpoints))

    (when (dap--cur-session)
      (-let [(&hash "source" (&hash "path" path)) (dap--debug-session-active-frame (dap--cur-session))]
        (when (string= buffer-file-name path)
          (dap-ui--stack-frame-changed (dap--cur-session))))))
   (t
    (remove-hook 'dap-breakpoints-changed-hook 'dap-ui--breakpoints-changed)
    (remove-hook 'dap-continue-hook 'dap-ui--clear-marker-overlay)
    (remove-hook 'dap-terminated-hook 'dap-ui--terminated)
    (remove-hook 'dap-stack-frame-changed-hook 'dap-ui--stack-frame-changed))))

(provide 'dap-ui)
;;; dap-ui.el ends here
