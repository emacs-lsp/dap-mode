;;; dap-mouse.el --- dap-mode mouse integration      -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
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

;;

;;; Code:

(require 'dap-mode)
(require 'dap-ui)
(require 'lsp-mode)
(require 'lsp-treemacs)
(require 'tooltip)

(defface  dap-mouse-eval-thing-face
  '((((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "#2aa1ae")))
  "Face used to display evaluation results at the end of line.
If `dap-overlays-use-font-lock' is non-nil, this face is
applied with lower priority than the syntax highlighting."
  :group 'dap
  :package-version '(dap "0.9.1"))

(defvar dap-mouse--hide-timer nil)

(defvar dap-mouse-posframe-properties
  (list :min-width 50
        :internal-border-width 2
        :internal-border-color (face-attribute 'tooltip :background)
        :width 50
        :min-height 10)
  "The properties which will be used for creating the `posframe'.")

(defconst dap-mouse-buffer "*dap-mouse*"
  "Buffer name for `dap-mouse'.")

(defun dap-mouse--hide-popup? ()
  (let ((buffer-under-mouse (window-buffer (cl-first (window-list (cl-first (mouse-position))))))
        (popup-buffer (get-buffer dap-mouse-buffer)))
    (not (or (and (eq (current-buffer) popup-buffer)
                  (eq buffer-under-mouse popup-buffer))
             (eq buffer-under-mouse popup-buffer)))))

(defcustom dap-mouse-popup-timeout 0.3
  "The time to wait after command before hiding the popup."
  :type 'float
  :group 'dap-mouse)

;;;###autoload
(define-minor-mode dap-tooltip-mode
  "Toggle the display of GUD tooltips."
  :global t
  :group 'dap-mouse
  :group 'tooltip (require 'tooltip)
  (cond
   (dap-tooltip-mode
    (add-hook 'tooltip-functions 'dap-tooltip-tips)
    (add-hook 'lsp-mode-hook 'dap-tooltip-update-mouse-motions-if-enabled)
    (define-key lsp-mode-map [mouse-movement] 'dap-tooltip-mouse-motion))
   ((not tooltip-mode)
    (remove-hook 'tooltip-functions 'dap-tooltip-tips)
    (define-key lsp-mode-map  [mouse-movement] 'ignore)
    (remove-hook 'lsp-mode-hook 'dap-tooltip-update-mouse-motions-if-enabled)))
  (dap-tooltip-update-mouse-motions-if-enabled))

(defcustom dap-tooltip-echo-area nil
  "Use the echo area instead of frames for DAP tooltips."
  :type 'boolean
  :group 'dap-mouse
  :group 'tooltip)

;;; Reacting on mouse movements

(defun dap-tooltip-update-mouse-motions-if-enabled ()
  "Reconsider for all buffers whether mouse motion events are desired."
  (remove-hook 'post-command-hook
               'dap-tooltip-update-mouse-motions-if-enabled)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if (and dap-tooltip-mode lsp-mode)
          (dap-tooltip-activate-mouse-motions t)
        (dap-tooltip-activate-mouse-motions nil)))))

(defvar dap-tooltip-mouse-motions-active nil
  "Locally t in a buffer if tooltip processing of mouse motion is enabled.")

(defun dap-tooltip-activate-mouse-motions (activatep)
  "Activate/deactivate mouse motion events for the current buffer.
ACTIVATEP non-nil means activate mouse motion events."
  (if activatep
      (progn
        (set (make-local-variable 'dap-tooltip-mouse-motions-active) t)
        (set (make-local-variable 'track-mouse) t))
    (when dap-tooltip-mouse-motions-active
      (kill-local-variable 'dap-tooltip-mouse-motions-active)
      (kill-local-variable 'track-mouse))))

(defun dap-tooltip-mouse-motion (event)
  "Command handler for mouse movement events in `dap-mode-map'.
EVENT is the last mouse movement event."
  (interactive "e")
  (tooltip-hide)
  (when (car (mouse-pixel-position))
    (tooltip-start-delayed-tip)
    (setq tooltip-last-mouse-motion-event event)))

(defun dap-tooltip-thing-bounds (point)
  "Return the thing at POINT that will be introspected.
If there is an active selection - return it."
  (if (and (region-active-p)
           (< (region-beginning) point (region-end)))
      (cons (region-beginning) (region-end))
    (save-excursion
      (goto-char point)
      (bounds-of-thing-at-point 'symbol))))

(defvar-local dap--tooltip-overlay nil)
(defun dap-tooltip-post-tooltip ()
  "Clean tooltip properties."

  (when dap-mouse--hide-timer
    (cancel-timer dap-mouse--hide-timer))
  (when (dap-mouse--hide-popup?)
    (setq
     dap-mouse--hide-timer
     (run-at-time
      dap-mouse-popup-timeout nil
      (lambda ()
        (when (dap-mouse--hide-popup?)
          (posframe-hide dap-mouse-buffer)
          (when dap--tooltip-overlay
            (delete-overlay dap--tooltip-overlay)
            ;; restore the selection
            (when (region-active-p)
              (let ((start (overlay-start dap--tooltip-overlay))
                    (end (overlay-end dap--tooltip-overlay)))
                (run-with-idle-timer
                 0.0
                 nil
                 (lambda ()
                   (let ((point (point)))
                     (push-mark start t t)
                     (goto-char end)
                     (unless (= point (point))
                       (exchange-point-and-mark))))))))
          (setq dap-mouse--hide-timer nil)
          (remove-hook 'post-command-hook #'dap-tooltip-post-tooltip)))))))

(defun dap-tooltip-at-point (&optional pos)
  "Show information about the variable under point.
The result is displayed in a `treemacs' `posframe'. POS,
defaulting to `point', specifies where the cursor is and
consequently where to show the `posframe'."
  (interactive)
  (let ((debug-session (dap--cur-session))
        (mouse-point (or pos (point))))
    (when (and (dap--session-running debug-session)
               mouse-point)
      (-when-let* ((active-frame-id (-some->> debug-session
                                      dap--debug-session-active-frame
                                      (gethash "id")))
                   (bounds (dap-tooltip-thing-bounds mouse-point))
                   ((start . end) bounds)
                   (expression (s-trim (buffer-substring start end))))
        (dap--send-message
         (dap--make-request "evaluate"
                            (list :expression expression
                                  :frameId active-frame-id
                                  :context "hover"))
         (dap--resp-handler
          (-lambda ((&hash "body" (&hash? "result"
                                          "variablesReference" variables-reference)))
            (setq dap--tooltip-overlay
                  (-doto (make-overlay start end)
                    (overlay-put 'mouse-face 'dap-mouse-eval-thing-face)))
            ;; Show a dead buffer so that the `posframe' size is consistent.
            (when (get-buffer dap-mouse-buffer)
              (kill-buffer dap-mouse-buffer))
            (unless (and (zerop variables-reference) (string-empty-p result))
              (apply #'posframe-show dap-mouse-buffer
                     :position start
                     :accept-focus t
                     dap-mouse-posframe-properties)
              (with-current-buffer (get-buffer-create dap-mouse-buffer)
                (dap-ui-render-value debug-session expression
                                     result variables-reference)))
            (add-hook 'post-command-hook 'dap-tooltip-post-tooltip))
          ;; TODO: hover failure will yield weird errors involving process
          ;; filters, so I resorted to this hack; we should proably do proper
          ;; error handling, with a whitelist of allowable errors.
          #'ignore)
         debug-session)))))

(defun dap-tooltip-tips (event)
  "Show tip for identifier or selection under the mouse.
The mouse must either point at an identifier or inside a selected
region for the tip window to be shown. In the case of a C program
controlled by GDB, show the associated #define directives when
program is not executing.

This function must return nil if it doesn't handle EVENT."
  (when (and (eventp event) dap-tooltip-mode)
    (dap-tooltip-at-point (posn-point (event-end event))))
  "")

(provide 'dap-mouse)
;;; dap-mouse.el ends here
