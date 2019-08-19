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
(require 'lsp-mode)
(require 'tooltip)

(defface  dap-mouse-eval-thing-face
  '((((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "#2aa1ae")))
  "Face used to display evaluation results at the end of line.
If `dap-overlays-use-font-lock' is non-nil, this face is
applied with lower priority than the syntax highlighting."
  :group 'dap
  :package-version '(dap "0.9.1"))

;;;###autoload
(define-minor-mode dap-tooltip-mode
  "Toggle the display of GUD tooltips."
  :global t
  :group 'dap-mouse
  :group 'tooltip (require 'tooltip)
  (if dap-tooltip-mode
      (progn
        (add-hook 'pre-command-hook 'tooltip-hide)
        (add-hook 'tooltip-functions 'dap-tooltip-tips)
        (add-hook 'lsp-mode-hook 'dap-tooltip-activate-mouse-motions-if-enabled)
        (define-key lsp-mode-map [mouse-movement] 'dap-tooltip-mouse-motion))
    (unless tooltip-mode
      (remove-hook 'pre-command-hook 'tooltip-hide)
      (remove-hook 'tooltip-functions 'dap-tooltip-tips)
      (define-key lsp-mode-map  [mouse-movement] 'ignore)
      (remove-hook 'lsp-mode-hook 'dap-tooltip-activate-mouse-motions-if-enabled)))
  (dap-tooltip-activate-mouse-motions-if-enabled))

(defcustom dap-tooltip-echo-area nil
  "Use the echo area instead of frames for DAP tooltips."
  :type 'boolean
  :group 'dap-mouse
  :group 'tooltip)

;;; Reacting on mouse movements

(defun dap-tooltip-activate-mouse-motions-if-enabled ()
  "Reconsider for all buffers whether mouse motion events are desired."
  (remove-hook 'post-command-hook
               'dap-tooltip-activate-mouse-motions-if-enabled)
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
  "Command handler for mouse movement events in `dap-mode-map'."
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

(defvar-local dap-tooltip-bounds nil)
(defvar-local dap-tooltip--request 0)

(defun dap-tooltip-post-tooltip ()
  "Clean tooltip properties."
  (remove-hook 'post-command-hook #'dap-tooltip-post-tooltip)

  (when dap-tooltip-bounds
    (remove-text-properties (car dap-tooltip-bounds)
                            (cdr dap-tooltip-bounds)
                            '(mouse-face))
    ;; restore the selection
    (when (region-active-p)
      (let ((bounds dap-tooltip-bounds))
        (run-with-idle-timer
         0.0
         nil
         (lambda ()
           (let ((point (point)))
             (push-mark (car bounds) t t)
             (goto-char (cdr bounds))
             (unless (= point (point))
               (exchange-point-and-mark)))))))
    (setq dap-tooltip-bounds nil)))

(defun dap-tooltip-tips (event)
  "Show tip for identifier or selection under the mouse.
The mouse must either point at an identifier or inside a selected
region for the tip window to be shown.  In the case of a C program
controlled by GDB, show the associated #define directives when program is
not executing.

This function must return nil if it doesn't handle EVENT."
  (setq dap-tooltip--request (1+ dap-tooltip--request))

  (let ((debug-session (dap--cur-session))
        (mouse-point (posn-point (event-end event)))
        (request-id dap-tooltip--request))
    (when (and (eventp event)
               (dap--session-running debug-session)
               dap-tooltip-mode
               mouse-point)
      (-when-let* ((active-frame-id (-some->> debug-session
                                              dap--debug-session-active-frame
                                              (gethash "id")))
                   (bounds (dap-tooltip-thing-bounds mouse-point))
                   ((start . end) bounds)
                   (expression (buffer-substring start end)))
        (setq dap-tooltip-bounds bounds)
        (dap--send-message
         (dap--make-request "evaluate"
                            (list :expression expression
                                  :frameId active-frame-id))
         (-lambda ((&hash "message" "body" (&hash? "result")))
           (when (= request-id dap-tooltip--request)
             (if result
                 (progn
                   (add-text-properties start end
                                        '(mouse-face dap-mouse-eval-thing-face))
                   (tooltip-show result
                                 (or dap-tooltip-echo-area tooltip-use-echo-area
                                     (not tooltip-mode)))
                   (add-hook 'post-command-hook 'dap-tooltip-post-tooltip))
               (message message))))
         debug-session))))
  "")

(provide 'dap-mouse)
;;; dap-mouse.el ends here
