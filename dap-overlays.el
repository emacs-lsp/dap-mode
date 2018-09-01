;;; dap-overlays.el --- Managing DAP overlays  -*- lexical-binding: t; -*-

;; Coppied from cider
;; Author: Artur Malabarba <bruce.connor.am@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; URL: https://github.com/yyoncho/dap-mode
;; Package-Requires: ((emacs "25.1"))
;; Version: 0.2

;;; Commentary:
;; Use `dap-overlays--make-overlay' to place a generic overlay at point. Or use
;; `dap-overlays--make-result-overlay' to place an interactive eval result overlay at

;;; Code:

(require 'subr-x)
(require 'cl-lib)


;;; Customization
(defface dap-result-overlay-face
  '((((class color) (background light))
     :background "grey90" :box (:line-width -1 :color "yellow"))
    (((class color) (background dark))
     :background "grey10" :box (:line-width -1 :color "black")))
  "Face used to display evaluation results at the end of line.
If `dap-overlays-use-font-lock' is non-nil, this face is
applied with lower priority than the syntax highlighting."
  :group 'dap
  :package-version '(dap "0.9.1"))

(defcustom dap-overlays-use-font-lock t
  "If non-nil, results overlays are font-locked as code.
If nil, apply `dap-result-overlay-face' to the entire overlay instead of
font-locking it."
  :group 'dap
  :type 'boolean
  :package-version '(dap . "0.10.0"))

(defcustom dap-overlays-use-overlays 'both
  "Whether to display evaluation results with overlays.
If t, use overlays.  If nil, display on the echo area.  If both, display on
both places.

Only applies to evaluation commands.  To configure the debugger overlays,
see `dap-debug-use-overlays'."
  :type '(choice (const :tag "End of line" t)
                 (const :tag "Bottom of screen" nil)
                 (const :tag "Both" both))
  :group 'dap
  :package-version '(dap . "0.10.0"))

(defcustom dap-overlays-eval-result-prefix "=> "
  "The prefix displayed in the minibuffer before a result value."
  :type 'string
  :group 'dap
  :package-version '(dap . "0.5.0"))

(defcustom dap-overlays-eval-result-duration 'command
  "Duration, in seconds, of DAP's eval-result overlays.
If nil, overlays last indefinitely.
If the symbol `command', they're erased after the next command.
Also see `dap-overlays-use-overlays'."
  :type '(choice (integer :tag "Duration in seconds")
                 (const :tag "Until next command" command)
                 (const :tag "Last indefinitely" nil))
  :group 'dap
  :package-version '(dap . "0.10.0"))


;;; Overlay logic
(defun dap-overlays--delete-overlay (ov &rest _)
  "Safely delete overlay OV.
Never throws errors, and can be used in an overlay's modification-hooks."
  (ignore-errors (delete-overlay ov)))

(defun dap-overlays--make-overlay (l r type &rest props)
  "Place an overlay between L and R and return it.
TYPE is a symbol put on the overlay's category property.  It is used to
easily remove all overlays from a region with:
    (remove-overlays start end 'category TYPE)
PROPS is a plist of properties and values to add to the overlay."
  (let ((o (make-overlay l (or r l) (current-buffer))))
    (overlay-put o 'category type)
    (overlay-put o 'dap-temporary t)
    (while props (overlay-put o (pop props) (pop props)))
    (push #'dap-overlays--delete-overlay (overlay-get o 'modification-hooks))
    o))

(defun dap-overlays--remove-result-overlay ()
  "Remove result overlay from current buffer.
This function also removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'dap-overlays--remove-result-overlay 'local)
  (remove-overlays nil nil 'category 'result))

(defun dap-overlays--remove-result-overlay-after-command ()
  "Add `dap-overlays--remove-result-overlay' locally to `post-command-hook'.
This function also removes itself from `post-command-hook'."
  (remove-hook 'post-command-hook #'dap-overlays--remove-result-overlay-after-command 'local)
  (add-hook 'post-command-hook #'dap-overlays--remove-result-overlay nil 'local))

(cl-defun dap-overlays--make-result-overlay (value &rest props &key where duration (type 'result)
                                        (format (concat " " dap-overlays-eval-result-prefix "%s "))
                                        (prepend-face 'dap-result-overlay-face)
                                        &allow-other-keys)
  "Place an overlay displaying VALUE at the end of line.
VALUE is used as the overlay's after-string property, meaning it is
displayed at the end of the overlay.  The overlay itself is placed from
beginning to end of current line.
Return nil if the overlay was not placed or if it might not be visible, and
return the overlay otherwise.

Return the overlay if it was placed successfully, and nil if it failed.

This function takes some optional keyword arguments:

  If WHERE is a number or a marker, apply the overlay over
  the entire line at that place (defaulting to `point').  If
  it is a cons cell, the car and cdr determine the start and
  end of the overlay.
  DURATION takes the same possible values as the
  `dap-overlays-eval-result-duration' variable.
  TYPE is passed to `dap-overlays--make-overlay' (defaults to `result').
  FORMAT is a string passed to `format'.  It should have
  exactly one %s construct (for VALUE).

All arguments beyond these (PROPS) are properties to be used on the
overlay."
  (declare (indent 1))
  (while (keywordp (car props))
    (setq props (cdr (cdr props))))
  ;; If the marker points to a dead buffer, don't do anything.
  (let ((buffer (cond
                 ((markerp where) (marker-buffer where))
                 ((markerp (car-safe where)) (marker-buffer (car where)))
                 (t (current-buffer)))))
    (with-current-buffer buffer
      (save-excursion
        (when (number-or-marker-p where)
          (goto-char where))
        ;; Make sure the overlay is actually at the end of the sexp.
        (skip-chars-backward "\r\n[:blank:]")
        (let* ((beg (if (consp where)
                        (car where)
                      (save-excursion
                        (backward-sexp 1)
                        (point))))
               (end (if (consp where)
                        (cdr where)
                      (line-end-position)))
               (display-string (format format value))
               (o nil))
          (remove-overlays beg end 'category type)
          (funcall (if dap-overlays-use-font-lock
                       #'font-lock-prepend-text-property
                     #'put-text-property)
                   0 (length display-string)
                   'face prepend-face
                   display-string)
          ;; If the display spans multiple lines or is very long, display it at
          ;; the beginning of the next line.
          (when (or (string-match "\n." display-string)
                    (> (string-width display-string)
                       (- (window-width) (current-column))))
            (setq display-string (concat " \n" display-string)))
          ;; Put the cursor property only once we're done manipulating the
          ;; string, since we want it to be at the first char.
          (put-text-property 0 1 'cursor 0 display-string)
          (when (> (string-width display-string) (* 3 (window-width)))
            (setq display-string
                  (concat (substring display-string 0 (* 3 (window-width)))
                          (substitute-command-keys
                           "...\nResult truncated. Type `\\[dap-inspect-last-result]' to inspect it."))))
          ;; Create the result overlay.
          (setq o (apply #'dap-overlays--make-overlay
                         beg end type
                         'after-string display-string
                         props))
          (pcase duration
            ((pred numberp) (run-at-time duration nil #'dap-overlays--delete-overlay o))
            (`command
             ;; If inside a command-loop, tell `dap-overlays--remove-result-overlay'
             ;; to only remove after the *next* command.
             (if this-command
                 (add-hook 'post-command-hook
                           #'dap-overlays--remove-result-overlay-after-command
                           nil 'local)
               (dap-overlays--remove-result-overlay-after-command))))
          (when-let (win (get-buffer-window buffer))
            ;; Left edge is visible.
            (when (and (<= (window-start win) (point) (window-end win))
                       ;; Right edge is visible. This is a little conservative
                       ;; if the overlay contains line breaks.
                       (or (< (+ (current-column) (string-width value))
                              (window-width win))
                           (not truncate-lines)))
              o)))))))


;;; Displaying eval result
(defun dap-overlays--display-interactive-eval-result (value &optional point)
  "Display the result VALUE of an interactive eval operation.
VALUE is syntax-highlighted and displayed in the echo area.
If POINT and `dap-use-overlays' are non-nil, it is also displayed in an
overlay at the end of the line containing POINT.
Note that, while POINT can be a number, it's preferable to be a marker, as
that will better handle some corner cases where the original buffer is not
focused."
  (let* ((font-value value)
         (used-overlay (when (and point dap-overlays-use-overlays)
                         (dap-overlays--make-result-overlay font-value
                           :where point
                           :duration dap-overlays-eval-result-duration))))
    (message
     "%s"
     (propertize (format "%s%s" dap-overlays-eval-result-prefix font-value)
                 ;; The following hides the message from the echo-area, but
                 ;; displays it in the Messages buffer. We only hide the message
                 ;; if the user wants to AND if the overlay succeeded.
                 'invisible (and used-overlay
                                 (not (eq dap-overlays-use-overlays 'both)))))))

(provide 'dap-overlays)
;;; dap-overlays.el ends here
