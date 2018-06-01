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

(defcustom dap-ui-stack-frames-loaded nil
  "Stack frames loaded."
  :type 'hook
  :group 'dap-ui)

(defun dap-ui--load-threads (tree)

  (let ((session (widget-get tree :session)))
    (if-let (threads (dap--debug-session-threads session))
        (mapcar (lambda (thread)
                  `(tree-widget :tag ,(gethash "name" thread)
                                :format "%[%t%]\n"
                                :thread ,thread))
                threads)
      (dap--send-message (dap--make-request "threads")
                         (lambda (threads-resp)
                           (let ((threads (gethash "threads" (gethash "body" threads-resp))))

                             (setf (dap--debug-session-threads session) threads)

                             (tree-mode-reflesh-tree tree)
                             (run-hook-with-args 'dap-ui-stack-frames-loaded session threads)))
                         session)
      (list '(tree-widget :tag "Loading..." :format "%[%t%]\n")))))

(defun dap-ui-list-sessions ()
  "Show currently active sessions and it's threads."
  (interactive)
  (lsp--cur-workspace-check)
  (let ((sessions (lsp-workspace-get-metadata "debug-sessions"))
        (buf (get-buffer-create "*sessions*")))
    (with-current-buffer buf
      (erase-buffer)
      ;; (setq header-line-format gdb-breakpoints-header)
      (mapc
       (lambda (session)
         (widget-create
          `(tree-widget
            :node (push-button :format "%[%t%]\n" :tag ,(dap--debug-session-name session))
            :open nil
            :session ,session
            :dynargs dap-ui--load-threads)))
       sessions))
    (pop-to-buffer buf)))

(provide 'dap-ui)
;;; dap-ui.el ends here
