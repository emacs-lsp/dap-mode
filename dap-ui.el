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

(defconst dap-ui--loading-tree-widget (list '(tree-widget :tag "Loading..." :format "%[%t%]\n")))

(defun dap-ui--stack-frames (thread-tree)
  "Method for expanding stackframe content.

THREAD-TREE will be widget element holding thread info."
  (let* ((session (widget-get thread-tree :session))
         (thread (widget-get thread-tree :thread))
         (thread-id (gethash "id" thread))
         (stack-frames (gethash thread-id (dap--debug-session-thread-stack-frames session))))
    (if stack-frames
        ;; aldready loaded
        (mapcar (lambda (stack-frame)
                  `(tree-widget :tag ,(gethash "name" stack-frame)
                                :format "%[%t%]\n"
                                :stack-frame ,stack-frame
                                :session ,session
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
        (mapcar (lambda (thread)
                  `(tree-widget
                    :node (push-button :tag ,(gethash "name" thread)
                                       :format "%[%t%]\n")
                    :thread ,thread
                    :session ,debug-session
                    :dynargs dap-ui--stack-frames
                    :open nil))
                threads)
      (dap--send-message (dap--make-request "threads")
                         (lambda (threads-resp)
                           (let ((threads (gethash "threads" (gethash "body" threads-resp))))

                             (setf (dap--debug-session-threads debug-session) threads)

                             (tree-mode-reflesh-tree session-tree)
                             (run-hook-with-args 'dap-ui-stack-frames-loaded debug-session threads)))
                         debug-session)
      dap-ui--loading-tree-widget)))

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
