;;; dapui.el --- DAP UI controls implemented using treemacs.                       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ivan Yonchovski

;; Author: Ivan Yonchovski <yyoncho@gmail.com>
;; Keywords: abbrev

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
(require 'dash)
(require 'dap-mode)
(require 'cl-lib)
(require 'treemacs)

(defvar dapui-theme "Default")

(defun dapui--loaded-sources-children (node)
  (let ((children (cl-second (treemacs-button-get node :pth))))
    (when (cl-first children)
      (->> children
           (dapui--group-by-map #'cl-first #'cl-rest)
           (-sort #'dapui--compare-source)))))

(defun dapui--loaded-symbols-goto-path (&rest _args)
  (-let* ((node (-some-> (treemacs-node-at-point)
                         (button-get :key)))
          ((source &as &hash "sourceReference" "path") (get-text-property 0 'source node)))
    (if (f-exists? path)
        (progn
          (select-window (get-lru-window nil nil t))
          (switch-to-buffer (find-file path)))
      (dap--send-message
       (dap--make-request "source"
                          (list :source source
                                :sourceReference sourceReference))
       (-lambda ((&hash? "body" (&hash "content")))
         (select-window (get-lru-window nil nil t))
         (switch-to-buffer (get-buffer-create path))
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert content)
           (goto-char (point-min))
           (setq-local buffer-file-name path)
           (delay-mode-hooks (set-auto-mode))
           (font-lock-ensure)))
       (dap--cur-session)))))

(defun dapui--loaded-sources-root ()
  (let ((lsp-file-truename-cache (ht)))
    (lsp-with-cached-filetrue-name
     (let ((session (dap--cur-session)))
       (when (dap--session-running session)
         (->> (dap--debug-session-loaded-sources session)
              (-map (-lambda ((source &as &hash "path"))
                      (let ((parts (f-split (if (f-absolute? path)
                                                (f-relative path (lsp-workspace-root path))
                                              path))))
                        (append (-butlast parts)
                                (list (propertize (-last-item parts) 'source source))))))
              (dapui--group-by-map #'cl-first #'cl-rest)
              (-sort 'dapui--compare-source)))))))

(treemacs-define-expandable-node dap-loaded-sources-node
  :icon-open-form (dapui--calculate-icon (treemacs-button-get node :pth) t)
  :icon-closed-form (dapui--calculate-icon (treemacs-button-get node :pth) nil)
  :query-function (dapui--loaded-sources-children node)
  :ret-action 'dapui--loaded-symbols-goto-path
  :render-action
  (treemacs-render-node
   :icon (dapui--calculate-icon item nil)
   :label-form (propertize (cl-first item) 'face 'default)
   :state treemacs-dap-loaded-sources-node-closed-state
   :key-form (cl-first item)
   :more-properties (:pth item)))

(defun dapui--group-by-map (fn map-fn list)
  (->> list
       (-group-by fn)
       (-map (-lambda ((fst . rst))
               (list fst (-map map-fn rst))))))

(defun dapui--compare-source (left right)
  (if (dapui--source-is-dir? left)
      (or (not (dapui--source-is-dir? right))
          (string< (cl-first left)
                   (cl-first right)))
    (not (or (dapui--source-is-dir? right)
             (string< (cl-first left)
                      (cl-first right))))))

(defun dapui--calculate-icon (item open?)
  (let ((dir? (dapui--source-is-dir? item)))
    (concat
     (cond
      (open? " ▾ ")
      (dir? " ▸ ")
      (t "   "))
     (if dir?
         (treemacs-get-icon-value 'dir-open nil dapui-theme)
       (treemacs-icon-for-file (cl-first item))))))

(treemacs-define-variadic-node dap-loaded-sources
  :query-function (dapui--loaded-sources-root)
  :render-action
  (treemacs-render-node
   :icon (dapui--calculate-icon item nil)
   :label-form (propertize (cl-first item) 'face 'default)
   :state treemacs-dap-loaded-sources-node-closed-state
   :key-form (cl-first item)
   :more-properties (:pth item))
  :root-key-form 'DAP-Loaded-Sources)

(defun dapui--source-is-dir? (item)
  (cl-first (cl-second item)))

(defun dapui-sources-refresh (&rest _args)
  (condition-case _err
      (let ((inhibit-read-only t))
        (with-current-buffer "*DAP Loaded Sources*"
          (treemacs-update-node '(:custom DAP-Loaded-Sources) t)))
    (error)))

(defun dapui--cleanup-sources-hook ()
  (remove-hook 'dap-terminated-hook 'dapui-sources-refresh)
  (remove-hook 'dap-session-changed-hook 'dapui-sources-refresh))

;;;###autoload
(defun dapui-loaded-sources ()
  (interactive)
  (let* ((buffer (get-buffer-create "*DAP Loaded Sources*"))
         (window (display-buffer-in-side-window buffer nil)))
    (select-window window)
    (set-window-dedicated-p window t)
    (treemacs-initialize)
    (setq-local treemacs-default-visit-action 'treemacs-RET-action)
    (treemacs-DAP-LOADED-SOURCES-extension)

    (add-hook 'dap-terminated-hook 'dapui-sources-refresh)
    (add-hook 'dap-session-changed-hook 'dapui-sources-refresh)
    (add-hook 'dap-loaded-sources-changed-hook 'dapui-sources-refresh)
    (add-hook 'kill-buffer-hook 'dapui--cleanup-sources-hook nil t)))

(provide 'dapui)
;;; dapui.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
