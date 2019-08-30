;;; dap-treemacs.el --- DAP                          -*- lexical-binding: t; -*-

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
(require 'treemacs)

(defun dap--loaded-sources-children (node)
  (let ((children (cl-second (treemacs-button-get node :pth))))
    (when (cl-first children)
      (->> children
           (dap--group-by-map #'cl-first #'cl-rest)
           (-sort #'dap--compare-source)))))

(defun dap--loaded-symbols-goto-path (&rest _args)
  (when-let (path (get-text-property 0
                                     'path
                                     (-some-> (treemacs-node-at-point)
                                              (button-get :key))))

    (select-window (get-lru-window nil nil t))
    (switch-to-buffer (find-file path))))


(defun dap-loaded-sources-root ()
  (->> (with-current-buffer "app.js"
         (dap--debug-session-loaded-sources (dap--cur-session)))
       (-map (-lambda ((&hash "path"))
               (let ((parts (f-split (if (f-absolute? path)
                                         (f-relative path
                                                     "/home/kyoncho/Sources/lsp-docker/demo-projects/Javascript/myapp/")
                                       path))))
                 (append (-butlast parts)
                         (list (propertize (-last-item parts)
                                           'path path))))))
       (dap--group-by-map #'cl-first #'cl-rest)
       (-sort 'dap--compare-source)))

(treemacs-define-expandable-node dap-loaded-sources-node
  :icon-open-form (dap--calculate-icon (treemacs-button-get node :pth) t)
  :icon-closed-form (dap--calculate-icon (treemacs-button-get node :pth) nil)
  :query-function (dap--loaded-sources-children node)
  :ret-action 'dap--loaded-symbols-goto-path
  :render-action
  (treemacs-render-node
   :icon (dap--calculate-icon item nil)
   :label-form (propertize (cl-first item) 'face 'default)
   :state treemacs-dap-loaded-sources-node-closed-state
   :key-form (cl-first item)
   :more-properties (:pth item)))

(defun dap--group-by-map (fn map-fn list)
  (->> list
       (-group-by fn)
       (-map (-lambda ((fst . rst))
               (list fst (-map map-fn rst))))))

(defun dap--compare-source (left right)
  (if (dap--is-dir? left)
      (or (not (dap--is-dir? right))
          (string< (cl-first left)
                   (cl-first right)))
    (not (or (dap--is-dir? right)
             (string< (cl-first left)
                      (cl-first right))))))

(treemacs-define-variadic-node dap-loaded-sources
  :query-function (dap-loaded-sources-root)
  :render-action
  (treemacs-render-node
   :icon (dap--calculate-icon item nil)
   :label-form (propertize (cl-first item) 'face 'default)
   :state treemacs-dap-loaded-sources-node-closed-state
   :key-form (cl-first item)
   :more-properties (:pth item))
  :root-key-form 'DAP-Loaded-Sources)

(defun dap--is-dir? (item)
  (cl-first (cl-second item)))

(defun dap--calculate-icon (item open?)
  (let ((dir? (dap--is-dir? item)))
    (concat
     (cond
      (open? " ▾ ")
      (dir? " ▸ ")
      (t "   "))
     (if dir?
         (treemacs-get-icon-value 'dir-open nil lsp-treemacs-theme)
       (treemacs-icon-for-file (cl-first item))))))

(let* ((buffer (get-buffer-create "*Loaded Sources*"))
       (window (display-buffer-in-side-window buffer nil)))
  (select-window window)
  (set-window-dedicated-p window t)
  (treemacs-initialize)
  (setq-local treemacs-default-visit-action 'treemacs-RET-action)
  (treemacs-DAP-LOADED-SOURCES-extension))

(provide 'dap-treemacs)
;;; dap-treemacs.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
