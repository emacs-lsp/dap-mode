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

(require 'lsp-lens)

(require 'dap-mode)
(require 'wid-edit)
(require 'dash)
(require 'bui)
(require 'comint)
(require 'compile)
(require 'gdb-mi)
(require 'lsp-treemacs)
(require 'posframe)

(defcustom dap-ui-breakpoints-ui-list-displayed-hook nil
  "List of functions to run when breakpoints list is displayed."
  :type 'hook
  :group 'dap-ui)

(defcustom dap-ui-locals-expand-depth 1
  "Locals expand strategy.
When nil - do not expand.
t - expand recursively
number - expand N levels."
  :type '(choice (const :tag "Do not expand" nil)
                 (const :tag "Expand recursively" t)
                 (number :tag "Expand level"))
  :group 'dap-ui)

(define-obsolete-variable-alias
  'dap-ui-expressiosn-expand-depth 'dap-ui-expressions-expand-depth
  "dap-mode 0.2.0"
  "This variable is obsolete because it is misspelled.")

(defcustom dap-ui-expressions-expand-depth nil
  "Expressions expand strategy.
When nil - do not expand.
t - expand recursively
number - expand N levels."
  :type '(choice (const :tag "Do not expand" nil)
                 (const :tag "Expand recursively" t)
                 (number :tag "Expand level"))
  :group 'dap-ui)

(defface dap-ui-compile-errline
  '((t (:inherit compilation-error)))
  "Face used for marking the line on which an error occurs."
  :group 'dap-ui)

(defface dap-ui-sessions-active-session-face
  '((t :weight bold))
  "Face used for marking current session in sessions list."
  :group 'dap-ui)

(defface dap-ui-sessions-terminated-face
  '((t :inherit shadow))
  "Face used for marking terminated session."
  :group 'dap-ui)

(defface dap-ui-sessions-terminated-active-face
  '((t :inherit shadow :weight bold))
  "Face used for marking terminated session."
  :group 'dap-ui)

(defface dap-ui-sessions-running-face
  '((t))
  "Face used for marking terminated session."
  :group 'dap-ui)

(defface dap-ui-locals-scope-face
  '((t :inherit font-lock-function-name-face :weight bold :underline t))
  "Face used for scopes in locals view."
  :group 'dap-ui)

(defface dap-ui-locals-variable-leaf-face
  '((t :inherit font-lock-builtin-face :italic t))
  "Face used for variables that does not have nested items."
  :group 'dap-ui)

(defface dap-ui-locals-variable-face
  '((t :inherit font-lock-builtin-face :weight bold))
  "Face used for marking terminated session."
  :group 'dap-ui)

(defface dap-ui-sessions-thread-face
  '((t))
  "Face used for threads in sessions view."
  :group 'dap-ui)

(defface dap-ui-sessions-thread-active-face
  '((t :weight bold))
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

(defface dap-ui-controls-fringe
  '((t :inherit fringe))
  "Face used for the background of debugger icons in fringe."
  :group 'dap-ui)

(defcustom  dap-ui-default-fetch-count 30
  "Default number of variables to load in inspect variables view for
array variables."
  :group 'dap-ui
  :type 'number)

(defconst dap-ui--locals-buffer "*dap-ui-locals*")
(defconst dap-ui--sessions-buffer "*dap-ui-sessions*")
(defconst dap-ui--debug-window-buffer "*debug-window*")
(defconst dap-ui--expressions-buffer "*dap-ui-expressions*")
(defconst dap-ui--breakpoints-buffer "*dap-ui-breakpoints*")
(defconst dap-ui--repl-buffer "*dap-ui-repl*")

(defvar dap-ui-buffer-configurations
  `((,dap-ui--locals-buffer . ((side . right) (slot . 1) (window-width . 0.20)))
    (,dap-ui--expressions-buffer . ((side . right) (slot . 2) (window-width . 0.20)))
    (,dap-ui--sessions-buffer . ((side . right) (slot . 3) (window-width . 0.20)))
    (,dap-ui--breakpoints-buffer . ((side . left) (slot . 2) (window-width . ,treemacs-width)))
    (,dap-ui--debug-window-buffer . ((side . bottom) (slot . 3) (window-width . 0.20)))
    (,dap-ui--repl-buffer . ((side . bottom) (slot . 1) (window-height . 0.45)))))

(defun dap-ui-session--calculate-face (debug-session)
  "Calculate the face of DEBUG-SESSION based on its state."
  (cond
   ((and (eq debug-session (dap--cur-session))
         (not (dap--session-running debug-session))) 'dap-ui-sessions-terminated-active-face)
   ((eq debug-session (dap--cur-session)) 'dap-ui-sessions-active-session-face)
   ((not (dap--session-running debug-session)) 'dap-ui-sessions-terminated-face)
   (t 'dap-ui-sessions-running-face)))

(defun dap-ui--make-overlay (beg end visuals &optional mouse-face buf)
  "Allocate a DAP UI overlay in range BEG and END.
TOOLTIP-TEXT, VISUALS, MOUSE-FACE will be used for the overlay.
BUF is the active buffer."
  (let ((ov (make-overlay beg end buf t t)))
    (overlay-put ov 'face           (plist-get visuals :face))
    (overlay-put ov 'mouse-face     mouse-face)
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

(defun dap-ui--make-overlay-at (file point visuals)
  "Create an overlay highlighting the given POINT in FILE.
VISUALS and MSG will be used for the overlay."
  (-when-let (buf (find-buffer-visiting file))
    (with-current-buffer buf
      ;; If point is provided, use it to define region
      (when (integer-or-marker-p point)
        (save-excursion
          (goto-char point)
          (dap-ui--make-overlay (point-at-bol) (point-at-eol) visuals nil buf))))))

(defvar-local dap-ui--breakpoint-overlays nil)

(defvar-local dap-ui--cursor-overlay nil)

(defun dap-ui--clear-breakpoint-overlays ()
  "Remove all overlays that ensime-debug has created."
  (mapc #'delete-overlay (-filter #'identity dap-ui--breakpoint-overlays))
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

(defvar dap-ui-menu-items
  `("Debug"
    :visible (bound-and-true-p dap-ui-mode)
    ["Start" dap-debug]
    ["Create Debug Template" dap-debug-edit-template]
    ["Debug last session" dap-debug-last]
    ("Recent Sessions"
     :filter ,(lambda (_)
                (-map (-lambda ((name . debug-args))
                        (vector name (lambda ()
                                       (interactive)
                                       (dap-debug debug-args))))
                      dap--debug-configuration))
     :active dap--debug-configuration)
    "--"
    ["Sessions" dap-ui-sessions]
    ["Locals" dap-ui-locals]
    ["Expressions" dap-ui-expressions]
    ["Sources" dapui-loaded-sources]
    ["Output" dap-go-to-output-buffer]
    ["Breakpoints" dap-ui-breakpoints]
    "---"
    ["Toggle Controls" dap-ui-controls-mode]
    ["Toggle Mouse Hover" dap-tooltip-mode]))

(defvar dap-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (easy-menu-define dap-ui-mode-menu map
      "Menu for DAP"
      dap-ui-menu-items)
    map)
  "Keymap for DAP mode.")

;;;###autoload
(define-minor-mode dap-ui-mode
  "Displaying DAP visuals."
  :init-value nil
  :global t
  :keymap dap-ui-mode-map
  :group 'dap-ui
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

(defun dap-ui--show-buffer (buf)
  "Show BUF according to defined rules."
  (when-let (win (display-buffer-in-side-window buf
                                                (or (-> buf
                                                        buffer-name
                                                        (assoc dap-ui-buffer-configurations)
                                                        cl-rest)
                                                    '((side . right)
                                                      (slot . 1)
                                                      (window-width . 0.20)))))
    (set-window-dedicated-p win t)
    (select-window win)))


;; breakpoints
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

;;;###autoload
(defun dap-ui-breakpoints-list ()
  "List breakpoints."
  (interactive)
  (let ((workspaces lsp--buffer-workspaces))
    (bui-get-display-entries 'dap-ui-breakpoints-ui 'list)
    (setq-local lsp--buffer-workspaces workspaces)
    (add-hook 'bui-after-redisplay-hook
              (lambda () (setq-local lsp--buffer-workspaces workspaces)))
    (run-hooks 'dap-ui-breakpoints-ui-list-displayed-hook)))


;; dap-ui posframe stuff
(defvar dap-ui--control-images-root-dir (f-join (f-dirname (file-truename (or load-file-name buffer-file-name))) "icons/vscode"))
(defvar dap-ui--control-buffer " *dap-ui*")

(defun dap-ui--create-command (image command hover-text)
  (propertize "    "
              'display `(image :type png
                               :file ,(f-join dap-ui--control-images-root-dir image)
                               :ascent center
                               :background ,(face-attribute 'dap-ui-controls-fringe :background nil 'default))
              'local-map (--doto (make-sparse-keymap)
                           (define-key it [mouse-1] command))
              'pointer 'hand
              'help-echo hover-text))

(defun dap-ui--update-controls (&rest _)
  (when (posframe-workable-p)
    (let* ((session (dap--cur-session))
           (stopped? (and session (dap--debug-session-active-frame session)))
           (running? (and session (dap--session-running session))))
      (if running?
          (let ((content (s-concat
                          (dap-ui--create-command "continue.png" #'dap-continue "Continue")
                          " "
                          (dap-ui--create-command (if stopped?
                                                      "step-over.png"
                                                    "step-over-disabled.png")
                                                  (when stopped? #'dap-next)
                                                  (if stopped? "Step over"
                                                    "Session not stopped?"))
                          " "
                          (dap-ui--create-command (if stopped? "step-out.png"
                                                    "step-out-disabled.png")
                                                  (when stopped? #'dap-step-out)
                                                  (if stopped? "Step out"
                                                    "Session not stopped? "))
                          " "
                          (dap-ui--create-command (if stopped? "step-into.png"
                                                    "step-into-disabled.png")
                                                  (when stopped? #'dap-step-in)
                                                  (if stopped? "Step in"
                                                    "Session not stopped?"))
                          " "
                          (dap-ui--create-command "disconnect.png" #'dap-disconnect "Disconnect")
                          " "
                          (dap-ui--create-command "restart.png" #'dap-debug-restart "Restart")))
                posframe-mouse-banish
                (pos-frame (-first
                            (lambda (frame)
                              (let ((buffer-info (frame-parameter frame 'posframe-buffer)))
                                (or (equal dap-ui--control-buffer (car buffer-info))
                                    (equal dap-ui--control-buffer (cdr buffer-info)))))
                            (frame-list))))
            (ignore posframe-mouse-banish)
            (when (eq (selected-frame) pos-frame)
              (select-frame (frame-parent pos-frame)))
            (posframe-show dap-ui--control-buffer
                           :string content
                           :poshandler #'posframe-poshandler-frame-top-center
                           :internal-border-width 8))
        (posframe-hide dap-ui--control-buffer)))))

;;;###autoload
(define-minor-mode dap-ui-controls-mode
  "Displaying DAP visuals."
  :init-value nil
  :global t
  :require 'dap-ui
  (cond
   (dap-ui-controls-mode
    (add-hook 'dap-session-changed-hook 'dap-ui--update-controls)
    (add-hook 'dap-terminated-hook 'dap-ui--update-controls )
    (add-hook 'dap-session-changed-hook 'dap-ui--update-controls)
    (add-hook 'dap-continue-hook 'dap-ui--update-controls)
    (add-hook 'dap-stack-frame-changed-hook 'dap-ui--update-controls)
    (setq posframe-mouse-banish nil)
    (dap-ui--update-controls))
   (t
    (remove-hook 'dap-session-changed-hook 'dap-ui--update-controls)
    (remove-hook 'dap-terminated-hook 'dap-ui--update-controls )
    (remove-hook 'dap-session-changed-hook 'dap-ui--update-controls)
    (remove-hook 'dap-continue-hook 'dap-ui--update-controls)
    (remove-hook 'dap-stack-frame-changed-hook 'dap-ui--update-controls)
    (setq posframe-mouse-banish t)
    (posframe-hide dap-ui--control-buffer))))


;; sessions

(defmacro dap-ui-define-action (name keys &rest body)
  (declare (doc-string 3) (indent 2))
  `(defun ,name (&rest args)
     ,(format "Code action %s" name)
     (interactive)
     (ignore args)
     (if-let (node (treemacs-node-at-point))
         (-let [,(cons '&plist keys) (button-get node :item)]
           ,@body)
       (treemacs-pulse-on-failure "No node at point"))))

(dap-ui-define-action dap-ui-session-select (:session)
  (dap--switch-to-session session))

(dap-ui-define-action dap-ui-thread-select (:session :thread-id)
  (setf (dap--debug-session-thread-id session) thread-id)
  (dap--switch-to-session session)
  (dap--select-thread-id session thread-id))

(dap-ui-define-action dap-ui-delete-session (:session)
  (dap-delete-session session))

(dap-ui-define-action dap-ui-disconnect (:session)
  (dap-disconnect session))

(dap-ui-define-action dap-ui-continue (:session :thread-id)
  (dap-continue session thread-id))

(dap-ui-define-action dap-ui-restart-frame (:session :stack-frame)
  (dap-restart-frame session (gethash "id" stack-frame)))

(dap-ui-define-action dap-ui-select-stack-frame (:session :thread-id :stack-frame)
  (setf (dap--debug-session-thread-id session) thread-id
        (dap--debug-session-active-frame session) stack-frame)
  (dap--switch-to-session session))

(dap-ui-define-action dap-ui-thread-stop (:session :thread-id)
  (dap-stop-thread-1 session thread-id))

(defvar dap-ui-session-mode-map
  (-doto (make-sparse-keymap)
    (define-key (kbd "X") #'dap-ui-disconnect)
    (define-key (kbd "D") #'dap-ui-delete-session)
    (define-key (kbd "C") #'dap-ui-continue)
    (define-key (kbd "S") #'dap-ui-thread-stop)))

(define-minor-mode dap-ui-sessions-mode
  "UI Session list minor mode."
  :init-value nil
  :group dap-ui
  :keymap dap-ui-session-mode-map)

(defun dap-ui--sessions-tree ()
  (->>
   "debug-sessions"
   (lsp-workspace-get-metadata)
   (reverse)
   (-map
    (-lambda ((session &as &dap-session 'name 'thread-states))
      (list
       :label (propertize name 'face (dap-ui-session--calculate-face session))
       :key name
       :session session
       :ret-action #'dap-ui-session-select
       :icon (if (dap--session-running session)
                 'session-started
               'session-terminated)
       :actions (if (dap--session-running session)
                    `(["Select" dap-ui-session-select]
                      ["Disconnect" dap-ui-disconnect]
                      ["Delete Session" dap-ui-delete-session]
                      "--"
                      ["Delete All Sessions" dap-delete-all-sessions])
                  `(["Select" dap-ui-session-select]
                    ["Delete Session" dap-ui-delete-session]
                    "--"
                    ["Delete All Sessions" dap-delete-all-sessions]))
       :children-async
       (when (dap--session-running session)
         (lambda (_node callback)
           (dap--send-message
            (dap--make-request "threads")
            (-lambda  ((&hash? "body" (&hash? "threads")))
              (funcall
               callback
               (-map
                (-lambda ((thread &as &hash "name" "id"))
                  (let* ((status (s-capitalize (gethash id thread-states "running")))
                         (stopped? (not (string= (s-downcase status) "running"))))
                    (list
                     :label (concat (propertize name
                                                'face (if (and (eq session (dap--cur-session))
                                                               (eq id (dap--debug-session-thread-id session)))
                                                          'dap-ui-sessions-thread-active-face
                                                        'dap-ui-sessions-thread-face))
                                    (when status (propertize (concat "  " status) 'face 'lsp-lens-face)))
                     :key (format "%s" id)
                     :icon (if stopped? 'thread-stopped 'thread-running)
                     :ret-action #'dap-ui-thread-select
                     :session session
                     :thread-id id
                     :actions (if stopped?
                                  `(["Select" dap-ui-thread-select]
                                    ["Continue" dap-ui-continue]
                                    "--"
                                    ["Delete All Sessions" dap-delete-all-sessions])
                                `(["Select" dap-ui-thread-select]
                                  ["Stop thread" dap-ui-thread-stop]
                                  "--"
                                  ["Delete All Sessions" dap-delete-all-sessions]))
                     :children-async
                     (when stopped?
                       (-lambda (_node callback)
                         (dap--send-message
                          (dap--make-request "stackTrace" (list :threadId id))
                          (-lambda ((&hash? "body" (&hash? "stackFrames" stack-frames)))
                            (funcall
                             callback
                             (-map
                              (-lambda ((stack-frame &as &hash "name" "line" "source" "instructionPointerReference"))
                                (let* ((current-session (dap--cur-session))
                                       (icon (if (and
                                                  (equal session current-session)
                                                  (eq id (dap--debug-session-thread-id current-session))
                                                  (equal stack-frame (dap--debug-session-active-frame current-session)))
                                                 'stack-stopped
                                               'stack)))
                                  (list
                                   :session session
                                   :actions (if stopped?
                                                `(["Select" dap-ui-select-stack-frame]
                                                  ["Continue" dap-ui-continue]
                                                  ["Restart Frame" dap-ui-restart-frame]
                                                  "--"
                                                  ["Delete All Sessions" dap-delete-all-sessions])
                                              `(["Select" dap-ui-select-stack-frame]
                                                ["Stop thread" dap-ui-thread-stop]
                                                "--"
                                                ["Delete All Sessions" dap-delete-all-sessions]))
                                   :thread-id id
                                   :stack-frame stack-frame
                                   :ret-action #'dap-ui-select-stack-frame
                                   :label (if source
                                              (concat (propertize
                                                       name
                                                       'face (if (and (eq session (dap--cur-session))
                                                                      (eq id (dap--debug-session-thread-id session))
                                                                      (equal name (-some->> current-session
                                                                                    dap--debug-session-active-frame
                                                                                    (gethash "name"))))
                                                                 'dap-ui-sessions-thread-active-face
                                                               'dap-ui-sessions-thread-face))
                                                      (if instructionPointerReference
                                                          (propertize (format " [%s]" instructionPointerReference) 'face 'dap-ui-sessions-thread-face)
                                                        "")
                                                      (propertize (format " (%s:%s)" (or (gethash "name" source)
                                                                                         (gethash "path" source))
                                                                          line)
                                                                  'face 'lsp-lens-face))
                                            (concat name (propertize "(Unknown source)" 'face 'lsp-lens-face)))
                                   :key name
                                   :icon icon)))
                              stack-frames)))
                          session))))))
                threads)))
            session))))))))

(defun dap-ui-sessions--refresh (&rest _)
  (save-excursion
    (lsp-treemacs-wcb-unless-killed dap-ui--sessions-buffer
      (setq-local lsp-treemacs-tree (dap-ui--sessions-tree))
      (lsp-treemacs-generic-refresh))))

(defun dap-ui-sessions--cleanup-hooks ()
  "Remove UI sessions related hooks."
  (remove-hook 'dap-terminated-hook #'dap-ui-sessions--refresh)
  (remove-hook 'dap-session-changed-hook #'dap-ui-sessions--refresh)
  (remove-hook 'dap-continue-hook #'dap-ui-sessions--refresh)
  (remove-hook 'dap-stack-frame-changed-hook 'dap-ui-sessions--refresh))

;;;###autoload
(defun dap-ui-sessions ()
  "Show currently active sessions."
  (interactive)
  (dap-ui--show-buffer
   (lsp-treemacs-render
    (dap-ui--sessions-tree)
    " Debug Sessions " nil
    dap-ui--sessions-buffer
    '(["Delete All Sessions" dap-delete-all-sessions])))
  (dap-ui-sessions-mode)
  (add-hook 'dap-terminated-hook #'dap-ui-sessions--refresh)
  (add-hook 'dap-session-changed-hook #'dap-ui-sessions--refresh)
  (add-hook 'dap-continue-hook #'dap-ui-sessions--refresh)
  (add-hook 'dap-stack-frame-changed-hook #'dap-ui-sessions--refresh)
  (add-hook 'kill-buffer-hook #'dap-ui-sessions--cleanup-hooks nil t))


;; locals

(defcustom dap-ui-variable-length 30
  "Default number of variables to load in inspect variables view for
array variables."
  :group 'dap-ui
  :type 'number)

(dap-ui-define-action dap-ui-set-variable-value (:session :variables-reference :value :name)
  (dap--send-message
   (dap--make-request "setVariable"
                      (list :variablesReference variables-reference
                            :name name
                            :value (read-string (format "Enter value for %s: " name ) value)))
   (dap--resp-handler)
   session))

(defun dap-ui-render-variables (debug-session variables-reference _node)
  "Render hierarchical variables for treemacs.
Usable as the `treemacs' :children argument, when DEBUG-SESSION
and VARIABLES-REFERENCE are applied partially.

DEBUG-SESSION specifies the debug session which will be used to
issue requests.

VARIABLES-REFERENCE specifies the handle returned by the debug
adapter for acquiring nested variables and must not be 0."
  (when (dap--session-running debug-session)
    (->>
     variables-reference
     (dap-request debug-session "variables" :variablesReference)
     (gethash "variables")
     (-map (-lambda ((&hash "value" "name"
                            "variablesReference" variables-reference))
             `(:label ,(concat (propertize (format "%s" name)
                                           'face 'font-lock-variable-name-face)
                               ": "
                               (propertize (s-truncate dap-ui-variable-length
                                                       (s-replace "\n" "\\n" value))
                                           'help-echo value))
               :icon dap-variable
               :value ,value
               :session ,debug-session
               :variables-reference ,variables-reference
               :name ,name
               :actions '(["Set value" dap-ui-set-variable-value])
               :key ,name
               ,@(unless (zerop variables-reference)
                   (list :children
                         (-partial #'dap-ui-render-variables debug-session
                                   variables-reference)))))))))

(defun dap-ui-render-value
    (debug-session expression value variables-reference)
  "Render a hover result to the current buffer.
VALUE is the evaluate result, DEBUG-SESSION the debug session as
usual and EXPRESSION the expression that was originally
evaluated. VARIABLES-REFERENCE is returned by the evaluate
request."
  (lsp-treemacs-render
   `((:key ,expression
      :label ,value
      :icon dap-field
      ,@(unless (zerop variables-reference)
          (list :children
                (-partial #'dap-ui-render-variables
                          debug-session
                          variables-reference)))))
   "" nil (buffer-name)))

(defun dap-ui-eval-in-buffer (expression)
  "Like `dap-eval', but in a new treemacs buffer."
  (interactive "sExpr: ")
  (let ((debug-session (dap--cur-active-session-or-die)))
    (if-let ((active-frame-id (-some->> debug-session
                                dap--debug-session-active-frame
                                (gethash "id"))))
        (dap--send-message
         (dap--make-request "evaluate"
                            (list :expression expression
                                  :frameId active-frame-id
                                  :context "hover"))
         (dap--resp-handler
          (-lambda ((&hash "body" (&hash? "result" "variablesReference"
                                          variables-reference)))
            (with-current-buffer
                (get-buffer-create (format "*evaluate %s*" expression))
              (let ((inhibit-read-only t)) (erase-buffer))
              (dap-ui-render-value debug-session expression result
                                   variables-reference)
              (display-buffer (current-buffer)))))
         debug-session)
      (error "`dap-eval-in-buffer': no stopped debug session"))))

(defun dap-ui-eval-variable-in-buffer ()
  "Evaluate the symbol at point in a new buffer."
  (interactive)
  (if-let ((sym (thing-at-point 'symbol)))
      (dap-ui-eval-in-buffer sym)
    (user-error "`dap-ui-eval-variable-in-buffer': no symbol at point")))

(defvar dap-ui--locals-timer nil)

(defun dap-ui-locals--refresh (&rest _)
  (save-excursion
    (setq dap-ui--locals-timer nil)
    (with-current-buffer (get-buffer-create dap-ui--locals-buffer)
      (or (-some--> (dap--cur-session)
            (dap--debug-session-active-frame it)
            (gethash "id" it)
            (dap-request (dap--cur-session) "scopes" :frameId it)
            (gethash "scopes" it)
            (-map (-lambda ((&hash "name" "variablesReference" variables-reference))
                    (list :key name
                          :label name
                          :icon 'dap-scope
                          :children (-partial #'dap-ui-render-variables
                                              (dap--cur-session)
                                              variables-reference)))
                  it)
            (lsp-treemacs-render it " Locals " dap-ui-locals-expand-depth  dap-ui--locals-buffer)
            (or it t))
          (lsp-treemacs-render
           '((:label "Nothing to display..."
                     :key "foo"
                     :icon :empty))
           " Locals :: no locals info "
           nil
           dap-ui--locals-buffer)))))

(defun dap-ui-locals--refresh-schedule (&rest _)
  (lsp-treemacs-wcb-unless-killed dap-ui--locals-buffer
    (when dap-ui--locals-timer (cancel-timer dap-ui--locals-timer))
    (setq-local dap-ui--locals-timer (run-with-idle-timer 0.2 nil #'dap-ui-locals--refresh))))

(defun dap-ui-locals--cleanup-hooks ()
  (remove-hook 'dap-terminated-hook #'dap-ui-locals--refresh-schedule)
  (remove-hook 'dap-session-changed-hook #'dap-ui-locals--refresh-schedule)
  (remove-hook 'dap-continue-hook #'dap-ui-locals--refresh-schedule)
  (remove-hook 'dap-stack-frame-changed-hook #'dap-ui-locals--refresh-schedule))

;;;###autoload
(defun dap-ui-locals ()
  (interactive)
  (dap-ui--show-buffer (get-buffer-create dap-ui--locals-buffer))
  (dap-ui-locals--refresh-schedule)
  (with-current-buffer dap-ui--locals-buffer
    (add-hook 'dap-terminated-hook #'dap-ui-locals--refresh-schedule)
    (add-hook 'dap-session-changed-hook #'dap-ui-locals--refresh-schedule)
    (add-hook 'dap-continue-hook #'dap-ui-locals--refresh-schedule)
    (add-hook 'dap-stack-frame-changed-hook #'dap-ui-locals--refresh-schedule)
    (add-hook 'kill-buffer-hook #'dap-ui-locals--cleanup-hooks nil t)))

;; watch expressions

(defcustom dap-ui-expressions nil
  "The watch expressions."
  :type '(repeat string)
  :group 'dap-ui)

(defun dap-ui-expressions-add (expression)
  (interactive (list (read-string
                      "Add watch expression: "
                      (cond
                       ((region-active-p) (buffer-substring-no-properties
                                           (region-beginning)
                                           (region-end)))
                       (t (symbol-name (symbol-at-point)))))))
  (when (-contains? dap-ui-expressions expression)
    (user-error "\"%s\" is already watched" expression))
  (add-to-list 'dap-ui-expressions expression)
  (dap-ui-expressions)
  (dap-ui-expressions-refresh))

(defun dap-ui-expressions-add-prompt (expression)
  "Prompts for an expression and adds it to `dap-ui-expressions'."
  (interactive (list (read-string "Add watch expression: ")))
  (dap-ui-expressions-add expression))

(defun dap-ui-expressions-remove (expression)
  (interactive (list (completing-read
                      "Select expression to remove: "
                      dap-ui-expressions
                      nil
                      t)))
  (unless (-contains? dap-ui-expressions expression)
    (user-error "\"%s\" is not present" expression))
  (setq dap-ui-expressions (remove expression dap-ui-expressions))
  (dap-ui-expressions-refresh))

(dap-ui-define-action dap-ui-expressions-mouse-remove (:expression)
  (dap-ui-expressions-remove expression))

(defun dap-ui-expressions-refresh ()
  (interactive)
  (save-excursion
    (with-current-buffer (get-buffer-create dap-ui--expressions-buffer)
      (lsp-treemacs-render
       (let ((debug-session (dap--cur-session)))
         (-map
          (if-let ((active-frame-id (when (dap--session-running debug-session)
                                      (-some->> debug-session
                                        dap--debug-session-active-frame
                                        (gethash "id")))))
              (lambda (expression)
                (condition-case err
                    (-let [(&hash "result" "variablesReference" variables-reference)
                           (dap-request
                            (dap--cur-session)
                            "evaluate"
                            :expression expression
                            :frameId active-frame-id
                            :context "watch")]
                      `(:key ,expression
                        :expression ,expression
                        :label ,(concat (propertize (format "%s: " expression) 'face 'font-lock-variable-name-face)
                                        (propertize (s-truncate dap-ui-variable-length
                                                                (s-replace "\n" "\\n" result))
                                                    'help-echo result))
                        :icon expression
                        ,@(when (and variables-reference (not (zerop variables-reference)))
                            (list :children (-partial #'dap-ui-render-variables debug-session variables-reference)))
                        :actions (["Remove" dap-ui-expressions-mouse-remove]
                                  "--"
                                  ["Add" dap-ui-expressions-add]
                                  ["Refresh" dap-ui-expressions-refresh])))
                  (error `(:key ,expression
                           :label ,(concat (propertize (format "%s: " expression) 'face 'font-lock-variable-name-face)
                                           (propertize (error-message-string err) 'face 'error))
                           :icon failed-expression
                           :actions (["Remove" dap-ui-expressions-mouse-remove]
                                     "--"
                                     ["Add" dap-ui-expressions-add]
                                     ["Refresh" dap-ui-expressions-refresh])))))
            (lambda (expression)
              `(:key ,expression
                :expression ,expression
                :label ,(concat
                         (propertize (format "%s: " expression) 'face 'font-lock-variable-name-face)
                         (propertize "not available" 'face 'italic))
                :icon expression
                :actions (["Remove" dap-ui-expressions-mouse-remove]
                          "--"
                          ["Add" dap-ui-expressions-add]
                          ["Refresh" dap-ui-expressions-refresh]))))
          dap-ui-expressions))
       " Expressions "
       dap-ui-expressions-expand-depth
       dap-ui--expressions-buffer
       '(["Add" dap-ui-expressions-add]
         ["Refresh" dap-ui-expressions-refresh])))))

(defvar dap-ui--watches-timer nil)

(defun dap-ui-expressions--refresh-schedule (&rest _)
  (lsp-treemacs-wcb-unless-killed dap-ui--expressions-buffer
    (when dap-ui--watches-timer (cancel-timer dap-ui--watches-timer))
    (setq-local dap-ui--watches-timer (run-with-idle-timer 0.2 nil #'dap-ui-expressions-refresh))))


(defun dap-ui-expressions--cleanup-hooks ()
  (remove-hook 'dap-terminated-hook #'dap-ui-expressions--refresh-schedule)
  (remove-hook 'dap-session-changed-hook #'dap-ui-expressions--refresh-schedule)
  (remove-hook 'dap-continue-hook #'dap-ui-expressions--refresh-schedule)
  (remove-hook 'dap-stack-frame-changed-hook #'dap-ui-expressions--refresh-schedule))

(defun dap-ui-expressions ()
  (interactive)
  (dap-ui--show-buffer (get-buffer-create dap-ui--expressions-buffer))
  (dap-ui-expressions-refresh)

  (add-hook 'dap-terminated-hook #'dap-ui-expressions--refresh-schedule)
  (add-hook 'dap-session-changed-hook #'dap-ui-expressions--refresh-schedule)
  (add-hook 'dap-continue-hook #'dap-ui-expressions--refresh-schedule)
  (add-hook 'dap-stack-frame-changed-hook #'dap-ui-expressions--refresh-schedule)
  (add-hook 'kill-buffer-hook #'dap-ui-expressions--cleanup-hooks nil t))

(make-obsolete 'dap-ui-inspect 'dap-ui-expressions-add "dap-mode 0.2")
(make-obsolete 'dap-ui-inspect-region 'dap-ui-expressions-add "dap-mode 0.2")
(make-obsolete 'dap-ui-inspect-thing-at-point 'dap-ui-expressions-add "dap-mode 0.2")



;; Breakpoints - new
(defvar dap-exception-breakpoints nil)

(dap-ui-define-action dap-ui-breakpoints-toggle (:filter :session :default)
  (let ((type (plist-get (dap--debug-session-launch-args session) :type)))
    (setf (alist-get
           filter
           (alist-get type dap-exception-breakpoints nil nil #'string=)
           nil nil #'string=)
          (not (dap--breakpoint-filter-enabled
                filter
                type
                default))))
  (dap--set-exception-breakpoints session #'dap-ui-breakpoints--refresh))

(dap-ui-define-action dap-ui-breakpoints-goto-breakpoint (:file-name :point)
  (select-window (get-mru-window (selected-frame) nil))
  (find-file file-name)
  (goto-char point))

(dap-ui-define-action dap-ui-breakpoint-delete (:file-name :breakpoint)
  (with-current-buffer (find-file-noselect file-name)
    (dap-breakpoint-delete breakpoint file-name)))

(dap-ui-define-action dap-ui-breakpoint-condition (:file-name :breakpoint)
  (dap-breakpoint-condition file-name breakpoint))

(dap-ui-define-action dap-ui-breakpoint-hit-condition (:file-name :breakpoint)
  (dap-breakpoint-hit-condition file-name breakpoint))

(dap-ui-define-action dap-ui-breakpoint-log-message (:file-name :breakpoint)
  (dap-breakpoint-log-message file-name breakpoint))

(defun dap-ui--breakpoints-data ()
  (-let (((debug-session &as &dap-session 'launch-args 'current-capabilities 'breakpoints all-session-breakpoints)
          (or (dap--cur-session)
              (make-dap--debug-session)))
         (lsp-file-truename-cache (ht)))
    (lsp-with-cached-filetrue-name
     (append
      (when (dap--session-running debug-session)
        (-some->> current-capabilities
          (gethash "exceptionBreakpointFilters")
          (-map (-lambda ((&hash "label" "filter" "default"))
                  (list :label (propertize
                                (format "%s %s"
                                        (if (dap--breakpoint-filter-enabled
                                             filter
                                             (plist-get launch-args :type)
                                             default)
                                            (propertize "☑" 'face 'success)
                                          (propertize "☐" 'face 'shadow))
                                        label)
                                'help-echo "Exception breakpoint")
                        :key filter
                        :filter filter
                        :icon 'icon
                        :session debug-session
                        :default default
                        :ret-action #'dap-ui-breakpoints-toggle)))))
      (->>
       (dap--get-breakpoints)
       (ht-map
        (lambda (file-name breakpoints)
          (let ((session-breakpoints (when all-session-breakpoints
                                       (gethash file-name all-session-breakpoints)))
                (workspace-root (lsp-workspace-root file-name)))
            (with-temp-buffer
              (insert-file-contents file-name)
              (-map
               (-lambda (((breakpoint &as &plist :point
                                      :condition :hit-condition :log-message) . remote-bp))
                 (let ((label (propertize
                               (format
                                "%s:%s %s%s"
                                (f-filename file-name)
                                (line-number-at-pos point)
                                (if workspace-root
                                    (concat
                                     (propertize
                                      (format "%s " (f-filename workspace-root))
                                      'face 'lsp-lens-face)
                                     "• ")
                                  "")
                                (propertize (f-dirname (f-relative file-name workspace-root))
                                            'face 'lsp-lens-face))
                               'help-echo (if (or condition hit-condition log-message)
                                              (->> (list (when condition (concat "Condition: " condition))
                                                         (when hit-condition (concat "Hit condition: " hit-condition))
                                                         (when log-message (concat "Log message: " log-message)))
                                                   (-filter #'identity)
                                                   (s-join "\n"))
                                            "Breakpoint"))))
                   (list :key label
                         :icon 'dap-breakpoint
                         :icon-literal (propertize
                                        "⬤ "
                                        'face (if (and remote-bp (gethash "verified" remote-bp))
                                                  'success
                                                'shadow))
                         :label label
                         :actions '(["Condition" dap-ui-breakpoint-condition]
                                    ["Hit Condition" dap-ui-breakpoint-hit-condition]
                                    ["Log Message" dap-ui-breakpoint-log-message]
                                    "--"
                                    ["Remove" dap-ui-breakpoint-delete])
                         :file-name file-name
                         :point point
                         :ret-action #'dap-ui-breakpoints-goto-breakpoint
                         :breakpoint breakpoint)))
               (-zip-fill nil breakpoints session-breakpoints))))))
       (-flatten-n 1))))))

(defvar dap-ui-breakpoints-mode-map
  (-doto (make-sparse-keymap)
    (define-key (kbd "D") #'dap-ui-breakpoint-delete)
    (define-key (kbd "C C") #'dap-ui-breakpoint-condition)
    (define-key (kbd "C H") #'dap-ui-breakpoint-hit-condition)
    (define-key (kbd "C L") #'dap-ui-breakpoint-log-message)))

(define-minor-mode dap-ui-breakpoints-mode
  "UI Breakpoints list minor mode."
  :init-value nil
  :group dap-ui
  :keymap dap-ui-breakpoints-mode-map)

(defun dap-ui-breakpoints--cleanup-hooks ()
  (remove-hook 'dap-terminated-hook #'dap-ui-breakpoints--refresh)
  (remove-hook 'dap-session-changed-hook #'dap-ui-breakpoints--refresh)
  (remove-hook 'dap-continue-hook #'dap-ui-breakpoints--refresh)
  (remove-hook 'dap-stack-frame-changed-hook #'dap-ui-breakpoints--refresh)
  (remove-hook 'dap-breakpoints-changed-hook #'dap-ui-breakpoints--refresh))

(defun dap-ui-breakpoints--refresh (&rest _args)
  (save-excursion
    (with-current-buffer (get-buffer-create dap-ui--breakpoints-buffer)
      (lsp-treemacs-render
       (dap-ui--breakpoints-data)
       " Breakpoints "
       nil
       dap-ui--breakpoints-buffer
       '(["Refresh" dap-ui-breakpoints])))))

(defun dap-ui-breakpoints ()
  (interactive)
  (dap-ui--show-buffer (dap-ui-breakpoints--refresh))
  (dap-ui-breakpoints-mode t)
  (add-hook 'dap-terminated-hook 'dap-ui-breakpoints--refresh)
  (add-hook 'dap-session-changed-hook #'dap-ui-breakpoints--refresh)
  (add-hook 'dap-continue-hook #'dap-ui-breakpoints--refresh)
  (add-hook 'dap-stack-frame-changed-hook #'dap-ui-breakpoints--refresh)
  (add-hook 'dap-breakpoints-changed-hook #'dap-ui-breakpoints--refresh)
  (add-hook 'kill-buffer-hook 'dap-ui-breakpoints--cleanup-hooks nil t))

(defvar dap-ui--many-windows-displayed nil)

(defun dap-ui--show-many-windows (_session)
  "Show auto configured feature windows."
  (unless dap-ui--many-windows-displayed
    (seq-doseq (feature-start-stop dap-auto-configure-features)
      (when-let (start-stop (alist-get feature-start-stop dap-features->windows))
        (funcall (car start-stop))))
    (setq dap-ui--many-windows-displayed t)))

(defun dap-ui--hide-many-windows (_session)
  "Hide all debug windows when sessions are dead."
  (when dap-ui--many-windows-displayed
    (seq-doseq (feature-start-stop dap-auto-configure-features)
      (when-let* ((feature-start-stop (alist-get feature-start-stop dap-features->windows))
                  (buffer-name (symbol-value (cdr feature-start-stop))))
        (when-let (window (get-buffer-window buffer-name))
          (delete-window window))
        (and (get-buffer buffer-name)
             (kill-buffer buffer-name))))
    (setq dap-ui--many-windows-displayed nil)))

;;;###autoload
(defun dap-ui-show-many-windows ()
  "Show auto configured feature windows."
  (interactive)
  (dap-ui--show-many-windows nil))

;;;###autoload
(defun dap-ui-hide-many-windows ()
  "Hide all debug windows when sessions are dead."
  (interactive)
  (dap-ui--hide-many-windows nil))

(define-minor-mode dap-ui-many-windows-mode
  "Shows/hide the windows from `dap-auto-configure-features`"
  :global t
  (cond
   (dap-ui-many-windows-mode
    (add-hook 'dap-stopped-hook #'dap-ui--show-many-windows)
    (add-hook 'dap-terminated-hook #'dap-ui--hide-many-windows))
   (t
    (remove-hook 'dap-stopped-hook #'dap-ui--show-many-windows)
    (remove-hook 'dap-terminated-hook #'dap-ui--hide-many-windows))))

(defcustom dap-ui-repl-prompt ">> "
  "Prompt string for DAP REPL."
  :type 'string
  :group 'dap-ui)

(defvar dap-ui-repl-welcome
  (propertize "*** Welcome to Dap-Ui ***\n"
              'font-lock-face 'font-lock-comment-face)
  "Header line to show at the top of the REPL buffer.
Hack notice: this allows log messages to appear before anything is
evaluated because it provides insertable space at the top of the
buffer.")

(defun dap-ui-repl-process ()
  "Return the process for the dap-ui REPL."
  (get-buffer-process (current-buffer)))

(define-derived-mode dap-ui-repl-mode comint-mode "DAP-REPL"
  "Provide a REPL for the active debug session."
  :group 'dap-ui
  :syntax-table emacs-lisp-mode-syntax-table
  (setq comint-prompt-regexp (concat "^" (regexp-quote dap-ui-repl-prompt))
        comint-input-sender 'dap-ui-input-sender
        comint-process-echoes nil)
  ;; Make opportunistic use of company-mode, but don't require it.
  ;; This means company-backends may be undeclared, so don't emit a
  ;; warning about it.
  (with-no-warnings
    (setq-local company-backends '(dap-ui-repl-company)))
  (unless (comint-check-proc (current-buffer))
    (insert dap-ui-repl-welcome)
    (start-process "dap-ui-repl" (current-buffer) nil)
    (set-process-query-on-exit-flag (dap-ui-repl-process) nil)
    (goto-char (point-max))
    (set (make-local-variable 'comint-inhibit-carriage-motion) t)
    (comint-output-filter (dap-ui-repl-process) dap-ui-repl-prompt)
    (set-process-filter (dap-ui-repl-process) 'comint-output-filter)))

(defun dap-ui-input-sender (_ input)
  "REPL comint handler.
INPUT is the current input."
  (let ((debug-session (dap--cur-active-session-or-die)))
    (if-let ((active-frame-id (-some->> debug-session
                                        dap--debug-session-active-frame
                                        (gethash "id"))))
        (dap--send-message
         (dap--make-request "evaluate"
                            (list :expression input
                                  :frameId active-frame-id
                                  :context "repl"))
         (-lambda ((&hash "success" "message" "body"))
           (-when-let (buffer (get-buffer dap-ui--repl-buffer))
             (with-current-buffer buffer
               (comint-output-filter (dap-ui-repl-process)
                                     (concat (if success (gethash "result" body) message)
                                             "\n"
                                             dap-ui-repl-prompt)))))
         debug-session)
      (error "There is no stopped debug session"))))

;;;###autoload
(defun dap-ui-repl ()
  "Start an adapter-specific REPL.
This could be used to evaluate JavaScript in a browser, to
evaluate python in the context of the debugee, ...."
  (interactive)
  (let ((repl-buf (get-buffer dap-ui--repl-buffer)))
    (unless repl-buf
      (with-current-buffer (get-buffer-create dap-ui--repl-buffer)
        (dap-ui-repl-mode)
        (when (functionp 'company-mode)
          (company-mode 1))
        (setq-local lsp--buffer-workspaces (lsp-workspaces))
        (setq repl-buf (current-buffer))))
    (dap-ui--show-buffer repl-buf)))

(defun dap-ui-repl--calculate-candidates ()
  "Calculate completion candidates.
TEXT is the current input."
  (when (-some->> (dap--cur-session)
          (dap--debug-session-current-capabilities)
          (gethash "supportsCompletionsRequest"))
    (let ((text (comint-get-old-input-default))
          (debug-session (dap--cur-active-session-or-die)))
      (if-let (frame-id (-some->> debug-session
                          dap--debug-session-active-frame
                          (gethash "id")))
          (cons :async
                (lambda (callback)
                  (dap--send-message
                   (dap--make-request "completions"
                                      (list :frameId frame-id
                                            :text text
                                            :column (- (length text) (- (point-at-eol) (point)))))
                   (dap--resp-handler
                    (lambda (result)
                      (-if-let (targets (-some->> result (gethash "body") (gethash "targets")))
                          (funcall callback (-map (-lambda ((item &as &hash "label" "text" "type"))
                                                    (propertize label :text text :type type :dap-completion-item item))
                                                  targets))
                        (funcall callback ()))))
                   debug-session)))))))

(defun dap-ui-repl--post-completion (candidate)
  "Post completion handling for CANDIDATE."
  (let ((to-insert (plist-get (text-properties-at 0 candidate) :text)))
    (when to-insert
      (delete-char (- (length candidate)))
      (insert to-insert))))

(defun dap-ui-repl--annotate (candidate)
  "Get annotation for CANDIDATE."
  (concat " " (plist-get (text-properties-at 0 candidate) :type)))

(defun dap-ui-repl-company (command &optional candidate &rest _args)
  "Dap-Ui REPL backend for company-mode.
See `company-backends' for more info about COMMAND and CANDIDATE."
  (interactive (list 'interactive))
  (cl-case command
    (interactive
     (with-no-warnings ;; opportunistic use of company-mode
       (company-begin-backend 'company-dap-ui-repl)))
    (prefix (dap-ui-repl-company-prefix))
    (ignore-case t)
    (sorted t)
    (match (length candidate))
    (annotation (dap-ui-repl--annotate candidate))
    (candidates (dap-ui-repl--calculate-candidates))
    (post-completion (dap-ui-repl--post-completion candidate))))

(defun dap-ui-repl-company-prefix ()
  "Prefix for company."
  (and (eq major-mode 'dap-ui-repl-mode)
       (or (with-no-warnings ;; opportunistic use of company-mode
             (company-grab-word))
           'stop)))

(provide 'dap-ui)
;;; dap-ui.el ends here
