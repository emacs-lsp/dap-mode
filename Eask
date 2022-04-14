(package "dap-mode"
         "8.0.0"
         "Debug Adapter Protocol mode")

(package-file "dap-mode.el")

(files "*.el")

(source "gnu")
(source "melpa")

(depends-on "emacs" "26.1")
(depends-on "lsp-java")
(depends-on "markdown-mode")
(depends-on "tree-mode")
(depends-on "bui")

(development
 (depends-on "ecukes")
 (depends-on "espuds")
 (depends-on "undercover")
 (depends-on "ert")
 (depends-on "ert-runner"))

;;(setq eask-verbosity 4)

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432

;; NOTE: treemacs also sets treemacs-no-load-time-warnings to t in its Makefile, so I guess it's OK?
(setq treemacs-no-load-time-warnings t)
