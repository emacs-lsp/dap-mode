(package "dap-mode"
         "0.7"
         "Debug Adapter Protocol mode")

(website-url "https://github.com/emacs-lsp/dap-mode")
(keywords "languages" "debug")

(package-file "dap-mode.el")

(files "*.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source "gnu")
(source "melpa")

(depends-on "emacs" "27.1")
(depends-on "lsp-mode")
(depends-on "lsp-treemacs")
(depends-on "lsp-docker")
(depends-on "posframe")
(depends-on "bui")
(depends-on "dash")
(depends-on "f")
(depends-on "s")
(depends-on "ht")

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
