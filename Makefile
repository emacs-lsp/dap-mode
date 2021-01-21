.PHONY: all build unix-compile windows-compile clean test

EMACS ?= emacs
CASK ?= cask

DAP-GENERAL := dap-launch.el dap-overlays.el dap-variables.el	\
		dap-mode.el dapui.el dap-ui.el dap-mouse.el \
		dap-hydra.el dap-utils.el

# TODO: make a clients/ directory and update melpa recipe
DAP-CLIENTS := dap-chrome.el dap-cpptools.el dap-edge.el		\
		dap-elixir.el dap-firefox.el dap-gdb-lldb.el		\
		dap-go.el dap-lldb.el dap-netcore.el dap-node.el	\
		dap-php.el dap-pwsh.el dap-python.el dap-ruby.el

all:
	$(CASK) build

build:
	$(CASK) install

# NOTE: treemacs also sets treemacs-no-load-time-warnings to t in its Makefile, so I guess it's OK?
unix-compile:
	@$(CASK) $(EMACS) -Q --batch \
	-L . \
	--eval '(setq treemacs-no-load-time-warnings t)' \
	-f batch-byte-compile $(DAP-GENERAL) $(DAP-CLIENTS)

windows-compile:
	@$(CASK) $(EMACS) -Q --batch \
	-l test/windows-bootstrap.el \
	-L . \
	--eval '(setq treemacs-no-load-time-warnings t)' \
	-f batch-byte-compile $(DAP-GENERAL) $(DAP-CLIENTS)

unix-ci: clean build unix-compile test
windows-ci: clean windows-compile test

clean:
	rm -rf .cask *.elc

test:
	$(CASK) exec ert-runner -L .
