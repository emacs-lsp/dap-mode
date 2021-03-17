.PHONY: all build unix-compile windows-compile clean unix-test windows-test

EMACS ?= emacs
CASK ?= cask

DAP-GENERAL := dap-launch.el dap-overlays.el dap-variables.el	\
		dap-mode.el dapui.el dap-ui.el dap-mouse.el \
		dap-hydra.el dap-utils.el

# TODO: make a clients/ directory and update melpa recipe
DAP-CLIENTS := dap-chrome.el dap-cpptools.el dap-edge.el		\
		dap-elixir.el dap-firefox.el dap-gdb-lldb.el		\
		dap-go.el dap-lldb.el dap-netcore.el dap-node.el	\
		dap-php.el dap-pwsh.el dap-python.el dap-ruby.el	\
		dap-codelldb.el

TEST-FILES := test/windows-bootstrap.el $(shell ls test/dap-*.el)
LOAD-FILE = -l $(test-file)
LOAD-TEST-FILES := $(foreach test-file, $(TEST-FILES), $(LOAD-FILE))

all:
	$(CASK) build

build:
	$(CASK) install

# NOTE: treemacs also sets treemacs-no-load-time-warnings to t in its Makefile, so I guess it's OK?
unix-compile:
	@$(CASK) $(EMACS) -Q --batch \
	-L . \
	--eval '(setq treemacs-no-load-time-warnings t)' \
	--eval '(setq byte-compile-error-on-warn t)' \
	-f batch-byte-compile $(DAP-GENERAL) $(DAP-CLIENTS)

windows-compile:
	@$(CASK) $(EMACS) -Q --batch \
	--eval '(setq emacs-lsp-ci t)' \
	-l test/windows-bootstrap.el \
	-L . \
	--eval '(setq treemacs-no-load-time-warnings t)' \
	--eval '(setq byte-compile-error-on-warn t)' \
	-f batch-byte-compile $(DAP-GENERAL) $(DAP-CLIENTS)

unix-ci: clean build unix-compile unix-test

windows-ci: CASK=
windows-ci: clean windows-compile windows-test

clean:
	rm -rf .cask *.elc

unix-test:
	$(CASK) exec ert-runner -L .

windows-test:
	@$(EMACS) -Q --batch \
		--eval '(setq emacs-lsp-ci t)' \
		-l test/windows-bootstrap.el \
		-L . \
		$(LOAD-TEST-FILES) \
		--eval "(ert-run-tests-batch-and-exit \
		'(and (not (tag no-win)) (not (tag org))))"
