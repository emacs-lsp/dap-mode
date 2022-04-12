.PHONY: all build compile clean test

EMACS ?= emacs
EASK ?= eask

DAP-GENERAL := dap-launch.el dap-overlays.el dap-variables.el	\
		dap-mode.el dapui.el dap-ui.el dap-mouse.el \
		dap-hydra.el dap-utils.el

# TODO: make a clients/ directory and update melpa recipe
DAP-CLIENTS := dap-chrome.el dap-cpptools.el dap-edge.el		\
		dap-elixir.el dap-firefox.el dap-gdb-lldb.el		\
		dap-go.el dap-lldb.el dap-netcore.el dap-node.el	\
		dap-php.el dap-pwsh.el dap-python.el dap-ruby.el	\
		dap-codelldb.el dap-erlang.el dap-swi-prolog.el

TEST-FILES := test/windows-bootstrap.el $(shell ls test/dap-*.el)
LOAD-FILE = -l $(test-file)
LOAD-TEST-FILES := $(foreach test-file, $(TEST-FILES), $(LOAD-FILE))

ci: clean build compile test

build:
	$(EASK) package
	$(EASK) install --verbose 4
	$(EASK) clean-elc

compile:
	@echo "Compiling..."
	$(EASK) compile

clean:
	$(EASK) clean-all

test:
	@echo "Testing..."
	$(EASK) install-deps --dev
	$(EASK) exec ert-runner -L .
