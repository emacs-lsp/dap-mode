.PHONY: all build compile clean test

EMACS ?= emacs
EASK ?= eask

# TODO: add `lint` and `checkdoc`
ci: clean build compile test

build:
	$(EASK) package
	$(EASK) install
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

checkdoc:
	@echo "Run checkdoc..."
	$(EASK) lint checkdoc

lint:
	@echo "Run package-lint..."
	$(EASK) lint package
