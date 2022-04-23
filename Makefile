EMACS ?= emacs
KEG ?= keg

.PHONY: test

all: test

test:
	@echo "Emacs binary at $(shell which $(EMACS))"
	$(KEG) exec $(EMACS) --batch -q \
             --exec "(princ (format \"Emacs version: %s\n\" (emacs-version)) t)" \
	     --exec "(princ (format \"Org version: %s\n\" (org-version)) t)"
	$(KEG) exec $(EMACS) --batch -q -l org-clock-convenience.el \
                             -l test/test-org-clock-convenience.el \
                             --eval "(ert-run-tests-batch-and-exit nil)"

