EMACS ?= emacs
KEG ?= keg

ifdef OCC_CONFIG
loadconf = -l $(OCC_CONFIG)
endif

.PHONY: test debug test-regx

all: test

test:
	@echo "Emacs binary at $(shell which $(EMACS))"
	$(KEG) exec $(EMACS) --batch -q \
             --exec "(princ (format \"Emacs version: %s\n\" (emacs-version)) t)" \
	     --exec "(princ (format \"Org version: %s\n\" (org-version)) t)"
	$(KEG) exec $(EMACS) --batch -q -l org-clock-convenience.el \
                             -l test/test-org-clock-convenience.el \
	                     $(loadconf) \
                             --eval "(ert-run-tests-batch-and-exit nil)"

debug:
	$(KEG) exec $(EMACS) -q -l org-clock-convenience.el \
                             -l test/test-org-clock-convenience.el \
                             --eval "(ert t)"

test-regx:
	$(KEG) exec $(EMACS) --batch -q -l org-clock-convenience.el \
                             -l test/test-org-clock-convenience.el \
	                     $(loadconf) \
                             --eval "(occ-print-all-agendaline-vals)"
