EMACS ?= emacs

# only use KEG if it is available in the PATH or the location is explicitely
# given
ifndef KEG
  ifneq (, $(shell which keg))
  KEGEXEC = keg exec
  endif
else
  KEGEXEC = $(KEG) exec
endif

ifdef OCC_CONFIG
loadconf = -l $(OCC_CONFIG)
endif

ifdef OCC_INIT
loadinit = -l $(OCC_INIT)
endif

ifdef TESTNAME
testcmd = (ert \"$(TESTNAME)\")
else
testcmd = (ert t)
endif

.PHONY: test debug test-regx

all: test

test:
	@echo "Emacs binary at $(shell which $(EMACS))"
	$(KEGEXEC) $(EMACS) --batch \
	                    $(loadinit) \
                            -l org-clock-convenience.el \
                            -l test/test-org-clock-convenience.el \
	                    $(loadconf) \
             --eval "(princ (format \"Emacs version: %s\n\" (emacs-version)) t)" \
	     --eval "(princ (format \"Org version: %s\n\" (org-version)) t)" \
             --eval "(ert-run-tests-batch-and-exit nil)"

debug:
	$(KEGEXEC) $(EMACS) -q \
	                    $(loadinit) \
                            -l org-clock-convenience.el \
                            -l test/test-org-clock-convenience.el \
	                    $(loadconf) \
                            --eval "(progn (setq occ-no-cleanup t)$(testcmd))"

test-regx:
	$(KEGEXEC) $(EMACS) --batch \
	                    $(loadinit) \
                            -l org-clock-convenience.el \
                            -l test/test-org-clock-convenience.el \
	                    $(loadconf) \
                            --eval "(occ-print-all-agendaline-vals)"
