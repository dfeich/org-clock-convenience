EMACS ?= emacs

# only use KEG if it is available in the PATH or the location is explicitely
# given
ifndef KEG
  ifneq (, $(shell which keg))
  KEGEXEC = keg exec
  KEG = keg
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

.PHONY: test debug test-regx clean

.SUFFIXES: .el .elc
.el.elc:
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<


all: test

compile: org-clock-convenience.elc
	@echo "Byte compiling $<"
	$(KEGEXEC) $(EMACS) --batch \
              --eval "(byte-compile-file \"org-clock-convenience.el\")"

test: org-clock-convenience.elc
	@echo "Emacs binary at $(shell which $(EMACS))"
	$(KEGEXEC) $(EMACS) --batch \
	                    $(loadinit) \
                            -l org-clock-convenience.elc \
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
clean:
	rm -f org-clock-convenience.elc
	@if test x"${KEGEXEC}" != x; then \
          echo "KEGEXEX is >${KEGEXEC}<" \
	  $(KEG) clean; \
        fi
