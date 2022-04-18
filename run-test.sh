#!/bin/bash

emacs --batch -q -l org-clock-convenience.el -l test/test-org-clock-convenience.el \
      --eval "(ert-run-tests-batch nil)"
