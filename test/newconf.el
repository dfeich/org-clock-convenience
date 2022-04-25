(setq org-agenda-prefix-format '((agenda . " %i %-12:c%?-12t %?-10e% s")
                                 (todo . " %i %-12:c")
                                 (tags . " %i %-12:c")
                                 (search . " %i %-12:c")))

(setq org-clock-convenience-clocked-agenda-re
      "^ +\\([^:]+\\):[[:space:]]*\\(\\([ \t012][0-9]\\):\\([0-5][0-9]\\)\\)\\(?:-\\(\\( ?[012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\|.*\\)?\\(?:[[:space:]]+\\([0-9:dhmin]+\\)\\)?[[:space:]]+Clocked:[[:space:]]+\\(([0-9]+:[0-5][0-9])\\|(-)\\)")

(setq org-clock-convenience-clocked-agenda-fields
      '(filename d1-time d1-hours d1-minutes d2-time d2-hours d2-minutes effort duration))
