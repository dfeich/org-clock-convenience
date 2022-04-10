(require 'ert)
(require 'org-clock-convenience)
(require 'cl-lib)

(defun occ-check-regexp (line fieldsvalues re re-fields)
  (let (val)
    (with-temp-buffer
      (insert line)
      (cl-loop
       for pair in fieldsvalues
       do (progn
            (goto-char (point-min))
            (setq val 
                  (org-clock-convenience-get-re-field
                   (car pair) re re-fields
                   (format "Regexp does not match line:\n%s" line))))
       collect (list (car pair) val) into results
       finally return results))))

(defun occ-check-agenda-regexp (agdline fieldsvalues)
  (occ-check-regexp agdline fieldsvalues
                    org-clock-convenience-clocked-agenda-re
                    org-clock-convenience-clocked-agenda-fields))

(defun occ-check-clockline-regexp (agdline fieldsvalues)
  (occ-check-regexp agdline fieldsvalues
                    org-clock-convenience-tr-re
                    org-clock-convenience-tr-fields))

;; Tests

;; older agenda default format lines contained space-padded hours (a space
;; after the dash and a single hour digit)
(ert-deftest agenda-re1 ()
  (let ((line "  tasks2020:   7:55- 9:20 Clocked:   (0:10) Email and Misc")
        (res '((d1-hours " 7")
               (d1-minutes "55")
               (d2-hours " 9")
               (d2-minutes "20"))))
    (should (equal res
                   (occ-check-agenda-regexp line res)))))

(ert-deftest agenda-re2 ()
  (let ((line "  tasks2020:   7:55-9:20 Clocked:   (0:10) Email and Misc")
        (res '((d1-hours " 7")
               (d1-minutes "55")
               (d2-hours "9")
               (d2-minutes "20"))))
    (should (equal res
                   (occ-check-agenda-regexp line res)))))

(ert-deftest clockline-re1 ()
  (let ((line "    CLOCK: [2016-01-15 Fri 15:25]--[2016-01-15 Fri 18:10] =>  2:45")
        (res '((d1-year "2016") (d1-month "01") (d1-day "15")
               (d1-hours "15") (d1-minutes "25")
               (d2-year "2016") (d2-month "01") (d2-day "15")
               (d2-hours "18") (d2-minutes "10"))))
    (should (equal res
                   (occ-check-clockline-regexp line res)))))




