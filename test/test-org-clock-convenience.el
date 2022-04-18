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

(defun occ-create-agenda(filename)
  "Create a test agenda file"
  (with-temp-buffer
    (insert "* TaskA
  :LOGBOOK:
  CLOCK: [2022-04-15 Fri 08:00]--[2022-04-15 Fri 09:00] =>  1:00
  :END:

* TaskB
  :LOGBOOK:
  CLOCK: [2022-04-15 Fri 10:00]--[2022-04-15 Fri 10:05] =>  0:05
  :END:

* TaskC
  :LOGBOOK:
  CLOCK: [2022-04-15 Fri 11:00]--[2022-04-15 Fri 12:00] =>  1:00
  :END:

")
    (write-file filename)))

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

(defun test-agenda ()
  (let* ((testfname "/tmp/testagenda.org"))
    (setq org-agenda-files `(,testfname)
          org-agenda-start-with-log-mode t)
    (occ-create-agenda testfname)
    (org-agenda-list 1 "2022-04-15")
    ;;(org-agenda-goto-date "2022-04-15")
    (with-current-buffer org-agenda-buffer
      (princ (buffer-string))
      
      (goto-char (point-min))
      (org-clock-convenience-forward-log-line)
      (org-clock-convenience-forward-log-line)
      (forward-line 0)
      (princ (format "PARSED: %s" 
                     (org-clock-convenience-get-re-field
                      'd1-time
                      org-clock-convenience-clocked-agenda-re
                      org-clock-convenience-clocked-agenda-fields))))
    
    
    )
  
  )

