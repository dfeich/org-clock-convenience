(require 'ert)
(require 'org-clock-convenience)
(require 'cl-lib)

(defun occ-extract-re-vals (fields re re-fields)
  (let ((pos (point))
        (line (buffer-substring (point)
                                (progn (forward-line 1) (point))))
        val)
    (cl-loop
     for field in fields
     do (progn
          (goto-char pos)
          (setq val 
                (org-clock-convenience-get-re-field
                 field re re-fields
                 (format "Regexp does not match line:\n%s" line))))
     collect (list field val) into results
     finally return results)))

(defun occ-extract-agdline-vals (fields)
  (occ-extract-re-vals fields
                       org-clock-convenience-clocked-agenda-re
                       org-clock-convenience-clocked-agenda-fields))

(defun occ-extract-clockline-vals (fields)
  (occ-extract-re-vals fields
                       org-clock-convenience-tr-re
                       org-clock-convenience-tr-fields))

(defun occ-test-regexp (line fieldsvalues re re-fields)
  (let (val)
    (should
     (equal fieldsvalues
            (with-temp-buffer
              (insert line)
              (goto-char (point-min))
              (occ-extract-re-vals (mapcar #'car  fieldsvalues)
                                   re re-fields))))))

(defun occ-test-agenda-regexp (line fieldsvalues)
  (occ-test-regexp line fieldsvalues
                   org-clock-convenience-clocked-agenda-re
                   org-clock-convenience-clocked-agenda-fields))

(defun occ-test-clockline-regexp (line fieldsvalues)
  (occ-test-regexp line fieldsvalues
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
    (occ-test-agenda-regexp line res)))

(ert-deftest agenda-re2 ()
  (let ((line "  tasks2020:   7:55-9:20 Clocked:   (0:10) Email and Misc")
        (res '((d1-hours " 7")
               (d1-minutes "55")
               (d2-hours "9")
               (d2-minutes "20"))))
    (occ-test-agenda-regexp line res)))

(ert-deftest clockline-re1 ()
  (let ((line "    CLOCK: [2016-01-15 Fri 15:25]--[2016-01-15 Fri 18:10] =>  2:45")
        (res '((d1-year "2016") (d1-month "01") (d1-day "15")
               (d1-hours "15") (d1-minutes "25")
               (d2-year "2016") (d2-month "01") (d2-day "15")
               (d2-hours "18") (d2-minutes "10"))))
    (occ-test-clockline-regexp line res)))


(defmacro occ-with-tempagenda (orgentries datestr body)
  "docstring"
  `(let* ((testfname (make-temp-file "occ-test-agenda-" nil ".org")))
     (with-temp-file testfname
       (insert ,orgentries))
     (setq org-agenda-files (list testfname)
           org-agenda-start-with-log-mode t)
     (org-agenda-list 1 ,datestr)
     (with-current-buffer org-agenda-buffer
       ,body)
     (set-buffer (get-file-buffer testfname))
     (save-buffer)
     (kill-this-buffer)
     (delete-file testfname)))

(defun occ-test-macro ()
  (occ-with-tempagenda
   "* TaskA
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

"
   "2022-04-15"
   (princ (format "testfile: %s\n%s\n" testfname (buffer-string)))))

(defun test-agenda ()
  (let* ((testfname (make-temp-file "occ-test-agenda-" nil ".org")))
    (setq org-agenda-files `(,testfname)
          org-agenda-start-with-log-mode t)
    (occ-create-agenda testfname)
    (org-agenda-list 1 "2022-04-15")
    (with-current-buffer org-agenda-buffer
      (princ (format "testfile: %s\n%s\n" testfname (buffer-string)))
      (goto-char (point-min))
      (org-clock-convenience-forward-log-line)
      (org-clock-convenience-forward-log-line)
      (forward-line 0)
      (princ (format "before: %s\n" (occ-extract-agdline-vals '(d1-time d2-time))))
      (forward-line 0)
      (org-clock-convenience-goto-agenda-tr-field 'd2-minutes)
      (org-clock-convenience-timestamp-up)
      (forward-line 0)
      (princ (format "after: %s\n" (occ-extract-agdline-vals '(d1-time d2-time)))))
    (set-buffer (get-file-buffer testfname))
    (save-buffer)
    (kill-this-buffer)
    (delete-file testfname)))
