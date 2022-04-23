(require 'ert)
(require 'org-clock-convenience)
(require 'cl-lib)

(defvar occ-testagenda1 "* TaskA
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions

(defun occ-extract-re-vals (fields re re-fields)
  (let ((pos (point))
        (line (buffer-substring (progn (forward-line 0) (point))
                                (progn (forward-line 1) (point))))
        val)
    (cl-loop
     for field in fields
     do (progn
          (goto-char pos)
          (forward-line 0)
          (setq val 
                (org-clock-convenience-get-re-field
                 field re re-fields
                 (format "Regexp does not match line:\n%s" line))))
     collect (list field val) into results
     finally do (goto-char pos)
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

(defmacro occ-with-tempagenda (orgentries datestr &rest body)
  "Create an angeda view from the ORGENTRIES string and execute BODY.
The agenda view will be created for the date given in DATESTR.
The agenda buffer will be current and point will be at point-min
when executing BODY."
  `(let* ((testfname (make-temp-file "occ-test-agenda-" nil ".org"))
          (org-agenda-files (list testfname))
          (org-agenda-start-with-log-mode t))
     (with-temp-file testfname
       (insert ,orgentries))
     (org-agenda-list 1 ,datestr)
     (with-current-buffer org-agenda-buffer
       (goto-char (point-min))
       ,@body)
     (set-buffer (get-file-buffer testfname))
     (save-buffer)
     (kill-this-buffer)
     (delete-file testfname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test definitions

(ert-deftest occ-test-clockline-re1 ()
  (let ((line "    CLOCK: [2016-01-15 Fri 15:25]--[2016-01-15 Fri 18:10] =>  2:45")
        (res '((d1-year "2016") (d1-month "01") (d1-day "15")
               (d1-hours "15") (d1-minutes "25")
               (d2-year "2016") (d2-month "01") (d2-day "15")
               (d2-hours "18") (d2-minutes "10"))))
    (occ-test-clockline-regexp line res)))

(ert-deftest occ-test-agenda-re1 ()
  (occ-with-tempagenda
   occ-testagenda1 "2022-04-15"
   (search-forward "TaskA")
   (should
    (equal (occ-extract-agdline-vals '(d1-time d2-time))
           '((d1-time " 8:00") (d2-time " 9:00"))))))

(ert-deftest occ-test-timestamp-change-hours ()
  (occ-with-tempagenda
   occ-testagenda1 "2022-04-15"
   (search-forward "TaskB")
   (forward-line 0)
   (org-clock-convenience-goto-agenda-tr-field 'd2-hours)
   (org-clock-convenience-timestamp-change 2)
   (should
    (equal (occ-extract-agdline-vals '(d1-time d2-time))
           '((d1-time "10:00") (d2-time "12:05"))))
   ;; also test whether the org source file was changed
   (org-clock-convenience-goto-ts)
   (should
    (equal (occ-extract-clockline-vals '(d1-time d2-time))
           '((d1-time " 10:00") (d2-time " 12:05"))))))

(ert-deftest occ-test-timestamp-change-minutes ()
  (occ-with-tempagenda
   occ-testagenda1 "2022-04-15"
   (search-forward "TaskB")
   (forward-line 0)
   (org-clock-convenience-goto-agenda-tr-field 'd2-minutes)
   (let ((org-time-stamp-rounding-minutes '(0 10)))
     (org-clock-convenience-timestamp-change 1))
   (should
    (equal (occ-extract-agdline-vals '(d1-time d2-time))
           '((d1-time "10:00") (d2-time "10:10"))))
   ;; also test whether the org source file was changed
   (org-clock-convenience-goto-ts)
   (should
    (equal (occ-extract-clockline-vals '(d1-time d2-time))
           '((d1-time " 10:00") (d2-time " 10:10"))))))

;; NOTE: There currently is a problem with UNDO
;; (ert-deftest occ-test-timestamp-change-undo ()
;;   (occ-with-tempagenda
;;    occ-testagenda1 "2022-04-15"
;;    (search-forward "TaskB")
;;    (forward-line 0)
;;    (org-clock-convenience-goto-agenda-tr-field 'd2-hours)
;;    (org-clock-convenience-timestamp-change 2)
;;    (undo)
;;    ;; (save-excursion
;;    ;;   (princ
;;    ;;    (format "DEBUG: %s"  (buffer-substring (progn (forward-line 0) (point))
;;    ;;                                           (progn (forward-line 1) (point))))))
;;    (should
;;     (equal (occ-extract-agdline-vals '(d1-time d2-time))
;;            '((d1-time "10:00") (d2-time "12:05"))))
;;    ;; also test whether the org source file was changed
;;    (org-clock-convenience-goto-ts)
;;    (should
;;     (equal (occ-extract-clockline-vals '(d1-time d2-time))
;;            '((d1-time " 10:00") (d2-time " 12:05"))))))

(ert-deftest occ-test-fill-gap-both ()
  (occ-with-tempagenda
   occ-testagenda1 "2022-04-15"
   (search-forward "TaskB")
   (org-clock-convenience-fill-gap-both)
   (should
    (equal (occ-extract-agdline-vals '(d1-time d2-time))
           '((d1-time "09:00") (d2-time "11:00"))))
   ;; also test whether the org source file was changed
   (forward-line 0)
   (org-clock-convenience-goto-agenda-tr-field 'd2-hours)
   (org-clock-convenience-goto-ts)
   ;; (save-excursion  (princ  (buffer-substring (progn (forward-line 0) (point))
   ;;                                            (progn (forward-line 1) (point)))))
   (should
    (equal (occ-extract-clockline-vals '(d1-time d2-time))
           '((d1-time " 09:00") (d2-time " 11:00"))))))

