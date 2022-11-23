;;; org-clock-convenience.el --- Convenience functions for org time tracking

;; Author: Derek Feichtinger <dfeich.gmail.com>
;; Keywords: convenience
;; Package-Requires: ((org "8") (emacs "24.3"))
;; Homepage: https://github.com/dfeich/org-clock-convenience
;; Version: 1.3

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; Convenience functions for easier time tracking.  Provides commands
;; for changing timestamps directly from the agenda view.
;;
;; `org-clock-convenience-timestamp-up' and
;; `org-clock-convenience-timestamp-down' can be used to modify the
;; clocked times in and agenda log line in steps (same as on
;; timestamps in a normal org file). `org-clock-convenience-fill-gap'
;; modifies the timestamp at point to connect to the previous/next
;; clocked timestamp. `org-clock-convenience-fill-gap-both' will fill
;; the gap of both the starting time as well as of the ending time of
;; the time range in the current agenda log line. Also provides a
;; number of utility functions to work with and find/analyze
;; timestamps.

;;; Code:
(require 'org)
(require 'org-element)
(require 'cl-lib)

;; save-mark-and-excursion in Emacs 25 works like save-excursion did before
(eval-when-compile
  (if (< emacs-major-version 25)
      (defmacro org-clock-convenience-save-mark-and-excursion (&rest body)
        `(save-excursion ,@body))
    (defmacro org-clock-convenience-save-mark-and-excursion (&rest body)
      `(save-mark-and-excursion ,@body)))
  (if (version< org-version "9.4")
      (defmacro org-clock-convenience-org-hide-drawer-toggle (&optional
                                                              force
                                                              no-error
                                                              element)
        `(org-flag-drawer nil))
    (defmacro org-clock-convenience-org-hide-drawer-toggle (&optional
                                                              force
                                                              no-error
                                                              element)
      `(org-hide-drawer-toggle ,force ,no-error ,element))))


(defvar org-clock-convenience-clocked-agenda-re
  "^ +\\([^:]+\\):[[:space:]]*\\(\\([ \t012][0-9]\\):\\([0-5][0-9]\\)\\)\\(?:-\\(\\( ?[012]?[0-9]\\):\\([0-5][0-9]\\)\\)\\|.*\\)?[[:space:]]+Clocked:[[:space:]]+\\(([0-9]+:[0-5][0-9])\\|(-)\\)"
  "Regexp of a clocked time range log line in the Org agenda buffer.")

(defvar org-clock-convenience-clocked-agenda-fields
  '(category d1-time d1-hours d1-minutes d2-time d2-hours d2-minutes duration)
  "Field names corresponding to submatches of `org-clock-convenience-clocked-agenda-re'.")

(defvar org-clock-convenience-tr-re
  (concat " *CLOCK: *\\["
	  org-ts-regexp0 "\\]\\(?:--\\[\\)?"
	  org-ts-regexp0 "?"
	  "\\(?:\\] *=> *\\([0-9]+:[0-9]\\{2\\}\\)\\)?")
  "Regexp of a clocked time range in an org file for field analysis.

match:    CLOCK: [2016-01-15 Fri 15:25]--[2016-01-15 Fri 18:10] =>  2:45
match:    CLOCK: [2016-01-08 Fri 14:30]--[2016-01-08] => -14:30
match:    CLOCK: [2016-01-08 Fri 10:30]
no:       CLOCK: [2016-01-08 Fri 10:30] => 2:30")

(defvar org-clock-convenience-tr-fields
  '(d1-timestamp d1-year d1-month d1-day d1-dayname d1-time d1-hours d1-minutes
		 d2-timestamp d2-year d2-month d2-day d2-dayname d2-time d2-hours d2-minutes
		 sum)
  "Field names corresponding to submatches of `org-clock-convenience-tr-re.")

(defun org-clock-convenience-goto-re-field (fieldname re fnames &optional errmsg)
  "Move cursor to the specified FIELDNAME in the regexp RE.
The fieldnames are given as a list of symbols in FNAMES.  An error message
for the case of the regexp not matching can be passed in ERRMSG."
  (let ((idx (or (cl-position fieldname fnames)
		 (error "No such field name: %s" fieldname))))
    (unless (looking-at re)
      (error (or errmsg
		 "Error: regexp for analyzing fields does not match here")))
    (goto-char (match-beginning (1+ idx)))))

(defun org-clock-convenience-get-re-field (fieldname re fnames &optional errmsg)
  "Return contents of field FIELDNAME defined by the regexp RE.
The fieldnames are given as a list of symbols in FNAMES.  An error message
for the case of the regexp not matching can be passed in ERRMSG."
  (let ((idx (or (cl-position fieldname fnames)
		 (error "No such field name: %s" fieldname))))
    (unless (looking-at re)
      (error (or errmsg
		 "Error: regexp for analyzing fields does not match here")))
    (match-string (1+ idx))))

(defun org-clock-convenience-get-fieldname (point re fnames &optional ignore-lst errmsg)
  "Return field name of submatch where POINT is located.
The field names are based on the sub-patterns defined by the
regexp RE and the passed field names list FNAMES. So, the field
name of the first submatch corresponds to the first name in the
field names list, etc.  RE must match from the beginning of
line.  The optional parameter IGNORE-LST can contain a list of
submatch field names to ignore (sometimes there's a subpattern
containing several other subpatterns, and one wants only the
names of the smaller subpatterns).  ERRMSG allows specifying an
error message if RE is not matching."
  (org-clock-convenience-save-mark-and-excursion
    (goto-char point)
    (beginning-of-line)
    (cl-assert (looking-at re) nil
	       (or errmsg
		   "Error: regexp for analyzing fields does not match here")))
  (cl-loop
   for field in fnames
   with cnt = 0
   do (cl-incf cnt)
   if (and (not (memq field ignore-lst))
	   (org-pos-in-match-range point cnt))
   return field
   finally return nil))

(defun org-clock-convenience-goto-tr-field (fieldname)
  "Position point inside a field of the clocked time range in the current line.
The field is defined by FIELDNAME and corresponds to one of the names
in `org-clock-convenience-tr-fields'."
  (beginning-of-line)
  (org-clock-convenience-goto-re-field fieldname org-clock-convenience-tr-re org-clock-convenience-tr-fields
				       "Error: not on a clocked time log line"))

(defun org-clock-convenience-goto-agenda-tr-field (fieldname)
  "Move cursor to the FIELDNAME of a agenda view clocked log line."
  (cl-assert (eq major-mode 'org-agenda-mode) nil "Error: Not in agenda mode")
  (beginning-of-line)
  (org-clock-convenience-goto-re-field fieldname org-clock-convenience-clocked-agenda-re
				       org-clock-convenience-clocked-agenda-fields
				       "Error: not on a clocked time log line"))

;;;###autoload
(defun org-clock-convenience-forward-log-line (&optional noerror)
  "Move cursor to the next agenda log line.
If NOERROR is non-nil, don't fail with an error, but return nil."
  (interactive)
  (if (re-search-forward org-clock-convenience-clocked-agenda-re nil t)
      (org-clock-convenience-goto-agenda-tr-field 'd1-minutes)
    (if noerror
        nil
      (error "Error: No next log line"))))

;;;###autoload
(defun org-clock-convenience-backward-log-line ()
  "Move cursor to the previous agenda log line."
  (interactive)
  (if (re-search-backward org-clock-convenience-clocked-agenda-re nil t)
      (org-clock-convenience-goto-agenda-tr-field 'd2-minutes)
    (error "Error: No previous log line")))

(defun org-clock-convenience-get-agenda-tr-fieldname (point)
  "Return field name of time range where POINT is located.
The field names are based of the sub-patterns defined by
org-clock-convenience-clocked-agenda-re.  The function can only be used
in a log line of the agenda buffer."
  (cl-assert (eq major-mode 'org-agenda-mode) nil "Error: Not in agenda mode")
  (org-clock-convenience-get-fieldname point
				       org-clock-convenience-clocked-agenda-re
				       org-clock-convenience-clocked-agenda-fields
				       '(d1-time d2-time)
				       "Error: not on a clocked time log line"))

(defun org-clock-convenience-at-timefield-p ()
  "Return non-nil if point is on a clocked time field in the log agenda view."
  (pcase (org-clock-convenience-get-agenda-tr-fieldname (point))
    ((or `d1-hours `d2-hours `d1-minutes `d2-minutes) t)
    (default nil)))

;;;###autoload
(defun org-clock-convenience-goto-ts ()
  "From agenda log line goto to corresponding timestamp position in org file.

Goto to position inside of the timestamp in the agenda file corresponding
to the current position of point in the agenda log line."
  (interactive)
  (let* ((fieldname (or (org-clock-convenience-get-agenda-tr-fieldname (point))
			(error "Error: Not on a time range field position")))
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (if (called-interactively-p 'any)
	(switch-to-buffer buffer)
      (set-buffer buffer))
    (goto-char pos)
    (org-clock-convenience-open-if-in-drawer)
    (org-clock-convenience-goto-tr-field fieldname)))

;; TODO: why do I need to hit g twice (rebuild agenda buffer) before I
;; can see the changes take effect?
(defun org-clock-convenience-timestamp-change (n)
  "Change timestamp by N in agenda buffer.
The change is carried out in the respective clock line of the
associated org agenda file."
  (let* ((pos (point))
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (fieldname (org-clock-convenience-get-agenda-tr-fieldname (point)))
         (timefield (pcase (cl-subseq (symbol-name fieldname) 0 3)
			  ("d1-" 'd1-time)
			  ("d2-" 'd2-time)))
         (agenda-fieldlen (org-clock-convenience-save-mark-and-excursion
                            (forward-line 0)
                            (length
                             (org-clock-convenience-get-re-field
                              timefield
                              org-clock-convenience-clocked-agenda-re
                              org-clock-convenience-clocked-agenda-fields))))
         updated-time)
    (org-with-remote-undo buffer
      (org-clock-convenience-save-mark-and-excursion
        (org-clock-convenience-goto-ts)
        (org-timestamp-change n nil 'updown)
        (beginning-of-line)
        (looking-at org-clock-convenience-tr-re)
        ;; a bit ugly. regrettably need to replace the leading space, because
        ;; the org-ts-regexp0 defines the leading space to be part of the pattern
        (setq updated-time
	      (replace-regexp-in-string
               " *" ""
	       (org-clock-convenience-get-re-field
                timefield
		org-clock-convenience-tr-re
		org-clock-convenience-tr-fields)))
        ;; Field in agenda maybe be shorter than field in clocked
        ;; LOGBOOK line, because newer org versions do not show
        ;; leading zero. The following moves the point accordingly
        ;; when going from 9:55 to 10:00.
        (cl-incf pos (- (length updated-time) agenda-fieldlen)))
      (org-clock-convenience-goto-agenda-tr-field timefield)
      (let* ((inhibit-read-only t)
             (props (text-properties-at (point))))
	(delete-char (length updated-time))
	(insert (propertize
                 updated-time
                 'face 'secondary-selection
                 'org-redo-cmd (plist-get props 'org-redo-cmd)
                 'org-agenda-type (plist-get props 'org-agenda-type)
                 'org-last-args (plist-get props 'org-last-args)
                 'org-series-cmd (plist-get props 'org-series-cmd)
                 'org-series-redo-cmd (plist-get props 'org-series-redo-cmd)))))
    (goto-char pos)))

;;;###autoload
(defun org-clock-convenience-timestamp-up (&optional arg)
  "Increase the date item at the cursor by one.
Used in a clocked line from the agenda view.  If the cursor is on
the hour field, change the hour.  If it is on the minutes field,
change the minutes.  With prefix ARG, change by that many units."
  (interactive "p")
  (org-clock-convenience-timestamp-change (prefix-numeric-value arg)))

;;;###autoload
(defun org-clock-convenience-timestamp-down (&optional arg)
  "Increase the date item at the cursor by one.
Used in a clocked line from the agenda view.  If the cursor is on
the hour field, change the hour.  If it is on the minutes field,
change the minutes.  With prefix ARG, change by that many units."
  (interactive "p")
  (org-clock-convenience-timestamp-change (- (prefix-numeric-value arg))))

;;;###autoload
(defun org-clock-convenience-fill-gap ()
  "Modify timestamp at cursor to connect to previous/next timerange.
Used from the agenda buffer by placing point on a log line of a
clocked entry.  If point is on the start time, the start time will
be modified to connect to the end time of the previous clocked
task.  If works accordingly if point is on the end time of the
current log entry.  If there is no newer logged clock line, the
end time will be set to the current time.

For performance reasons the previous/next clock item is found
based on a search for the previous/next clocked log line in the
agenda buffer, so it can only connect to a time range visible in
the current agenda buffer."
  (interactive)
  (cl-assert (eq major-mode 'org-agenda-mode) nil "Error: Not in agenda mode")
  (let* ((fieldname (org-clock-convenience-get-fieldname
		     (point)
		     org-clock-convenience-clocked-agenda-re
		     org-clock-convenience-clocked-agenda-fields
		     nil
		     "Error: Not on an agenda clock log line."))
	 updated-ts updated-time marker buffer tsname tmname)
    (org-clock-convenience-save-mark-and-excursion
      ;; find next/previous log line and fetch the appropriate time
      ;; stamp from the respective org file
      (pcase (cl-subseq (symbol-name fieldname) 0 3)
        ("d1-" (progn
		 (setq tsname 'd2-timestamp tmname 'd2-time)
		 (beginning-of-line)
		 (unless (search-backward-regexp org-clock-convenience-clocked-agenda-re
						 (point-min) t)
		   (error "Error: Cannot find previous log line in buffer"))))
        ("d2-" (progn
		 (setq tsname 'd1-timestamp tmname 'd1-time)
		 (forward-line 1)
		 (unless (search-forward-regexp org-clock-convenience-clocked-agenda-re
					        (point-max) t)
		   (setq tsname 'now tmname 'now))))
        (default (error "Error: Not on a clock field in an agenda log line")))

      (if (equal tsname 'now)
	  (let ((time (current-time)))
	    (setq updated-ts (format-time-string
			      (concat "["
				          (if (version<= org-version "9.5")
                              (substring (cdr org-time-stamp-formats)
						                 1 -1)
                            (cdr org-time-stamp-formats))
				      "]")
			      time)
		  updated-time (format-time-string "%H:%M" time)))
        (beginning-of-line)
        (setq marker (or (org-get-at-bol 'org-marker)
			 (org-agenda-error)))
        (setq buffer (marker-buffer marker))
        (set-buffer buffer)
        (goto-char (marker-position marker))
        (org-clock-convenience-open-if-in-drawer)
        (setq updated-ts (concat "["
				 (org-clock-convenience-get-re-field
				  tsname
				  org-clock-convenience-tr-re
				  org-clock-convenience-tr-fields)
				 "]")
	      updated-time (replace-regexp-in-string
			    " *" "" (org-clock-convenience-get-re-field
				     tmname
				     org-clock-convenience-tr-re
				     org-clock-convenience-tr-fields)))))
    ;; (message "fieldname: %s   tsname: %s  upd-ts: %s upd-time: %s"
    ;; 	     fieldname tsname updated-ts updated-time)
    (setq marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
    (setq buffer (marker-buffer marker))
    (org-with-remote-undo buffer
      (org-clock-convenience-save-mark-and-excursion
        ;; replace time in log line
        (org-clock-convenience-goto-agenda-tr-field fieldname)
        (let ((inhibit-read-only t))
	  (delete-char (length updated-time))
	  (insert (propertize updated-time 'face 'secondary-selection)))
        ;; now replace timestamp in org file
        (org-clock-convenience-goto-ts)
        (search-backward "[")
        (search-forward-regexp org-ts-regexp-inactive)
        (replace-match (concat updated-ts))
        (org-clock-update-time-maybe)))))

;;;###autoload
(defun org-clock-convenience-fill-gap-both ()
  "Modify both timestamps at cursor to fill gap to last/next timerange.
Performs `org-clock-convenience-fill-gap' sequentially on the
starting time and the ending of the time range. Can be executed
from anywhere within a valid clocked time range line."
  ;; the undo behavior is a bit unsatisfying, since undoing the whole
  ;; operation requires two invocations of undo.
  (interactive)
  (cl-assert (eq major-mode 'org-agenda-mode) nil "Error: Not in agenda mode")
  (save-excursion
    (beginning-of-line)
    (cl-assert (looking-at org-clock-convenience-clocked-agenda-re) nil
	       "Error: Not on a clocked time range line")
    (org-clock-convenience-goto-re-field 'd1-time
					 org-clock-convenience-clocked-agenda-re
					 org-clock-convenience-clocked-agenda-fields)
    (org-clock-convenience-fill-gap)
    (beginning-of-line)
    (org-clock-convenience-goto-re-field 'd2-time
					 org-clock-convenience-clocked-agenda-re
					 org-clock-convenience-clocked-agenda-fields)
    (org-clock-convenience-fill-gap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-clock-convenience-find-last-clockout (buffer)
  "Find the last clock-out time in BUFFER.
Return position, time string, and headline in a list"
  (with-current-buffer buffer
    (org-clock-convenience-save-mark-and-excursion
     (save-restriction
       (widen)
       (let* ((cpattern (concat "^ *" org-clock-string
				".*\\]--\\(\\[[^]]+\\]\\)"))
	      (parsetree (org-element-parse-buffer 'headline))
	      (clocklist
	       (org-element-map parsetree 'headline
		 (lambda (hl)
		   (goto-char (org-element-property :begin hl))
		   (let* ((end (org-element-property :end hl))
			  (srend
			   (org-clock-convenience-save-mark-and-excursion
                            (end-of-line)
			    (or (re-search-forward "^\\\*" end t)
				end))))
		     (if (re-search-forward cpattern srend t)
			 (list
			  (copy-marker  (- (point)
					   (length (match-string-no-properties 1))))
			  (match-string-no-properties 1)
			  (org-element-property :title hl))
		       nil))))))
	 (cl-loop with mx = (list 0 "<1970-01-02 Thu>")
		  for elem in clocklist
		  if (org-time> (nth 1 elem) (nth 1 mx))
		  do (setq mx elem)
		  ;;and collect mx into hitlist
		  ;;finally return (list mx hitlist clocklist)
		  finally return mx))))))

(defun org-clock-convenience-open-if-in-drawer ()
  "If pos is within drawer, open the drawer."
  (let ((element (org-element-at-point)))
    (while (and element
		(not (memq (org-element-type element)
			   '(drawer property-drawer))))
      (setq element (org-element-property :parent element)))
    (when element
      (let ((pos (point)))
	(goto-char (org-element-property :begin element))
	(org-clock-convenience-org-hide-drawer-toggle 'off)
	(goto-char pos)))))

;;;###autoload
(defun org-clock-convenience-goto-last-clockout (&optional buffer)
  "Jump to the position of the last clockout in BUFFER."
  (interactive)
  (let* ((buf (switch-to-buffer (or buffer (current-buffer))))
	 (mark (car (org-clock-convenience-find-last-clockout buf))))
    (org-goto-marker-or-bmk mark)
    (org-reveal)
    (org-clock-convenience-open-if-in-drawer)))


(provide 'org-clock-convenience)
;;; org-clock-convenience.el ends here
