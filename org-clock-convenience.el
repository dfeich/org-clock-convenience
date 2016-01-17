;;; org-clock-convenience --- convenience functions for org time tracking

;; Author: Derek Feichtinger <dfeich.gmail.com>
;; Keywords: org
;; Homepage: https://github.com/dfeich/org-clock-convenience

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

;;; Code:
(require 'org)
(require 'org-element)
(require 'cl-lib)

(defvar org-clock-conv-clocked-agenda-re
  "^ +\\([^:]+\\): +\\(\\([ 012][0-9]\\):\\([0-5][0-9]\\)\\)-\\(\\([ 012][0-9]\\):\\([0-5][0-9]\\)\\) +Clocked: +([0-9]+:[0-5][0-9])"
  "Regexp of a clocked time range log line in the Org agenda buffer.")

(defvar org-clock-conv-clocked-agenda-fields
  '(filename d1-time d1-hours d1-minutes d2-time d2-hours d2-minutes duration)
  "Field names corresponding to submatches of `org-clock-conv-clocked-agenda-re'.")

(defvar org-clock-conv-tr-re
  (concat " *CLOCK: *\\["
	  org-ts-regexp0 "\\]\\(?:--\\[\\)?"
	  org-ts-regexp0 "?"
	  "\\(?:\\] *=> *\\([0-9]+:[0-9]\\{2\\}\\)\\)?")
  "Regexp of a clocked time range in an org file for field analysis.

match:    CLOCK: [2016-01-15 Fri 15:25]--[2016-01-15 Fri 18:10] =>  2:45
match:    CLOCK: [2016-01-08 Fri 14:30]--[2016-01-08] => -14:30
match:    CLOCK: [2016-01-08 Fri 10:30]
no:       CLOCK: [2016-01-08 Fri 10:30] => 2:30")

(defvar org-clock-conv-tr-fields
  '(d1-timestamp d1-year d1-month d1-day d1-dayname d1-time d1-hours d1-minutes
		 d2-timestamp d2-year d2-month d2-day d2-dayname d2-time d2-hours d2-minutes
		 sum)
  "Field names corresponding to submatches of `org-clock-conv-tr-re.")

(defun org-clock-conv-goto-re-field (fieldname re fnames &optional errmsg)
  "Move cursor to the specified FIELDNAME in the regexp RE.
The fieldnames are given as a list of symbols in FNAMES.  An error message
for the case of the regexp not matching can be passed in ERRMSG."
  (let ((idx (or (cl-position fieldname fnames)
		 (error "No such field name: %s" fieldname))))
    (unless (looking-at re)
      (error (or errmsg
		 "Error: regexp for analyzing fields does not match here")))
    (goto-char (match-beginning (1+ idx)))))

(defun org-clock-conv-get-re-field (fieldname re fnames &optional errmsg)
  "Return contents of field FIELDNAME defined by the regexp RE.
The fieldnames are given as a list of symbols in FNAMES.  An error message
for the case of the regexp not matching can be passed in ERRMSG."
  (let ((idx (or (cl-position fieldname fnames)
		 (error "No such field name: %s" fieldname))))
    (unless (looking-at re)
      (error (or errmsg
		 "Error: regexp for analyzing fields does not match here")))
    (match-string (1+ idx))))

(defun org-clock-conv-get-fieldname (point re fnames &optional ignore-lst errmsg)
  "Return field name of submatch where POINT is located.
The field names are based of the sub-patterns defined by the
regexp RE and the passed field names list FNAMES.  RE must match
from the beginning of line.  The optional parameter IGNORE-LST can
contain a list of submatch field names to ignore (sometimes there
are subpattern which contain several other subpatterns, and one
wants only the names of the smaller subpatterns).  ERRMSG allows
specifying an error message if RE is not matching."
  (save-excursion
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

(defun org-clock-conv-goto-tr-field (fieldname)
  "Position point inside a field of the clocked time range in the current line.
The field is defined by FIELDNAME and corresponds to one of the names
in `org-clock-conv-tr-fields'."
  (beginning-of-line)
  (org-clock-conv-goto-re-field fieldname org-clock-conv-tr-re org-clock-conv-tr-fields
				"Error: not on a clocked time log line"))

(defun org-clock-conv-goto-agenda-tr-field (fieldname)
  "Move cursor to the FIELDNAME of a agenda view clocked log line."
  (cl-assert (eq major-mode 'org-agenda-mode) nil "Error: Not in agenda mode")
  (beginning-of-line)
  (org-clock-conv-goto-re-field fieldname org-clock-conv-clocked-agenda-re
				org-clock-conv-clocked-agenda-fields
				"Error: not on a clocked time log line"))

(defun org-clock-conv-get-agenda-tr-fieldname (point)
  "Return field name of time range where POINT is located.
The field names are based of the sub-patterns defined by
org-clock-conv-clocked-agenda-re.  The function can only be used
in a log line of the agenda buffer."
  (cl-assert (eq major-mode 'org-agenda-mode) nil "Error: Not in agenda mode")
  (org-clock-conv-get-fieldname point
				org-clock-conv-clocked-agenda-re
				org-clock-conv-clocked-agenda-fields
				'(d1-time d2-time)
				"Error: not on a clocked time log line"))

(defun org-clock-conv-at-timefield-p ()
  "Return true if point is on a clocked time field in the log agenda view."
  (pcase (org-clock-conv-get-agenda-tr-fieldname (point))
    ((or `d1-hours `d2-hours `d1-minutes `d2-minutes) t)
    (default nil)))

(defun org-clock-conv-goto-ts ()
  "Goto to position in agenda file according to location of point."
  (interactive)
  (let* ((fieldname (or (org-clock-conv-get-agenda-tr-fieldname (point))
			(error "Error: Not on a time range field position")))
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (pos (marker-position marker)))
    (if (called-interactively-p 'any)
	(switch-to-buffer buffer)
      (set-buffer buffer))
    (goto-char pos)
    (org-clock-conv-open-if-in-drawer)
    (org-clock-conv-goto-tr-field fieldname)))

;; TODO: why do I need to hit g twice (rebuild agenda buffer) before I
;; can see the changes take effect?
(defun org-clock-conv-timestamp-change (n)
  "Change timestamp by N in agenda buffer.
The change is carried out in the respective clock line of the
associated org agenda file."
  (let* ((pos (point))
	 (marker (or (org-get-at-bol 'org-marker)
		     (org-agenda-error)))
	 (buffer (marker-buffer marker))
	 (fieldname (org-clock-conv-get-agenda-tr-fieldname (point)))
	 timefield updated-time)
    (org-with-remote-undo buffer
      (save-excursion
	(org-clock-conv-goto-ts)
	(org-timestamp-change n nil 'updown)
	(beginning-of-line)
	(looking-at org-clock-conv-tr-re)
	(setq timefield (pcase (cl-subseq (symbol-name fieldname) 0 3)
			  ("d1-" 'd1-time)
			  ("d2-" 'd2-time)))
	;; a bit ugly. regrettably need to replace the leading space, because
	;; the org-ts-regexp0 defines the leading space to be part of the pattern
	(setq updated-time
	      (replace-regexp-in-string " *" ""
					(org-clock-conv-get-re-field timefield
								     org-clock-conv-tr-re
								     org-clock-conv-tr-fields))))
      (org-clock-conv-goto-agenda-tr-field timefield)
      (let ((inhibit-read-only t))
	(delete-char (length updated-time))
	(insert (propertize updated-time 'face 'secondary-selection))))
    (goto-char pos)
    )
  )

(defun org-clock-conv-timestamp-up (&optional arg)
  "Increase the date item at the cursor by one.
Used in a clocked line from the agenda view.  If the cursor is on
the hour field, change the hour.  If it is on the minutes field,
change the minutes.  With prefix ARG, change by that many units."
  (interactive "p")
  (org-clock-conv-timestamp-change (prefix-numeric-value arg)))

(defun org-clock-conv-timestamp-down (&optional arg)
  "Increase the date item at the cursor by one.
Used in a clocked line from the agenda view.  If the cursor is on
the hour field, change the hour.  If it is on the minutes field,
change the minutes.  With prefix ARG, change by that many units."
  (interactive "p")
  (org-clock-conv-timestamp-change (- (prefix-numeric-value arg))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun org-clock-conv-find-last-clockout (buffer)
  "Find the last clock-out time in BUFFER.
Return position, time string, and headline in a list"
  (with-current-buffer buffer
    (save-excursion
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
			   (srend (save-excursion (end-of-line)
						  (or (re-search-forward "^\\\*" end t)
						      end))))
		      (if (re-search-forward cpattern srend t)
					;(list (point) (org-time-string-to-time (match-string 1)))
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

(defun org-clock-conv-open-if-in-drawer ()
  "If pos is within drawer, open the drawer."
  (let ((element (org-element-at-point)))
    (while (and element
		(not (memq (org-element-type element)
			   '(drawer property-drawer))))
      (setq element (org-element-property :parent element)))
    (when element
      (let ((pos (point)))
	(goto-char (org-element-property :begin element))
	(org-flag-drawer nil)
	(goto-char pos)))))

(defun org-clock-conv-goto-last-clockout (&optional buffer)
  "Jump to the position of the last clockout in BUFFER."
  (interactive)
  (let* ((buf (switch-to-buffer (or buffer (current-buffer))))
	 (mark (car (org-clock-conv-find-last-clockout buf))))
    (org-goto-marker-or-bmk mark)
    (org-reveal)
    (org-clock-conv-open-if-in-drawer)))


(provide 'org-clock-convenience)
;;; org-clock-convenience.el ends here
