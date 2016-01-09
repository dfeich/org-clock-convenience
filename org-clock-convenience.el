;;; org-clock-convenience --- convenience functions for org time tracking

;;; Commentary:
;; Convenience functions for easier time tracking

;;; Code:
(require 'org)
(require 'org-element)

(defvar org-clock-conv-clocked-regexp
  "^ +\\([^:]+\\): +\\([012][0-9]\\):\\([0-5][0-9]\\)-\\([012][0-9]\\):\\([0-5][0-9]\\) +Clocked: +([0-9]+:[0-5][0-9])"
  "Regexp of a clocked time range in the Org agenda buffer.")

(defvar org-clock-conv-clocked-fields
  '(filename d1-hours d1-minutes d2-hours d2-minutes duration)
  "Field names corresponding to submatches of org-clock-conv-clocked-regexp.")

(defun org-clock-conv-get-fieldname (point)
  "Return field name of time range where POINT is located.
The field names are based of the sub-patterns defined by
org-clock-conv-clocked-regexp.  The function can only be used
in an agenda buffer."
  (cl-assert (eq major-mode 'org-agenda-mode) nil "Error: Not in agenda mode")
  (save-excursion
    (beginning-of-line)
    (cl-assert (looking-at org-clock-conv-clocked-regexp) nil
	       "Error: not on a clocked time line"))
  (cl-loop
   for field in org-clock-conv-clocked-fields
   with cnt = 0
   do (cl-incf cnt)
   if (org-pos-in-match-range point cnt) return field
   finally return nil)
  )

(defun org-clock-conv-at-timefield-p ()
  "Return true if point is on a clocked time field in the agenda."
  (pcase (org-clock-conv-get-fieldname (point))
    ((or `d1-hours `d2-hours `d1-minutes `d2-minutes) t)
    (default nil)))

(defun org-clock-conv-goto-tr-field (fieldname)
  "Position point inside a field of the time range in the current line.
The field is defined by FIELDNAME."
  (beginning-of-line)
  (search-forward-regexp (concat org-clock-line-re " *")
			 (line-end-position) t )
  (let* ((d1-hours-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} +[^]+0-9>\r\n -]+ *")
	 (d1-minutes-re (concat d1-hours-re "[0-9]\\{1,2\\}:"))
	 (d2-hours-re (concat d1-minutes-re "[0-9]\\{1,2\\}\\]--\\[" d1-hours-re))
	 (d2-minutes-re (concat d2-hours-re "[0-9]\\{1,2\\}:"))
	 (rgx
	  (pcase fieldname
	    (`d1-hours d1-hours-re)
	    (`d1-minutes d1-minutes-re)
	    (`d2-hours d2-hours-re)
	    (`d2-minutes d2-minutes-re)
	    (default nil))))
    (cl-assert rgx nil "Error: No such field name: %s" fieldname)
    (unless
	(search-forward-regexp rgx (line-end-position) t)
      (error "Error: Cannot locate field %s on this line" fieldname))))

(defun org-clock-conv-goto-ts ()
  "Goto to position in agenda file according to location of point."
  (interactive)
  (let* ((fieldname (or (org-clock-conv-get-fieldname (point))
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

(defun org-clock-conv-timestamp-change (n)
  "Change timestamp by N in agenda buffer.
The change is carried out in the respective clock line of the
associated org agenda file."
  (let ((pos (point)))
    (save-excursion
      (org-clock-conv-goto-ts)
      (org-timestamp-change n nil 'updown))
    ;; I should not use org-agenda-redo. Too expensive and does not play well
    ;; with org-with-remote-undo
    (org-agenda-redo)
    (goto-char pos)))

;; if I use org-with-remote-undo and include the org-agenda-redo in
;; its form, then the agenda buffer somehow gets stale after an
;; undo. org-get-at-bol does not seem to deliver a valid buffer marker any more
;;
;; (let* ((pos (point))
;;        (marker (or (org-get-at-bol 'org-marker)
;; 		   (org-agenda-error)))
;;        (buffer (marker-buffer marker)))
;;   (org-with-remote-undo buffer
;;     (save-excursion
;;       (org-clock-conv-goto-ts)
;;       (org-timestamp-change n nil 'updown))
;;     (org-agenda-redo))
;;   (goto-char pos))

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
