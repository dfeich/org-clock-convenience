;;; org-clock-convenience --- convenience functions for org time tracking

;;; Commentary:
;; Convenience functions for easier time tracking

;;; Code:
(require 'org)
(require 'org-element)

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
