 (defun org-refile-to-weektree (&optional file)
   "Refile a subtree to a datetree corresponding to it's timestamp.

The current time is used if the entry has no timestamp. If FILE
is nil, refile in the current file."
   (interactive "f")
   (let* ((oldbuf (current-buffer))
	  (default-date (or (org-entry-get nil "CREATED" t)
			      (org-entry-get nil "SCHEDULED" t)
			      (org-entry-get nil "TIMESTAMP_IA" t)
			      (org-entry-get nil "TIMESTAMP" t)
			      (org-entry-get nil "DEADLINE" t)
			      (org-read-date nil nil "now")))
	    (datetree-date (org-read-date nil nil nil nil (org-read-date nil t default-date)))
	    (date (org-date-to-gregorian datetree-date))
	    )
       (print date)
       (if file
	   (save-excursion
	     (org-cut-subtree)
	     (find-file file)
	     (widen)
	     (org-datetree-find-iso-week-create date)
	     (org-show-set-visibility "tree")
	     (org-narrow-to-subtree)
	     (show-subtree)
	     (org-end-of-subtree t)
	     (goto-char (point-max))
	     (org-paste-subtree 4)
	     (widen)
	     (switch-to-buffer oldbuf)
	     ))))
