;; -*- mode: elisp -*-

(defun pcl/heading-created-during-week (&optional target-date-str)
  "Returns #t if the target heading was created on given week, nil otherwise"
  (let ((heading-date (date-to-time (org-entry-get nil "CREATED")))
	(target-date (if target-date-str (date-to-time target-date-str))))
    (and (equal (format-time-string "%W" target-date)
		(format-time-string "%W" heading-date))
	 (equal (format-time-string "%Y" target-date)
		(format-time-string "%Y" heading-date)))))

(defun pcl/skip-unless-created-this-week (target-date)
  (let ((subtree-end (save-excursion (org-end-of-subtree t))))
    (if (and (re-search-forward ":CREATED:" subtree-end t)
	     (pcl/heading-created-during-week target-date))
        nil
      subtree-end))) ; tag not found, continue after end of subtree

(defun pcl/org-agenda-weekly-review (&optional target-week)
  (let ((org-agenda-custom-commands
	 '(("X" "Weekly Review"
	    (
	     (org-ql-block '(and (tags-all "ACCOMPLISHMENT")
				 (not (tags "IGNORE"))
				 (created :from (pcl/ts-week-start target-week)
					  :to (pcl/ts-week-end target-week)))
			   ((org-ql-block-header "Accomplishments")))
	     
	     (org-ql-block '(and (tags-all "DISAPPOINTMENT")
				 (not (tags "IGNORE"))
				 (created :from (pcl/ts-week-start target-week)
					  :to (pcl/ts-week-end target-week)))
			   ((org-ql-block-header "Disappointments")))
	     
	     (org-ql-block '(and (tags-all "HIGH")
				 (not (tags "IGNORE"))
				 (created :from (pcl/ts-week-start target-week)
					  :to (pcl/ts-week-end target-week)))
			   ((org-ql-block-header "Highs")))
	     
	     (org-ql-block '(and (tags-all "LOW_OR_STRUGGLE")
				 (not (tags "IGNORE"))
				 (created :from (pcl/ts-week-start target-week)
					  :to (pcl/ts-week-end target-week)))
			   ((org-ql-block-header "Lows / Struggles")))
	     
	     (org-ql-block '(and (tags-all "LEARNING")
				 (not (tags "IGNORE"))
				 (created :from (pcl/ts-week-start target-week)
					  :to (pcl/ts-week-end target-week)))
			   ((org-ql-block-header "Learnings")))
	     )
	    (
	     (org-agenda-files '("~/org/!Reflections.org"))
	     (org-agenda-sorting-strategy '(tag-up))
             (org-agenda-hide-tags-regexp "\\|*")
	     ))
	   )))
    (org-agenda nil "X")))

(defun pcl/org-agenda-active-projects ()
    (let ((org-agenda-custom-commands
	   '(("X" "Projects"
	    (
	     (tags "+PROJECT+ACTIVE"
		   ((org-agenda-overriding-header "Projects (Active)")))
	     (tags "+PROJECT-ACTIVE"
		   ((org-agenda-overriding-header "Projects (On-Hold)")))	     
	     )
	    (
	     (org-agenda-files '("~/org/Alignment.org"))
	     (org-agenda-prefix-format " %s")
             (org-agenda-hide-tags-regexp (regexp-opt '("PROJECT" "ACTIVE")))
	     )
	    ))
	 ))
    (org-agenda nil "X")))
