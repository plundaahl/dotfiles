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
  (let ((week-str (format-time-string "(Week %W) " (date-to-time target-week))))
    (let ((org-agenda-custom-commands

	   '(("X" "Weekly Review"
	    (
	     (tags "+ACCOMPLISHMENT-IGNORE"
		   ((org-agenda-overriding-header (concat week-str "Accomplishments"))))
	     (tags "+DISAPPOINTMENT-IGNORE"
		   ((org-agenda-overriding-header (concat week-str "Disappointments"))))
	     (tags "+HIGH-IGNORE"
		   ((org-agenda-overriding-header (concat week-str "Highs"))))
	     (tags "+LOW_OR_STRUGGLE-IGNORE"
		   ((org-agenda-overriding-header (concat week-str "Lows/Struggles"))))
	     (tags "+LEARNING-IGNORE"
		   ((org-agenda-overriding-header (concat week-str "Learning"))))
	     )
	    (
	     (org-agenda-files '("~/org/!Reflections.org"))
	     (org-agenda-sorting-strategy '(tag-up))
	     (org-agenda-prefix-format " %s")
             (org-agenda-hide-tags-regexp "\\|*")
	     (org-agenda-skip-function
	      '(lambda () (pcl/skip-unless-created-this-week target-week)))
	     )
	    ))
	 ))
    (org-agenda nil "X"))))

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
