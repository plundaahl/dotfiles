;; -*- mode: elisp -*-
;; Personal Configuration File

(load (rel-file "package-defs/geiser/base.el"))
(load (rel-file "package-defs/geiser-mit/base.el"))
(let ((override-config (defun override-config ()
 		       (org-babel-do-load-languages
			'org-babel-load-languages
			'((emacs-lisp . t)
			  (http . t)
			  (shell . t)
			  (typescript . t)
  			  (scheme . t)
			  (js . t)
			  (plantuml . t))))))
  (load (rel-file "package-defs/org/base.el"))
  (load (rel-file "package-defs/org-ql/base.el")))
(load (rel-file "package-defs/org-super-links/base.el"))
(load (rel-file "package-defs/sicp/base.el"))
(load (rel-file "func/org-metrics.el"))

(setq org-agenda-files '("~/org/!Tasks.org"
			 "~/org/inbox_notes.org"
			 "~/org/inbox_tasks.org"
			 "~/org/Contacts.org"
			 "~/org/Alignment.org"
			 "~/org/!Reflections.org"
			 "~/org/vault"))

(setq org-priority-lowest ?F)
(setq org-priority-default ?F)

(setq org-log-done "time")

(setq org-todo-keywords '((sequence "TODO" "BLOCKED(b@)" "|" "DONE(d!)" "CANCELLED(c!)")))

(setq org-agenda-custom-commands
   '(("d" "Daily Action View"
      ((agenda ""
	       ((org-agenda-overriding-header "Agenda")
		(org-agenda-span 1)
		(org-agenda-skip-scheduled-if-done t)
		(org-agenda-skip-function
		 '(org-agenda-skip-entry-if 'regexp ":REMINDER:"))
		(org-agenda-hide-tags-regexp "\\|*")))
       (tags "+SCHEDULED>\"<-1d>\"+SCHEDULED<\"<+1d>\"+REMINDER-TODO=\"DONE\"-TODO=\"CANCELLED\""
	     ((org-agenda-overriding-header "Reminders & Events")
	      (org-agenda-prefix-format "- ")
	      (org-agenda-hide-tags-regexp "\\|*")))
       (tags "+CLOSED>\"<-1d>\""
	     ((org-agenda-overriding-header "Complete")))))

     ("p" . "Plan...")

     ("pw" "Plan Week"
       ((agenda ""
		((org-agenda-span 'week)
		 (org-agenda-start-day "+3")
		 (org-agenda-skip-scheduled-if-done t)
		 (org-agenda-prefix-format " %i %-12:c% s")))
	(tags "+ACTIVE+TODO=\"NEXT\"-SCHEDULED={.}-ROUTINE"
	      ((org-agenda-overriding-header "Next Tasks"))))
       ((org-agenda-tag-filter-preset '("-ROUTINE"))
	(org-agenda-hide-tags-regexp "\\|*")))

     ("v" . "View...")

     ("vp" "View Projects"
      ((tags "+PROJECT+ACTIVE"
	     ((org-agenda-overriding-header "Projects (Active)")))
       (tags "+PROJECT-ACTIVE"
	     ((org-agenda-overriding-header "Projects (Inactive)")))
       )
      ((org-agenda-hide-tags-regexp "\\|*")
       (org-agenda-prefix-format " ")))
     ))

(setq org-tag-persistent-alist '(;; UTILITIES
				 ("IGNORE" . ?I)
				 ("ROUTINE" . ?R)
				 ("REMINDER" . ?r)
				 ;; ALIGNMENT HIERARCHY
				 (:startgroup)
				 ("GOAL")
				 ("PROJECT")
				 (:endgroup)
				 ;; PILLARS
				 ("admin")
				 ("career")
				 ("exploration_expression")
				 ("family")
				 ("health")
				 ("home")
				 ("mental_dev")
				 ("operations")
				 ("perspective")
				 ("product_dev")
				 ("self_employment")
				 ("skills")
				 ))

(setq org-tags-exclude-from-inheritance '(;; UTILITIES
					  "IGNORE"
					  ;; ALIGNMENT HIERARCHY
					  "GOAL"
					  "PROJECT"
					  ))

(setq org-capture-templates '(
     ("t" "Todo" entry
      (file "~/org/inbox_tasks.org")
      "** TODO %?"
      :kill-buffer 1
      :empty-lines-after 1)

     ("n" "Note" entry
      (file "~/org/inbox_notes.org")
      "* %?"
      :kill-buffer 1
      :empty-lines-after 1)

     ("f" "Reflection" entry
      (file "~/org/!Reflections.org")
      "* %?"
      :empty-lines-after 1)

     ("d" "Decision" entry
      (file "~/org/decisions.org")
      (file "~/dotfiles/emacs/capture-templates/decision.org"))

     ("r" "Weekly Review" entry
      (file+headline "~/org/Reviews.org" "Weeks")
      (file "~/dotfiles/emacs/capture-templates/personal/review-weekly.org")
      :immediate-finish t
      :jump-to-captured t
      :time-prompt t)

     ("p" "Weekly Plan" entry
      (file+datetree "~/org/!Tasks.org")
      (file "~/dotfiles/emacs/capture-templates/personal/plan-weekly.org")
      :tree-type week
      :time-prompt t)

     ("P" "Project" entry
      (file+headline "~/org/Alignment.org" "Projects")
      (file "~/dotfiles/emacs/capture-templates/personal/project.org"))

     ("c" "Contact")
     ("cp" "Person" entry
      (file+headline "~/org/Contacts.org" "People")
      (file "~/dotfiles/emacs/capture-templates/personal/contact-person.org"))
     ("cc" "Company" entry
      (file+headline "~/org/Contacts.org" "Companies")
      (file "~/dotfiles/emacs/capture-templates/personal/contact-company.org"))
     ))

;; Refile item to task list
(defun org-refile-to-tasklist () "" (interactive) (org-refile-to-weektree "~/org/!Tasks.org"))
