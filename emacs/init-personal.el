;; -*- mode: elisp -*-
;; Personal Configuration File

;;;;;;;;;;;;;;
;; PACKAGES ;;
;;;;;;;;;;;;;;
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
			    (plantuml . t)))))))

(load (rel-file "package-defs/org/base.el"))
(load (rel-file "package-defs/org-ql/base.el"))
(load (rel-file "package-defs/org-super-links/base.el"))
(load (rel-file "package-defs/sicp/base.el"))
(load (rel-file "func/org-metrics.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE CONFIGURATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-priority-lowest ?F)
(setq org-priority-default ?F)
(setq org-log-done "time")

(setq org-agenda-files
      '(
	"~/org/!Tasks.org"
	"~/org/inbox_notes.org"
	"~/org/inbox_tasks.org"
	"~/org/Contacts.org"
	"~/org/Alignment.org"
	"~/org/!Reflections.org"
	"~/org/vault"
	))

(setq org-todo-keywords
      '((sequence
	 "TODO"
	 "BLOCKED(b@)"
	 "|"
	 "DONE(d!)"
	 "CANCELLED(c!)"
	 )))

(setq org-tag-persistent-alist
      '(;; UTILITIES
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

(setq org-tags-exclude-from-inheritance
      '(;; UTILITIES
	"IGNORE"
	"ROUTINE"
	"REMINDER"
	;; ALIGNMENT HIERARCHY
	"GOAL"
	"PROJECT"
	))

(setq org-agenda-custom-commands
      '(
	("d" . "Dashboards")
	("dd" "Daily Action"
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

	("dp" "Projects"
	 ((tags "+PROJECT+ACTIVE"
		((org-agenda-overriding-header "Projects (Active)")))
	  (tags "+PROJECT-ACTIVE-COMPLETE-ABANDONED"
		((org-agenda-overriding-header "Projects (Inactive)")))
	  )
	 ((org-agenda-hide-tags-regexp "\\|*")
	  (org-agenda-prefix-format " ")))

	("dg" "Goals"
	 ((tags "+GOAL+ACTIVE"
		((org-agenda-overriding-header "Goals (Active)")))
	  (tags "+GOAL-ACTIVE-ABANDONED-COMPLETE"
		((org-agenda-overriding-header "Goals (Inactive)")))
	  )
	 ((org-agenda-hide-tags-regexp "\\|*")
	  (org-agenda-prefix-format " ")))
	
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
	 ))

(setq org-capture-templates
      '(
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

	("j" "Journal")
	("ja" "Accomplishment" entry
	 (file+headline "~/org/!Reflections.org" "Accomplishments")
	 "* %? :ACCOMPLISHMENT:"
	 :empty-lines-after 1)
	("jd" "Dissapointment" entry
	 (file+headline "~/org/!Reflections.org" "Disappointments")
	 "* %? :DISAPPOINTMENT:"
	 :empty-lines-after 1)
	("jh" "High" entry
	 (file+headline "~/org/!Reflections.org" "Highs")
	 "* %? :DISAPPOINTMENT:"
	 :empty-lines-after 1)
	("js" "Low/Struggle" entry
	 (file+headline "~/org/!Reflections.org" "Lows and Struggles")
	 "* %? :LOW_OR_STRUGGLE:"
	 :empty-lines-after 1)
	("jl" "Learning" entry
	 (file+headline "~/org/!Reflections.org" "Learnings")
	 "* %? :LEARNING:"
	 :empty-lines-after 1)
	("jm" "Misc" entry
	 (file+headline "~/org/!Reflections.org" "Misc")
	 "* %? :JOURNAL:"
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
	
	("G" "Goal" entry
	 (file+headline "~/org/Alignment.org" "Goals")
	 (file "~/dotfiles/emacs/capture-templates/personal/goal.org"))
	
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
