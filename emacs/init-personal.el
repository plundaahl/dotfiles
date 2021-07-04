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
(load (rel-file "package-defs/ts/base.el"))
(load (rel-file "func/org-metrics.el"))
(load (rel-file "func/org-hooks.el"))
(load (rel-file "func/orgq.el"))
(load (rel-file "func/ts-ext.el"))
(load (rel-file "func/plist-ext.el"))
(load (rel-file "func/org-skip.el"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ORG MODE CONFIGURATION ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq org-modules
      '(
	ol-bbdb
	ol-docview
	org-habit
	ol-info
	org-id
	))
(setq org-priority-lowest ?F)
(setq org-priority-default ?F)
(setq org-log-done "time")

h(setq org-habit-following-days 1)
(setq org-habit-graph-column 60)
(setq org-habit-show-done-always-green nil)
(setq org-habit-show-habits-only-for-today nil)

(setq org-agenda-files
      '(
	"~/org/!Tasks.org"
	"~/org/Reviews.org"
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
	(:startgrouptag)
	("ALIGNMENT")
	(:grouptags)
	("AREA")
	("GOAL")
	("PROJECT")
	("ASPIRATION")
	("LIST")
	(:endgrouptag)
	))

(setq org-tags-exclude-from-inheritance
      '(;; UTILITIES
	"IGNORE"
	"ROUTINE"
	"REMINDER"
	;; ALIGNMENT HIERARCHY
	"GOAL"
	"PROJECT"
	"AREA"
	))

(setq org-agenda-custom-commands
      '(
	("d" . "Dashboards")

	("dd" "Daily Action"
	 ((tags "ALLTAGS={WEEK\\|MONTH\\|QUARTER\\|YEAR}&+TODO=\"CURRENT\""
		((org-agenda-overriding-header "Cycles")
		 (org-agenda-prefix-format "  ")
		 (org-agenda-hide-tags-regexp "\\|*")))
	  (tags "+SCHEDULED>\"<-1d>\"+SCHEDULED<\"<+1d>\"+REMINDER-TODO=\"DONE\"-TODO=\"CANCELLED\""
		((org-agenda-overriding-header "Reminders & Events")
		 (org-agenda-prefix-format "- ")
		 (org-agenda-hide-tags-regexp "\\|*")))
	  (agenda ""
		  ((org-agenda-overriding-header "Agenda")
		   (org-agenda-span 1)
		   (org-agenda-skip-scheduled-if-done t)
                   (org-agenda-prefix-format " %i %-12:c")
		   (org-agenda-skip-function
		    '(org-agenda-skip-entry-if 'regexp ":REMINDER:"))
		   (org-agenda-hide-tags-regexp "\\|*")))
	  (tags "+CLOSED>=\"<today>\""
		((org-agenda-overriding-header "Complete")))
	  )
	 )

	("dl" "Lists"
	 ((tags "+LIST-INACTIVE"
		((org-agenda-overriding-header "Lists")
		 (org-agenda-prefix-format "  ")
		 (org-agenda-hide-tags-regexp "\\|*")))
	  ))

	("dg" "Goals"
	 ((tags "+GOAL-INACTIVE"
		((org-agenda-overriding-header "Goals (Active)")))
	  (tags "+GOAL+INACTIVE-ABANDONED-COMPLETE"
		((org-agenda-overriding-header "Goals (Inactive)")))
	  )
	 ((org-agenda-hide-tags-regexp "\\|*")
	  (org-agenda-prefix-format " ")))

	;; PROJECT VIEWS
	("p" . "Project")

	("pW" "Projects (All Week-Scale)"
	 ((tags "+PROJECT+SCALE=\"Weeks\""
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"pg\")][Projects (Scale: Weeks)]]"))))
	 ((org-agenda-prefix-format " %i %-12:c")
	  (org-agenda-sorting-strategy '(todo-state-down priority-down))))

	("pM" "Projects (All Month-Scale)"
	 ((tags "+PROJECT+SCALE=\"Months\""
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"pg\")][Projects (Scale: Months)]]"))))
	 ((org-agenda-prefix-format " %i %-12:c")
	  (org-agenda-sorting-strategy '(todo-state-down priority-down))))

	("pQ" "Projects (All Quarter-Scale)"
	 ((tags "+PROJECT+SCALE=\"Quarters\""
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"pg\")][Projects (Scale: Quarters)]]"))))
	 ((org-agenda-prefix-format " %i %-12:c")
	  (org-agenda-sorting-strategy '(todo-state-down priority-down))))

	("pY" "Projects (All Year-Scale)"
	 ((tags "+PROJECT+SCALE=\"Years\""
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"pg\")][Projects (Scale: Years)]]"))))
	 ((org-agenda-prefix-format " %i %-12:c")
	  (org-agenda-sorting-strategy '(todo-state-down priority-down))))

	("pw" "Projects (Active Week-Scale)"
	 ((tags "+PROJECT+SCALE=\"Weeks\"-INACTIVE"
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"pG\")][Active Projects (Scale: Weeks)]]"))))
	 ((org-agenda-prefix-format " %i %-12:c")
	  (org-agenda-sorting-strategy '(todo-state-down priority-down))))

	("pm" "Projects (Active Month-Scale)"
	 ((tags "+PROJECT+SCALE=\"Months\"-INACTIVE"
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"pG\")][Active Projects (Scale: Months)]]"))))
	 ((org-agenda-prefix-format " %i %-12:c")
	  (org-agenda-sorting-strategy '(todo-state-down priority-down))))

	("pq" "Projects (Active Quarter-Scale)"
	 ((tags "+PROJECT+SCALE=\"Quarters\"-INACTIVE"
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"pG\")][Active Projects (Scale: Quarters)]]"))))
	 ((org-agenda-prefix-format " %i %-12:c")
	  (org-agenda-sorting-strategy '(todo-state-down priority-down))))

	("py" "Projects (Active Year-Scale)"
	 ((tags "+PROJECT+SCALE=\"Years\"-INACTIVE"
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"gG\")][Active Projects (Scale: Years)]]"))))
	 ((org-agenda-prefix-format " %i %-12:c")
	  (org-agenda-sorting-strategy '(todo-state-down priority-down))))

	("pP" "Projects (All)"
	 (
	  (tags "+PROJECT+SCALE=\"Years\""
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"gy\")][Projects (Scale: Years)]]")))
	  (tags "+PROJECT+SCALE=\"Quarters\""
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"gq\")][Projects (Scale: Quarters)]]")))
	  (tags "+PROJECT+SCALE=\"Months\""
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"gm\")][Projects (Scale: Months)]]")))
	  (tags "+PROJECT+SCALE=\"Weeks\""
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"gw\")][Projects (Scale: Weeks)]]")))
          (tags "+PROJECT+SCALE=\"\""
		((org-agenda-overriding-header
		  "Projects (No Assigned Scale)")))
	  )
	 ((org-agenda-prefix-format " %i %-12:c")
	  (org-agenda-sorting-strategy '(todo-state-down priority-down))))

	("pp" "Projects (All Active)"
	 (
	  (tags "+PROJECT+SCALE=\"Years\"-INACTIVE"
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"gY\")][Active Projects (Scale: Years)]]")))
	  (tags "+PROJECT+SCALE=\"Quarters\"-INACTIVE"
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"gQ\")][Active Projects (Scale: Quarters)]]")))
	  (tags "+PROJECT+SCALE=\"Months\"-INACTIVE"
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"gM\")][Active Projects (Scale: Months)]]")))
	  (tags "+PROJECT+SCALE=\"Weeks\"-INACTIVE"
		((org-agenda-overriding-header
		  "[[elisp:(org-agenda nil \"gW\")][Active Projects (Scale: Weeks)]]")))
          (tags "+PROJECT+SCALE=\"\"-INACTIVE"
		((org-agenda-overriding-header
		  "Projects (No Assigned Scale)")))
	  )
	 ((org-agenda-prefix-format " %i %-12:c")
	  (org-agenda-sorting-strategy '(todo-state-down priority-down))))

	("P" . "Plan...")

	("Pw" "Plan Week"
	 ((agenda ""
		  ((org-agenda-span 'week)
		   (org-agenda-start-day "+3")
		   (org-agenda-skip-scheduled-if-done t)
		   (org-agenda-prefix-format " %i %-12:c% s")))
	  (tags "-INACTIVE+TODO=\"NEXT\"-SCHEDULED={.}-ROUTINE"
		((org-agenda-overriding-header "Next Tasks"))))
	 ((org-agenda-tag-filter-preset '("-ROUTINE" "-WEEK" "-MONTH" "-QUARTER"))
	  (org-agenda-hide-tags-regexp "\\|*")))

	("r" . "Revisit...")

	("rd" "Aspirations/Dreams"
	 ((tags "+ASPIRATION-INACTIVE"
		((org-agenda-overriding-header "Aspirations (Active)")))
	  (tags "+ASPIRATION+INACTIVE"
		((org-agenda-overriding-header "Aspirations (Inactive)"))))
	 ((org-agenda-hide-tags-regexp "\\|*")))

	("ra" "Areas"
	 ((tags "+AREA-INACTIVE"
		((org-agenda-overriding-header "Areas (Active)")))
	  (tags "+AREA+INACTIVE"
		((org-agenda-overriding-header "Areas (Inactive)"))))
	 ((org-agenda-hide-tags-regexp "\\|*")
	  (org-agenda-prefix-format "  ")))
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
	 "* %? :HIGH:"
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

	("r" "Review")
	("rw" "Weekly Review" entry
	 (file+headline "~/org/Reviews.org" "Weeks")
	 (file "~/dotfiles/emacs/capture-templates/personal/review-weekly.org")
	 :jump-to-captured t
	 :time-prompt t)

	("rm" "Monthly Review" entry
	 (file+headline "~/org/Reviews.org" "Months")
	 (file "~/dotfiles/emacs/capture-templates/personal/review-monthly.org")
	 :jump-to-captured t
	 :time-prompt t)

	("rq" "Quarterly Review" entry
	 (file+headline "~/org/Reviews.org" "Quarters")
	 (file "~/dotfiles/emacs/capture-templates/personal/review-quarterly.org")
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

(defun pcl/org-journal-view (unit period)
  (let ((org-agenda-custom-commands
	 '(("X" "Reflections"
	    (
	     (tags "+ACCOMPLISHMENT-IGNORE" ((org-agenda-overriding-header "Accomplishments")))
	     (tags "+DISAPPOINTMENT-IGNORE" ((org-agenda-overriding-header "Disappointments")))
	     (tags "+HIGH-IGNORE" ((org-agenda-overriding-header "Highs")))
	     (tags "+LOW_OR_STRUGGLE-IGNORE" ((org-agenda-overriding-header "Lows/Struggles")))
	     (tags "+LEARNING-IGNORE" ((org-agenda-overriding-header "Learnings")))
	     )
	    ((org-agenda-skip-function
	      '(pcl/org-skip-unless
		`(:created-in ,unit :of ,period)))
	     (org-agenda-prefix-format "  ")
	     (org-agenda-hide-tags-regexp "\\|*")
	     )))))
    (org-agenda nil "X")))

;; Refile item to task list
(defun org-refile-to-tasklist () "" (interactive) (org-refile-to-weektree "~/org/!Tasks.org"))
