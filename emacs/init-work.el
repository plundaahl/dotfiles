;; -*- mode: elisp -*-
;; Work Configuration File

;; PACKAGES
(let ((override-conf (lambda ()
		       (org-babel-do-load-languages
			'org-babel-load-languages
			'((emacs-lisp . t)
			  (http . t)
			  (shell . t)
			  (typescript . t)
			  (js . t)
			  (plantuml . t))))))
  (load (rel-file "package-defs/org/base.el"))
  (load (rel-file "package-defs/org-super-links/base.el"))
  (load (rel-file "package-defs/org-ql/base.el")))

(setq org-modules
      '(
	ol-bbdb
	ol-docview
	ol-info
	org-id
	))
(setq org-priority-lowest ?F)
(setq org-priority-default ?F)

(setq org-tags-exclude-from-inheritance
      '("PROJECT"
	"REMINDER"
	"1_1_Mark"
	"MEETING"))

(setq org-agenda-files
      '("~/org"
	"~/org/vault"
	))

(setq org-refile-targets
      '(
	("projects.org" :level . 1)
	("areas.org" :level . 1)
	))

(setq org-tag-persistent-alist
      '(
        ("KEEP" . ?k)
        ("QUICKLINK" . ?q)
	("Appointment" . ?A)
	("MEETING" . ?M)
	("REMINDER" . ?r)
	(:startgroup)
	(:grouptags)
	("SUBJECT" . ?S)
	("Index" . ?I)
	("Tool" . ?s)
	("Article" . ?a)
	("Book" . ?b)
	("Video" . ?v)
	("Concept" . ?c)
	("Idea" . ?i)
	("Note" . ?n)
	("Technique" . ?T)
	(:endgroup)
	))

(setq org-todo-keywords
      '((sequence
	 ;; Standard Tasks
	 "BLOCKED(b)"
	 "TODO(t)"
	 "NEXT(n)"
	 "|"
	 "DONE(d)"
	 "CANCELLED(c)"
	 )
	(sequence
	 ;; Meetings
	 "PENDING(p)"
	 "|"
	 "ATTENDED(a)"
	 "SKIPPED(s)"
	 )))

(setq org-tags-exclude-from-inheritance '(
					  "PROJECT"
					  "IGNORE"
					  ))

(setq org-agenda-custom-commands
      '(("d" "Daily Action View"
	 ((tags "+SCHEDULED>\"<-1d>\"+SCHEDULED<\"<+1d>\"+REMINDER-TODO=\"DONE\"-TODO=\"CANCELLED\""
		((org-agenda-overriding-header "Reminders & Events")
		 (org-agenda-prefix-format "- ")
		 (org-agenda-hide-tags-regexp "\\|*")))
          (tags "QUICKLINK"
		(
		 (org-agenda-hide-tags-regexp "\\|*")
		 (org-agenda-overriding-header "Pinned")))
	  (agenda ""
		  ((org-agenda-overriding-header "Agenda")
		   (org-agenda-tag-filter-preset
		    '("-REMINDER" "-INACTIVE"))
		   (org-agenda-span 1)
		   (org-agenda-skip-scheduled-if-done t)
		   (org-agenda-skip-deadline-if-done t)
		   (org-agenda-hide-tags-regexp "\\|*")
		   ))
	  (tags "+CLOSED>\"<-1d>\""
		((org-agenda-overriding-header "Complete"))))
	 ((org-agenda-tag-filter-preset
	   '("-INACTIVE"))))
	))

(setq org-capture-templates '(
     ("t" "Todo" entry
      (file+headline "~/org/inbox.org" "Tasks")
       "* TODO %?
SCHEDULED: %t"
       :empty-lines-after 1
       :kill-buffer 1
       :time-prompt t)

     ("n" "Note" entry
      (file+headline "~/org/inbox.org" "Notes")
       "** %?"
       :empty-lines-after 1
       :time-prompt t)

     ("r" "Review...")
     
     ("rw" "Weekly Review" entry
      (file+headline "~/org/reviews.org" "Weekly")
      (file "~/dotfiles/emacs/capture-templates/work/review-weekly.org")
      :empty-lines-after 1
      :jump-to-captured 1
      :time-prompt t)

     ("m" "Meeting" entry
      (file+datetree "~/org/!Tasks.org")
       "* PENDING %? %u :MEETING:
SCHEDULED: %T"
       :tree-type week
       :empty-lines 1
       :time-prompt t)

     ("c" "Clock Item" entry
      (file+datetree "~/org/!Tasks.org")
       "* %? %u"
       :tree-type week
       :clock-in t
       :clock-resume t
       :empty-lines 1
       :time-prompt t)

     ("d" "Daily Item" entry
      (file+datetree "~/org/!Tasks.org")
		     "* %? %u"
		     :tree-type week
		     :empty-lines 1
		     :time-prompt t)

     ("t" "Templates")
     ("td" "Design" entry
      (file+headline "~/org/inbox.org" "Notes")
      (file "~/dotfiles/emacs/capture-templates/work/design-template.org")
      :empty-lines-after 1
      :jump-to-captured 1)

     ("a" "Accomplishment" entry
      (file+datetree "~/org/!Tasks.org")
       "* %? :ACCOMPLISHMENT:"
       :tree-type week
       :empty-lines 1)))
