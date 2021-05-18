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
  (load (rel-file "package-defs/org/base.el")))
(load (rel-file "package-defs/org-super-links/base.el"))
(load (rel-file "package-defs/sicp/base.el"))

(setq org-agenda-files '("~/org/!Tasks.org"
			 "~/org/inbox.org"
			 "~/org/Alignment.org"
			 "~/org/vault"))

(setq org-priority-lowest ?F)
(setq org-priority-default ?F)

(setq org-agenda-custom-commands
   '(("d" "Daily Action View"
      ((agenda ""
	       ((org-agenda-overriding-header "Agenda")
		(org-agenda-span 1)
		(org-agenda-hide-tags-regexp "\\|*")))
       (tags "+SCHEDULED>\"<-1d>\"+SCHEDULED<\"<+1d>\"+REMINDER-TODO=\"DONE\""
	     ((org-agenda-overriding-header "Reminders & Events")
	      (org-agenda-prefix-format "- ")
	      (org-agenda-hide-tags-regexp "\\|*"))))
      ("p" . "Planning Views")
      ("pw" "Weekly Planning"
       ((agenda ""
		((org-agenda-span 'week)
		 (org-agenda-start-day "+3")
		 (org-agenda-prefix-format " %i %-12:c% s")))
	(tags "+ACTIVE+TODO=\"NEXT\"-SCHEDULED={.}-ROUTINE"
	      ((org-agenda-overriding-header "Next Tasks"))))
       ((org-agenda-tag-filter-preset
	 '("-ROUTINE"))
	(org-agenda-hide-tags-regexp "\\|*"))
      nil)
      nil)))

(setq org-tag-persistent-alist '(;; UTILITIES
				 ("IGNORE" . ?I)
				 ("ROUTINE" . ?R)
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

(setq org-capture-templates '(
     ("i" "Inbox Item" entry
      (file "~/org/inbox.org")
      (file "~/dotfiles/emacs/capture-templates/inbox-item.org"))

     ("t" "Todo" entry
      (file "~/org/inbox_tasks.org")
      "** TODO %?
:PROPERTIES:
:CREATED: %U
:END:"
      :kill-buffer 1
      :empty-lines-after 1)

     ("n" "Note" entry
      (file "~/org/inbox_notes.org")
      "* %?
:PROPERTIES:
:CREATED: %U
:END:"
      :kill-buffer 1
      :empty-lines-after 1)

     ("q" "Question" entry
      (file+datetree "~/org/!Tasks.org")
      "** QUESTION %?" :tree-type week :time-prompt t)

     ("d" "Decision" entry
      (file "~/org/decisions.org")
      (file "~/dotfiles/emacs/capture-templates/decision.org"))

     ("r" "Weekly Review" entry
      (file+datetree "~/org/!Tasks.org")
      (file "~/dotfiles/emacs/capture-templates/personal/review-weekly.org")
      :tree-type week
      :time-prompt t)

     ("p" "Weekly Plan" entry
      (file+datetree "~/org/!Tasks.org")
      (file "~/dotfiles/emacs/capture-templates/personal/plan-weekly.org")
      :tree-type week
      :time-prompt t)

     ("P" "Project" entry
      (file+headline "~/org/Alignment.org" "Projects")
      (file "~/dotfiles/emacs/capture-templates/personal/project.org"))
     ))

;; Refile item to task list
(defun org-refile-to-tasklist () "" (interactive) (org-refile-to-weektree "~/org/!Tasks.org"))
