;; -*- mode: elisp -*-
;; Personal Configuration File

(let ((override-conf (lambda ()
		       (org-babel-do-load-languages
			'org-babel-load-languages
			'((emacs-lisp . t)
			  (http . t)
			  (shell . t)
			  (typescript . t)
			  (js . t)
			  (plantuml . t))))))
  (load (rel-file "package-defs/org/base.el")))
(load (rel-file "package-defs/org-super-links/base.el"))
(load (rel-file "package-defs/geiser/base.el"))
(load (rel-file "package-defs/sicp/base.el"))

(setq org-agenda-files '("~/org/!Tasks.org"
			 "~/org/Projects.org"
			 "~/org/Alignment.org"
			 "~/org/vault"))

(setq org-tag-alist '(;; UTILITIES
		      ("IGNORE" . ?i)
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
      (file+datetree "~/org/!Tasks.org")
      "** TODO %?" :tree-type week :time-prompt t)

     ("n" "Note" entry
      (file+datetree "~/org/!Tasks.org")
      "** %?" :tree-type week :time-prompt t)

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
      :time-prompt t)))

;; Refile item to task list
(defun org-refile-to-tasklist () "" (interactive) (org-refile-to-weektree "~/org/!Tasks.org"))
