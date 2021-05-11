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
(load (rel-file "package-defs/geiser/base.el"))

(setq org-agenda-files '("~/org/!Tasks.org"
			 "~/org/Projects.org"
			 "~/org/vault"))

(setq org-tag-alist '(;; UTILITIES
		      ("IGNORE")
		      
		      ;; PILLARS
		      ("admin" . "a")
		      ("career" . "c")
		      ("exploration_expression" . "x")
		      ("family" . "f")
		      ("health" . "h")
		      ("home" . "H")
		      ("mental_dev" . "m")
		      ("operations" . "o")
		      ("perspective" . "p")
		      ("product_dev" . "r")
		      ("self_employment" . "e")
		      ("skills" . "s")
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
