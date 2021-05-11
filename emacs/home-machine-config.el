;; -*- mode: elisp -*-

(use-package geiser
  :ensure t
  :pin melpa-stable
  :config
  (setq geiser-active-implementations '(mit))
  (setq geiser-mode-eval-last-sexp-to-buffer t)
  (setq geiser-mode-eval-to-buffer-prefix "")
  (setq geiser-mode-start-repl-p t))


(setq org-agenda-files '("~/org/!Tasks.org" "~/org/Projects.org" "~/org/vault"))
(setq org-capture-templates '(
     ("i" "Inbox Item" entry
      (file "~/org/inbox.org")
      "* %?
%t")

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
      "* %?
*Date* %t (morning/afternoon/evening)

*Mental/Physical State*

*Problem Statement or Frame*

*Variables that Govern the Situation*

*Alternatives Considered and Reasons for Decision*

*Range of Possible Outcomes*

*Expected Outcomes, Reasoning, and Probabilities*")

     ("r" "Weekly Review" entry
      (file+datetree "~/org/!Tasks.org")
      "** WEEKLY REVIEW
***** Checklist

*Declutter*
- [ ] Clean desk
- [ ] Capture and sort anything you haven't gotten to (browser tabs, papers, etc.)
- [ ] Process inbox
- [ ] Write down anything that's on your mind (under a misc. thoughts section)

*Get Current*
- [ ] Review last week's tasks
- [ ] Reflect on last week
- [ ] Review upcoming tasks and deadlines
- [ ] Review projects and assign next-steps
- [ ] Roughly plan upcoming week

*Get Creative*
- [ ] Review someday list
- [ ] Dream

***** Misc Thoughts
***** Weekly Reflections
***** Dreams and Ideas
" :tree-type week :time-prompt t)

     ("p" "Weekly Plan" entry
      (file+datetree "~/org/!Tasks.org")
      "* WEEKLY PLAN
%?" :tree-type week :time-prompt t)))

;; Refile item to task list
(defun org-refile-to-tasklist () "" (interactive) (org-refile-to-weektree "~/org/!Tasks.org"))
