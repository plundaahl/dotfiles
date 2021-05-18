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
  (load (rel-file "package-defs/org/base.el")))

(setq org-tags-exclude-from-inheritance '("PROJECT" "1_1_Mark" "MEETING"))

(setq org-agenda-files '("~/org"
			 "~/org/areas"
			 "~/org/vault"
			 "~/org/projects"))

(setq org-capture-templates '(
     ("t" "Todo" entry
      (file+headline "~/org/inbox.org" "Tasks")
       "* TODO %?
SCHEDULED: %t
:PROPERTIES:
:CREATED: %U
:END:"
       :empty-lines-after 1
       :kill-buffer 1
       :time-prompt t)

     ("n" "Note" entry
      (file+headline "~/org/inbox.org" "Notes")
       "** %?
:PROPERTIES:
:CREATED: %U
:END:"
       :empty-lines-after 1
       :time-prompt t)

     ("w" "Weekly Review" entry
      (file+headline "~/org/reviews.org" "Weekly")
      (file "~/dotfiles/emacs/capture-templates/work/review-weekly.org")
      :empty-lines-after 1
      :jump-to-captured 1
      :time-prompt t)
     
     ("m" "Meeting" entry
      (file+datetree "~/org/!Tasks.org")
       "* TODO Meeting: %? %u :MEETING:
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

     ("a" "Accomplishment" entry
      (file+datetree "~/org/!Tasks.org")
       "* %? :ACCOMPLISHMENT:"
       :tree-type week
       :empty-lines 1)))
