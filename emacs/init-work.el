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

(setq org-capture-templates '(
     ("t" "Todo" entry
      (file+datetree "~/org/!Tasks.org")
       "* TODO %?
SCHEDULED: %t"
       :tree-type week
       :empty-lines 1
       :kill-buffer
       :time-prompt t)

     ("n" "Note" entry
      (file+headline "~/org/inbox.org" "Notes")
       "** %?
:PROPERTIES:
:CREATED: %U
:END:"
       :empty-lines-after 1
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
