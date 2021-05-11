;; -*- mode: elisp -*-
(setq column-number-mode t)
(setq global-hl-line-mode t)
(setq inhibit-startup-screen nil)
(setq ns-alternate-modifier 'super)
(setq ns-command-modifier 'meta)
(setq org-adapt-indentation nil)
(setq org-agenda-files '("~/org"))
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-skip-archived-trees nil)
(setq org-agenda-todo-ignore-scheduled 0)
(setq org-agenda-todo-list-sublevels nil)
(setq org-agenda-window-setup 'reorganize-frame)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-enforce-todo-dependencies t)
(setq org-hide-emphasis-markers t)
(setq org-link-frame-setup
      '((vm . vm-visit-folder-other-frame)
	(vm-imap . vm-visit-imap-folder-other-frame)
	(gnus . org-gnus-no-new-news)
	(file . find-file)
	(wl . wl-other-frame)))
(setq org-log-into-drawer t)
(setq org-plantuml-jar-path "~/plantuml.jar")
(setq org-return-follows-link t)
(setq org-roam-directory "~/org")
(setq org-roam-title-sources '((title alias headline) alias))
(setq org-startup-truncated nil)
(setq org-startup-with-inline-images t)
(setq org-stuck-projects '("+PROJECT/-DONE" ("TODO") nil "SCHEDULED<"))
(setq org-tags-exclude-from-inheritance '("PROJECT" "IGNORE"))
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; MODES
(global-visual-line-mode 1)
(transient-mark-mode 1)
