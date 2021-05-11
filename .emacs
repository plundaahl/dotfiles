;; -*- mode: elisp -*-

;; CUSTOMIZED VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(global-hl-line-mode t)
 '(inhibit-startup-screen nil)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(org-adapt-indentation nil)
 '(org-agenda-files '("~/org"))
 '(org-agenda-restore-windows-after-quit t)
 '(org-agenda-skip-archived-trees nil)
 '(org-agenda-todo-ignore-scheduled 0)
 '(org-agenda-todo-list-sublevels nil)
 '(org-agenda-window-setup 'reorganize-frame)
 '(org-enforce-todo-checkbox-dependencies t)
 '(org-enforce-todo-dependencies t)
 '(org-hide-emphasis-markers t)
 '(org-link-frame-setup
   '((vm . vm-visit-folder-other-frame)
     (vm-imap . vm-visit-imap-folder-other-frame)
     (gnus . org-gnus-no-new-news)
     (file . find-file)
     (wl . wl-other-frame)))
 '(org-log-into-drawer t)
 '(org-plantuml-jar-path "~/plantuml.jar")
 '(org-return-follows-link t)
 '(org-roam-directory "~/org")
 '(org-roam-title-sources '((title alias headline) alias))
 '(org-startup-truncated nil)
 '(org-startup-with-inline-images t)
 '(org-stuck-projects '("+PROJECT/-DONE" ("TODO") nil "SCHEDULED<"))
 '(org-tags-exclude-from-inheritance '("PROJECT" "IGNORE"))
 '(package-selected-packages
   '(ox-gfm quelpa-use-package auto-package-update org-roam use-package ob-http))
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "#303030" :underline nil)))))


;; PACKAGES
;;;;;;;;;;;
(let ((custom-conf-dir "~/dotfiles/emacs/"))
  (load "~/dotfiles/emacs/starter-conf.el"))


;; MODE CONFIGURATIONS
;;;;;;;;;;;;;;;;;;;;;;

(global-visual-line-mode 1)
(transient-mark-mode 1)

;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;

;; Flash the current mode line
(if (file-exists-p "~/.emacs-per-machine.el")
    (load "~/.emacs-per-machine.el"))

(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
  backup-by-copying t    ; Don't delink hardlinks
  version-control t      ; Use version numbers on backups
  delete-old-versions t  ; Automatically delete excess backups
  kept-new-versions 20   ; how many of the newest versions to keep
  kept-old-versions 5    ; and how many of the old
  )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
