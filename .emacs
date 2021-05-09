;; -*- mode: elisp -*-

(load "~/dotfiles/emacs/init-use-package.el")
(load "~/dotfiles/emacs/org-archive-subtree.el")
(load "~/dotfiles/emacs/refile-to-date-tree.el")
(load "~/dotfiles/emacs/kill-other-buffers.el")

(load-theme 'wombat t)

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
 '(org-tags-exclude-from-inheritance '("PROJECT"))
 '(package-selected-packages
   '(quelpa-use-package auto-package-update org-roam use-package ob-http))
 '(ring-bell-function 'flash-mode-line)
 '(visible-bell t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "#303030" :underline nil)))))


;; PACKAGES
;;;;;;;;;;;
(use-package auto-package-update
  :ensure t
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))

(use-package quelpa
  :ensure t
  :pin melpa-stable)

(use-package quelpa-use-package
  :ensure t
  :after quelpa)

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1))

(use-package ob-http
  :ensure t
  :pin melpa-stable)

(use-package ob-typescript
  :ensure t)

(use-package org
  :ensure t
  :pin melpa-stable
  :bind (("C-c c" . org-capture)
	 ("C-c l" . org-store-link)
	 ("C-c a" . org-agenda))
  :config
  (org-babel-do-load-languages
    'org-babel-load-languages
    '(
      (emacs-lisp . t)
      (http . t)
      (shell . t)
      (typescript . t)
      (js . t)
      (plantuml . t)))
  ;; Fix for ob-js
  ;; Taken from https://gist.github.com/mrspeaker/c3b7b8d0b0b96b1a012d736b22d12b2e
  (setq org-babel-js-function-wrapper
      "process.stdout.write(JSON.stringify(require('util').inspect(function(){\n%s\n}(), { maxArrayLength: null, maxStringLength: null, breakLength: Infinity, compact: true })))")
  (setq org-image-actual-width nil)
  (setq org-id-link-to-org-use-id t)
  )

(use-package ox-gfm
  :defer 3
  :after org)

(use-package helm
  :ensure t
  :pin melpa-stable
  :init
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x C-f" . helm-find-files)
	 ("C-h a" . helm-apropos))
  :config
  (setq helm-completing-read-handlers-alist
	  '((find-tag . helm-completing-read-default-find-tag)
	    (xref-find-definitions . helm-completing-read-default-find-tag)
	    (xref-find-references . helm-completing-read-default-find-tag)
	    (ggtags-find-tag-dwim . helm-completing-read-default-find-tag)
	    (tmm-menubar)
	    (find-file)
	    (execute-extended-command)
	    (dired-do-rename . helm-read-file-name-handler-1)
            (dired-do-copy . helm-read-file-name-handler-1)
            (dired-do-symlink . helm-read-file-name-handler-1)
            (dired-do-relsymlink . helm-read-file-name-handler-1)
            (dired-do-hardlink . helm-read-file-name-handler-1)
            (basic-save-buffer . helm-read-file-name-handler-1)
            (write-file . helm-read-file-name-handler-1)
            (write-region . helm-read-file-name-handler-1)
            (org-set-tags-command))))

(use-package org-roam
  :ensure t
  :pin melpa-stable
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org")
  :bind (:map org-roam-mode-map
	  (("C-c n l" . org-roam)

           ("C-c n g" . org-roam-graph))
	  :map org-mode-map
          (("C-c n i" . org-roam-insert))
          (("C-c n I" . org-roam-insert-immediate))))

(use-package org-super-links
  :after org
  :quelpa (org-super-links
	   :fetcher github
	   :upgrade t
	   :repo "toshism/org-super-links")
  :init
  (unbind-key "C-c l" org-mode-map)
  (global-unset-key (kbd "C-c C-l"))
  :bind (("C-c s l" . org-super-links-store-link)
	 ("C-c s C-l" . org-super-links-insert-link)
	 ("C-c s s" . org-super-links-link))
  )

(use-package ob-async
  :ensure t
  :pin melpa-stable)

;; MODE CONFIGURATIONS
;;;;;;;;;;;;;;;;;;;;;;

(global-visual-line-mode 1)
(transient-mark-mode 1)

;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;

;; Flash the current mode line
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

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
