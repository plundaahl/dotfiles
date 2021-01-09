;; -*- mode: elisp -*-

(load "~/dotfiles/emacs/init-use-package.el")

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
 '(org-agenda-todo-ignore-scheduled 0)
 '(org-agenda-todo-list-sublevels nil)
 '(org-agenda-window-setup 'reorganize-frame)
 '(org-capture-templates
   '(("t" "Todo" entry
      (file "~/org/!Tasks.org")
      "* TODO %?
%t" :prepend t)
     ("m" "Meeting" entry
      (file "~/org/!Tasks.org")
      "* TODO Meeting%?
%t" :clock-in t :clock-resume t)
     ("o" "Opp (Sales)" entry
      (file "~/org/opps.org")
      "* %^{opp name}
*Message for Mark*
Hi Mark,

I'm trying to make sure I have context for %\\1 before I dive in and start working. Would you be able to help me fill in any of the following pieces of information?

1. Who are the primary stakeholders? (AE, SC, anyone else involved)
2. What communications channels are being used, and can I be invited to them?
3. What is the timeline on upcoming demos?
4. What existing environments, code, artifacts are there, if any?
5. Is there any historical context that might be useful?

Thanks!

*Overview*

*Communications*
- Slack Channel
- Box Notes

** Stakeholders
** Timeline
** Environments, Code, Artifacts")
     ("d" "Decision" entry
      (file "~/org/decisions.org")
      "* %?
*Date* %t (morning/afternoon/evening)

*Mental/Physical State*

*Problem Statement or Frame*

*Variables that Govern the Situation*

*Alternatives Considered and Reasons for Decision*

*Range of Possible Outcomes*

*Expected Outcomes, Reasoning, and Probabilities*")))
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
 '(org-roam-file-exclude-regexp "/\\!.+\\.org/")
 '(org-roam-title-sources '((title alias headline) alias))
 '(org-startup-truncated nil)
 '(org-stuck-projects '("+PROJECT/-DONE" ("TODO") nil "SCHEDULED<"))
 '(org-tags-exclude-from-inheritance '("PROJECT"))
 '(package-selected-packages '(org-roam use-package ob-http))
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
(use-package undo-tree
  :ensure t
  :pin melpa-stable
  :init
  (global-undo-tree-mode 1))

(use-package ob-http
  :ensure t
  :pin melpa-stable)

(use-package ob-typescript
  :ensure t
  :pin melpa-stable)

(use-package org
  :ensure t
  :pin melpa-stable
  :bind (("C-c c" . org-capture))
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
  )

(use-package helm
  :ensure t
  :pin melpa-stable
  :init
  (helm-mode 1)
  :bind (("M-x" . helm-M-x)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x C-f" . helm-find-files)
	 ("C-h a" . helm-apropos))
  )

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
