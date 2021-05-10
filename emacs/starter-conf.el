;; -*- mode: elisp -*-

(defun load-rel (file) (load (concat custom-conf-dir file)))

(load-rel "func/flash-mode-line.el")
(load-rel "func/init-use-package.el")
(load-rel "func/org-archive-subtree.el")
(load-rel "func/refile-to-date-tree.el")
(load-rel "func/kill-other-buffers.el")
(load-rel "func/append-capture-template.el")

(load-theme 'wombat t)

;; PACKAGE MANAGEMENT
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

;; ESSENTIALS
(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode 1))

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

;; ORG
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
  (setq org-startup-folded t)
  :init
  (require 'org-id)
  )

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

(use-package ox-gfm
  :defer 3
  :after org)

;; BABEL
(use-package ob-http
  :ensure t
  :pin melpa-stable)

(use-package ob-typescript
  :ensure t)

(use-package ob-async
  :ensure t
  :pin melpa-stable)

