;; -*- mode: elisp -*-

(load-theme 'wombat t)

(eval-when-compile
  (require 'use-package))

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)


;; CUSTOMIZED VARIABLES
;;;;;;;;;;;;;;;;;;;;;;;
(custom-set-variables
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(inhibit-startup-screen nil)
 '(ns-alternate-modifier 'super)
 '(ns-command-modifier 'meta)
 '(org-adapt-indentation nil)
 '(org-agenda-files '("~/org"))
 '(org-hide-emphasis-markers t)
 '(org-return-follows-link t)
 '(org-startup-truncated nil)
 '(package-selected-packages '(use-package ob-http))
 '(ring-bell-function 'flash-mode-line)
 '(visible-bell t))
(custom-set-faces
 ;; If there is more than one, they won't work right.
 )


;; PACKAGES
;;;;;;;;;;;
(use-package ob-http
  :ensure t
  :pin melpa-stable)

(use-package org
  :ensure t
  :pin melpa-stable
  :config
  (org-babel-do-load-languages
    'org-babel-load-languages
    '(
      (emacs-lisp . t)
      (http . t)
      (shell . t)
    )
  ))

(use-package helm
  :ensure t
  :pin melpa-stable
  :bind (("M-x" . helm-M-x)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x C-f" . helm-find-files)
	 ("C-h a" . helm-apropos))
  )

;; MODE CONFIGURATIONS
;;;;;;;;;;;;;;;;;;;;;;

(global-visual-line-mode 1)
(transient-mark-mode 1)
(helm-mode 1)

;; CUSTOM FUNCTIONS
;;;;;;;;;;;;;;;;;;;

;; Flash the current mode line
(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))
