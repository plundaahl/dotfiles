;; -*- mode: elisp -*-

(use-package org
  :pin melpa-stable
  :bind (("C-c c" . org-capture)
	 ("C-c l" . org-store-link)
	 ("C-c a" . org-agenda))
  :config
  ;; Fix for ob-js
  ;; Taken from https://gist.github.com/mrspeaker/c3b7b8d0b0b96b1a012d736b22d12b2e
  (setq org-babel-js-function-wrapper
      "process.stdout.write(JSON.stringify(require('util').inspect(function(){\n%s\n}(), { maxArrayLength: null, maxStringLength: null, breakLength: Infinity, compact: true })))")
  (setq org-image-actual-width nil)
  (setq org-id-link-to-org-use-id t)
  (setq org-startup-folded t)

  ;; load custom configs if present
  (if (boundp 'override-config) (override-config))
  :init
  (require 'org-id)
  )
