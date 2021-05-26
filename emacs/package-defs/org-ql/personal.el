;; -*- mode: elisp -*-

(use-package org-ql
  :pin melpa-stable
  :after (org)
  :config
  (load (rel-file "pred-created.el")))
