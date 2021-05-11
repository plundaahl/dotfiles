;; -*- mode: elisp -*-

(use-package geiser
  :pin melpa-stable
  :config
  (setq geiser-active-implementations '(mit))
  (setq geiser-mode-eval-last-sexp-to-buffer t)
  (setq geiser-mode-eval-to-buffer-prefix "")
  (setq geiser-mode-start-repl-p t))
