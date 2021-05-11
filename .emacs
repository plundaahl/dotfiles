;; -*- mode: elisp -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 100)
 '(package-selected-packages
   '(ox-gfm quelpa-use-package auto-package-update org-roam use-package ob-http)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:extend t :background "#303030" :underline nil)))))

;; PACKAGES
;;;;;;;;;;;
(let ((custom-conf "init-personal.el"))
  (load "~/dotfiles/emacs/init.el"))
