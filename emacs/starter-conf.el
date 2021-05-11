;; -*- mode: elisp -*-
;; Personal Configuration File

((lambda ()
    (defun rel-file (file) (concat (file-name-directory load-file-name) file))

    ;; ======== UTIL FUNCTIONALITY ========
    (load (rel-file "func/flash-mode-line.el"))
    (load (rel-file "func/init-use-package.el"))
    (load (rel-file "func/org-archive-subtree.el"))
    (load (rel-file "func/refile-to-date-tree.el"))
    (load (rel-file "func/kill-other-buffers.el"))
    (load (rel-file "func/append-capture-template.el"))

    (setq use-package-always-ensure t)
    (load-theme 'wombat t)

    ;; ======== PACKAGES ========
    ;; Package Management
    (load (rel-file "package-defs/auto-package-update/base.el"))
    (load (rel-file "package-defs/quelpa/base.el"))
    (load (rel-file "package-defs/quelpa-use-package/base.el"))
    
    ;; Basic Editing & Navigation
    (load (rel-file "package-defs/undo-tree/base.el"))
    (load (rel-file "package-defs/helm/base.el"))
    
    ;; Org Mode
    (let ((override-conf (lambda ()
			   (org-babel-do-load-languages
			    'org-babel-load-languages
			    '((emacs-lisp . t)
			      (http . t)
			      (shell . t)
			      (typescript . t)
			      (js . t)
			      (plantuml . t))))))
      (load (rel-file "package-defs/org/base.el")))

    (load (rel-file "package-defs/org-super-links/base.el"))
    (load (rel-file "package-defs/ox-gfm/base.el"))

    (load (rel-file "package-defs/ob-http/base.el"))
    (load (rel-file "package-defs/ob-typescript/base.el"))
    (load (rel-file "package-defs/ob-async/base.el"))

    ;; ======== CONIFIGURATION ========
    (load (rel-file "conf-shared.el"))
    ))
