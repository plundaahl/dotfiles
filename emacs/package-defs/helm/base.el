;; -*- mode: elisp -*-

(use-package helm
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
            (org-set-tags-command)
	    (org-tags-completion-function))))
