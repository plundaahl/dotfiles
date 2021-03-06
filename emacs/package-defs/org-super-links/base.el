;; -*- mode: elisp -*-

(defun pcl/wrapped-org-super-links-get-location ()
  (let ((org-refile-targets '((org-agenda-files :regexp . "*"))))
    (org-super-links-get-location)))

(use-package org-super-links
  :after org
  :requires quelpa-use-package
  :quelpa (:fetcher github
	   :upgrade t
	   :repo "toshism/org-super-links")
  :init
  ;; Unbind keys
  (unbind-key "C-c l" org-mode-map)
  (global-unset-key (kbd "C-c C-l"))
  ;; Custom Functions
  (defun org-super-links-link-to-last-capture ()
    "Adds a backlink to the most recently-captured item."
    (interactive)
    (org-with-point-at org-capture-last-stored-marker (org-super-links-store-link))
    (org-super-links-insert-link))
  :bind
  (("C-c s l" . org-super-links-store-link)
   ("C-c s C-l" . org-super-links-insert-link)
   ("C-c s s" . org-super-links-link)
   ("C-c s c" . org-super-links-link-to-last-capture))
  :config
  (setq org-super-links-search-function 'pcl/wrapped-org-super-links-get-location)
  )
