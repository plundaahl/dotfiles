;; -*- mode: elisp -*-

(defun pcl/org-kill-property (&optional property pom)
  (interactive)
  (let ((property (or property (org-read-property-name)))
	(pom (or pom (point))))
    (kill-new (org-entry-get pom property))))
