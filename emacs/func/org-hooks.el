;; -*- mode: elisp -*-

(add-hook 'org-after-todo-state-change-hook 'pcl/org-hook-toggle-inactive-tag)
(defun pcl/org-hook-toggle-inactive-tag ()
  ""
  (org-toggle-tag "INACTIVE"
		  (cond ((string= "FUTURE" org-state) 'on)
			((string= "PAUSED" org-state) 'on)
			((string= "COMPLETE" org-state) 'on)
			((string= "ABANDONED" org-state) 'on)
			('t 'off))))
