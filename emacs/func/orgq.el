;; -*- mode: elisp -*-

;; Queries for org headlines
(defun pcl/orgq (pom query &rest args)
  "Queries org headlines (runs QUERY at point POM with ARGS). Queries can be
configured via the variable pcl/orgq-queries, so you can add new ones as you
feel like it"
  (save-excursion
    (goto-char (or pom (point)))
    (org-back-to-heading-or-point-min t)
    (apply (plist-get pcl/orgq-queries query)
	   (point)
	   args)))

;; Set of available queries
(setq pcl/orgq-queries
      '(
	:prop
	(lambda (pom property)
       	  (let ((element (org-element-headline-parser pom)))
	    (when (equal (car element) 'headline)
	      (plist-get (cadr element) property))))
	))
