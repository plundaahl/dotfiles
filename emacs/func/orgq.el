;; -*- mode: elisp -*-

;; Queries for org headlines
(defun pcl/orgq (pom-or-element query &rest args)
  "Queries org headlines (runs QUERY at point POM with ARGS). Queries can be
configured via the variable pcl/orgq-queries, so you can add new ones as you
feel like it"
  (save-excursion
    (let ((element (cond ((listp pom-or-element) pom-or-element)
			 ('t (goto-char (or pom-or-element (point)))
			     (org-back-to-heading-or-point-min t)
			     (org-element-headline-parser (point))))))
      (apply (plist-get pcl/orgq-queries query)
	     element
	     args))))

;; Set of available queries
(setq pcl/orgq-queries
      '(
	:prop
	(lambda (element property)
	  (when (equal (car element) 'headline)
	    (plist-get (cadr element) property))))
      )
