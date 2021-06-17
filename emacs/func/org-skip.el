;; -*- mode: elisp -*-

(defun pcl/org-skip-unless (&rest predicates)
  ""
  (if (seq-every-p
       ;; Evaluate each predicate
       (lambda (p)
	 (apply (plist-get pcl/org-skip-predicates (car p))
		(cdr p)))
	predicates)
      nil
    (pcl/orgq (point) ':prop :end)))

;; List of custom predicates that I can use in pcl/org-skip-unless
(setq pcl/org-skip-predicates
      '(:created-in
	(lambda (unit &key of)
	  (let ((time (pcl/orgq (point) ':prop :CREATED)))
	    (and (not (null time))
		 (pcl/ts-in-p time of unit))))
	))
