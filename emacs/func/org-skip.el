;; -*- mode: elisp -*-

(defun pcl/org-skip-unless (&rest predicates)
  ""
  (if (seq-every-p 'pcl/eval-org-skip-predicate predicates)
      nil
    (pcl/orgq (point) ':prop :end)))

(defun pcl/eval-org-skip-predicate (predicate)
  (apply (plist-get pcl/org-skip-predicates (car predicate))
	 (cdr predicate)))

;; List of custom predicates that I can use in pcl/org-skip-unless
(setq pcl/org-skip-predicates
      '(:and
	(lambda (&rest predicates)
	  (seq-every-p 'pcl/eval-org-skip-predicate predicates))

	:or
	(lambda (&rest predicates)
	  (not (seq-every-p (lambda (p)
			      (not (pcl/eval-org-skip-predicate p)))
			    predicates)))

	:not
	(lambda (predicate)
	  (not (pcl/eval-org-skip-predicate predicate)))

	:created-before
	(lambda (before-point)
  	  (let ((time (pcl/orgq (point) ':prop :CREATED)))
	    (and (not (null time))
		 (ts< (pcl/ts-parse time) (pcl/ts-parse before-point)))))

	:created-in
	(lambda (unit &key of)
	  (let ((time (pcl/orgq (point) ':prop :CREATED)))
	    (and (not (null time))
		 (pcl/ts-in-p time of unit))))
	))
