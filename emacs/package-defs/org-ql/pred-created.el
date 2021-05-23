;; -*- mode: elisp -*-

(defun pcl/as-ts (obj)
  "Returns nil if OBJ is nil.
Returns OBJ if OBJ is a ts structure.
Parses OBJ to ts if OBJ is a string"
  (cond ((equal obj nil) nil)
	((ts-p obj) (ts-fill obj))
	((stringp obj) (ts-fill (ts-parse-org obj)))
	(else (error "OBJ is wrong type"))))

(defun pcl/ts-week-start (time)
  (let ((time (pcl/as-ts time)))
    (ts-fill (ts-dec 'day (ts-dow (ts-dec 'day 1 time)) time))))

(defun pcl/ts-week-end (time)
  (pcl/ts-this-week (ts-inc 'day 6 (pcl/as-ts time))))

(org-ql--defpred created (&key from to on)
  "Search for entries with \"CREATED\" property in range or on date"
  :body (if (and on (or from to))
	    (error "Either specify FROM and/or TO, or ON")
	  (let ((heading-time (pcl/as-ts (org-entry-get (point) "CREATED")))
		(on (pcl/as-ts on))
		(from (pcl/as-ts (or on from)))
		(to (pcl/as-ts (or on to))))
	      (and t
		   (if from (ts<= from heading-time) t)
		   (if to (ts< heading-time (ts-adjust 'day +1 to)) t)
		   ))))
