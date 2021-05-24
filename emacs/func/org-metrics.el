;; ENTRIES
;; Individual entries in a given metric
(defun org-metric--entry-from-element (element)
  (let ((entry-data (cadr element)))
    (cons (ts-format "[%Y-%m-%d]"
		     (pcl/as-ts (plist-get entry-data ':CREATED)))
	  (plist-get entry-data ':title))))
(defun org-metric--entry-date (entry) (car entry))
(defun org-metric--entry-value (entry) (cadr entry))

;; METRICS
;; TODO:
;; - Add ID if not present (need to figure out how to visit element)
(defun org-metric--metric-from-element (header &optional restriction)
  (let ((header-props (cadr header)))
    (let ((title (car (plist-get header-props ':title)))
	  (id (plist-get header-props ':ID)))
      (let ((entries (mapcar 'org-metric--entry-from-element
			     (org-ql-query
			       :select 'element
			       :from (buffer-file-name)
			       :where `(and (parent (property "ID"
							      ,id))
					    ,restriction)))))
	(list title id entries)))))


(defun org-metric--metric-title (metric) (car metric))
(defun org-metric--metric-id (metric) (cadr metric))
(defun org-metric--metric-entries (metric) (caddr metric))
(defun org-metric--metric-link (metric)
  (concat "[[id:"
	  (org-metric--metric-id metric)
	  "]["
	  (org-metric--metric-title metric)
	  "]]"))
(defun org-metric--metric-dates (metric)
  (sort
   (seq-uniq
    (mapcar 'org-metric--entry-date
	    (org-metric--metric-entries metric)))
   'string<))
(defun org-metric--metric-value-for-date (metric date)
  (let ((entry (seq-find (lambda (e)
			   (= (ts-difference (ts-parse-org (org-metric--entry-date e))
					     (ts-parse-org date)) 0))
			 (org-metric--metric-entries metric))))
    (if entry (org-metric--entry-value entry) "")))



;; METRICS (Containers of individual metrics)
(defun org-metric--metrics-from-query (query &optional restriction)
  (mapcar (lambda (m)
	    (org-metric--metric-from-element m
					     restriction))
	  (eval query)))

(defun org-metric--metrics-dates (metrics)
  (seq-uniq
   (flatten-list
    (mapcar 'org-metric--metric-dates
	    metrics))))

(defun org-metric--metrics-titles (metrics)
  (mapcar 'org-metric--metric-title metrics))

(defun org-metric--metrics-ids (metrics)
  (mapcar 'org-metric--metric-id metrics))

(defun org-metric--metrics-links (metrics)
  (mapcar 'org-metric--metric-link metrics))

(defun org-metric--metrics-metric-for-title (metrics title)
  (seq-find (lambda (e) (string= title (org-metric--metric-title e)))
	    metrics))

(defun org-metric--metrics-metric-for-id (metrics id)
  (seq-find (lambda (e) (string= id (org-metric--metric-id e)))
	    metrics))

(defun org-metric--metrics-value (metrics id date)
  (org-metric--metric-value-for-date
   (org-metric--metrics-metric-for-id metrics id)
   date))

(defun org-metric--metrics-to-table (metrics)
  (let ((ids (org-metric--metrics-ids metrics))
	(links (org-metric--metrics-links metrics))
	(dates (org-metric--metrics-dates metrics)))
    (cons (cons "Date" links)
	  (mapcar (lambda (date)
		    (cons date
			  (mapcar (lambda (id)
				    (org-metric--metrics-value metrics
							       id
							       date))
				  ids)))
		  dates))))

(defun org-metric-get (query &optional restriction)
  (org-metric--metrics-from-query query restriction))

(defun org-metrics (&optional &key query &key where-entry)
  (if (equal query nil)
      (error "QUERY is required")
    (org-metric--metrics-to-table
     (org-metric--metrics-from-query query where-entry))))
