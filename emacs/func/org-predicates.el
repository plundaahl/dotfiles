;; -*- mode: elisp -*-

;;;;;;;;;;;;;;;;;;
;; TIME HELPERS ;;
;;;;;;;;;;;;;;;;;;
(require 'ts)
(defun pcl/ts-floor (time unit)
  "Rounds TIME down to the nearest UNIT, returning the result as a ts struct.

TIME may be either a timestamp string (as parsed by 'parse-time-string') or a ts struct.

UNIT must be quoted, and must be one of:
- 'second 'sec 's
- 'minute 'min 'm
- 'hour 'hr 'h
- 'day 'd
- 'week 'wk 'w
- 'month 'mon 'M
- 'quarter 'qrt 'q
- 'year 'yr 'y"
  (let ((ts (cond ((ts-p time) time)
		  ((stringp time) (ts-parse time))))
	(radix (cond ((or (equal unit 'second) (equal unit 'sec) (equal unit 's)) 0)
		     ((or (equal unit 'min) (equal unit 'minute) (equal unit 'm)) 1)
		     ((or (equal unit 'hr) (equal unit 'hour) (equal unit 'h)) 2)
		     ((or (equal unit 'day) (equal unit 'd)) 3)
		     ((or (equal unit 'week) (equal unit 'wk) (equal unit 'w)) 4)
		     ((or (equal unit 'mon) (equal unit 'month) (equal unit 'M)) 5)
		     ((or (equal unit 'qrt) (equal unit 'quarter) (equal unit 'q)) 6)
		     ((or (equal unit 'yr) (equal unit 'year) (equal unit 'y)) 7))))
    ;; Error handling
    (when (null ts) (error "Unrecognized type for paramater time" time))
    (when (null radix) (error "Invalid unit" unit))
    ;; Conversion:                                                    Floor to nearest...
    (when (>= radix 1) (ts-adjustf ts 'second (- (ts-second ts))))    ;; minute
    (when (>= radix 2) (ts-adjustf ts 'minute (- (ts-minute ts))))    ;; hour
    (when (>= radix 3) (ts-adjustf ts 'hour (- (ts-hour ts))))        ;; day
    (when (= radix 4) (ts-adjustf ts 'day (- (ts-dow ts))))           ;; week
    (when (>= radix 5) (ts-adjustf ts 'day (- 1 (ts-day ts))))        ;; month
    (when (= radix 6) (ts-adjustf ts 'month (- (% (ts-month ts) 3)))) ;; quarter
    (when (>= radix 7) (ts-adjustf ts 'month (- 1 (ts-month ts))))    ;; year
    ;; Result
    ts))

(defun pcl/ts-in-p (time period unit)
  "Returns true if TIME and PERIOD both round down to the same UNIT

TIME and PERIOD may both be either a ts struct or a timestamp string.
UNIT must be one of:
- 'second 'sec 's
- 'minute 'min 'm
- 'hour 'hr 'h
- 'day 'd
- 'week 'wk 'w
- 'month 'mon 'M
- 'quarter 'qrt 'q
- 'year 'yr 'y"
  (let ((ts (pcl/ts-floor time))
	(beg (pcl/ts-floor period unit)))
    (ts= ts beg)))

;;;;;;;;;;;;;;;;;;;;;;;
;; ORG QUERY HELPERS ;;
;;;;;;;;;;;;;;;;;;;;;;;
;; Queries for org headlines
(defun pcl/orgq (pom query &rest args)
  "Queries org headlines (runs QUERY at point POM with ARGS). Queries can be
configured via the variable pcl/orgq-queries, so you can add new ones as you
feel like it"
  (apply (plist-get pcl/orgq-queries query) (or pom (point)) args))

(setq pcl/orgq-queries
      '(:prop (lambda (pom property)
		(let ((element (org-element-headline-parser pom)))
		  (when (equal (car element) 'headline)
		    (plist-get (cadr element) property))))))

;;;;;;;;;;;;;;;;;;;
;; PLIST HELPERS ;;
;;;;;;;;;;;;;;;;;;;
(defun pcl/plist-keys (plist)
  "Returns all keys in plist PLIST"
  (if (null plist)
      '()
       (cons (car plist) (pcl/plist-keys (cddr plist)))))

(defun pcl/plist-map (plist function)
  "Runs FUNCTION for every pair in PLIST. FUNCTION should take two arguments, the key and the value"
  (if (null plist)
      nil
    (cons (funcall function (car plist) (cadr plist))
 	  (pcl/plist-map (cddr plist) function))))
