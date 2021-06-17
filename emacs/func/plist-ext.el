;; -*- mode: elisp -*-

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
