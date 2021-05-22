;; -*- mode: elisp -*-
;; Most of this is cobbled together from https://stackoverflow.com/a/36230471

(defun pcl/insert-created-timestamp-if-not-present ()
  "Adds a CREATED property to the current heading as an inactive timestamp, but only if required"
  (when (not (org-entry-get nil "CREATED"))
    (org-entry-put nil "CREATED" (format-time-string "[%Y-%m-%d %a %H:%M]"))))

(add-hook 'org-insert-heading-hook
         #'(lambda()
               (save-excursion
                    (org-back-to-heading)
                    (pcl/insert-created-timestamp-if-not-present))))

(add-hook 'org-capture-before-finalize-hook
         #'(lambda()
               (save-excursion
                    (org-back-to-heading)
                    (pcl/insert-created-timestamp-if-not-present))))
