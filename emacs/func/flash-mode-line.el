;; -*- mode: elisp -*-

(defun flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil 'invert-face 'mode-line))

(setq ring-bell-function 'flash-mode-line)
(setq visible-bell t)
