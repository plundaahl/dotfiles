;; -*- mode: elisp -*-

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer
          (remq (current-buffer)
                (cl-remove-if-not 'buffer-file-name (buffer-list)))))
