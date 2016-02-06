;;; Personal functions

;; For loading personal configurations
(defun personal (library)
  (load (concat "~/.emacs.d/personal/" (symbol-name library)) 'no-error))

;; Insert the filepath of buffer into the active buffer
(defun insert-other-buffer-file-name ()
  "Insert the full filepath of a buffer into the current buffer"
  (interactive)
  (insert (buffer-file-name
	   (get-buffer
	    (ido-completing-read
	     "Select a buffer to get filename from: "
	     (loop for buf being the buffers
		   when (buffer-file-name buf)
		   collect (buffer-name buf) into file-buffers
		   finally return file-buffers))))))
