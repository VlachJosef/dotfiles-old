(defun is-first-object ()
  (save-excursion
    (skip-chars-backward " \n")
    (when (eq ?\} (preceding-char))
      ",")))

(defun is-last-object ()
  (save-excursion
    (skip-chars-forward " \n")
    (when (eq ?\{ (following-char))
      ",")))
