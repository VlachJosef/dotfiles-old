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

;;https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(define-key ctl-x-4-map "t" 'toggle-window-split)

(defun normalize-import (import)
  (let* ((normalized (s-replace " " "" (s-collapse-whitespace import)))
	 (prefixAndClassList (s-split "{" normalized)))
    (pcase (length prefixAndClassList)
      (`1 (cons import nil)) ;; return input as is
      (`2 (let* ((prefix (car prefixAndClassList))
		 (classList (s-split "," (s-replace "}" "" (nth 1 prefixAndClassList))))
		 (value))
	    (dolist (element classList value)
	      (setq value (cons (concat prefix element) value)))))
      (size nil)))) ; input is broken, it is not valid scala import, let's return empty list

(defun normalize-and-sort (input)
  (sort (apply #'append (mapcar 'normalize-import input)) 'string<))

(defun search-import (search-term)
  "Use ag to search lines in project starting with keyword import and containing text `search-term'
By default all import clauses are normalized, meaning that any import including `import selector clause'
enclosed in braces is broken down into its own import clause.
Prefix arguments:
   no arg  - normalize
   C-u     - normalize and copy current import to kill ring
   C--     - don't normalize
   C-- C-u - don't normalize and copy current import to kill ring"
  (interactive
   (list (read-from-minibuffer
          (projectile-prepend-project-name "Ag search for import: ")
          (projectile-symbol-or-selection-at-point))))
  (let ((normalization-function)
	(copy-to-kill-ring))
    (pcase current-prefix-arg
      (`nil (setq normalization-function `normalize-and-sort)
	    (setq copy-to-kill-ring nil))
      (`- (setq normalization-function `identity)
	  (setq copy-to-kill-ring nil))
      (`(,n . nil) (if (< 0 n)
		       (progn (setq normalization-function `normalize-and-sort)
			      (setq copy-to-kill-ring t))
		     (progn (setq normalization-function `identity)
			    (setq copy-to-kill-ring t)))))
    (if (and (executable-find "ag") (executable-find "sort") (executable-find "uniq"))
	(let* ((default-directory (projectile-project-root))
	       (res-raw (shell-command-to-string (format "ag import.*%s --nonumbers --noheading --nofilename --nobreak --ignore-case | sort | uniq" search-term)))
	       (lines (split-string (s-replace "import " "" (s-trim res-raw)) "\n"))
	       (import (with-temp-buffer
			 (insert (mapconcat (lambda (elm) (s-trim-left elm)) lines "\n"))
			 (sort-lines nil (point-min) (point-max))
			 (ido-completing-read "Select an import: " (funcall normalization-function (split-string (buffer-string) "\n"))))))
	  (if copy-to-kill-ring
	      (progn (kill-new (format "import %s" import))
		     (message "%s added to kill ring" import))
	    (ensime-insert-import import)))
      (error "Commands 'ag', 'sort' and 'uniq' are required to use this command"))))

(define-key global-map (kbd "s-i" ) `search-import)

(define-key global-map (kbd "C-x 4 s" ) `sbt:switch-to-active-sbt-buffer)

;; Taken from
;; https://www.emacswiki.org/emacs/KeyboardMacrosTricks
(defun save-macro (name)
    "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
     (interactive "SName of the macro :")  ; ask for the name of the macro
     (kmacro-name-last-macro name)         ; use this name for the macro
     (find-file user-init-file)            ; open ~/.emacs or other user init file
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro
     (newline)                             ; insert a newline
     (switch-to-buffer nil))               ; return to the initial buffer

;; Used by yasnippet generating Play json formatter - Json.format[Foo]
(defun downcase-first-letter-only (string)
  "Lower case first letter of provided string"
  (when (> (length string) 0)
    (let ((first-letter (substring string 0 1))
	  (rest-of-letters (substring string 1)))
      (format "%s%s" (downcase first-letter) rest-of-letters))))

(defun insert-or-replace-word (word)
  (move-beginning-of-line 1)
  (let ((defs '("class" "object" "trait" "val" "def")))
    (when (member word defs)
      (cond ((member (word-at-point) defs)
	     (progn
	       (delete-region (beginning-of-thing 'word) (+ 1 (end-of-thing 'word)))
	       (insert (format "%s " word))
	       (move-end-of-line 1)))
	    (t
	     (progn
	       (insert (format "%s " word))
	       (move-end-of-line 1)))))))

(defun delete-word ()
  (move-beginning-of-line 1)
  (let ((defs '("class" "object" "trait" "val" "def")))
    (cond ((member (word-at-point) defs)
	   (progn (delete-region (beginning-of-thing 'word) (+ 1 (end-of-thing 'word)))
		  (move-end-of-line 1)))))
  (move-end-of-line 1))

(defhydra scala-minibuffer-search ()
  "
Search for _c_ class _t_ trait _o_ object _v_ val _d_ def _q_ quit"
    ("c" (insert-or-replace-word "class") nil)
    ("t" (insert-or-replace-word "trait") nil)
    ("o" (insert-or-replace-word "object") nil)
    ("d" (insert-or-replace-word "def") nil)
    ("v" (insert-or-replace-word "val") nil)
    ("q" (delete-word) nil :color blue))

(defun mini-hook ()
  (if (and
       (functionp 'sbt:find-root)
       (sbt:find-root)
       (string-match "Ag search for:" (minibuffer-prompt)))
      (scala-minibuffer-search/body)))

(add-hook 'minibuffer-setup-hook 'mini-hook)

(defhydra run-mongo ()
  "
Run mongo: _r_ reset _s_ start _n_ start no-auth _e_ eof _q_ quit"
  ("r" (mongo "reset") nil)
  ("s" (mongo "start") nil)
  ("n" (mongo "start-no-auth") nil)
  ("e" (send-eof) nil)
  ("q" nil nil :color blue))

(defmacro with-shell-in-sbt-project (body)
  `(let* ((sbt-root (sbt:find-root))
          (buffer-name (format "*shell* %s" sbt-root))
          (buffer (get-buffer buffer-name)))
     (if sbt-root
       (progn
         (unless buffer
           (with-current-buffer (shell)
             (rename-buffer buffer-name)
             (setq buffer (current-buffer))))
         (with-current-buffer buffer
           ,body))
     (message "Not in sbt project."))))

(defun send-eof ()
  (with-shell-in-sbt-project
   (comint-send-eof)))

(defun mongo (command)
  (interactive)
  (with-shell-in-sbt-project
   (comint-send-string (current-buffer) (concat sbt-root (format "run-mongo.sh %s" command) "\n"))))
