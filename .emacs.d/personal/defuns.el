;;; Personal functions

(require 'cl-lib)

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

(defun normalize-and-sort (input search-term)
  (let ((all-imports (sort (apply #'append (mapcar 'normalize-import input)) 'string<)))
    (cl-remove-if-not (lambda (import) (string-match search-term import)) all-imports)))

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
  (let ((identity2 (lambda (input search-term) input))
        (normalization-function)
	(copy-to-kill-ring))
    (pcase current-prefix-arg
      (`nil (setq normalization-function `normalize-and-sort)
	    (setq copy-to-kill-ring nil) `identity2)
      (`- (setq normalization-function (lambda (input search-term) input))
	  (setq copy-to-kill-ring nil))
      (`(,n . nil) (if (< 0 n)
		       (progn (setq normalization-function `normalize-and-sort)
			      (setq copy-to-kill-ring t))
		     (progn (setq normalization-function `identity2)
			    (setq copy-to-kill-ring t)))))
    (if (and (executable-find "ag") (executable-find "sort") (executable-find "uniq"))
	(let* ((default-directory (projectile-project-root))
	       (res-raw (shell-command-to-string (format "ag import.*%s --nonumbers --noheading --nofilename --nobreak --ignore-case | sort | uniq" search-term)))
	       (lines (split-string (s-replace "import " "" (s-trim res-raw)) "\n"))
	       (import (with-temp-buffer
			 (insert (mapconcat (lambda (elm) (s-trim-left elm)) lines "\n"))
			 (sort-lines nil (point-min) (point-max))
			 (ido-completing-read "Select an import: " (funcall normalization-function (split-string (buffer-string) "\n") search-term)))))
	  (if copy-to-kill-ring
	      (progn (kill-new (format "import %s" import))
		     (message "%s added to kill ring" import))
	    (ensime-insert-import import)))
      (error "Commands 'ag', 'sort' and 'uniq' are required to use this command"))))

(define-key global-map (kbd "s-i" ) `search-import)

;;(define-key global-map (kbd "C-x 4 s" ) `sbt-switch-to-active-sbt-buffer)
(define-key global-map (kbd "C-x 4 s" ) `sbt-or-sgm:sbt-or-ghci)

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
  (let ((search-for (string-match "\\[.*] Ag search for (default \\(.*)\\): " (minibuffer-prompt)))
        (result (match-string-no-properties 1)))
    (when (string= "" (minibuffer-contents-no-properties))
          (insert (format "%s" result)))
    (move-beginning-of-line 1)
    (let ((defs '("new" "class" "object" "trait" "val" "def" "type")))
      (when (member word defs)
        (cond ((member (word-at-point) defs)
               (progn
                 (delete-region (beginning-of-thing 'word) (+ 1 (end-of-thing 'word)))
                 (insert (format "%s " word))
                 (move-end-of-line 1)))
              (t
               (progn
                 (insert (format "%s" word))
                 (move-end-of-line 1))))))))

(defun delete-word ()
  (move-beginning-of-line 1)
  (let ((defs '("new" "class" "object" "trait" "val" "def" "type")))
    (cond ((member (word-at-point) defs)
	   (progn (delete-region (beginning-of-thing 'word) (+ 1 (end-of-thing 'word)))
		  (move-end-of-line 1)))))
  (move-end-of-line 1))

(defhydra scala-minibuffer-search ()
  "
Search for _n_ new _c_ class _t_ trait _o_ object _v_ val _d_ def _y_ type _q_ quit"
    ("n" (insert-or-replace-word "new") nil)
    ("c" (insert-or-replace-word "class") nil)
    ("t" (insert-or-replace-word "trait") nil)
    ("o" (insert-or-replace-word "object") nil)
    ("d" (insert-or-replace-word "def") nil)
    ("v" (insert-or-replace-word "val") nil)
    ("y" (insert-or-replace-word "type") nil)
    ("q" (delete-word) nil :color blue))

(defun mini-hook ()
  (if (and
       (functionp 'sbt:find-root)
       (sbt:find-root)
       (string-match "\\[.*] Ag search for (default .*): " (minibuffer-prompt)))
      (scala-minibuffer-search/body)))

(add-hook 'minibuffer-setup-hook 'mini-hook)

(defhydra run-mongo ()
  "
Run mongo: _r_ reset _s_ start _n_ start no-auth _e_ eof _t_ shell _q_ quit"
  ("r" (mongo "reset") nil)
  ("s" (mongo "start") nil)
  ("n" (mongo "start-no-auth") nil)
  ("e" (send-eof) nil)
  ("t" (switch-to-shell) nil)
  ("q" nil nil :color blue))

(defun switch-to-shell ()
  "Switch to shell with running mongo db"
  (let* ((sbt-root (sbt:find-root))
         (buffer-name (format "*shell* %s" sbt-root))
         (buffer (get-buffer buffer-name)))
    (if (and sbt-root buffer)
        (switch-to-buffer-other-window buffer)
     (message "Not in sbt project."))))

(defmacro with-shell-in-sbt-project (body)
  `(let* ((sbt-root (sbt:find-root))
          (buffer-name (format "*shell* %s" sbt-root))
          (buffer (get-buffer buffer-name)))
     (if sbt-root
       (progn
         (unless buffer
           (with-current-buffer (shell)
             (rename-buffer buffer-name)
             (comint-send-string (current-buffer) (concat "invoked-from-directory " sbt-root "\n"))
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

(defun restclient-suppress-by-default ()
  (interactive)
  (pcase current-prefix-arg
      (`nil (define-key restclient-mode-map [remap restclient-http-send-current-suppress-response-buffer] 'restclient-http-send-current)
            (define-key restclient-mode-map [remap restclient-http-send-current] 'restclient-http-send-current-suppress-response-buffer)
            (message "rest-mode C-c command remapped."))
      (`(,n . nil) ;; run with C-u
       (define-key restclient-mode-map [remap restclient-http-send-current-suppress-response-buffer] nil)
       (define-key restclient-mode-map [remap restclient-http-send-current] nil)
       (message "rest-mode commands keys has been reseted."))))

(defvar restclient:current-rest-calls-buffer nil)

(defun restclient:call-last ()
  (let ((cb (if restclient:current-rest-calls-buffer
                restclient:current-rest-calls-buffer
              (ido-completing-read "Switch to rest calls buffer: "
                                   (cl-loop for buffer being the buffers
                                            when (string-match "^restCalls.*" (buffer-name buffer))
                                            collect (buffer-name buffer) into file-buffers
                                            finally return file-buffers)))))
    (setq restclient:current-rest-calls-buffer cb)
    (with-current-buffer cb
      (restclient-http-send-current-suppress-response-buffer))))

(defun restclient:open-current-rest-calls ()
  (if restclient:current-rest-calls-buffer
      (switch-to-buffer-other-window restclient:current-rest-calls-buffer)
    (message "No restCall buffer to open."))
  )

(defun restclient:reset-open-current-rest-calls ()
  (setq restclient:current-rest-calls-buffer nil)
  (message "restclient:current-rest-calls-buffer set to nil."))

(defun restclient:save-some-buffer-and-make-rest-call ()
  (interactive)
  (pcase current-prefix-arg
    (`nil
     (if (equal (save-some-buffers) "(No files need saving)")
         (restclient:call-last)
       (restclient:hydra/body)))
    (`(,n . nil) ;; run with C-u
     (restclient:hydra/body))))

(defun restclient:save-single-buffer-and-make-rest-call ()
  (interactive)
  (pcase current-prefix-arg
    (`nil
     (cond ((string-match "^*sbt*" (buffer-name))
            (restclient:call-last))
           ((equal (progn
                      (setq current-prefix-arg '(4)) ; C-u
                      (basic-save-buffer)) "(No changes need to be saved)")
            (restclient:call-last))
           (t
            (restclient:hydra/body))))
    (`(,n . nil) ;; run with C-u
     (restclient:hydra/body))))

;;(bind-key "C-c s" 'restclient:save-some-buffer-and-make-rest-callb)
(bind-key "C-c C-s" 'restclient:save-single-buffer-and-make-rest-call)

(defhydra restclient:hydra ()
  "
Rest client: _s_ last _d_ open _r_ reset _q_ quit"
  ("s" (restclient:call-last) nil :color blue)
  ("d" (restclient:open-current-rest-calls) nil :color blue)
  ("r" (restclient:reset-open-current-rest-calls) nil)
  ("q" nil nil :color blue))

(defun tut-toggle-between-scala-and-markdown()
  (interactive)

  (when (equal (buffer-name) "slides.html")
    (if (equal major-mode 'scala-mode)
        (progn
          (widen)
          (markdown-mode))
      (save-excursion
        (search-backward "```tut")
        (next-line)
        (let ((start (point)))
          (search-forward "```")
          (beginning-of-line)
          (narrow-to-region start (point))
          (scala-mode))))))

(define-key global-map (kbd "C-x m" ) `tut-toggle-between-scala-and-markdown)


(defun nxml-pretty-format ()
    (interactive)
    (save-excursion
        (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t)
        (nxml-mode)
        (indent-region begin end)))


(defun defly ()
  (interactive)
  (let* ((cmd-and-args (funcall (flymake-get-init-function buffer-file-name)))
         (cmd          (nth 0 cmd-and-args))
         (args         (nth 1 cmd-and-args))
         (dir          (nth 2 cmd-and-args))
         (process (apply 'start-file-process
                         "flymake-proc-2" (current-buffer) cmd args)))

    (set-process-sentinel process 'my-flymake-process-sentinel)
    (set-process-filter process 'my-flymake-process-filter)
    (message "cmd: %s" cmd)
    (message "args: %s" args)
    ;;(message "dir: %s" dir)
    )

)

(defun my-flymake-process-sentinel (process _event)
  "Sentinel for syntax check buffers."
  (message "my-flymake-process-sentinel process: %s" process)
  (message "my-flymake-process-sentinel _event : %s" _event)
   (when (memq (process-status process) '(signal exit))
     (message "HAHAHAHAHAHAHHAHAH")
     )
  )

(defun my-flymake-process-filter (process output)
  (message "my-flymake-process-filter process: %s" process)
  (message "my-flymake-process-filter output : %s" output)
  )

(defun no-test-line ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-excursion
      (flush-lines "^test/" (point-min) (point-max) t)
      (flush-lines "src/test/" (point-min) (point-max) t))))

(defun pre-process-kill-ring-element (element)
  (replace-regexp-in-string "^[[:space:]]+" ""
                            (replace-regexp-in-string "[[:space:]]+$" "" (substring-no-properties element))))

(defun preprocess-kill-ring ()
  (let ((result nil)
        (element nil))
    (dolist (element kill-ring)
      (progn
        (setq element (pre-process-kill-ring-element element))
        (when (not (or
                    (eq 0 (length element))
                    (string-match-p "[\r\n]+" element)))
          (setq result (cons element result)))))
    (reverse result)))

(defun browse-kill-ring ()
  (interactive)
  (insert (ivy-read "Pick an element: "
                    (preprocess-kill-ring))))

(global-set-key (kbd "C-M-y") 'browse-kill-ring)

(defun haskell-new ()
  (interactive)
  (let ((name (read-string "Project name: ")))
    (if (string-empty-p name) (message "Project name cannot be empty.")
      (when (y-or-n-p (format "Create project %s in %s?" name default-directory))
        (with-temp-buffer
          (shell-command (format "stack new %s https://raw.githubusercontent.com/VlachJosef/simple-ghci-mode/master/simple-ghci.hsfiles" name) t)
          (let ((default-directory (concat default-directory "/" name)))
            (shell-command "git init" t)
            (shell-command "git add ." t)
            (find-file "src/Lib.hs")
            (end-of-buffer)))))))

(defun upload-template ()
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (insert "# -*- restclient -*-

POST http://localhost:9196/gform/formtemplates
Content-Type: application/json; charset=utf-8
Csrf-Token: nocheck
X-requested-with: foo

")
    (restclient-mode)
    (restclient-http-send-current-stay-in-window)
    (beginning-of-buffer)
    (kill-whole-line 7)
    (js-mode)
    (save-buffer)
    )
  )
