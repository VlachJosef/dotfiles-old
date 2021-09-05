;;; Personal functions

(require 'cl-lib)

;; For loading personal configurations
(defun personal (library)
  (load (concat "~/.emacs.d/personal/" (symbol-name library)) 'no-error))

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
  (let ((all-imports (delete-dups (sort (apply #'append (mapcar 'normalize-import input)) 'string<))))
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
          (projectile-prepend-project-name "rg search for import: ")
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
    (if (and (executable-find "rg") (executable-find "sort") (executable-find "uniq"))
	(let* ((default-directory (projectile-project-root))
	       (res-raw (shell-command-to-string (format "rg import.*%s --no-line-number --no-filename --no-heading | sort | uniq" search-term)))
	       (lines (split-string (s-replace "import " "" (s-trim res-raw)) "\n"))
	       (import (with-temp-buffer
			 (insert (mapconcat (lambda (elm) (s-trim-left elm)) lines "\n"))
			 (sort-lines nil (point-min) (point-max))
			 (ivy-completing-read "Select an import: " (funcall normalization-function (split-string (buffer-string) "\n") search-term) nil nil (concat "." search-term "$")))))
	  (if copy-to-kill-ring
	      (progn (kill-new (format "import %s" import))
		     (message "%s added to kill ring" import))
	    (ensime-insert-import import)))
      (error "Commands 'rg', 'sort' and 'uniq' are required to use this command"))))

(define-key global-map (kbd "s-i" ) 'search-import)

;;(define-key global-map (kbd "C-x 4 s" ) `sbt-switch-to-active-sbt-buffer)
(define-key global-map (kbd "C-x 4 s" ) 'sbt-or-sgm:sbt-or-ghci)

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

(defun insert-or-replace-word (scala-symbol)
  (let ((search-for (string-match ".*rg: \\(.*\\)" (minibuffer-prompt)))
        (result (match-string-no-properties 1)))
    (when (string= "" (minibuffer-contents-no-properties))
      (insert (format "%s" result)))
    (save-excursion
      (move-beginning-of-line 1)
      (let ((defs '(new class object trait val def type)))
        (let ((name (symbol-name scala-symbol))
              (name-at-point (symbol-at-point)))
          (cond ((member name-at-point defs)
                 (delete-region (beginning-of-thing 'symbol) (+ 1 (end-of-thing 'symbol)))
                 (when (not (equal scala-symbol name-at-point))
                   (insert (format "%s " name))))
                (t
                 (insert (format "%s " name)))))))))

(defun delete-word ()
  (save-excursion
    (move-beginning-of-line 1)
    (let ((defs '(new class object trait val def type)))
      (cond ((member (symbol-at-point) defs)
	     (progn (delete-region (beginning-of-thing 'symbol) (+ 1 (end-of-thing 'symbol)))
		    (move-end-of-line 1)))))))

(defhydra scala-minibuffer-search ()
  "
Search for _n_ new _c_ class _t_ trait _o_ object _v_ val _d_ def _y_ type _q_ quit"
    ("n" (insert-or-replace-word 'new) nil)
    ("c" (insert-or-replace-word 'class) nil)
    ("t" (insert-or-replace-word 'trait) nil)
    ("o" (insert-or-replace-word 'object) nil)
    ("d" (insert-or-replace-word 'def) nil)
    ("v" (insert-or-replace-word 'val) nil)
    ("y" (insert-or-replace-word 'type) nil)
    ("q" (delete-word) nil :color blue))

(defun mini-hook ()
  (if (and
       (functionp 'sbt:find-root)
       (or (sbt:find-root)
           (string= default-directory "/Users/pepa/develop-itv/bruce/")) ;; projectile root and sbt root differs for bruce-service differs
       ;;(string-match ".*rg ?(app)*?:.*" (minibuffer-prompt)))
       (string-match ".*rg.*" (minibuffer-prompt)))
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
;;(bind-key "C-c C-s" 'restclient:save-single-buffer-and-make-rest-call)
;;(bind-key "C-c C-s" 'resend-last)

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
  (let ((buffer-string (buffer-string))
        (point (point))
        (exit-code (shell-command-on-region (point-min) (point-max) "xmllint --format -" (buffer-name) t "*xmllint errors*" t)))
    (cond ((zerop exit-code)
           (deactivate-mark)
           (nxml-mode))
          ((eq 1 exit-code)
           (when (s-blank? (buffer-string))
             (insert buffer-string)
             (goto-char point))))))


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

(defconst gform-environments
  '("http://localhost:9196/gform/formtemplates"
    "http://localhost:9396/gform/formtemplates"
    "https://www.qa.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/formtemplates"
    "https://www.staging.tax.service.gov.uk/submissions/test-only/proxy-to-gform/gform/formtemplates"))

(defvar gform-last-url (car gform-environments))

;; TODO Rewrite to macro
(defun resend-last-suppress-response-buffer (prefix-arg)
  (interactive "p")
  (when-let ((buffer (seq-some (lambda (buffer)
                                 (with-current-buffer buffer
                                   (when (and (stringp mode-name)
                                              (or (string= mode-name "JSON")
                                                  (string= mode-name "REST Client")))
                                     buffer)))
                               (buffer-list))))

    (with-current-buffer buffer
      (pcase mode-name
        ("JSON"
         (upload-template prefix-arg))
        ("REST Client"
         (set-buffer-multibyte nil)
         (restclient-http-send-current-suppress-response-buffer)
         (set-buffer-multibyte t)
         (message "Uploading rest-client buffer %s" (buffer-name buffer)))))))

(defun resend-last (prefix-arg)
  (interactive "p")
  (when-let ((buffer (seq-some (lambda (buffer)
                                 (with-current-buffer buffer
                                   (when (and (stringp mode-name)
                                              (or (string= mode-name "JSON")
                                                  (string= mode-name "REST Client")))
                                     buffer)))
                               (buffer-list))))

    (with-current-buffer buffer
      (pcase mode-name
        ("JSON"
         (upload-template prefix-arg))
        ("REST Client"
         (set-buffer-multibyte nil)
         (restclient-http-send-current)
         (set-buffer-multibyte t)
         (message "Uploading rest-client buffer %s" (buffer-name buffer)))))))



(defun upload-template (prefix-arg)
  (interactive "p")

  (when (eq 4 prefix-arg)
    (let ((url (projectile-completing-read  "Choose environment: " gform-environments)))
      (setq gform-last-url url)))

  (let ((json (buffer-string)))
    (with-temp-buffer
      (insert (format "# -*- restclient -*-

POST %s
Content-Type: application/json; charset=utf-8
Csrf-Token: nocheck
X-requested-with: foo

" gform-last-url))
      (insert json)
      (restclient-mode)
      (set-buffer-multibyte nil)
      (restclient-http-send-current-stay-in-window))
    (message "Uploading json-mode buffer %s to %s" (buffer-name (current-buffer)) gform-last-url)))


(defconst gform-environments-new-form
  '("http://localhost/submissions/new-form/"
    "http://localhost:9295/submissions/new-form/"
    "https://www.qa.tax.service.gov.uk/submissions/new-form/"
    "https://www.staging.tax.service.gov.uk/submissions/new-form/"))

(defvar gform-last-new-form-url (car gform-environments-new-form))

(defun open-template-in-browser (prefix-arg)
  (interactive "p")
  (let ((find-template-id-regex "\"_id\"[[:space:]]*:[[:space:]]*\"\\([a-zA-Z0-9-]*\\)\""))
    (when (eq 4 prefix-arg)
      (let ((url (projectile-completing-read  "Choose environment: " gform-environments-new-form)))
        (setq gform-last-new-form-url url)))
    (save-excursion
      (goto-char (point-min))
      (re-search-forward find-template-id-regex)
      (browse-url (concat gform-last-new-form-url (match-string 1))))))

(defun gform-hmrc-start ()
  (interactive)
  (with-current-buffer (dired-noselect "/Users/pepa/develop-hmrc/gform-frontend")
    (sbt-hydra)
    (add-hook 'sbt-hydra:after-create-hook 'gform-hmrc-sbt-run)))

(defun gform-hmrc-sbt-run ()
  (with-current-buffer (dired-noselect "/Users/pepa/develop-hmrc/gform")
    (sbt-hydra)
    (remove-hook 'sbt-hydra:after-create-hook 'gform-hmrc-sbt-run)))

(defun gform-foldright-start ()
  (interactive)
  (with-current-buffer (dired-noselect "/Users/pepa/develop-foldright/gform-frontend")
    (sbt-hydra)
    (add-hook 'sbt-hydra:after-create-hook 'gform-foldright-sbt-run)))

(defun gform-foldright-sbt-run ()
  (gform-run-microservice)
  (with-current-buffer (dired-noselect "/Users/pepa/develop-foldright/gform")
    (sbt-hydra)
    (remove-hook 'sbt-hydra:after-create-hook 'gform-foldright-sbt-run)
    (add-hook 'sbt-hydra:after-create-hook 'gform-foldright-launch)))

(defun gform-foldright-launch ()
  (gform-run-microservice)
  (remove-hook 'sbt-hydra:after-create-hook 'gform-foldright-launch))

(defun gform-run-microservice ()
  (sbt-hydra:run "microservice"))

(defun playframework-project-app-dir ()
  "If current project is play framework project return its app directory, otherwise return nil"
  (when-let ((project-root (projectile-project-root))
             (app-dir (concat project-root "app")))
    (when (and (file-directory-p app-dir)
               (eq 'sbt (projectile-project-type app-dir)))
      app-dir)))

(defun my/symbol-or-selection-at-point ()
  (let ((selection (projectile-symbol-or-selection-at-point)))
    ;;(ivy-toggle-regexp-quote)
    ;;(regexp-quote (replace-regexp-in-string " " "  " selection))
    ;;(regexp-quote selection)
    (unless (string= "Recent" selection) ;; Exception for Recent word from magit status buffer
      selection)))

(defvar look-for-thing-at-point-in-app-only-p t)
(defvar literal-counsel-rg-p t)

(defun look-for-thing-at-point ()
  (interactive)
  ;; Sort result by path
  (let ((app-dir (playframework-project-app-dir)))
    (if (and look-for-thing-at-point-in-app-only-p app-dir)
        (if literal-counsel-rg-p
            (run-literal-rg app-dir)
          (run-vanilla-rg app-dir))
      (if literal-counsel-rg-p
          (run-literal-rg nil)
        (run-vanilla-rg nil)))))

(defun run-literal-rg (app-dir)
  (let ((counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --fixed-strings %s")
        (counsel-ag-command "rg -M 240 --with-filename --no-heading --line-number --fixed-strings %s"))
    (literal-counsel-rg (my/symbol-or-selection-at-point) app-dir (format "literal-rg%s: " (if app-dir " (app)" "")))))

(defun run-vanilla-rg (app-dir)
  (let ((counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number  --color never %s"))
    (counsel-rg (my/symbol-or-selection-at-point) app-dir nil (format "vanilla-rg%s: " (if app-dir " (app)" "")))))

(defun my-shell-quote-argument (argument)
  (format "'%s'" argument))

(defun literal-counsel-rg (&optional initial-input initial-directory ag-prompt)
  "Grep for a string in a root directory using ag.

By default, the root directory is the first directory containing a .git subdirectory.

INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
CALLER is passed to `ivy-read'.

With a `\\[universal-argument]' prefix argument, prompt for INITIAL-DIRECTORY.
With a `\\[universal-argument] \\[universal-argument]' prefix argument, \
prompt additionally for EXTRA-AG-ARGS."
  (interactive)
  (let ((default-directory (or initial-directory (counsel--git-root))))
    (ivy-read ag-prompt
              #'literal-counsel-rg-function
              :initial-input initial-input
              :dynamic-collection t
              :keymap counsel-ag-map
              :history 'counsel-git-grep-history
              :action #'counsel-git-grep-action
              :require-match t
              :caller 'counsel-rg)))

(defun literal-counsel-rg-function (string)
  "Grep in the current directory for STRING."
  (let* ((command-args (counsel--split-command-args string))
         (search-term (cdr command-args)))
    (or
     (let ((ivy-text search-term))
       (ivy-more-chars))
     (let* ((regex search-term)
            (switches (concat (car command-args)
                              (counsel--ag-extra-switches regex)
                              (if (ivy--case-fold-p string)
                                  " -i "
                                " -s "))))

       (counsel--async-command (counsel--format-ag-command switches (format "'%s'" regex)))
       nil))))

(defun test-2 ()
  (let ((proc (start-file-process-shell-command
              "test-name"
              (get-buffer-create "test-name")
              "rg -M 242 --with-filename --no-heading --line-number  --color never  --fixed-strings  Future\\[ValidatedType\\[ValidatorsResult\\]\\]")))
   (set-process-sentinel proc #'counsel--async-sentinel)
   (message "[counsel-ag-function] proc: %s" proc)))

(defun counsel-rg (&optional initial-input initial-directory extra-rg-args rg-prompt)
  "Grep for a string in the current directory using rg.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-RG-ARGS string, if non-nil, is appended to `counsel-rg-base-command'.
RG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.

Example input with inclusion and exclusion file patterns:
    require i -- -g*.el"
  (interactive)
  (let ((counsel-ag-base-command
         (if (listp counsel-rg-base-command)
             (append counsel-rg-base-command (counsel--rg-targets))
           (concat counsel-rg-base-command " "
                   (mapconcat #'shell-quote-argument (counsel--rg-targets) " "))))
        (counsel--grep-tool-look-around
         (let ((rg (car (if (listp counsel-rg-base-command) counsel-rg-base-command
                          (split-string counsel-rg-base-command))))
               (switch "--pcre2"))
           (and (eq 0 (call-process rg nil nil nil switch "--pcre2-version"))
                switch))))
    (counsel-ag initial-input initial-directory extra-rg-args rg-prompt
                :caller 'counsel-rg)))

(cl-defun counsel-ag (&optional initial-input initial-directory extra-ag-args ag-prompt
                      &key caller)
  "Grep for a string in a root directory using ag.

By default, the root directory is the first directory containing a .git subdirectory.

INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search.
EXTRA-AG-ARGS, if non-nil, is appended to `counsel-ag-base-command'.
AG-PROMPT, if non-nil, is passed as `ivy-read' prompt argument.
CALLER is passed to `ivy-read'.

With a `\\[universal-argument]' prefix argument, prompt for INITIAL-DIRECTORY.
With a `\\[universal-argument] \\[universal-argument]' prefix argument, \
prompt additionally for EXTRA-AG-ARGS."
  (interactive)
  (setq counsel-ag-command counsel-ag-base-command)
  (setq counsel--regex-look-around counsel--grep-tool-look-around)
  (counsel-require-program counsel-ag-command)
  (let ((prog-name (car (if (listp counsel-ag-command) counsel-ag-command
                          (split-string counsel-ag-command))))
        (arg (prefix-numeric-value current-prefix-arg)))
    (when (>= arg 4)
      (setq initial-directory
            (or initial-directory
                (counsel-read-directory-name (concat
                                              prog-name
                                              " in directory: ")))))
    (when (>= arg 16)
      (setq extra-ag-args
            (or extra-ag-args
                (read-from-minibuffer (format "%s args: " prog-name)))))
    (setq counsel-ag-command (counsel--format-ag-command (or extra-ag-args "") "%s"))
    (let ((default-directory (or initial-directory
                                 (counsel--git-root)
                                 default-directory)))
      (message "[counsel-rg] counsel--regex-look-around: %s" counsel--regex-look-around)
      (message "[counsel-rg] counsel-ag-command        : %s" counsel-ag-command)
      (message "[counsel-rg] initial-input             : %s" initial-input)
      (ivy-read (or ag-prompt
                    (concat prog-name ": "))
                #'counsel-ag-function
                :initial-input initial-input
                :dynamic-collection t
                :keymap counsel-ag-map
                :history 'counsel-git-grep-history
                :action #'counsel-git-grep-action
                :require-match t
                :caller (or caller 'counsel-ag)))))


(defun whole-project-search (x)
  (when (eq this-command 'ivy-dispatching-done)
    (setq look-for-thing-at-point-in-app-only-p (not look-for-thing-at-point-in-app-only-p))
    (look-for-thing-at-point)))

(defun literal-vanilla-rg-toggle (x)
  (when (eq this-command 'ivy-dispatching-done)
    (setq literal-counsel-rg-p (not literal-counsel-rg-p))
    (look-for-thing-at-point)))

(defun my-action-1 (x)
  (message "action-1: %s" x))

(ivy-set-actions
 'counsel-rg
 '(("m" whole-project-search "search in whole project")
   ("l" literal-vanilla-rg-toggle "literal / vanilla rg")))

(defun ivy-switch-buffer-plain ()
  (interactive)
  (let ((ivy-display-style 'plain)) ;; 'fancy display style is somehow broken in switch-buffer minibuffer completion
    (ivy-switch-buffer)))

(defun show-messages ()
  (interactive)
  (delete-other-windows)
  (let* ((messages (get-buffer-create "*Messages*"))
         (left (selected-window))
         (right (split-window left nil t)))
    (set-window-buffer right messages)
    (with-current-buffer messages
      (set-window-point right (point-max)))))

(defun scala-docs ()
  (interactive)
  (let ((libs-versions
         '(("2.13.5" . "https://www.scala-lang.org/api/2.13.5/index.html")
           ("2.12.13" . "https://www.scala-lang.org/api/2.12.13/index.html")
           ("cats" . "https://typelevel.org/cats/api/cats/index.html")
           ("ce-2" . "https://typelevel.org/cats-effect/api/2.x/")
           ("ce-3" . "https://typelevel.org/cats-effect/api/3.x/")
           ("fs2-3.0.1" . "https://javadoc.io/doc/co.fs2/fs2-core_2.13/3.0.1/fs2/index.html"))))
    (ivy-read "Open docs: " libs-versions
             :caller 'scala-docs
             :action (lambda (x)
                       (eww (cdr x))))))

(defun itv-programme-id-to-api-encoded ()
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (subst-char-in-region beg end ?# ?.)
        (subst-char-in-region beg end ?/ ?_))
    (message "No active region.")))

(defun itv-programme-id-from-api-encoded ()
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (subst-char-in-region beg end ?. ?#)
        (subst-char-in-region beg end ?_ ?/))
    (message "No active region.")))


(defun http-compare-last-two-responses ()
  (interactive)
  (let* ((compare-candidates (seq-filter (lambda (buffer)
                                           (eq 0 (string-match "*HTTP GET" (buffer-name buffer))))
                                         (buffer-list)))
         (buffer-a (car compare-candidates))
         (buffer-b (cadr compare-candidates)))

    (if (and buffer-a buffer-b)
        (ediff-buffers buffer-a buffer-b)
      (message "No availabel buffers to compare. Setting to `restclient-same-buffer-response' to nil")
      (setq restclient-same-buffer-response nil))))
