;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-
(setq debug-on-error nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global settings for built-in emacs parameters
(setq
 initial-scratch-message nil
 make-backup-files nil
 dabbrev-case-fold-search nil
 column-number-mode t
 compilation-skip-threshold 2
 compilation-scroll-output 'first-error
 scroll-error-top-bottom t
 kill-do-not-save-duplicates t
 ensime-startup-snapshot-notification nil
 )

(setq
 ;; allows projectile to see .log files, even though git ignores them
 projectile-git-command "cat <(git ls-files -zco --exclude-standard) <(find . -path '*src_managed*' -and -name '*.scala' -maxdepth 9 -print0) <(find . -path '*resources*' -and -name 'application.json' -maxdepth 4 -print0) <(echo -n restCalls)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for setup functions that are built-in to emacs
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(set-keyboard-coding-system nil)
(scroll-bar-mode -1)
(setq next-screen-context-lines 5)
(setq inhibit-startup-screen t)
(setq scroll-preserve-screen-position 1)
(setq message-log-max 10000)
(toggle-frame-fullscreen)
;; Do not display Ediff control window in separate frame, since it cause problem in Fullscreen on Mac OS
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; delete trailing whitespace before file is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;(add-hook 'before-save-hook 'clear-sbt-compilation-buffer)
;;(add-hook 'after-save-hook 'run-last-sbt-command)

;; (add-hook 'after-save-hook (lambda ()
;; 			     ;;(message "FROM AFTER SAVE")
;; 			     (run-last-sbt-command)))

;;(add-hook 'sbt-mode-hook' (lambda () (add-hook 'before-save-hook 'clear-sbt-compilation-buffer)))
(add-hook 'sbt-mode-hook (lambda ()
			    (add-hook 'before-save-hook 'sbt-hydra:check-modified-buffers)))

;;(add-hook 'after-save-hook 'sbt-run-previous-command)


;;(add-hook 'after-save-hook 'sbt-run-previous-command)
;;(add-hook 'after-save-hook 'aaa-bbb)


;; (defun clear-sbt-compilation-buffer ()
;;   (let ((current-sbt-root (sbt:find-root)))
;;     (loop for process being the elements of (process-list)
;; 	  for current-process-buffer = (process-buffer process)
;; 	  if (and
;; 	      (bufferp current-process-buffer) ;; process must have associated buffer
;; 	      (with-current-buffer current-process-buffer
;; 		(and
;; 		 (sbt:mode-p)
;; 		 (process-live-p process)
;; 		 (string= (sbt:find-root) current-sbt-root))))
;; 	  do (progn
;; 	       (sbt:clear current-process-buffer)))))

;;    (let ((pos (point-min)))
;;      (while (setq pos (next-single-property-change pos 'compilation-message))
;;	(when (setq msg (get-text-property pos 'compilation-message))
;;	  (let ((loc (compilation--message->loc msg)))
;;	    ;;(setf (compilation--loc->col loc) nil)
;;	    (setf (compilation--loc->line loc) nil)
;;	    (setf (compilation--loc->file-struct loc) nil)
;;	    (setf (compilation--loc->marker loc) nil)
;;	    ;;(setf (compilation--loc->visited loc) nil)



;;(load-file "~/.emacs.d/compile-.el")
;;(load-file "~/.emacs.d/compile+.el")
;;(require 'compile+)
(require 'package)

(add-to-list 'package-archives
         '("melpa" . "http://melpa.milkbox.net/packages/") t)
;;;;(add-to-list 'package-archives
;;;;         '("SC" . "http://joseito.republika.pl/sunrise-commander/") t)
(package-initialize)

;; this needs to be after package-initialize to overwrite default melpa packages
(add-to-list 'load-path "~/develop-ensime/emacs-sbt-mode/")
(add-to-list 'load-path "~/develop-emacs/restclient.el/")


;;-(require 'smex)
;;-(smex-initialize)

(require 'use-package)

(add-hook 'org-mode-hook
          (lambda ()
	    (smartparens-mode)
            (subword-mode)))

(use-package recentf
  :config
  (setq recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package session
  :init
  (setq session-jump-undo-threshold 80) ; change positions must differ by 80 characters
  (global-set-key [(control ?.)] 'session-jump-to-last-change))

;;(add-hook 'after-init-hook 'session-initialize)
;;(desktop-save-mode)

(use-package saveplace
  :init
  (setq-default save-place t)
  (setq save-place-file "~/.emacs.d/saved-places"))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1))

(use-package hydra
   :ensure t)

(use-package move-dup
  :config
  (global-move-dup-mode 1))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "jj" 'avy-goto-char-timer)
  (key-chord-define-global "jk" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line))

(use-package ag
  :commands ag
  :init
  (setq ag-highlight-search t
        ag-group-matches nil)
  :config
  (add-hook 'ag-search-finished-hook (lambda () (pop-to-buffer next-error-last-buffer))))

(use-package projectile
  :demand
  ;; nice to have it on the modeline
  :init (setq
         projectile-use-git-grep t
         projectile-completion-system 'ido
	 ;;projectile-completion-system 'ivy
         )
  :ensure    projectile
  :config
    (projectile-mode)
  :bind (("s-f" . projectile-find-file)
	 ("s-F" . projectile-ag)))

(defun find-class ()
  (interactive)
  (call-interactively 'projectile-ag)
  (message ":::: %s" (buffer-name))
  (my/smarter-move-beginning-of-line))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind (("s-h" . highlight-symbol)
	 ("s-n" . highlight-symbol-next)
	 ("s-p" . highlight-symbol-prev)
	 ("s-r" . highlight-symbol-remove-all)))

(use-package zygospore
  :bind ("C-x 1" . zygospore-toggle-delete-other-windows))

(use-package general-close
  :bind ("C-;" . general-close))

;;-(use-package git-gutter
;;-  :diminish git-gutter-mode
;;-  :commands git-gutter-mode
;;-  :bind ("C-c C-p" . git-gutter:previous-hunk)
;;-  :bind ("C-c C-n" . git-gutter:next-hunk)
;;-  :bind ("C-c C-k" . git-gutter:popup-hunk)
;;-  :bind ("C-c C-j" . git-gutter:revert-hunk))

(use-package magit
  :commands magit-status magit-blame
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(add-hook 'js-mode-hook
          (lambda ()
	    (smartparens-mode)
	    (show-paren-mode)
            (subword-mode)
	    (glasses-mode)))

(add-hook 'markdown-mode-hook
          (lambda ()
	    (smartparens-mode)
            (subword-mode)))

;;(use-package anzu
;;  :ensure t
;;  :bind (("M-%" . anzu-query-replace)
;;         ("C-M-%" . anzu-query-replace-regexp))
;;  :config
;;  (global-anzu-mode))

;;set javaOptions in jobs := Seq("-Dpure.conf.file=myconf.conf", "-Dconfig.file=myconf.conf", "-Dlogger.file=logback-debug.xml", "-Dpure.env=development")
;;set javaOptions in (jobs, run) := Seq("-Dpure.conf.file=myconf.conf", "-Dconfig.file=myconf.conf", "-Dlogger.file=logback-debug.xml", "-Dpure.env=development")

(use-package furl)

(use-package sbt-mode
  :init
  (setq sbt:scroll-to-bottom-on-output nil))

(use-package sbt-mode-hydra
  :bind (("C-c v" . sbt-hydra:hydra)))

(use-package company
  ;;:diminish company-mode
  :commands company-mode
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-minimum-prefix-length 4)
  :config
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil))

(use-package yasnippet
  ;;:diminish yas-minor-mode
  :commands yas-minor-mode
  :config
  (yas-reload-all)
  (define-key yas-minor-mode-map [tab] #'yas-expand))

(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(which-key-mode nil)
;; Standard key for next and previous are C-s and C-r no need to replace with C-n and C-p
;;(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; inspiration https://github.com/rmm5t/dotfiles/blob/df77009c326a9d09f23e7bedcd44e9658f26bc6f/emacs.d/init.el
(load "~/.emacs.d/personal/defuns")
(load "~/.emacs.d/personal/pure360")
(load "~/.emacs.d/personal/private")

(personal 'kbd-macros)

;;(add-hook 'sbt-mode-hook' (lambda () (run-last-sbt-command)))

;; Javascript indentation 2
(setq js-indent-level 2)

;; Must read: Setting the PATH variable
;; http://emacsredux.com/blog/2015/05/09/emacs-on-os-x/
;;;;(when (memq window-system '(mac ns))
;;;;  (exec-path-from-shell-initialize))

;;(setq exec-path (append exec-path '("/Users/pepa/bin/")))

(when (not package-archive-contents)
  (package-refresh-contents))

;;(setenv "PATH" (concat "/Users/pepa/bin/:" (getenv "PATH")))
;;(setenv "PATH" (shell-command-to-string "source ~/.bashrc; echo -n $PATH"))

(load-theme 'zenburn :no-confirm)

(add-to-list 'load-path (concat user-emacs-directory "apib-mode"))

(autoload 'apib-mode "apib-mode"
        "Major mode for editing API Blueprint files" t)
(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))

(use-package tramp) ;; needed by crux

(use-package crux
  :ensure t
  :bind (
	 ("C-c f" . crux-recentf-ido-find-file)
         ("M-k" . crux-smart-open-line)
         ("s-k" . crux-kill-whole-line)
         ("M-j" . crux-smart-open-line-above)))

(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(use-package scala-mode
  ;;//:interpreter
  ;;("scala" . scala-mode)
  :init
  (setq
   scala-indent:use-javadoc-style t
   scala-indent:align-parameters t)
  :config
  (bind-key "RET" `scala-mode-newline-comments scala-mode-map)
  (bind-key "C-c c" `sbt-command scala-mode-map)
  (bind-key "M-SPC" `scala-indent:fixup-whitespace)
  (bind-key "C-M-j" `scala-indent:join-line)
  (bind-key "C-c e" `next-error))

;(use-package protobug-mode)
(add-hook 'protobuf-mode-hook
	  (lambda()
	    (message "Running protobuf-mode-hook")
	    (smartparens-mode) ;; smartparens must be below electric-*-modes
	    (show-paren-mode)
	    (subword-mode)
	    (glasses-mode)
	    ;;(git-gutter-mode)
	    ))

(use-package inf-mongo
  :init
  (setq
   inf-mongo-command "/Users/pepa/develop-sensible/mongodb-osx-x86_64-3.2.7/bin/mongo 127.0.0.1:27007"))

(use-package glasses
  :init
  (setq
   glasses-separator ""
   glasses-face '("bold" "italic")
   glasses-original-separator ""
   glasses-separate-parentheses-p nil))

(use-package smartparens
  :diminish smartparens-mode
  :commands
  ;;smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "C-s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")
  ;;(sp-local-pair '(c-mode java-mode scala-mode) "(" nil :post-handlers '(("||\n[i]" "RET")))
  ;;(sp-local-pair '(c-mode java-mode) "{" nil :post-handlers '(("||\n[i]" "RET")))
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)
  (bind-key "M-<backspace>" nil smartparens-mode-map)
  (bind-key "s-{" `sp-rewrap-sexp smartparens-mode-map)
  (bind-key "s-[" `sp-backward-unwrap-sexp smartparens-mode-map)
  (bind-key "s-]" `sp-unwrap-sexp smartparens-mode-map))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
	    (setq indent-tabs-mode nil)
            (rainbow-delimiters-mode)
	    (smartparens-mode) ;; smartparens must be below electric-*-modes
	    (show-paren-mode)
	    (prettify-symbols-mode)
	    (eldoc-mode)))

;;-;; assuming you put the repository in ~/.emacs.d/ensime
;;-(add-to-list 'load-path (concat user-emacs-directory "ensime-emacs"))

(defun ensime-edit-definition-with-fallback ()
  "Variant of `ensime-edit-definition' with ctags if ENSIME is not available."
  (interactive)
;;  (message "ENSIME LOOKUP RUNNING %S" (ensime-connection-or-nil))
  (if (ensime-connection-or-nil)
      (let ((lookup-definition-result (ensime-edit-definition)))
	    (if (stringp lookup-definition-result) (projectile-find-tag)))
    (projectile-find-tag)))

(use-package ensime
  :commands ensime ensime-mode
  :demand
  :init
  (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  (setq
   ensime-default-buffer-prefix "ENSIME-" ;; default value was inferior-ensime-server
   ;;ensime-prefer-noninteractive t
   ensime-refactor-enable-beta t
   ensime-refactor-preview t
   ensime-refactor-preview-override-hunk 10
   sbt:default-command "projects"
   ;;sbt:prompt-regexp "^\\[.*\\]>[ ]*"
   )
  :config

  ;;(bind-key "s-n" 'ensime-search ensime-mode-map)
  (bind-key "s-t" 'ensime-print-type-at-point ensime-mode-map)
  (bind-key "M-." 'ensime-edit-definition-with-fallback ensime-mode-map))

(add-hook 'scala-mode-hook
	  (lambda()
	    (message "Running scala-mode-hook")
	    (ensime-mode)
	    (setq comment-start "/* "
		  comment-end " */"
		  comment-style 'multi-line
		  comment-empty-lines t)
	    ;;(electric-pair-mode)
	    (electric-indent-mode)
	    (yas-minor-mode t)
	    (company-mode t)
	    (smartparens-mode) ;; smartparens must be below electric-*-modes
	    (show-paren-mode)
	    (subword-mode)
	    (glasses-mode)
	    (define-key yas-minor-mode-map [tab] #'yas-expand)
	    ;;(git-gutter-mode)
	    ))

(add-hook 'apib-mode-hook
	  (lambda()
	    (message "Running apib-mode-hook")
	    (setq indent-tabs-mode nil)
	    (electric-indent-mode)
	    (smartparens-mode) ;; smartparens must be below electric-*-modes
	    (show-paren-mode)
	    (subword-mode)
	    (glasses-mode)
	    ))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Menlo")))))

;;-(global-linum-mode 1)
(beacon-mode 1)
(setq beacon-color 0.4)

(setq ring-bell-function 'ignore)

;; set up ace-jump-mode
;;-(add-to-list 'load-path "~/.emacs.d/elpa/ace-jump-mode-20140616.115/")
;;-(require 'ace-jump-mode)
;;(define-key global-map (kbd "C-c C-SPC" ) 'ace-jump-mode)

(require 'ace-jump-zap)
(define-key global-map (kbd "C-z" ) `ace-jump-zap-up-to-char)

;; set up ido mode
(require `ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

(defun bind-ido-keys ()
  "Keybindings for ido mode."
  (define-key ido-completion-map "\C-p" 'ido-prev-match)
  (define-key ido-completion-map "\C-n" 'ido-next-match))

(add-hook 'ido-setup-hook #'bind-ido-keys)

;;(global-unset-key (kbd "C-z"))

(global-set-key (kbd "s-s") `replace-string)
;;(global-set-key (kbd "s-s") `vr/replace)
(global-set-key (kbd "M-x") `smex)
(global-set-key (kbd "M-X") `smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") `execute-extended-command)
(global-set-key (kbd "M-o") `other-window)
(global-set-key (kbd "s-W") `toggle-truncate-lines)

;;;; taken from https://github.com/bbatsov/prelude/blob/05dc795f2befb192f6ab16ef66fbb632ca2e3189/core/prelude-core.el#L138
(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'my/smarter-move-beginning-of-line)

;;;;

;;; Filters ido-matches setting acronynm matches in front of the results
(defadvice ido-set-matches-1 (after ido-smex-acronym-matches activate)
  (if (and (fboundp 'smex-already-running) (smex-already-running)
           (> (length ido-text) 1))
      (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
            (acronym-matches (list))
            (remove-regexes '("-menu-")))
        ;; Creating the list of the results to be set as first
        (dolist (item items)
          (if (string-match ido-text item) ;; exact match
              (add-to-list 'acronym-matches item)
            (if (string-match (concat regex "[^-]*$") item) ;; strict match
                (add-to-list 'acronym-matches item)
              (if (string-match regex item) ;; appending relaxed match
                  (add-to-list 'acronym-matches item t)))))

        ;; Filtering ad-return-value
        (dolist (to_remove remove-regexes)
          (setq ad-return-value
                (delete-if (lambda (item)
                             (string-match to_remove item))
                           ad-return-value)))

        ;; Creating resulting list
        (setq ad-return-value
              (append acronym-matches
                      ad-return-value))

        (delete-dups ad-return-value)
        (reverse ad-return-value))))

(set-face-attribute 'region nil :background "DeepPink4")

;;-(custom-set-variables
;;- ;; custom-set-variables was added by Custom.
;;- ;; If you edit it by hand, you could mess it up, so be careful.
;;- ;; Your init file should contain only one such instance.
;;- ;; If there is more than one, they won't work right.
;;- '(ansi-color-faces-vector
;;-   [default default default italic underline success warning error])
;;- '(ansi-color-names-vector
;;-   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
;;- '(beacon-color 0.4)
;;- '(cursor-type (quote box))
;;- '(custom-safe-themes
;;-   (quote
;;-    ("f024aea709fb96583cf4ced924139ac60ddca48d25c23a9d1cd657a2cf1e4728" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
;;- '(delete-selection-mode t)
;;- '(fci-rule-color "#d6d6d6")
;;- '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
;;- '(package-selected-packages
;;-   (quote
;;-    (ensime zygospore zenburn-theme yasnippet which-key visual-regexp use-package sunrise-commander smex smartparens session sbt-mode rainbow-delimiters protobuf-mode projectile popup org-present nyan-mode nlinum move-dup magit lua-mode key-chord json-mode ido-vertical-mode highlight-symbol highlight-indentation haskell-mode groovy-mode golden-ratio git-gutter git-gutter+ general-close expand-region exec-path-from-shell discover-my-major crux counsel company color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized beacon avy anzu ag ace-jump-zap)))
;;- '(sbt:default-command "~compile")
;;- '(vc-annotate-background nil)
;;- '(vc-annotate-color-map
;;-   (quote
;;-    ((20 . "#c82829")
;;-     (40 . "#f5871f")
;;-     (60 . "#eab700")
;;-     (80 . "#718c00")
;;-     (100 . "#3e999f")
;;-     (120 . "#4271ae")
;;-     (140 . "#8959a8")
;;-     (160 . "#c82829")
;;-     (180 . "#f5871f")
;;-     (200 . "#eab700")
;;-     (220 . "#718c00")
;;-     (240 . "#3e999f")
;;-     (260 . "#4271ae")
;;-     (280 . "#8959a8")
;;-     (300 . "#c82829")
;;-     (320 . "#f5871f")
;;-     (340 . "#eab700")
;;-     (360 . "#718c00"))))
;;- '(vc-annotate-very-old-color nil))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ensime-sbt-perform-on-save "compile")
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256"))))
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(fset 'Search\ for\ text\ from\ cursor\ to\ end\ of\ line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([11 67108911 8388678 1 25 12 11 return] 0 "%d")) arg)))


(fset 'search-for-class-json-format
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217830 134217830 6 134217751 8388678 1 99 108 97 115 115 32 return] 0 "%d")) arg)))
