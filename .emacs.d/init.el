;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

(set-register ?i "g")
(set-register ?o "d")
(set-register ?p "++3.0.0-RC2 update")
(set-register ?f "\"format\": \"shortText\",")
(set-register ?j "Json.prettyPrint(Json.toJson())")

;; (describe-personal-keybindings)
;; You can always see what's going on under the hood of use-package (or any other elisp macro) by calling M-x pp-macroexpand-last-sexp after the form
(setq debug-on-error nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global settings for built-in emacs parameters
(setq-default indent-tabs-mode nil)
(setq lsp-java-jdt-download-url  "https://download.eclipse.org/jdtls/milestones/0.57.0/jdt-language-server-0.57.0-202006172108.tar.gz")
(setq
 initial-scratch-message nil
 make-backup-files nil
 dabbrev-case-fold-search nil
 column-number-mode t
 compilation-skip-threshold 2
 ;;compilation-scroll-output 'first-error
 ;;next-error-recenter '(4)
 next-error-recenter '(4)
 compilation-scroll-output nil
 scroll-error-top-bottom t
 kill-do-not-save-duplicates t
 dired-dwim-target t
 dired-listing-switches "-alo"
 reb-auto-match-limit 2000
 switch-to-buffer-preserve-window-point t
 frame-resize-pixelwise t
 split-window-keep-point nil
 initial-major-mode 'emacs-lisp-mode)

(global-hl-line-mode -1)

(setq gnus-select-method '(nntp "news.gmane.org"))

(setq
 ;; allows projectile to see .log files, even though git ignores them
 projectile-git-command "cat <(git ls-files -zco --exclude-standard) <(find . -path '*src_managed*' -and -name '*.scala' -maxdepth 9 -print0) <(find . -path '*resources*' -and -name 'application.json' -maxdepth 4 -print0) <(echo -n restCalls)")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for setup functions that are built-in to emacs
(defalias 'yes-or-no-p 'y-or-n-p)
(save-place-mode 1)
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
;;     ;;(message "FROM AFTER SAVE")
;;     (run-last-sbt-command)))

(add-hook 'sbt-mode-hook' (lambda () (add-hook 'before-save-hook 'clear-sbt-compilation-buffer)))

(add-hook 'sbt-mode-hook (lambda ()
                           (define-key comint-mode-map [remap comint-write-output] 'resend-last)
                           (define-key comint-mode-map "\C-c\C-b" 'resend-last-suppress-response-buffer)
                           (add-hook 'before-save-hook 'sbt-hydra:check-modified-buffers)
                           ))


;; (add-hook 'simple-ghci-mode-hook (lambda ()
;;                                    (smartparens-mode)
;;                                    (subword-mode)
;;                                    (rainbow-delimiters-mode)))

;;(add-hook 'after-save-hook 'sbt-run-previous-command)


;;(add-hook 'after-save-hook 'sbt-run-previous-command)
;;(add-hook 'after-save-hook 'aaa-bbb)

(defun clear-sbt-compilation-buffer ()
  (let ((current-sbt-root (sbt:find-root)))
    (loop for process being the elements of (process-list)
          for current-process-buffer = (process-buffer process)
          if (and
              (buffer-live-p current-process-buffer)
              (bufferp current-process-buffer) ;; process must have associated buffer
              (with-current-buffer current-process-buffer
                (and
                 (sbt:mode-p)
                 (string= (sbt:find-root) current-sbt-root))))
          do (progn
               (sbt:clear current-process-buffer)))))

;;(load-file "~/.emacs.d/compile-.el")
;;(load-file "~/.emacs.d/compile+.el")
;;(require 'compile+)
(require 'package)

;;;;(add-to-list 'package-archives
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;;;;(add-to-list 'package-archives
;;;;         '("SC" . "http://joseito.republika.pl/sunrise-commander/") t)
(package-initialize)

;; this needs to be after package-initialize to overwrite default melpa packages
(add-to-list 'load-path "~/develop-emacs/emacswiki")
;;(add-to-list 'load-path "~/develop-emacs/hs-lint")
;;(add-to-list 'load-path "~/develop-purescript/purescript-mode/")
(add-to-list 'load-path "~/develop-ensime/emacs-sbt-mode/")
(add-to-list 'load-path "~/develop-emacs/restclient.el/")
(add-to-list 'load-path "~/develop-emacs/println-debugger/")
(add-to-list 'load-path "~/develop-emacs/spark-runner/")
(add-to-list 'load-path "~/develop-emacs/scala-utils/")
(add-to-list 'load-path "~/develop-emacs/ag-haskell-hydra/")
(add-to-list 'load-path "~/develop-emacs/simple-ghci-mode/")
;;(add-to-list 'load-path "~/develop-nix/nix-mode/")
;;(add-to-list 'load-path "~/.emacs.d/json-mode/") ;; https://github.com/UwUnyaa/json-mode
(add-to-list 'load-path "~/.emacs.d/emacs-sdcv/") ;; https://github.com/gucong/emacs-sdcv
;;(add-to-list 'load-path "~/.emacs.d/gited/")
;;(add-to-list 'load-path "~/.emacs.d/ghcid/")
(add-to-list 'load-path "~/develop-godot/emacs-gdscript-mode/")
(add-to-list 'load-path "~/develop-emacs/sbt-rpc-client.el/")
(add-to-list 'load-path "~/develop-emacs/sbt-test-runner.el/")
(add-to-list 'load-path "~/develop-emacs/semanticdb-mode.el/")
(add-to-list 'load-path "~/develop-emacs/javap-mode/")
;(add-to-list 'load-path "~/develop-godot/emacs-gdscript-mode-debugger/")
(add-to-list 'load-path "~/.emacs.d/ensime-emacs/")

;;(require 'hs-lint)
;;(require 'ag-haskell-hydra)
;;(require 'simple-ghci-mode)
;;(require 'jsonb-mode)
(require 'sdcv-mode)
(require 'ensime-editor)
(require 'sbt-rpc-client-connect)
(require 'sbt-test-munit)
(require 'scala-semanticdb-mode)
(require 'javap-mode)
;;(require 'gited)
;;(define-key dired-mode-map "\C-x\C-g" 'gited-list-branches)


(use-package so-long
  :config (global-so-long-mode 1))

;;-(require 'smex)
;;-(smex-initialize)
;;(require 'ghcid)

(require 'gdscript-mode)

;;(require 'gdscript-debugger)

(require 'use-package)

(add-hook 'org-mode-hook
          (lambda ()
            (smartparens-mode)
            (subword-mode)))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(use-package isearch
  :config
  (setq
   isearch-lazy-count t
   lazy-highlight-cleanup nil
   isearch-yank-on-move 'shift))

(use-package erc
  :ensure t
  ;;:commands (erc)
  ;;:bind (("C-x e" . erc))
  :config
  (let
      ((nick "pipahask"))
    (load "~/.ercpass")
    (require 'erc-services)
    (erc-services-mode 1)

    (require 'erc-join)
    (erc-autojoin-mode t)

    (setq erc-server "irc.freenode.net"
          erc-port 6667
          erc-nick nick
          erc-prompt-for-nickserv-password nil
          erc-nickserv-passwords `((freenode ((,nick . ,freenode-pipahask-pass)))))

    (setq erc-autojoin-channels-alist
          '(("freenode.net" "#godotengine" ;; "#haskell" "#haskell-ide-engine"
             )))

    (require 'erc-match)
    (setq erc-keywords `(,nick))
    (erc-match-mode t)

    (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT"))
    (erc-timestamp-mode t)
    (erc-track-mode t)))

(use-package scala-utils)
(global-set-key (kbd "C-s-:") 'scala-utils:wrap-in-braces)

(defface face-ghci-link
  '((t :foreground "yellow green")) "highligh links in simple-ghci-mode mode") ;; run list-colors-display to view predefined colors

(use-package goto-addr
  :config
  (setq goto-address-url-face 'face-ghci-link))

(use-package prettier-js
  :config
  (add-hook 'js-mode-hook
            (lambda ()
              (prettier-js-mode))))

;; (use-package simple-ghci-mode
;;   :config
;;   (add-hook 'simple-ghci-mode-hook
;;             (lambda ()
;;               ;;(smartparens-mode) ;; conflict with C-M-n compilation-next-error and C-M-p compilation-previous-error
;;               (face-remap-add-relative 'rainbow-delimiters-unmatched-face '(:foreground "white"))
;;               (rainbow-delimiters-mode)
;;               (electric-pair-local-mode)
;;               (subword-mode)
;;               (goto-address-mode)))
;;   (bind-key "C-x 4 s" `sgm:switch-to-ghci-buffer simple-ghci-mode-map) ;; This doesn't override entry in global-map done in defuns.el
;;   (bind-key "s-F" `ahh:projectile-ag-regexp))

(use-package sbt-or-ghci)

(use-package color-identifiers-mode
  :ensure t
  :config
  (setq color-identifiers-coloring-method 'hash))

(add-hook 'after-init-hook 'global-color-identifiers-mode)

(use-package engine-mode
  :ensure t
  :config
  (engine-mode t)

  ;; C-x / d
  (defengine github
    "https://github.com/search?q=org:hmrc+%s"
    :keybinding "m")

  (defengine github-internal
    "https://github.tools.tax.service.gov.uk/q=org:hmrc+%s"
    :keybinding "n")

  (defengine hoogle
    "http://localhost:8080/?hoogle=%s"
    :keybinding "h")

  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s"
    :keybinding "s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"))

(use-package iedit
  :ensure t)

(use-package psci
  :ensure t)

(use-package psc-ide
  :ensure t
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (smartparens-mode)
              (subword-mode)
              (turn-on-purescript-indentation)))
  (bind-key "M-n" `flycheck-next-error purescript-mode-map)
  (bind-key "M-p" `flycheck-previous-error purescript-mode-map))

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

(require 'bookmark+)

;; (use-package ido-vertical-mode
;;   :config
;;   (ido-vertical-mode 1))

(use-package hydra
   :ensure t)

;; (use-package move-dup
;;   :config
;;   (global-move-dup-mode 1))

(use-package key-chord
  :config
  (key-chord-mode 1)
  (key-chord-define-global "jj" 'avy-goto-char-timer)
  (key-chord-define-global "jk" 'avy-goto-word-1)
  (key-chord-define-global "jl" 'avy-goto-line))

(use-package shell
  :bind (:map shell-mode-map
              ("C-c C-v" . comint-clear-buffer)))

(use-package rg
  ;;:config
  )

(use-package ag
  :commands ag
  :init
  (setq ag-highlight-search t
        ag-group-matches nil)
  :config
  (add-hook 'ag-search-finished-hook #'ahh:go-to-source)
  :bind (:map ag-mode-map
              ("t" . no-test-line)))

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  :config
  (setq ivy-display-style 'fancy)
  ;;(setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "") ;; get rid of ^ in M-x prompt
  :bind
  (:map ivy-mode-map
        ("C-x b" . ivy-switch-buffer-plain))
  (:map ivy-minibuffer-map
        ("M-o" . other-window)
        ("s-F" . scala-minibuffer-search/body)
        ("M-k" . ivy-dispatching-done)))

(add-hook 'graphql-mode-hook
          (lambda ()
            (rainbow-delimiters-mode)
            (smartparens-mode)
            (show-paren-mode)
            (subword-mode)
            (glasses-mode)))

(use-package counsel
  :config
  (setcdr (assoc 'counsel-M-x ivy-initial-inputs-alist) "")) ;; get rid of ^ in M-x prompt)

(use-package ivy-hydra
  :ensure t
  :after (hydra))

(use-package projectile
  :diminish projectile-mode
  :demand
  ;; nice to have it on the modeline
  :init (setq
         projectile-use-git-grep t
         ;;projectile-completion-system 'ido
         projectile-completion-system 'ivy
         projectile-switch-project-action 'projectile-commander
         ;;projectile-mode-line '(:eval (format " P[%s]" (projectile-project-name)))
         )
  :ensure projectile
  :config
  (projectile-mode)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :bind (("s-f" . projectile-find-file)
         ("s-F" . look-for-thing-at-point)))

(use-package winner
  :config (winner-mode 1)
  :bind (("C-s-p" . winner-undo)   ;; Control + Shift + Cmd + p
         ("C-s-n" . winner-redo))) ;; Control + Shift + Cmd + n

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

;; (use-package general-close
;;   :bind ("C-;" . general-close))

;;-(use-package git-gutter
;;-  :diminish git-gutter-mode
;;-  :commands git-gutter-mode
;;-  :bind ("C-c C-p" . git-gutter:previous-hunk)
;;-  :bind ("C-c C-n" . git-gutter:next-hunk)
;;-  :bind ("C-c C-k" . git-gutter:popup-hunk)
;;-  :bind ("C-c C-j" . git-gutter:revert-hunk))

(use-package magit
  :init
  (setq magit-visit-ref-behavior '(checkout-any focus-on-ref))
  :commands magit-status magit-blame
  :bind (("s-g" . magit-status)
         ("s-m" . magit-status)
         ("s-b" . magit-blame)))

;; (use-package magit-gh-pulls
;;   :init
;;   (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls))

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
 (setq sbt:sbt-prompt-regexp "^\\(\\[[^\]]*\\] \\)?[>$][ ]*"
       ;;sbt:program-options '("-Djline.terminal=auto" "-Dsbt.supershell=false")
       sbt-hydra:allowed-files-regexp '(".*.scala$" ".*/routes$" ".*.scala.html$"))
 ;;:bind (("C-c C-s" . restclient:save-single-buffer-and-make-rest-call))
 )
;;
(use-package sbt-mode-hydra
  :bind (("C-c v" . sbt-hydra)))

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
  ;':commands yas-minor-mode
  :config
  (yas-reload-all) ;; Load snippets when launching Emacs
  ;; (define-key yas-minor-mode-map [tab] #'yas-expand)
  )

;;(require 'ido-vertical-mode)
;;(ido-mode 1)
;;(ido-vertical-mode 1)
(which-key-mode nil)
;; Standard key for next and previous are C-s and C-r no need to replace with C-n and C-p
;;(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; inspiration https://github.com/rmm5t/dotfiles/blob/df77009c326a9d09f23e7bedcd44e9658f26bc6f/emacs.d/init.el
(load "~/.emacs.d/personal/defuns")
;;(load "~/.emacs.d/personal/pure360")
(load "~/.emacs.d/personal/private")
;;(load "~/.emacs.d/lambdacalc.el")

(personal 'kbd-macros)

;;(add-hook 'sbt-mode-hook' (lambda () (run-last-sbt-command)))

;; Javascript indentation 2
(setq js-indent-level 2)

;; Must read: Setting the PATH variable
;; http://emacsredux.com/blog/2015/05/09/emacs-on-os-x/
;;;;(when (memq window-system '(mac ns))
;;;;  (exec-path-from-shell-initialize))

;;(setq exec-path (append exec-path '("/Users/pepa/bin/")))
(setq exec-path (append exec-path '("/Users/pepa/.nvm/versions/node/v10.15.1/bin/")))

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
         ("C-c f" . crux-recentf-find-file)
         ("M-k" . crux-smart-open-line)
         ("s-k" . crux-kill-whole-line)
         ("M-j" . crux-smart-open-line-above)
         ("C-c d" . crux-duplicate-current-line-or-region)))

(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(define-key global-map (kbd "C-c e" ) 'next-error)

(use-package emacs
  :init
  (setq backward-delete-char-untabify-method nil)
  :bind ("C-M-j" . join-line))

(use-package scala-mode
  :init
  (setq scala-indent:use-javadoc-style t
        scala-indent:align-parameters t)
  ;; Any binding occurring before the first use of :map are applied to the global keymap
  :bind (:map
         scala-mode-map
         ([remap delete-backward-char] . backward-delete-char-untabify )
         ("RET" . scala-mode-newline-comments)
         ("M-SPC" . scala-indent:fixup-whitespace)
         ("C-M-j" . scala-indent:join-line)))

(add-hook 'scala-mode-hook
          (lambda ()
            ;;(message "Running scala-mode-hook")
            (setq electric-indent-inhibit t ;; don't indent previous line when pressing enter
                  comment-start "/* "
                  comment-end " */"
                  comment-style 'multi-line
                  comment-empty-lines t)
            ;;(electric-pair-mode)
            (rainbow-delimiters-mode)
            (company-mode t)
            (smartparens-mode) ;; smartparens must be below electric-*-modes
            (show-paren-mode)
            (subword-mode)
            (glasses-mode)
            (auto-revert-mode)
            ;;(define-key yas-minor-mode-map [tab] #'yas-expand)
            ;;(local-set-key (kbd "-") 'left-arrow)
            ))

;(use-package protobug-mode)
(add-hook 'protobuf-mode-hook
          (lambda()
            (message "Running protobuf-mode-hook")
            (smartparens-mode) ;; smartparens must be below electric-*-modes
            (show-paren-mode)
            (subword-mode)
            (glasses-mode)))

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
  (sp-pair "[" "]" :wrap "C-s-{") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "C-{")
  ;;(sp-local-pair '(c-mode java-mode scala-mode) "(" nil :post-handlers '(("||\n[i]" "RET")))
  ;;(sp-local-pair '(c-mode java-mode) "{" nil :post-handlers '(("||\n[i]" "RET")))
  ;;(bind-key "C-M-n" nil smartparens-mode-map)
  ;;(bind-key "C-M-p" nil smartparens-mode-map)
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)
  (bind-key "M-<backspace>" nil smartparens-mode-map)
  (bind-key "s-{" `sp-rewrap-sexp smartparens-mode-map)
  (bind-key "s-[" `sp-backward-unwrap-sexp smartparens-mode-map)
  (bind-key "s-]" `sp-unwrap-sexp smartparens-mode-map))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (rainbow-delimiters-mode)
            (smartparens-mode) ;; smartparens must be below electric-*-modes
            (show-paren-mode)
            (prettify-symbols-mode)
            (eldoc-mode)))

(use-package spark-runner
  :config
  (setq spark-submit-executable "/Users/pepa/spark/spark-3.0.1-bin-hadoop3.2/bin/spark-submit")
  :after (scala-mode sbt-mode)
  :bind (:map scala-mode-map
         ("C-c r" . spark-package)
         :map sbt:mode-map
         ("C-c r" . spark-package)))

(use-package println-debugger
  :bind (:map
         global-map ("C-x C-k P" . print-ln) ;; fallback while when package is unstable
         :map
         emacs-lisp-mode-map
         ([remap sp-previous-sexp] . println-insert-before)
         ([remap sp-next-sexp] . println-insert-after)
         ("C-M-p" . println-insert-before)
         ("C-M-n" . println-insert-after)
         :map scala-mode-map
         ([remap sp-previous-sexp] . println-insert-before)
         ([remap sp-next-sexp] . println-insert-after)
         ("C-M-p" . println-insert-before)
         ("C-M-n" . println-insert-after)
         :map gdscript-mode-map
         ([remap sp-previous-sexp] . println-insert-before)
         ([remap sp-next-sexp] . println-insert-after)
         ("C-M-p" . println-insert-before)
         ("C-M-n" . println-insert-after)
         :map js-mode-map
         ([remap sp-previous-sexp] . println-insert-before)
         ([remap sp-next-sexp] . println-insert-after)
         ("C-M-p" . println-insert-before)
         ("C-M-n" . println-insert-after)))

;; (use-package println-debugger-scala
;;   :after scala-mode
;;   :bind (
;;          :map scala-mode-map
;;          ([remap sp-previous-sexp] . println-insert-before)
;;          ([remap sp-next-sexp] . println-insert-after)
;;          ("C-M-p" . println-insert-before)
;;          ("C-M-n" . println-insert-after)))
;;
;;
;;
;; (use-package println-debugger-emacs-lisp
;;   :bind (
;;          :map emacs-lisp-mode-map
;;          ([remap sp-previous-sexp] . println-insert-before)
;;          ([remap sp-next-sexp] . println-insert-after)
;;          ("C-M-p" . println-insert-before)
;;          ("C-M-n" . println-insert-after)))
;;
;; (use-package println-debugger-javascript
;;   :after js
;;   :bind (
;;          :map js-mode-map
;;          ([remap sp-previous-sexp] . println-insert-before)
;;          ([remap sp-next-sexp] . println-insert-after)
;;          ("C-M-p" . println-insert-before)
;;          ("C-M-n" . println-insert-after)))
;;
;; (use-package println-debugger-gdcsript
;;   :after gdscript-mode
;;   :bind (
;;          :map gdscript-mode-map
;;          ([remap sp-previous-sexp] . println-insert-before)
;;          ([remap sp-next-sexp] . println-insert-after)
;;          ("C-M-p" . println-insert-before)
;;          ("C-M-n" . println-insert-after)))

(use-package diminish
  :ensure t)

;; (defun right-arrow ()
;;   (interactive)
;;   (cond ((looking-back "=")
;;          (backward-delete-char 1) (insert "⇒"))
;;         ((looking-back "-")
;;          (backward-delete-char 1) (insert "→"))
;;        (t (insert ">"))))

;; (defun left-arrow ()
;;   (interactive)
;;   (if (looking-back "-")
;;       (progn (backward-delete-char 1)
;;              "//"))
;;   (insert "-"))


;;(electric-indent-mode -1)

(add-hook 'java-mode-hook #'lsp)
;; (add-hook 'java-mode-hook (lambda()
;;                             (message "Running java-mode-hook")
;;                             (electric-indent-mode)
;;                             (yas-minor-mode t)
;;                             (company-mode t)
;;                             (smartparens-mode) ;; smartparens must be below electric-*-modes
;;                             (show-paren-mode)
;;                             (subword-mode)
;;                             (glasses-mode)
;;                             (define-key yas-minor-mode-map [tab] #'yas-expand)

;;                             ))

(add-hook 'apib-mode-hook
          (lambda()
            (message "Running apib-mode-hook")
            (smartparens-mode) ;; smartparens must be below electric-*-modes
            (show-paren-mode)
            (subword-mode)
            (glasses-mode)))

;; == Useful commands ==
;; (list-faces-display)
;; (list-colors-display)
;; (describe-face "diary-anniversary")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "dark cyan"))))
 '(vc-conflict-state ((t (:foreground "Red"))))
 '(vc-edited-state ((t (:background "#555511" :foreground "#E0CF9F"))))
 '(vc-locally-added-state ((t (:foreground "hot pink"))))
 '(vc-locked-state ((t (:foreground "Red"))))
 '(vc-missing-state ((t (:foreground "Red"))))
 '(vc-needs-update-state ((t (:foreground "Red"))))
 '(vc-removed-state ((t (:foreground "Red"))))
 '(vc-state-base ((t (:foreground "RosyBrown1"))))
 '(vc-up-to-date-state ((t (:foreground "chartreuse3")))))

;;-(global-linum-mode 1)
;;(beacon-mode 1)
(setq beacon-color 0.4)

(setq ring-bell-function 'ignore)

;; set up ace-jump-mode
;;-(add-to-list 'load-path "~/.emacs.d/elpa/ace-jump-mode-20140616.115/")
;;-(require 'ace-jump-mode)
;;(define-key global-map (kbd "C-c C-SPC" ) 'ace-jump-mode)

(add-to-list 'load-path (concat user-emacs-directory "ensime-emacs"))

(require 'ace-jump-zap)
(define-key global-map (kbd "C-z" ) `ace-jump-zap-up-to-char)

;; set up ido mode
;;(require `ido)
;;(setq ido-enable-flex-matching t)
;;(setq ido-everywhere t)
;;(ido-mode 1)

;; (defun bind-ido-keys ()
;;   "Keybindings for ido mode."
;;   (define-key ido-completion-map "\C-p" 'ido-prev-match)
;;   (define-key ido-completion-map "\C-n" 'ido-next-match))

;; (add-hook 'ido-setup-hook #'bind-ido-keys)

;;(global-unset-key (kbd "C-z"))

(global-set-key (kbd "ESC ESC") 'keyboard-escape-quit) ;; Rebind from ESC ESC ESC
(global-set-key (kbd "s-s") `replace-string)
;;(global-set-key (kbd "s-s") `vr/replace)
(global-set-key (kbd "M-x") `counsel-M-x)
(global-set-key (kbd "M-z") `look-for-thing-at-point)
;;(global-set-key (kbd "C-c b") `ido-switch-buffer)                   ;; Broken x fix
(global-set-key (kbd "C-c b") `counsel-switch-buffer)
(global-set-key (kbd "C-c s") `save-some-buffers)                     ;; Broken x fix
(global-set-key (kbd "C-c C-s") `save-buffer)                         ;; Broken x fix
;;(global-set-key (kbd "C-c k") `ido-kill-buffer)                       ;; Broken x fix
(global-set-key (kbd "C-c 1") `zygospore-toggle-delete-other-windows) ;; Broken x fix
(global-set-key (kbd "M-X") `smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") `execute-extended-command)
(global-set-key (kbd "M-o") `other-window)
(global-set-key (kbd "s-W") `toggle-truncate-lines)


;; Haskell
;;(autoload 'ghc-init "ghc" nil t)
;;(autoload 'ghc-debug "ghc" nil t)
;;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
;; (add-hook 'haskell-mode-hook
;;   (lambda ()
;;     (setq company-backends '(company-ghc))))

;; (use-package ghc
;;   :disabled t
;;   :ensure t
;;   :init (ghc-init))

(use-package anzu
  :commands (isearch-foward isearch-backward)
  :config (global-anzu-mode))

;; (use-package haskell-customize
;;   :after haskell-mode
;;   :config
;;   (progn
;;     (setq haskell-process-args-stack-ghci '("--ghci-options=-ferror-spans -fshow-loaded-modules" "--no-build" "--no-load"))
;;     ))

;; (use-package company-ghc
;;   :ensure t
;;   :config
;;   (add-to-list 'company-backends
;;                '(company-ghc :with company-dabbrev-code))
;;   (custom-set-variables '(company-ghc-show-info t)))

;; (add-to-list 'company-backends 'company-ghc)

;; (use-package company-quickhelp
;;   :config
;;   (company-quickhelp-mode 1))!

;;hindent - format haskell code automatically
;;https://github.com/chrisdone/hindent
;; (when (executable-find "hindent")
;;   (use-package hindent
;;     ;;:diminish hindent-mode
;;     :config
;;     (add-hook 'haskell-mode-hook #'hindent-mode)
;;     ;;(setq hindent-reformat-buffer-on-save t)
;;     ))


;; (use-package dante
;;   :ensure t
;;   :after haskell-mode
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   (add-hook 'haskell-mode-hook 'flycheck-mode))



(use-package haskell-mode
  :config
  (setq haskell-stylish-on-save t)
  ;;(setq hs-lint-replace-with-suggestions t)
  ;;(setq compilation-skip-threshold 1)
  ;;(bind-key "C-x 4 s" `my-ghci:switch-to-ghci-buffer haskell-mode-map)
  ;;(bind-key "s-F" `ahh:projectile-ag-regexp haskell-mode-map)
  :bind (:map haskell-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)
              ("C-x 4 s" . sgm:switch-to-ghci-buffer)
              ("C-c v" . sgm:run-hydra)
              ("C-c e" . sgm:next-error)
              ("s-F" . ahh:projectile-ag-regexp)))

;; (use-package json-mode
;;   :config
;;   (local-unset-key [(control meta ?x)]))

(use-package json-mode
  :config
  :bind (:map json-mode-map
              ("C-c C-c" . upload-template)
              ("C-c C-o" . open-template-in-browser)))

(add-hook 'json-mode-hook (lambda ()
                               (subword-mode)
                               (glasses-mode)
                               (smartparens-mode)
                               (company-mode)
                               (rainbow-delimiters-mode)
                               (yas-minor-mode)))


;; (use-package hs-lint
;;   :config
;;   (setq compilation-skip-threshold 1)
;;   (put 'hs-lint-command 'safe-local-variable
;;      (lambda (command)
;;        (stringp command)))
;;   (defun hs-lint-all ()
;;     "Run HLint for all source files in stack project"
;;     (interactive)
;;     (save-some-buffers hs-lint-save-files)

;;     (let ((default-directory (progn (sgm:find-root)
;;                                     (if sgm:buffer-project-root
;;                                         sgm:buffer-project-root
;;                                       default-directory))))
;;       (compilation-start (concat hs-lint-command " .")
;;                         'hs-lint-mode))))

(add-hook 'haskell-mode-hook (lambda ()
                               ;;(setq flymake-log-level 3)
                               ;;(set (make-local-variable 'flymake-err-line-patterns) '(("^\\(.*\.hs\\):\\([0-9]+\\):\\([0-9]+\\): \\(.*\\(?:\n.+\\)+\\)" 1 2 3 4)))
                               (subword-mode)
                               ;;(interactive-haskell-mode)
                               (glasses-mode)
                               (haskell-indentation-mode)
                               (smartparens-mode)
                               (company-mode)
                               (rainbow-delimiters-mode)
                               ;;(add-hook 'before-save-hook 'sgm:check-modified-buffers)
                               ;;(flymake-mode)
                               ;;(flymake-hlint-load)
                               ;;(hlint-refactor-mode)
                               ;;(ghc-init)
                               ))




(use-package web-mode)

(add-to-list 'auto-mode-alist '("\\.scala.html\\'" . web-mode))

(add-hook 'web-mode-hook (lambda ()
                           (setq electric-indent-inhibit t)
                           (subword-mode)
                           (glasses-mode)
                           (smartparens-mode)
                           (company-mode)
                           (rainbow-delimiters-mode)))

;; (use-package intero
;;   :ensure t
;;   :init
;;   (add-hook 'haskell-mode-hook (lambda ()
;;                                  (intero-mode)
;;                                  (subword-mode)
;;                                  (smartparens-mode)
;;                                  (company-mode)
;;                                  (rainbow-delimiters-mode)))
;;  :config
;;  (bind-key "M-n" `flycheck-next-error intero-mode-map)
;;  (bind-key "M-p" `flycheck-previous-error intero-mode-map))

;; mess with highlighting
;; (when (executable-find "structured-haskell-mode")
;;   (use-package shm
;;     :config
;;     (add-hook 'haskell-mode-hook #'structured-haskell-mode)))

;; (add-hook 'haskell-mode-hook (lambda ()
;;                                (subword-mode)
;;                                (interactive-haskell-mode)
;;                                (haskell-indentation-mode)
;;                                (smartparens-mode)
;;                                (company-mode)
;;                                (rainbow-delimiters-mode)
;;                                (ghc-init)))

;; (add-hook 'haskell-interactive-mode-hook (lambda ()
;;                                            (subword-mode)
;;                                            (smartparens-mode)
;;                                            (rainbow-delimiters-mode)
;;                                            (company-mode)))

(add-hook 'minibuffer-setup-hook #'subword-mode)

;; https://github.com/lunaryorn/swsnr.de/blob/master/_posts/2014-07-26-make-your-emacs-mode-line-more-useful.md
(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   mode-line-frame-identification
   mode-line-buffer-identification
   "   "
   mode-line-position
   lunaryorn-projectile-mode-line ; Project information
   (vc-mode vc-mode) ;;my-vc-mode
   "  "
   mode-line-modes
   mode-line-misc-info
   mode-line-end-spaces))

;; (defun vc-default-mode-line-string (backend file)
;;   "Return a string for `vc-mode-line' to put in the mode line for FILE.
;; Format:
;;
;;   \"BACKEND-REV\"        if the file is up-to-date
;;   \"BACKEND:REV\"        if the file is edited (or locked by the calling user)
;;   \"BACKEND:LOCKER:REV\" if the file is locked by somebody else
;;   \"BACKEND@REV\"        if the file was locally added
;;   \"BACKEND!REV\"        if the file contains conflicts or was removed
;;   \"BACKEND?REV\"        if the file is under VC, but is missing
;;
;; This function assumes that the file is registered."
;;   (message "CALLED vc-default-mode-line-string: %s %s" backend file)
;;   (let* ((backend-name (symbol-name backend))
;; 	 (state   (vc-state file backend))
;; 	 (state-echo nil)
;; 	 (face nil)
;; 	 (rev     (vc-working-revision file backend))
;;          (res
;;           (propertize
;;                (cond ((or (eq state 'up-to-date)
;;                           (eq state 'needs-update))
;;                       (setq state-echo "Up to date file")
;;                       (setq face 'vc-up-to-date-state)
;;                       (concat backend-name "-" rev))
;;                      ((stringp state)
;;                       (setq state-echo (concat "File locked by" state))
;;                       (setq face 'vc-locked-state)
;;                       (concat backend-name ":" state ":" rev))
;;                      ((eq state 'added)
;;                       (setq state-echo "Locally added file")
;;                       (setq face 'vc-locally-added-state)
;;                       (concat backend-name "@" rev))
;;                      ((eq state 'conflict)
;;                       (setq state-echo "File contains conflicts after the last merge")
;;                       (setq face 'vc-conflict-state)
;;                       (concat backend-name "!" rev))
;;                      ((eq state 'removed)
;;                       (setq state-echo "File removed from the VC system")
;;                       (setq face 'vc-removed-state)
;;                       (concat backend-name "!" rev))
;;                      ((eq state 'missing)
;;                       (setq state-echo "File tracked by the VC system, but missing from the file system")
;;                       (setq face 'vc-missing-state)
;;                       (concat backend-name "?" rev))
;;                      (t
;;                       ;; Not just for the 'edited state, but also a fallback
;;                       ;; for all other states.  Think about different symbols
;;                       ;; for 'needs-update and 'needs-merge.
;;                       (setq state-echo "Locally modified file")
;;                       (setq face 'vc-edited-state)
;;                       (concat backend-name ":" rev)))
;;                'face face
;;                'help-echo (concat state-echo " under the " backend-name
;;                                   " version control system"))
;;           ))
;;     (message "vc-edited-state: %s" 'vc-edited-state)
;;     (message "face: %s" face)
;;     (message "res: %s" res)
;;              res
;;
;;     ))


;; See vc-default-mode-line-string

	    ;; (setq state-echo "Up to date file")
	    ;; (setq face 'vc-up-to-date-state)
	    ;; (setq state-echo (concat "File locked by" state))
	    ;; (setq face 'vc-locked-state)
            ;; (setq state-echo "Locally added file")
	    ;; (setq face 'vc-locally-added-state)
            ;; (setq state-echo "File contains conflicts after the last merge")
	    ;; (setq face 'vc-conflict-state)
            ;; (setq state-echo "File removed from the VC system")
	    ;; (setq face 'vc-removed-state)
            ;; (setq state-echo "File tracked by the VC system, but missing from the file system")
	    ;; (setq face 'vc-missing-state)
	    ;; (setq state-echo "Locally modified file")
	    ;; (setq face 'vc-edited-state)

;; (defun vc-git-mode-line-string (file)
;;   "Return a string for `vc-mode-line' to put in the mode line for FILE."
;;   (message "XXX file: %s" file)
;;   (let* ((rev (vc-working-revision file 'Git))
;;          (disp-rev (or (vc-git--symbolic-ref file)
;;                        (substring rev 0 7)))
;;          (def-ml (vc-default-mode-line-string 'Git file))
;;          (help-echo (get-text-property 0 'help-echo def-ml))
;;          (face   (get-text-property 0 'face-git-branch def-ml)))
;;     (propertize (concat (substring def-ml 0 4) disp-rev)
;;                 'face face
;;                 'help-echo (concat help-echo "\nCurrent revision: " rev))))

(defface face-projectile-project
  '((t :foreground "DarkOliveGreen1")) "highlight less important text")

;; https://emacs.stackexchange.com/a/10964
;; (defvar my-vc-mode
;;   '((:propertize
;;      (:eval vc-mode)
;;      face face-git-branch)))
;; (put 'my-vc-mode 'risky-local-variable t)

(defvar lunaryorn-projectile-mode-line
  '(:propertize
    (:eval (when (ignore-errors (projectile-project-root))
             (concat " " (projectile-project-name))))
    face face-projectile-project)
  "Mode line format for Projectile.")
(put 'lunaryorn-projectile-mode-line 'risky-local-variable t)


;; (use-package eyebrowse
;;   :config
;;   (progn
;;     (eyebrowse-mode t)))

;; (use-package nix-sandbox)

;; (use-package nix-mode
;;   :mode "\\.nix\\'"
;;   :config

;;   (use-package nixos-options
;;     :config
;;     (use-package company-nixos-options)
;;     (add-to-list 'company-backends 'company-nixos-options)))

;; (add-hook 'nix-mode-hook (lambda ()
;;                            (company-mode)))

;; (use-package nixos-options)

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
;; (defadvice ido-set-matches-1 (after ido-smex-acronym-matches activate)
;;   (if (and (fboundp 'smex-already-running) (smex-already-running)
;;            (> (length ido-text) 1))
;;       (let ((regex (concat "^" (mapconcat 'char-to-string ido-text "[^-]*-")))
;;             (acronym-matches (list))
;;             (remove-regexes '("-menu-")))
;;         ;; Creating the list of the results to be set as first
;;         (dolist (item items)
;;           (if (string-match ido-text item) ;; exact match
;;               (add-to-list 'acronym-matches item)
;;             (if (string-match (concat regex "[^-]*$") item) ;; strict match
;;                 (add-to-list 'acronym-matches item)
;;               (if (string-match regex item) ;; appending relaxed match
;;                   (add-to-list 'acronym-matches item t)))))
;;
;;         ;; Filtering ad-return-value
;;         (dolist (to_remove remove-regexes)
;;           (setq ad-return-value
;;                 (delete-if (lambda (item)
;;                              (string-match to_remove item))
;;                            ad-return-value)))
;;
;;         ;; Creating resulting list
;;         (setq ad-return-value
;;               (append acronym-matches
;;                       ad-return-value))
;;
;;         (delete-dups ad-return-value)
;;         (reverse ad-return-value))))

(set-face-attribute 'default nil :height 135)
(set-face-attribute 'region nil :background "DeepPink4")
(set-face-attribute 'region nil :background "DeepPink4")


;; (use-package 0blayout
;;   :ensure t

;;   ;; Optionally set default layout name
;;   :init (setq-default 0blayout-default "my-default-layout-name")

;;   ;; Load the mode
;;   :config (0blayout-mode t))

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)

(fset 'Search\ for\ text\ from\ cursor\ to\ end\ of\ line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([11 67108911 8388678 1 25 12 11 return] 0 "%d")) arg)))


(fset 'search-for-class-json-format
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([134217830 134217830 6 134217751 8388678 1 99 108 97 115 115 32 return] 0 "%d")) arg)))

;; (defun mac-switch-meta nil
;;   "switch meta between Option and Command"
;;   (interactive)
;;   (if (eq mac-option-modifier nil)
;;       (progn
;; 	(setq mac-option-modifier 'meta)
;; 	(setq mac-command-modifier 'hyper)
;; 	)
;;     (progn
;;       (setq mac-option-modifier nil)
;;       (setq mac-command-modifier 'meta))))
;;
;; (mac-switch-meta)

;; (load-file (let ((coding-system-for-read 'utf-8))
;;                 (shell-command-to-string "agda-mode locate")))
(put 'list-timers 'disabled nil)

(use-package glsl-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.shader\\'" . glsl-mode))
  (setq glsl-additional-keywords '("shader_type")
        glsl-additional-built-ins
        '(
          "TIME"
          "UV" "COLOR" "TEXTURE" "WORLD_MATRIX" "EXTRA_MATRIX" "PROJECTION_MATRIX" "INSTANCE_CUSTOM" "AT_LIGHT_PASS" "VERTEX" "TEXTURE_PIXEL_SIZE" "MODULATE" "POINT_SIZE" "FRAGCOORD"
          "NORMAL" "NORMALMAP" "NORMALMAP_DEPTH" "NORMAL_TEXTURE" "SCREEN_UV" "SCREEN_PIXEL_SIZE" "POINT_COORD" "SCREEN_TEXTURE"
          "LIGHT_VEC" "SHADOW_VEC" "LIGHT_HEIGHT" "LIGHT_COLOR" "LIGHT_UV" "SHADOW_COLOR" "LIGHT"
          ))
  :config
  (setq c-basic-offset 4)
  :bind (:map glsl-mode-map
              ("C-c r" . gdscript-hydra-show)
              ("C-c C-r C-o" . glsl-find-man-page))
  :hook ((glsl-mode . (lambda ()
                        (setq tab-width 4)))
         (glsl-mode . smartparens-mode)
         (glsl-mode . subword-mode))
  )

(use-package gdscript-mode
  :config
  (setq
   gdscript-docs-force-online-lookup t
   gdscript-godot-executable "/Applications/Godot.app/Contents/MacOS/Godot"
   ;;gdscript-godot-executable "/Users/pepa/develop-godot/godot/bin/godot.osx.tools.64"
   gdscript-indent-guess-indent-offset nil
   gdscript-gdformat-line-length 120
   gdscript-gdformat-save-and-format t
   gdscript-debug-emacs-executable "Emacs-27.1"
   rg-custom-type-aliases '(("gdscript" ."*.gd *.tscn")))


  :bind (:map
         gdscript-mode-map
         ("C-c v" . gdscript-hydra-show)
         ("C-c C-r C-a" . gdscript-docs-browse-api)
         ("C-c C-r C-o" . gdscript-docs-browse-symbol-at-point)
         :map
         gdscript-comint--mode-map
         ("C-c v" . gdscript-hydra-show)
         ("C-c C-v" . comint-clear-buffer)
         :map
         gdscript-debug--inspector-mode-map
         ("C-c C-r C-o" . gdscript-docs-browse-symbol-at-point)
         :map
         gdscript-debug--scene-tree-mode-map
         ("C-c C-r C-o" . gdscript-docs-browse-symbol-at-point)
         :map
         gdscript-debug--stack-frame-vars-mode-map
         ("C-c C-r C-o" . gdscript-docs-browse-symbol-at-point))

  :hook ((gdscript-mode . smartparens-mode)
         (gdscript-mode . subword-mode)
         ;;(gdscript-mode . lsp)
         ;; (eww-after-render . (lambda ()
         ;;                       (gdscript-documentation-rename-eww-buffer)
         ;;                       (setq multi-isearch-next-buffer-function nil)))
         ))

;; (defun save-buffer-before-hydra ()
;;   (unless (derived-mode-p 'godot-mode)
;;     (save-buffer)))

;;(advice-add 'gdscript-hydra-show :before #'save-buffer-before-hydra)

(use-package eww
  :config
  (setq shr-use-fonts nil))

(use-package conf-mode ;; Mode for Unix and Windows Conf files and Java properties
  :bind (:map conf-mode-map
              ("C-c r" . gdscript-hydra-show)))

(use-package c++-mode
  :config
  (setq c-basic-offset 8)
  :hook ((c++-mode . smartparens-mode)
         (c++-mode . subword-mode)))

(use-package hexl
  :custom-face
  (highlight ((t (:background "dark cyan")))) ;; this is global change. To make it local: https://emacs.stackexchange.com/questions/2957/how-to-customize-syntax-highlight-for-just-a-given-mode
  )

(use-package restclient-mode
  :hook ((restclient-mode . smartparens-mode)
         (restclient-mode . subword-mode))
  :bind (:map
         restclient-mode-map ("C-c C-o" . open-template-in-browser)))

(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))
(use-package lsp-ui)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
;;(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package ibuffer
  :config
  :bind (("M-o" . other-window)
         ("C-x C-b" . ibuffer)))

(use-package sql-interactive-mode
  :hook ((sql-interactive-mode . smartparens-mode)
         (sql-interactive-mode . subword-mode)))

(use-package transpose-frame
  :config
  :bind (:map ctl-x-4-map ("t" . transpose-frame)))

;; From use-package.info,  Node: bind bind*,
;; ...
;; use ‘M-x describe-personal-keybindings’ to see all such
;; keybindings you’ve set throughout your ‘.emacs’ file
;; ...

(use-package org
  :config
  (setq
   org-special-ctrl-a/e t
   org-M-RET-may-split-line nil))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/bookmarks")
 '(company-ghc-show-info t)
 '(company-quickhelp-use-propertized-text nil)
 '(erc-modules
   '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track))
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   '(("programming"
      (or
       (derived-mode . prog-mode)
       (mode . ess-mode)
       (mode . compilation-mode)))
     ("text document"
      (and
       (derived-mode . text-mode)
       (not
        (starred-name))))
     ("TeX"
      (or
       (derived-mode . tex-mode)
       (mode . latex-mode)
       (mode . context-mode)
       (mode . ams-tex-mode)
       (mode . bibtex-mode)))
     ("web"
      (or
       (derived-mode . sgml-mode)
       (derived-mode . css-mode)
       (mode . javascript-mode)
       (mode . js2-mode)
       (mode . scss-mode)
       (derived-mode . haml-mode)
       (mode . sass-mode)))
     ("gnus"
      (or
       (mode . message-mode)
       (mode . mail-mode)
       (mode . gnus-group-mode)
       (mode . gnus-summary-mode)
       (mode . gnus-article-mode)))))
 '(magit-commit-arguments nil)
 '(magit-log-arguments
   '("--graph" "--color" "--decorate" "--show-signature" "-n256"))
 '(package-selected-packages
   '(graphql-mode dired-du hydra json-mode ivy counsel transpose-frame info-colors whole-line-or-region lsp-ui lsp-java typescript-mode glsl-mode rg lsp-mode prettier-js ansi feature-mode color-identifiers-mode overseer bookmark+ bookmarks+ dante ialign wgrep-ag idris-mode nodejs-repl mustache-mode package-build shut-up epl git commander dash s iedit psc-ide aggressive-indent engine-mode company-quickhelp company-nixos-options revive expand-region zygospore yaml-mode web-mode use-package suggest smex smartparens shm session scss-mode restclient rainbow-delimiters puppet-mode protobuf-mode projectile popup-imenu play-routes-mode pcre2el octopress markdown-mode key-chord js2-mode inf-mongo hindent highlight-symbol grizzl git-timemachine furl flycheck csv-mode crux company-ghc cask beacon avy anzu ag ace-jump-zap))
 '(safe-local-variable-values
   '((haskell-stylish-on-save)
     (intero-targets "simple-hpack:test:simple-hpack-test")
     (haskell-process-args-stack-ghci "--ghci-options=-ferror-spans" "--no-build" "--no-load"))))
