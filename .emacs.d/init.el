;;; init.el --- Emacs configuration -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for global settings for built-in emacs parameters
(setq
 initial-scratch-message nil
 make-backup-files nil
 dabbrev-case-fold-search nil
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This section is for setup functions that are built-in to emacs
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(set-keyboard-coding-system nil)
(scroll-bar-mode -1)
(setq next-screen-context-lines 5)

;; delete trailing whitespace before file is saved
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;(setq debug-on-error t)

;; inspiration https://github.com/rmm5t/dotfiles/blob/df77009c326a9d09f23e7bedcd44e9658f26bc6f/emacs.d/init.el
(load "~/.emacs.d/personal/defuns")

(personal 'kbd-macros)

(require 'recentf)
(recentf-mode 1)

(require 'package)
(add-to-list 'package-archives
         '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
         '("SC" . "http://joseito.republika.pl/sunrise-commander/") t)
(package-initialize)


(require 'smex)
(smex-initialize)

(require 'use-package)

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1))

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
  (setq ag-highlight-search t)
  :config
  (add-hook 'ag-search-finished-hook (lambda () (pop-to-buffer next-error-last-buffer))))

(use-package projectile
  ;; nice to have it on the modeline
  :init (setq
         projectile-use-git-grep t)
  :config
  (projectile-global-mode))

(use-package highlight-symbol
  :diminish highlight-symbol-mode
  :commands highlight-symbol
  :bind ("s-h" . highlight-symbol)
  :bind ("s-n" . highlight-symbol-next)
  :bind ("s-p" . highlight-symbol-prev)
  :bind ("s-r" . highlight-symbol-remove-all))

(use-package general-close
  :bind ("C-;" . general-close))

(use-package git-gutter
  :diminish git-gutter-mode
  :commands git-gutter-mode
  :bind ("C-c C-p" . git-gutter:previous-hunk)
  :bind ("C-c C-n" . git-gutter:next-hunk)
  :bind ("C-c C-k" . git-gutter:popup-hunk)
  :bind ("C-c C-j" . git-gutter:revert-hunk))

(use-package magit
  :commands magit-status magit-blame
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(which-key-mode 1)
;; Standard key for next and previous are C-s and C-r no need to replace with C-n and C-p
;;(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Javascript indentation 2
(setq js-indent-level 2)

;; Must read: Setting the PATH variable
;; http://emacsredux.com/blog/2015/05/09/emacs-on-os-x/
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;;(setq exec-path (append exec-path '("/Users/pepa/bin/")))

(when (not package-archive-contents)
  (package-refresh-contents))

;;(setenv "PATH" (concat "/Users/pepa/bin/:" (getenv "PATH")))

(load-theme 'zenburn :no-confirm)

(defun scala-mode-newline-comments ()
  "Custom newline appropriate for `scala-mode'."
  ;; shouldn't this be in a post-insert hook?
  (interactive)
  (newline-and-indent)
  (scala-indent:insert-asterisk-on-multiline-comment))

(use-package scala-mode2
  ;;//:interpreter
  ;;("scala" . scala-mode)
  :init
  (setq
   scala-indent:use-javadoc-style t
   scala-indent:align-parameters t)
  :config
  (bind-key "RET" 'scala-mode-newline-comments scala-mode-map)
  (bind-key "C-c c" 'sbt-command scala-mode-map)
  (bind-key "M-SPC" 'scala-indent:fixup-whitespace)
  (bind-key "C-M-j" 'scala-indent:join-line))


(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (rainbow-delimiters-mode)))

(require 'ensime)
(add-hook 'scala-mode-hook
	  (lambda()
	    (message "Running scala-mode-hook")
	    (ensime-mode)
	    (show-paren-mode)
	    (subword-mode)))
	    ;(git-gutter-mode)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 140 :family "Menlo")))))

(global-linum-mode 1)
(beacon-mode 1)
(setq beacon-color 0.4)

(setq ring-bell-function 'ignore)

;; set up ace-jump-mode
(add-to-list 'load-path "~/.emacs.d/elpa/ace-jump-mode-20140616.115/")
(require 'ace-jump-mode)
;;(define-key global-map (kbd "C-c C-SPC" ) 'ace-jump-mode)

(require 'ace-jump-zap)
(define-key global-map (kbd "C-z" ) 'ace-jump-zap-up-to-char)

;; set up ido mode
(require `ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;;(global-unset-key (kbd "C-z"))

(global-set-key (kbd "s-s") 'replace-string)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

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

(set-face-attribute 'region nil :background "DarkOrchid3")
(set-face-attribute 'region nil :background "DeepPink4")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(beacon-color 0.4)
 '(cursor-type (quote box))
 '(custom-safe-themes
   (quote
    ("f024aea709fb96583cf4ced924139ac60ddca48d25c23a9d1cd657a2cf1e4728" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(delete-selection-mode t)
 '(fci-rule-color "#d6d6d6")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#c82829")
     (40 . "#f5871f")
     (60 . "#eab700")
     (80 . "#718c00")
     (100 . "#3e999f")
     (120 . "#4271ae")
     (140 . "#8959a8")
     (160 . "#c82829")
     (180 . "#f5871f")
     (200 . "#eab700")
     (220 . "#718c00")
     (240 . "#3e999f")
     (260 . "#4271ae")
     (280 . "#8959a8")
     (300 . "#c82829")
     (320 . "#f5871f")
     (340 . "#eab700")
     (360 . "#718c00"))))
 '(vc-annotate-very-old-color nil))
