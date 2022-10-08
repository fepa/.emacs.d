;;; init.el --- Felix Panozzo's emacs configuration

;; Package managment
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)

;; Separate custom file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file 'noerror)

;; Colors
(set-face-background 'mode-line "DarkViolet")
(set-face-foreground 'mode-line "honeydew2")
(set-background-color "#121212")
(set-foreground-color "honeydew2")
(set-cursor-color "DarkViolet")
(set-face-background 'region "DarkViolet")

;; Font
(set-frame-font "-DAMA-Ubuntu Mono-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")
(global-font-lock-mode 3)
(setq-default line-spacing 3)

;; Remove menus and toolbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Remove trailing white space on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Empty scratch buffer
(setq initial-scratch-message "")

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Make *Messages* store more messages
(setq message-log-max 10000)

;; Spaces instead of tabs
(setq indent-tabs-mode nil)
;; 4 spaces offset
(setq tab-width 4)

;; Parenthesis matching
(show-paren-mode t)

;; Line & column numbers
(global-display-line-numbers-mode 1)
(column-number-mode 1)

;; Delete selections
(delete-selection-mode t)

;; Always end file with newline
(setq require-final-newline t)

;;Enable copy outside emacs
(setq select-enable-clipboard t)

;; Smoother scrolling, scroll one line at a time (less "jumpy" than default)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Disable all alarms and bells
(setq ring-bell-function 'ignore)

;; Save backup files in a central directory
(setq backup-directory-alist '((".*" . "~/.Trash")))

;; HTML checkboxes in org-mode
(setq org-html-checkbox-type 'html)

(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("WAITING" . "yellow"))
      )


;; Allow 20MB of memory (instead of 0.76MB) before calling
;; garbage collection. This means GC runs less often, which speeds
;; up some operations.
(setq gc-cons-threshold 20000000)

; Treat CamelCaseSubWords as separate words in every programming mode
(global-subword-mode t)

;; Don't assume sentences should have 2 spaces after periods
(setq sentence-end-double-space nil)

;; Ask if you're sure that you want to close Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Add file sizes in human-readable units (KB, MB, etc) to
;; dired buffers
(setq-default dired-listing-switches "-alh")

;; Don't ask `yes/no?', ask `y/n?'
(fset 'yes-or-no-p 'y-or-n-p)

;; Configure elpy to use python3
;; (setq elpy-rpc-python-command "python3")
;; (elpy-enable)

(ido-mode t)
(winner-mode)
(ido-everywhere)
;; (autoload 'global-flycheck-mode "global-flycheck-mode")
;; Read desktop file on startup
(setq desktop-dirname "~/.emacs.d/")
(desktop-read)

(global-set-key [f5] 'toggle-truncate-lines)
(global-set-key (kbd "<XF86WakeUp>") 'execute-extended-command) ;; Bind Fn to M-x
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\C-c\C-c" 'comment-region)
(global-set-key (kbd "C-ö") 'ace-jump-mode)
(global-set-key (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(global-set-key "\C-c\C-g" 'magit-status)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-ä") 'mc/mark-next-like-this)
(global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-ö") 'mc/mark-all-like-this)
(global-set-key "\C-cd" 'zeal-at-point)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.j2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jinja2\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))

(setq web-mode-markup-indent-offset 4
      web-mode-css-indent-offset 4
      web-mode-code-indent-offset 4
      web-mode-script-padding 4
      web-mode-style-padding 4
      js-indent-level 2
      ruby-indent-level 4
)

(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

(autoload 'jsonnet-mode "jsonnet-mode")
(add-to-list 'auto-mode-alist '("\\.jsonnet\\'" . jsonnet-mode))

(autoload 'dumb-jump-mode "dump-jump-mode")
(global-git-gutter-mode 1 1)

(editorconfig-mode 1)

;; Disable encoding comment insertion in ruby
(setq-default ruby-insert-encoding-magic-comment nil)

(setq-default ido-enable-flex-matching t)

(setq markdown-command "/usr/bin/pandoc")

(setq dired-listing-switches "-alh")

;;; Hooks:

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4))))

(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq js2-strict-missing-semi-warning nil)))

; Remove log statement about flymake legacy
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)


;;; Code:

(defun ensure-region-active (func &rest args)
  "Only run FUNC with ARGS when region is active."
  (when (region-active-p)
    (apply func args)))
(advice-add 'upcase-region :around 'ensure-region-active)
(advice-add 'downcase-region :around 'ensure-region-active)

;;; init.el ends here
