;;; init.el --- Felix Panozzo's emacs configuration

;;; Commentary:

;; Package managment
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
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
(set-frame-font "Ubuntu Mono:pixelsize=14:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=10:scalable=true")
(global-font-lock-mode 3)
(setq line-spacing 2)

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
(setq-default indent-tabs-mode nil)
;; 4 spaces offset
(setq-default tab-width 4)

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
(setq x-select-enable-clipboard t)

;; Smoother scrolling, scroll one line at a time (less "jumpy" than default)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

;; Disable all alarms and bells
(setq ring-bell-function 'ignore)

(yas-global-mode 1)
(nyan-mode)
(ido-mode t)
(winner-mode)
(global-git-gutter-mode)
(ido-everywhere)
(global-flycheck-mode)
;; Read desktop file on startup
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
(add-to-list 'auto-mode-alist '("\\.scss\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

(setq-default web-mode-markup-indent-offset 2
              web-mode-css-indent-offset 2
              web-mode-code-indent-offset 2
              web-mode-script-padding 2
              web-mode-style-padding 2
)

;; Disable encoding comment insertion in ruby
(setq-default ruby-insert-encoding-magic-comment nil)


;;; Hooks:

(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))

(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4))))


;;; Code:

(defun ensure-region-active (func &rest args)
  "Only run FUNC with ARGS when region is active."
  (when (region-active-p)
    (apply func args)))
(advice-add 'upcase-region :around 'ensure-region-active)
(advice-add 'downcase-region :around 'ensure-region-active)

(defun gtd ()
  "Open gtd files."
   (interactive)
   (find-file "~/Dropbox/org/inbox.org")
   (split-window-right)
   (find-file "~/Dropbox/org/projects.org")
   )

;;; init.el ends here
