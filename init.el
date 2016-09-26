;; -*-no-byte-compile: t; -*-

;; Package managment
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize) ;; You might already have this line

;; List of installed packages
;; (describe-variable package-activated-list)
(setq package-list '(web-mode git-gutter-fringe fringe-helper git-gutter
                              multiple-cursors yasnippet yaml-mode
                              nyan-mode ace-jump-mode magit
                              multiple-cursors git-gutter-fringe
                              flycheck ))
;; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(load-file "~/.emacs.d/ui.el")
(load-file "~/.emacs.d/general.el")
(load-file "~/.emacs.d/keybinds.el")
(load-file "~/.emacs.d/fill-column-indicator.el")

;; YASnippet
(require 'yasnippet)
(yas-global-mode 1)

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; nyan mode
(require 'nyan-mode)
(nyan-mode)

;; ace jump mode
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;; Key bind for jumping
(define-key global-map (kbd "C-ö") 'ace-jump-mode)
;; jump back function
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;; fill column indicator mode
(require 'fill-column-indicator)
(setq fci-rule-color "#222222")
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; markdown mode
;;(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
;;(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
;;(add-hook 'markdown-mode-hook (lambda () (variable-pitch-mode t)))

;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; magit
(require 'magit)

;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-ä") 'mc/mark-next-like-this)
(global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-ö") 'mc/mark-all-like-this)

;; git gutter (fringe)
(require 'git-gutter-fringe)
(set-face-foreground 'git-gutter-fr:modified "DarkViolet")
(fringe-helper-define 'git-gutter-fr:modified nil
  "...XX..."
  "..X..X.."
  ".X....X."
  "X......X"
  "X......X"
  ".X....X."
  "..X..X.."
  "...XX...")
(global-git-gutter-mode)

;; winner mode (undoing buffer splits)
(winner-mode)
(put 'upcase-region 'disabled nil)

(require 'iso-transl) ;; Fixes typing accent characters

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
(setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
(setq flycheck-standard-error-navigation nil)

(global-flycheck-mode t)

;; flycheck errors on a tooltip (doesnt work on console)
(when (display-graphic-p (selected-frame))
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))
