;; -*-no-byte-compile: t; -*-

;; Package managment
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  ;; List of installed packages
  ;; (describe-variable package-activated-list)
  (setq package-list '(web-mode git-gutter-fringe fringe-helper git-gutter
                                multiple-cursors))
  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package)))
  )

;; Setup load paths
(defvar emacs-submodules-path "~/.emacs.d/submodules/")
(add-to-list 'load-path emacs-submodules-path)

(load-file "~/.emacs.d/ui.el")
(load-file "~/.emacs.d/general.el")
(load-file "~/.emacs.d/keybinds.el")

;; YASnippet
(add-to-list 'load-path
              "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; Coffee mode
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
;; configure tab width
(defun coffee-custom ()
  "coffee-mode-hook"
  ;; Coffeescript uses 2 spaces
  (set (make-local-variable 'tab-width) 2)
  ;; If you don't want your compiled files to be wrapped
  (setq coffee-args-compile '("-c" "--bare"))
  ;; *Messages* spam
  (setq coffee-debug-mode t)
  ;; Emacs key binding
  (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)
)
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; yaml mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; haml mode
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . sass-mode))

;; sass mode (requires haml mode)
(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

;; scss mode
(setq exec-path (cons (expand-file-name "/usr/local/bin/sass") exec-path))
(defvar scss-path (concat emacs-submodules-path "scss-mode/"))
(add-to-list 'load-path scss-path)
(autoload 'scss-mode "scss-mode")
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq scss-compile-at-save nil)
(setq css-indent-offset 2)
(require 'scss-mode)

;; pony-mode
(require 'pony-mode)


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
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook (lambda () (variable-pitch-mode t)))

;; Associate Rake files with ruby-mode
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
;; jump, inf-ruby and rinari
(require 'jump)
(require 'rinari)
(global-rinari-mode)
(setq ruby-insert-encoding-magic-comment nil)

;; Associate erb files with web-mode
(add-to-list 'auto-mode-alist '("\\.erb$" . web-mode))

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

(require 'smooth-scrolling)
(setq smooth-scroll-margin 5)
(setq scroll-conservatively 9999
      scroll-preserve-screen-position t)

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
