;; -*-no-byte-compile: t; -*-

;; Package managment
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; Setup load paths
(defvar emacs-submodules-path "~/.emacs.d/submodules/")
(add-to-list 'load-path emacs-submodules-path)

(load-file "~/.emacs.d/ui.el")
(load-file "~/.emacs.d/general.el")
(load-file "~/.emacs.d/keybinds.el")

;; YASnippet
(defvar yasnippet-path (concat emacs-submodules-path "yasnippet/"))
(add-to-list 'load-path yasnippet-path)
;; custom dropdown-list colours
(defface dropdown-list-face '((t (:background "lightgray" :foreground "black"))) "*Bla." :group 'dropdown-list)
(defface dropdown-list-selection-face '((t (:background "DarkViolet" :foreground "white"))) "*Bla." :group 'dropdown-list)
;; load yasnippet
(require 'yasnippet)
;(setq yas/snippet-dirs
;      (list (concat yasnippet-path "snippets")
;	    (concat emacs-sync-path "submodules/slantsix/snippets")
;	    )
;      )
(yas/global-mode 1)
(setq yas/prompt-functions '(yas/dropdown-prompt))

;; Coffee mode
(defvar coffee-path (concat emacs-submodules-path "coffee-mode/"))
(add-to-list 'load-path coffee-path)
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
  ;; Compile '.coffee' files on every save
  ;;(add-hook 'coffee-mode-hook '(lambda () (coffee-cos-mode t)))
)
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; yaml mode
(defvar yaml-path (concat emacs-submodules-path "yaml-mode/"))
(add-to-list 'load-path yaml-path)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; haml mode
(defvar haml-path (concat emacs-submodules-path "haml-mode/"))
(add-to-list 'load-path haml-path)
(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . sass-mode))

;; sass mode (requires haml mode)
(defvar sass-path (concat emacs-submodules-path "sass-mode/"))
(add-to-list 'load-path sass-path)
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
(defvar pony-path (concat emacs-submodules-path "pony-mode/"))
(add-to-list 'load-path (concat pony-path "src"))
(yas/load-directory (concat pony-path "snippets"))
(require 'pony-mode)

;; hardcore mode
;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)
;;If hardcore-mode is too hardcore for you, you can add these before you require the mode:
;;(setq too-hardcore-backspace t)
;;(setq too-hardcore-return t)
;;(defvar hardcore-path (concat emacs-submodules-path "hardcore-mode"))
;;(add-to-list 'load-path hardcore-path)
;;(require 'hardcore-mode)
;;(global-hardcore-mode)

;; nyan mode
(defvar nyan-path (concat emacs-submodules-path "nyan-mode"))
(add-to-list 'load-path nyan-path)
(require 'nyan-mode)
(nyan-mode)

;; ace jump mode
(defvar ace-path (concat emacs-submodules-path "ace-jump-mode"))
(add-to-list 'load-path ace-path)
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
(defvar fill-column-indicator-path (concat emacs-submodules-path "fill-column-indicator"))
(add-to-list 'load-path fill-column-indicator-path)
(require 'fill-column-indicator)
(setq fci-rule-color "#222222")
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; markdown mode
(defvar markdown-path (concat emacs-submodules-path "markdown-mode"))
(add-to-list 'load-path markdown-path)
(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
(add-hook 'markdown-mode-hook (lambda () (variable-pitch-mode t)))

;; Associate Rake files with ruby-mode
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
;; jump, inf-ruby and rinari
(defvar jump-path (concat emacs-submodules-path "jump"))
(add-to-list 'load-path jump-path)
(require 'jump)
(defvar inf-ruby-path (concat emacs-submodules-path "inf-ruby"))
(add-to-list 'load-path inf-ruby-path)
(defvar rinari-path (concat emacs-submodules-path "rinari"))
(add-to-list 'load-path rinari-path)
(require 'rinari)
(global-rinari-mode)

;; ido mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;; magit
(defvar magit-path (concat emacs-submodules-path "magit"))
(add-to-list 'load-path magit-path)
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
