;; -*-no-byte-compile: t; -*-

; ====================
; Load Paths
; ====================
(defvar emacs-submodules-path "~/.emacs.d/submodules/")
; add various load paths
(add-to-list 'load-path emacs-submodules-path)

; ====================
; Configurations
; ====================
;; Separate custom file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file 'noerror)

;; Make *Messages* store more messages
(setq message-log-max t)

;;Line numbers
(global-linum-mode 1)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; 4 spaces offset
(setq-default tab-width 4)

;; Delete selections
(delete-selection-mode t)

;; Show time in modeline
(setq display-time-day-and-date t
 display-time-24hr-format t)
(display-time)

;;Colors
(set-face-background 'modeline "DarkViolet")
(set-face-foreground 'modeline "honeydew2")
;;Set font to Inconsolata
(set-frame-font "Inconsolata:pixelsize=14:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")
;; Color settings
(set-background-color "Black")
(set-foreground-color "honeydew2")
(set-cursor-color "DarkViolet")
(global-font-lock-mode 3)

;;Remove UI (toolbar, scrollbar, menu)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;Remove splash screen
(setq inhibit-splash-screen t)
;;Change scratch message
(setq initial-scratch-message "Welcome to emacs. Use the force and achieve your goals, young internet ninja.")

;;Enable copy outside emacs
(setq x-select-enable-clipboard t)

;; Smoother scrolling, scroll one line at a time (less "jumpy" than default)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time

; ====================
; Keybindings
; ====================

;;Change 'add new todo'-shortcut
(global-set-key "\C-c\r" 'org-insert-todo-heading)

;; Bind alt-x => ctrl-x
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)

;;Kill entire word rather than letter-by-letter
(global-set-key "\C-w" 'backward-kill-word)
;;(global-set-key "\C-x\C-k" 'kill-region)
;;(global-set-key "\C-c\C-k" 'kill-region)

;;Paste from clipboard
(global-set-key "\C-x\C-y" 'clipboard-yank)
(global-set-key "\C-x\C-w" 'clipboard-kill-region)

; =====================
; Modes
; =====================

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
  (add-hook 'coffee-mode-hook '(lambda () (coffee-cos-mode t)))
)
(add-hook 'coffee-mode-hook
  '(lambda() (coffee-custom)))

;; yaml mode
(defvar yaml-path (concat emacs-submodules-path "yaml-mode/"))
(add-to-list 'load-path yaml-path)
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

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
(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))

;; pony-mode
(defvar pony-path (concat emacs-submodules-path "pony-mode/"))
(add-to-list 'load-path (concat pony-path "src"))
(yas/load-directory (concat pony-path "snippets"))
(require 'pony-mode)

;; ecb
(defvar ecb-path (concat emacs-submodules-path "ecb/"))
(add-to-list 'load-path ecb-path)
(require 'ecb)