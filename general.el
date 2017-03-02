;; Separate custom file
(setq custom-file "~/.emacs.d/.emacs-custom.el")
(load custom-file 'noerror)

;; Make *Messages* store more messages
(setq message-log-max 10000)

;;Line & column numbers
(global-linum-mode 1)
(column-number-mode 1)

;; Spaces instead of tabs
(setq-default indent-tabs-mode nil)
;; 4 spaces offset
(setq-default tab-width 4)
;; 2 spaces for javascript and css
(setq js-indent-level 2)

;; Delete selections
(delete-selection-mode t)

;; Remove trailing white space on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Always end file with newline
(setq require-final-newline t)

;; Show time in modeline
(setq display-time-day-and-date t
 display-time-24hr-format t)
(display-time)

;;Enable copy outside emacs
(setq x-select-enable-clipboard t)

;; Smoother scrolling, scroll one line at a time (less "jumpy" than default)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
