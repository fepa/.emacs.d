;; Colors
(set-face-background 'mode-line "DarkViolet")
(set-face-foreground 'mode-line "honeydew2")
(set-background-color "#121212")
(set-foreground-color "honeydew2")
(set-cursor-color "DarkViolet")
(set-face-background 'region "DarkViolet")

;; Font
(set-frame-font "Ubuntu Mono:pixelsize=14:foundry=unknown:weight=normal:slant=normal:width=normal:spacing=100:scalable=true")
(global-font-lock-mode 3)

;; Remove UI (toolbar, scrollbar, menu)
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;; Remove splash screen
(setq inhibit-splash-screen t)

;; Change scratch message
(setq initial-scratch-message "Welcome to emacs. Use the force and achieve your goals, young internet ninja.")

;; Highlight current line
(global-hl-line-mode 1)
(set-face-background 'highlight "#222222")

;; Parenthesis matching
(show-paren-mode t)
