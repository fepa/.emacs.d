(use-package yasnippet
  :commands (yas-global-mode 1)
  )

(use-package yaml-mode
  :mode "\\.yml$"
  :mode "\\.yaml$")

(use-package nyan-mode
:commands (nyan-mode))

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

;;(autoload 'markdown-mode "markdown-mode.el" "Major mode for editing Markdown files" t)
;;(setq auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist))
;;(add-hook 'markdown-mode-hook (lambda () (variable-pitch-mode t)))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

(use-package magit)
(global-set-key "\C-c\C-g" 'magit-status)

(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-ä") 'mc/mark-next-like-this)
(global-set-key (kbd "C-å") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-ö") 'mc/mark-all-like-this)

(use-package git-gutter-fringe)
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

(winner-mode)
(put 'upcase-region 'disabled nil)

;; Fixes typing accent characters
(use-package iso-transl)

(use-package flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq flycheck-checkers (delq 'emacs-lisp-checkdoc flycheck-checkers))
(setq flycheck-checkers (delq 'html-tidy flycheck-checkers))
(setq flycheck-standard-error-navigation nil)

(global-flycheck-mode t)

;; flycheck errors on a tooltip
(when (display-graphic-p (selected-frame))
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))))

(use-package web-mode
  :ensure t
  :mode (("\\.erb\\'" . web-mode)
         ("\\.html?\\'" . web-mode)
         ("\\.j2\\'" . web-mode)
         ("\\.jinja2\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.ejs\\'" . web-mode))
  :config (progn
            (setq web-mode-markup-indent-offset 2
                  web-mode-css-indent-offset 2
                  web-mode-code-indent-offset 2
                  web-mode-script-padding 2
                  web-mode-style-padding 2
                  )))

(setq python-indent-offset 4)
(add-hook 'python-mode-hook
          (function (lambda ()
                      (setq indent-tabs-mode nil
                            tab-width 4))))

;; Zeal
(global-set-key "\C-cd" 'zeal-at-point)

;; Disable encoding comment insertion in ruby
(setq ruby-insert-encoding-magic-comment nil)

(defun gtd ()
   (interactive)
   (find-file "~/Dropbox/org/inbox.org")
   (split-window-right)
   (find-file "~/Dropbox/org/projects.org")
 )

(setq ring-bell-function 'ignore)
