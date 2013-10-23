;; Truncate lines
(define-key global-map [f5] 'toggle-truncate-lines)

;; auto indent
(add-hook 'lisp-mode-hook '(lambda ()
      (local-set-key (kbd "RET") 'newline-and-indent)))

;; Bind Fn to M-x
(global-set-key (kbd "<XF86WakeUp>") 'execute-extended-command)

;; Kill entire word rather than letter-by-letter
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-c\C-k" 'kill-region)

;; Paste from clipboard
(global-set-key "\C-x\C-y" 'clipboard-yank)
(global-set-key "\C-x\C-w" 'clipboard-kill-region)

;; Comment region
(global-set-key "\C-c\C-c" 'comment-region)
