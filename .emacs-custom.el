(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-options-version "2.40")
 '(elpy-syntax-check-command "pyflakes")
 '(exec-path
   '("/usr/local/bin" "/usr/bin" "/bin" "/usr/local/games" "/usr/games" "/usr/local/libexec/emacs/26.3/x86_64-pc-linux-gnu" "~/.local/bin"))
 '(fill-column 80)
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".svn" ".git" ".hg" ".bzr" "_MTN" "_darcs" "{arch}" "node_modules" "log" "tmp" ".jhw-cache" "env"))
 '(grep-find-ignored-files
   '(".#*" "*.o" "*~" "*.bin" "*.lbin" "*.so" "*.a" "*.ln" "*.blg" "*.bbl" "*.elc" "*.lof" "*.glo" "*.idx" "*.lot" "*.fmt" "*.tfm" "*.class" "*.fas" "*.lib" "*.mem" "*.x86f" "*.sparcf" "*.dfsl" "*.pfsl" "*.d64fsl" "*.p64fsl" "*.lx64fsl" "*.lx32fsl" "*.dx64fsl" "*.dx32fsl" "*.fx64fsl" "*.fx32fsl" "*.sx64fsl" "*.sx32fsl" "*.wx64fsl" "*.wx32fsl" "*.fasl" "*.ufsl" "*.fsl" "*.dxl" "*.lo" "*.la" "*.gmo" "*.mo" "*.toc" "*.aux" "*.cp" "*.fn" "*.ky" "*.pg" "*.tp" "*.vr" "*.cps" "*.fns" "*.kys" "*.pgs" "*.tps" "*.vrs" "*.pyc" "*.pyo" "development.log"))
 '(indent-tabs-mode nil)
 '(json-reformat:indent-width 2)
 '(line-spacing 3)
 '(org-agenda-files '("~/Dropbox/org/inbox.org" "~/Dropbox/org/reference.org"))
 '(package-selected-packages
   '(vue-html-mode php-mode lsp-mode vue-mode haskell-mode typescript-mode markdown-toc git-gutter-fringe dockerfile-mode docker elpy dumb-jump json-reformat editorconfig anaconda-mode yasnippet-snippets scss-mode jsonnet-mode rjsx-mode react-snippets zeal-at-point yasnippet yaml-mode web-mode multiple-cursors markdown-mode magit flycheck emoji-display ace-jump-mode))
 '(python-shell-interpreter "python3")
 '(safe-local-variable-values
   '((eval progn
           (setq web-mode-script-padding 0)
           (setq web-mode-style-padding 0)
           (when
               (fboundp 'project-setup-hook)
             (project-setup-hook "nordicds/standard-app")))
     (eval progn
           (defun project-command
               (cmd)
             (projectile-with-default-dir
              (projectile-project-root)
              (zeshell-command cmd)))
           (defun project-vagrant-command
               (cmd)
             (project-command
              (concat "vagrant ssh -c 'bash -i -c \"" cmd "\"'")))
           (global-set-key
            [f1]
            (lambda nil
              (interactive)
              (project-command "vagrant up")))
           (global-set-key
            [f2]
            (lambda nil
              (interactive)
              (project-vagrant-command "manager build all")))
           (global-set-key
            [f3]
            (lambda nil
              (interactive)
              (project-vagrant-command "manager test all")))
           (global-set-key
            [f4]
            (lambda nil
              (interactive)
              (project-vagrant-command "manager package all")))
           (global-set-key
            [f7]
            (lambda nil
              (interactive)
              (project-command "vagrant halt")))
           (global-set-key
            [f8]
            (lambda nil
              (interactive)
              (project-command "vagrant destroy"))))
     (org-confirm-babel-evaluate)
     (eval progn
           (add-to-list 'auto-mode-alist
                        '("\\.xml\\'" . web-mode))
           (add-to-list 'auto-mode-alist
                        '("\\.hbs\\'" . web-mode))
           (setq web-mode-engines-alist
                 '(("django" . "\\.\\(xml\\|html\\)\\'")
                   ("ctemplate" . "\\.\\(hbs\\)\\'")))
           (when
               (fboundp 'project-setup-hook)
             (project-setup-hook "nordicds/osign-server")))
     (eval setq create-lockfiles nil)))
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "white smoke"))))
 '(company-scrollbar-fg ((t (:background "slate gray"))))
 '(company-tooltip ((t (:background "steel blue" :foreground "white smoke"))))
 '(company-tooltip-selection ((t (:background "slate gray")))))
