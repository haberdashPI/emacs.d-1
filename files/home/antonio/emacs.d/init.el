;; do not load outdated compiled files
(setq load-prefer-newer t)

(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(setq quelpa-update-melpa-p nil)
(unless (require 'quelpa nil t)
  (with-temp-buffer
    (url-insert-file-contents "http://raw.github.com/quelpa/quelpa/master/bootstrap.el")
    (eval-buffer)))

(if (getenv "EMACS_DEBUG_INIT")
    (progn
      (require 'benchmark-init)))

(quelpa 'auto-compile)
(require 'auto-compile)
(setq auto-compile-display-buffer nil)
(setq auto-compile-mode-line-counter t)
(auto-compile-on-load-mode 1)
(auto-compile-on-save-mode 1)

(quelpa 'use-package)
(eval-when-compile
  (require 'use-package))
(require 'bind-key)

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keymap.el")
(load "~/.emacs.d/functions.el")

(use-package align
  :commands (align align-regexp)
  :config
  (add-to-list 'align-rules-list
               '(hash-fat-arrow-pairs
                 (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
                 (repeat . t)
                 (modes  . '(cperl-mode ruby-mode))))
  (add-to-list 'align-rules-list
               '(ruby-arguments
                 (regexp . ",\\(\\s-*\\)[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-mode))))
  (add-to-list 'align-rules-list
               '(ruby-hash-colon-pairs
                 (regexp . "[A-Za-z0-9_]:\\(\\s-*\\)[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-mode))))
  (add-to-list 'align-rules-list
               '(ruby-assignment-literal
                 (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
                 (repeat . t)
                 (modes  . '(ruby-mode)))))

(use-package aggressive-indent
  :ensure t
  :init
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'puppet-mode)
  (add-to-list 'aggressive-indent-excluded-modes 'coffee-mode))

(use-package buffer-move
  :ensure t
  :bind (
         ("C-M-h" . buf-move-left)
         ("C-M-j" . buf-move-down)
         ("C-M-k" . buf-move-up)
         ("C-M-l" . buf-move-right)))

(use-package bundler
  :ensure t
  :commands (bundle-check bundle-console bundle-install bundle-open bundle-update))

(use-package coffee-mode
  :ensure t
  :mode "\\.coffee\\'")

(use-package company
  :ensure t
  :commands (global-company-mode)
  :init (global-company-mode)
  :config
  (progn
    (defvar-local company-fci-mode-on-p nil)

    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))

    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))

    (defun antonio-company-show-doc-buffer ()
      "Temporarily show the documentation buffer for the selection."
      (interactive)
      (let* ((selected (nth company-selection company-candidates))
             (doc-buffer (or (company-call-backend
                              'doc-buffer selected)
                             (error "No documentation available"))))
        (with-current-buffer doc-buffer
          (goto-char (point-min)))
        (display-buffer doc-buffer t)))
    (define-key company-active-map (kbd "C-h") #'antonio-company-show-doc-buffer)

    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
    (setq company-idle-delay 0.1)
    (use-package company-quickhelp
      :ensure t
      :commands (company-quickhelp-mode)
      :init (company-quickhelp-mode 1))

    (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
    (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort)))

(use-package dash-at-point
  :ensure t
  :commands (dash-at-point dash-at-point-with-docset)
  :bind (("C-c d" . dash-at-point)
         ("C-c e" . dash-at-point-with-docset))
  :config
  (progn
    (add-to-list 'dash-at-point-mode-alist '(ruby-mode . "ruby"))
    (add-to-list 'dash-at-point-mode-alist '(lisp-mode . "lisp"))
    (add-to-list 'dash-at-point-mode-alist '(emacs-lisp-mode . "elisp"))))

(use-package dired+
  :ensure t
  :commands (dired-jump)
  :bind (("C-x C-j" . dired-jump)))

(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'"
  :config (add-hook 'dockerfile-mode-hook (lambda () (setq require-final-newline nil))))

(use-package eldoc
  :ensure t
  :commands (eldoc-mode)
  :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

(use-package emmet-mode
  :ensure t
  :commands (emmet-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda ()
                               (define-key emmet-mode-keymap (kbd "C-c w") 'web-mode-element-wrap)
                               (setq emmet-preview-default nil)
                               (setq emmet-indentation 2)))
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)))

(use-package expand-region
  :ensure t
  :commands (er/expand-region)
  :bind ("C-=" . er/expand-region))

(use-package ethan-wspace
  :ensure t
  :commands (global-ethan-wspace-mode)
  :config
  (progn
    (defun remove-tabs-from-ethan-wspace-errors ()
      (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors)))
    (add-hook 'go-mode-hook 'remove-tabs-from-ethan-wspace-errors))
  :init (global-ethan-wspace-mode))

(use-package evil
  :ensure t
  :bind (("C-M-o" . evil-jump-backward)
         ("C-M-i" . evil-jump-forward))
  :commands (evil-mode)
  :init
  (setq evil-toggle-key "M-V"
        evil-want-C-i-jump nil)
  (evil-mode 1)
  :config
  (setq evil-ex-substitute-global t)

  (define-key evil-insert-state-map [remap newline] 'newline)
  (define-key evil-insert-state-map [remap newline-and-indent] 'newline-and-indent)
  (evil-define-key 'normal global-map (kbd "%") 'ck/dispatch-goto-matching)
  (evil-define-key 'normal global-map (kbd "SPC") 'evil-search-forward)
  (evil-define-key 'visual global-map (kbd "SPC") 'evil-search-forward)
  (evil-define-key 'normal global-map (kbd "j") 'evil-next-visual-line)
  (evil-define-key 'normal global-map (kbd "k") 'evil-previous-visual-line)
  (evil-define-key 'insert global-map (kbd "TAB") 'tab-indent-or-complete)
  (evil-define-key 'insert global-map (kbd "M-RET") 'antonio-open-newline)

  (add-to-list 'evil-emacs-state-modes 'git-rebase-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-status-mode)
  (add-to-list 'evil-emacs-state-modes 'paradox-menu-mode)

  (add-to-list 'evil-insert-state-modes 'git-commit-mode)

  (use-package evil-leader
    :ensure t
    :config
    (progn
      (global-evil-leader-mode)
      (evil-leader/set-leader ",")

      (evil-leader/set-key
        "." 'helm-projectile
        "b" 'blame
        "c" 'evilnc-comment-or-uncomment-lines
        "C" 'evilnc-comment-or-uncomment-paragraphs
        "f" 'antonio-ag-in-project
        "g" 'git-messenger:popup-message
        "h" 'helm-resume
        "o" 'helm-semantic-or-imenu
        "s" 'helm-swoop
        "S" 'helm-multi-swoop
        "r" 'ruby-test-run-at-point
        "R" 'ruby-test-run
        "p" 'helm-show-kill-ring
        "t" 'helm-gtags-select)))

  (use-package evil-matchit
    :ensure t
    :commands (evil-matchit-mode)
    :config (add-hook 'web-mode-hook 'evil-matchit-mode))

  (use-package evil-nerd-commenter
    :ensure t
    :commands (evilnc-comment-or-uncomment-lines))

  (use-package evil-numbers
    :ensure t
    :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
    :init
    (progn
      (define-key evil-normal-state-map (kbd "C-x C-a") 'evil-numbers/inc-at-pt)
      (define-key evil-normal-state-map (kbd "C-x C-x") 'evil-numbers/dec-at-pt)))

  (use-package evil-surround
    :ensure t
    :config (global-evil-surround-mode 1)))

(use-package hydra
  :ensure t
  :commands (defhydra))

(defun antonio-git-messenger-before-popup-hook (args)
  (turn-off-fci-mode)
  (linum-mode 0))

(defun antonio-git-messenger-after-popup-hook (args)
  (turn-on-fci-mode)
  (linum-mode 1))

(use-package git-messenger
  :ensure t
  :config
  (setq git-messenger:show-detail t)
  (add-hook 'git-messenger:before-popup-hook 'antonio-git-messenger-before-popup-hook)
  (add-hook 'git-messenger:after-popup-hook 'antonio-git-messenger-after-popup-hook)
  :commands (git-messenger:popup-message))

(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :init (add-hook 'go-mode-hook (lambda () (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors))))
  :config (add-hook 'before-save-hook 'gofmt-before-save))

(use-package fic-mode
  :ensure t
  :commands (fic-mode)
  :init (add-hook 'prog-mode-hook 'fic-mode))

(use-package fill-column-indicator
  :ensure t
  :commands (fci-mode)
  :init
  (progn
    (add-hook 'prog-mode-hook 'fci-mode)
    (defvar sanityinc/fci-mode-suppressed nil)
    (defadvice popup-create (before suppress-fci-mode activate)
      "Suspend fci-mode while popups are visible"
      (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
      (when fci-mode
        (turn-off-fci-mode)))
    (defadvice popup-delete (after restore-fci-mode activate)
      "Restore fci-mode when all popups have closed"
      (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
        (setq sanityinc/fci-mode-suppressed nil)
        (turn-on-fci-mode)))
    (setq fci-rule-color "#333333"
          fci-rule-column 80)))

(use-package flycheck
  :ensure t
  :init
  (progn
    (add-hook 'ruby-mode-hook 'flycheck-mode)
    (add-hook 'go-mode-hook 'flycheck-mode)
    (add-hook 'puppet-mode-hook 'flycheck-mode))
  :commands (flycheck-mode))

(use-package ggtags
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'ggtags-mode))

(use-package haml-mode
  :ensure t
  :mode ("\\.haml\\'" . haml-mode))

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x))
  :commands (helm-M-x
             helm-projectile-find-file
             helm-projectile-switch-to-buffer
             helm-mode
             helm-swoop
             helm-multi-swoop
             helm-semantic-or-imenu
             helm-show-kill-ring
             helm-gtags-select)
  :init
  (setq helm-completing-read-handlers-alist ())
  :config
  (progn
    (add-to-list 'helm-completing-read-handlers-alist '(cd . ido))
    (add-to-list 'helm-completing-read-handlers-alist '(ibuffer-find-file . ido))
    (add-to-list 'helm-completing-read-handlers-alist '(switch-to-buffer . ido))
    (add-to-list 'helm-completing-read-handlers-alist '(find-file . ido))
    (add-to-list 'helm-completing-read-handlers-alist '(dired-do-rename . ido))
    (add-to-list 'helm-completing-read-handlers-alist '(persp-switch . ido))

    (setq helm-imenu-execute-action-at-once-if-one nil)

    (use-package helm-ag
      :ensure t
      :commands (helm-ag))

    (use-package helm-gtags
      :ensure t
      :commands (helm-gtags-select helm-gtags-find-pattern helm-gtags-find-tag))

    (use-package helm-projectile
      :ensure t
      :config (setq projectile-switch-project-action 'helm-projectile))

    (use-package helm-swoop
      :ensure t
      :commands (helm-swoop helm-multi-swoop)
      :config
      (progn
        ;; disable pre-input
        (setq helm-swoop-pre-input-function #'ignore)
        (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-evil-search)))

    (helm-mode 1)))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")

(use-package linum
  :ensure t
  :init (global-linum-mode t)
  )

(use-package lua-mode
  :ensure t
  :config (setq lua-indent-level 2)
  :mode "\\.lua\\'")

(use-package magit
  :ensure t
  :commands (magit-status)
  :bind (("C-x g" . magit-status)
         ("C-x C-q" . magit-quit-session))
  :config
  (progn
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register (intern (format "magit-fullscreen-%s" (persp-name persp-curr))))
      ad-do-it (delete-other-windows))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register (intern (format "magit-fullscreen-%s" (persp-name persp-curr)))))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
    (define-key magit-status-mode-map (kbd "K") 'magit-discard-item)
    (define-key magit-status-mode-map (kbd "j") 'next-line)
    (define-key magit-status-mode-map (kbd "k") 'previous-line)
    (define-key magit-diff-mode-map (kbd "j") 'next-line)
    (define-key magit-diff-mode-map (kbd "k") 'previous-line)

    (defun antonio/visit-pull-request-url ()
      "Visit the current branch's PR on Github."
      (interactive)
      (browse-url
       (let ((remote-branch (magit-get "remote" (magit-get-current-remote) "url")))
         (format "https://github.com/%s/compare/%s"
                 (if (string-prefix-p "http" remote-branch)
                     (replace-regexp-in-string ".+github\.com/\\(\\w+/\\w+\\).*" "\\1" remote-branch)
                   (replace-regexp-in-string "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1" remote-branch))
                 (magit-get-current-branch)))))

    (eval-after-load 'magit
      '(define-key magit-mode-map "V"
         #'antonio/visit-pull-request-url))))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package org
  :ensure t
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
    (auto-fill-mode)

    (setq org-log-done t
          org-todo-keywords '((sequence "TODO(t)" "FUTURE(f)" "WAIT(w@)" "|" "CANCELED(c@)" "DONE(d)")))

    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)

    (setq org-agenda-files '("~/Dropbox/org/plan.org"
                             "~/Dropbox/org/habits.org"
                             "~/Dropbox/org/github.org"
                             "~/Dropbox/org/readlist.org"
                             "~/Dropbox/org/talks.org"
                             "~/Dropbox/org/google.org")
          org-agenda-window-setup 'current-window
          org-default-notes-file (concat org-directory "/plan.org")
          org-archive-location "~/Dropbox/org/archives/%s::datetree/"
          org-agenda-start-on-weekday nil
          org-tag-alist '(("COMPUTER" . ?c)
                          (:startgroup . nil)
                          ("HOME" . ?h) ("OFFICE" . ?o)
                          (:endgroup . nil)
                          ("READING" . ?r)
                          ("FUTURE" . ?f))
          org-completion-use-ido t
          org-refile-targets '((org-agenda-files :maxlevel . 9))
          org-clock-persist 'history
          org-habit-following-days 1
          org-habit-graph-column 100
          org-habit-show-habits-only-for-today t

          org-capture-templates '(("t" "Task" entry (file+headline "~/Dropbox/org/plan.org" "INBOX")
                                   "* TODO %?\n")))

    (add-hook 'org-capture-mode-hook 'delete-other-windows)
    (add-hook 'org-capture-mode-hook 'evil-insert-state)

    (define-key global-map "\C-cc" (lambda () (interactive) (org-capture nil "t")))

    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (define-key org-agenda-mode-map "j" 'evil-next-line)
                (define-key org-agenda-mode-map "\C-j" 'org-agenda-goto-date)
                (define-key org-agenda-mode-map "k" 'evil-previous-line)))

    (org-clock-persistence-insinuate)

    (add-hook 'org-mode-hook
              (lambda ()
                ;; stop the annoying 'still has buffer messages'
                (remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)))

    (use-package org-habit)
    (use-package org-trello
      :ensure t))
  :init
  (progn
    (setq org-directory "~/Dropbox/org")
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    (setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/")))

(use-package paradox
  :ensure t
  :bind (("C-x C-u" . paradox-upgrade-packages))
  :config (setq paradox-execute-asynchronously t)
  :commands (paradox-upgrade-packages paradox-list-packages))

(use-package perspective
  :ensure t
  :commands (persp-mode)
  :init (persp-mode))

(use-package popwin
  :ensure t
  :config
  (progn
    (popwin-mode)
    (setq popwin:popup-window-height 30)
    (push '("*" :regexp t :height 30) popwin:special-display-config)
    (push '("*helm" :regexp t :height 30) popwin:special-display-config)
    (push '("*compilation" :regexp t :height 50) popwin:special-display-config)
    (push '("*Bundler" :regexp t :height 50) popwin:special-display-config)))

(use-package powerline
  :ensure t
  :commands (powerline-default-theme powerline-center-evil-theme)
  :init
  (powerline-center-evil-theme))

(use-package projectile
  :ensure t
  :commands (projectile-switch-project
             projectile-persp-switch-project)
  :bind (("C-c p p" . projectile-switch-project))
  :config
  (progn
    (setq projectile-remember-window-configs t)
    (projectile-global-mode)))

(use-package puppet-mode
  :ensure t
  :commands (puppet-mode)
  :mode "\\.pp\\'"
  :config (evil-define-key 'insert puppet-mode-map (kbd "C-.") 'antonio-insert-hashrocket))

(quelpa '(rotate-text :fetcher github :repo "nschum/rotate-text.el"))
(use-package rotate-text
  :commands (rotate-text)
  :bind (("C-x C-t" . rotate-text))
  :config
  (progn
    (setq rotate-text-words '(("width" "height") ("left" "right" "top" "bottom") ("true" "false") ("assert" "refute")))))

(use-package ruby-mode
  :ensure t
  :commands (ruby-mode)
  :mode "\\.rb\\'"
  :config
  (progn
    ;; (use-package robe
    ;;   :config
    ;;   (progn
    ;;     (add-hook 'ruby-mode-hook 'robe-mode)
    ;;     (inf-ruby-console-auto)
    ;;     (robe-start)
    ;;     (push 'company-robe company-backends)))

    (use-package ruby-additional
      :ensure t)

    (use-package ruby-block
      :ensure t
      :config (ruby-block-mode))

    (use-package ruby-hash-syntax
      :ensure t
      :config (bind-key (kbd "C-c h")  'antonio-ruby-toggle-hash-syntax ruby-mode-map))

    (use-package ruby-test-mode
      :ensure t)

    (use-package ruby-tools
      :ensure t
      :config
      (progn
        (bind-key (kbd "C-c :")  'ruby-tools-to-symbol ruby-mode-map)
        (bind-key (kbd "C-c '")  'ruby-tools-to-single-quote-string ruby-mode-map)
        (bind-key (kbd "C-c \"") 'ruby-tools-to-double-quote-string ruby-mode-map)
        ))

    (bind-key (kbd "C-x l") 'antonio-ruby-spec-var-to-let ruby-mode-map)
    (evil-define-key 'insert ruby-mode-map (kbd "C-.") 'antonio-insert-hashrocket)
    (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)
    (add-hook 'ruby-mode-hook (lambda () (setq require-final-newline nil))))
  :mode (("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
         ("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package scss-mode
  :ensure t
  :mode "\\.scss\\'"
  :init
  (progn
    (setq scss-compile-at-save nil)
    (setq css-indent-offset 2)))

(use-package slim-mode
  :ensure t
  :mode (("\\.slim\\'" . slim-mode)
         ("\\.slim.html\\'" . slim-mode)))

(use-package smartparens
  :ensure t
  :commands (smartparens-global-mode smartparens-mode)
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode)
  :config
  (require 'smartparens-config)
  (setq sp-autoescape-string-quote nil)

  (defun space-and-space-on-each-side (&rest _ignored)
    (save-excursion
      (insert "  ")))

  (defun space-on-each-side (&rest _ignored)
    (when (or (looking-back "=")
              (looking-back "#"))
      (insert " ")
      (save-excursion
        (insert " "))))

  (sp-local-pair 'web-mode "%" "%"
                 :unless '(sp-in-string-or-word-p)
                 :post-handlers '((space-and-space-on-each-side "SPC")
                                  (space-on-each-side "=" "#")))

  (sp-pair "{" "}" :post-handlers '(("||\n[i]" "RET")))
  (sp-pair "(" ")" :post-handlers '(("| " "SPC"))))

(use-package undo-tree
  :ensure t
  :init
  (progn
    ;; (setq undo-tree-history-directory-alist (quote (("." . "~/.undo/"))))
    ;; (setq undo-tree-auto-save-history t)
    (global-undo-tree-mode)))

(use-package uniquify
  :defer t
  :config (setq uniquify-buffer-name-style 'forward))

(use-package web-mode
  :ensure t
  :mode "\\.\\(erb\\|html?\\)\\'"
  :commands (web-mode)
  :config
  (progn
    (add-hook 'web-mode-hook 'turn-off-fci-mode)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-enable-auto-pairing nil)))

(use-package workgroups2
  :ensure t
  :commands (workgroups-mode antonio-save-perspective
                             antonio-load-perspective
                             wg-create-workgroup wg-open-session)
  :config
  (progn
    (setq wg-prefix-key (kbd "C-c z"))
    (setq wg-session-load-on-start nil)
    (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
    (setq wg-emacs-exit-save-behavior nil)
    (setq wg-workgroups-mode-exit-save-behavior nil)
    (setq wg-mode-line-display-on nil)
    (setq wg-flag-modified nil)
    (workgroups-mode 1))
  :bind (("C-x x S" . antonio-save-perspective)
         ("C-x x l" . antonio-load-perspective)))

(use-package yaml-mode
  :ensure t
  :commands (yaml-mode)
  :mode "\\.yml\\'")

(use-package yafolding
  :ensure t
  :commands (yafolding-mode)
  :init (add-hook 'prog-mode-hook 'yafolding-mode))

(use-package yasnippet
  :ensure t
  :init
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  :commands (yas-minor-mode)
  :config
  (yas-reload-all)
  (defun check-expansion ()
    (save-excursion
      (if (looking-at "\\_>") t
        (backward-char 1)
        (if (looking-at "\\.") t
          (backward-char 1)
          (if (looking-at "->") t nil)))))

  (defun do-yas-expand ()
    (let ((yas-fallback-behavior 'return-nil))
      (yas-expand)))

  (defun tab-indent-or-complete ()
    (interactive)
    (if (minibufferp)
        (minibuffer-complete)
      (if (or (not yas-minor-mode)
              (null (do-yas-expand)))
          (if (check-expansion)
              (company-complete-common)
            (indent-for-tab-command))))))

(quelpa '(zoom-window :fetcher github :repo "syohex/emacs-zoom-window"))
(use-package zoom-window
  :commands (zoom-window-zoom)
  :bind (("C-x z" . zoom-window-zoom))
  :config (setq zoom-window-mode-line-color "#202020"))

(if (getenv "EMACS_DEBUG_INIT")
    (benchmark-init/show-durations-tree))
