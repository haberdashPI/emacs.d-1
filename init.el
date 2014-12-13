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

(quelpa 'benchmark-init)
(require 'benchmark-init)

(quelpa 'use-package)
(require 'use-package)
(setq use-package-idle-interval 0.5)

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keymap.el")
(load "~/.emacs.d/functions.el")

(quelpa 'ansible-doc)
(use-package ansible-doc
  :mode "\\.yml\\'"
  :init (add-hook 'yaml-mode-hook #'ansible-doc-mode))

(quelpa 'buffer-move)
(use-package buffer-move
  :bind (
         ("C-M-h" . buf-move-left)
         ("C-M-j" . buf-move-down)
         ("C-M-k" . buf-move-up)
         ("C-M-l" . buf-move-right)))

(quelpa 'bundler)
(use-package bundler
  :commands (bundle-check bundle-console bundle-install bundle-open bundle-update))

(quelpa 'coffee-mode)
(use-package coffee-mode
  :mode "\\.coffee\\'")

(quelpa 'company)
(use-package company
  :commands (global-company-mode)
  :idle (global-company-mode)
  :idle-priority 2
  :config
  (progn
    (setq company-idle-delay 0.1)
    (define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)))

(quelpa 'dash-at-point)
(use-package dash-at-point
  :commands (dash-at-point dash-at-point-with-docset)
  :bind (("C-c d" . dash-at-point)
         ("C-c e" . dash-at-point-with-docset))
  :config
  (progn
    (add-to-list 'dash-at-point-mode-alist '(ruby-mode . "ruby"))
    (add-to-list 'dash-at-point-mode-alist '(lisp-mode . "lisp"))
    (add-to-list 'dash-at-point-mode-alist '(emacs-lisp-mode . "elisp"))))

(quelpa 'dired+)
(use-package dired+
  :commands (dired-jump)
  :bind (("C-x C-j" . dired-jump)))

(quelpa 'dockerfile-mode)
(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :config (add-hook 'dockerfile-mode-hook (lambda () (setq require-final-newline nil))))

(quelpa 'eldoc)
(use-package eldoc
  :commands (eldoc-mode)
  :init (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode))

(quelpa 'emmet-mode)
(use-package emmet-mode
  :commands (emmet-mode)
  :config
  (add-hook 'emmet-mode-hook (lambda ()
                               (setq emmet-preview-default nil)
                               (setq emmet-indentation 2)))
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)))

(quelpa 'expand-region)
(use-package expand-region
  :commands (er/expand-region)
  :bind ("C-=" . er/expand-region))

(quelpa 'ethan-wspace)
(use-package ethan-wspace
  :commands (global-ethan-wspace-mode)
  :idle-priority 10
  :idle (global-ethan-wspace-mode))

(quelpa 'evil)
(quelpa 'evil-jumper)
(quelpa 'evil-leader)
(quelpa 'evil-matchit)
(quelpa 'evil-numbers)
(quelpa 'evil-surround)
(use-package evil
  :idle-priority 1
  :commands (evil-mode)
  :idle (evil-mode 1)
  :pre-load (setq evil-toggle-key "M-V")
  :config
  (progn
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
     (add-to-list 'evil-insert-state-modes 'git-commit-mode)

     (use-package evil-jumper
       :bind (("C-o" . evil-jumper/backward)
              ("C-i" . evil-jumper/forward)))

     (use-package evil-leader
       :config
       (progn
         (global-evil-leader-mode)
         (evil-leader/set-leader ",")

         (evil-leader/set-key
           "." 'helm-projectile-find-file
           "b" 'helm-projectile-switch-to-buffer
           "c" 'comment-dwim
           "f" 'antonio-helm-ag
           "o" 'helm-semantic-or-imenu
           "s" 'helm-swoop
           "S" 'helm-multi-swoop
           "r" 'ruby-test-run-at-point
           "R" 'ruby-test-run
           "p" 'helm-show-kill-ring
           "t" 'helm-gtags-select)))

     (use-package evil-matchit
       :commands (evil-matchit-mode)
       :config (add-hook 'web-mode-hook 'evil-matchit-mode))

     (use-package evil-numbers
       :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
       :init
       (progn
         (define-key evil-normal-state-map (kbd "C-x C-a") 'evil-numbers/inc-at-pt)
         (define-key evil-normal-state-map (kbd "C-x C-x") 'evil-numbers/dec-at-pt)))

     (use-package evil-surround
       :config (global-evil-surround-mode 1))))

(quelpa 'go-mode)
(use-package go-mode
  :mode "\\.go\\'"
  :init (add-hook 'go-mode-hook (lambda () (setq ethan-wspace-errors (remove 'tabs ethan-wspace-errors))))
  :config (add-hook 'before-save-hook 'gofmt-before-save))

(quelpa 'fic-mode)
(use-package fic-mode
  :commands fic-mode
  :init (add-hook 'prog-mode-hook 'fic-mode))

(quelpa 'flycheck)
(use-package flycheck
  :init
  (progn
    (add-hook 'ruby-mode-hook 'flycheck-mode)
    (add-hook 'go-mode-hook 'flycheck-mode)
    (add-hook 'puppet-mode-hook 'flycheck-mode))
  :commands (flycheck-mode))

(quelpa 'haml-mode)
(use-package haml-mode
  :mode ("\\.haml\\'" . haml-mode))

(quelpa 'helm)
(quelpa 'helm-ag)
(quelpa 'helm-gtags)
(quelpa 'helm-projectile)
(quelpa 'helm-swoop)
(use-package helm
  :bind (("M-x" . helm-M-x))
  :commands (helm-M-x
             helm-projectile-find-file
             helm-projectile-switch-to-buffer
             antonio-helm-ag
             helm-imenu
             helm-swoop
             helm-multi-swoop
             helm-show-kill-ring
             helm-gtags-select)
  :config
  (progn
    (use-package helm-ag
      :commands (helm-ag))

    (use-package helm-gtags
      :commands (helm-gtags-select helm-gtags-find-pattern helm-gtags-find-tag))

    (use-package helm-projectile
      :config (setq projectile-switch-project-action 'helm-projectile))

    (use-package helm-swoop
      :commands (helm-swoop helm-multi-swoop)
      :config (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-evil-search))))

(quelpa 'json-mode)
(use-package json-mode
  :mode "\\.json\\'")

(quelpa 'linum)
(use-package linum
  :init (global-linum-mode t)
  )

(quelpa 'lua-mode)
(use-package lua-mode
  :config (setq lua-indent-level 2)
  :mode "\\.lua\\'")

(quelpa 'magit)
(quelpa 'magit-gh-pulls)
(use-package magit
  :commands (magit-status)
  :bind (("C-x g" . magit-status))
  :config
  (progn
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it (delete-other-windows))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
    (define-key magit-status-mode-map (kbd "K") 'magit-discard-item)
    (define-key magit-status-mode-map (kbd "j") 'magit-goto-next-section)
    (define-key magit-status-mode-map (kbd "k") 'magit-goto-previous-section)

    (use-package magit-gh-pulls)))

(quelpa 'markdown-mode)
(use-package markdown-mode
  :commands (markdown-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(quelpa 'org-plus-contrib)
(quelpa 'org-pomodoro)
(quelpa 'org-trello)
(use-package org
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

    (define-key global-map "\C-cc" (lambda () (interactive) (org-capture nil "t")))

    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (define-key org-agenda-mode-map "j" 'evil-next-line)
                (define-key org-agenda-mode-map "\C-j" 'org-agenda-goto-date)
                (define-key org-agenda-mode-map "k" 'evil-previous-line)))

    (org-clock-persistence-insinuate)

    (add-hook 'org-mode-hook (lambda () (setq evil-auto-indent nil)))

    (require 'org-habit)
    (require 'org-pomodoro)
    (require 'org-trello))
  :init
  (progn
    (setq org-directory "~/Dropbox/org")
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    (setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/")))

(quelpa 'popwin)
(use-package popwin
  :config
  (progn
    (popwin-mode)
    (setq popwin:popup-window-height 30)
    (push '("*" :regexp t :height 30) popwin:special-display-config)
    (push '("*helm" :regexp t :height 30) popwin:special-display-config)
    (push '("*compilation" :regexp t :height 50) popwin:special-display-config)
    (push '("*Bundler" :regexp t :height 50) popwin:special-display-config)))

(quelpa 'puppet-mode)
(use-package puppet-mode
  :commands (puppet-mode)
  :mode "\\.pp\\'"
  :config (evil-define-key 'insert puppet-mode-map (kbd "C-.") 'antonio-insert-hashrocket))

(quelpa 'perspective)
(use-package perspective
  :init (persp-mode))

(quelpa 'projectile)
(use-package projectile
  :commands (projectile-switch-project
             projectile-persp-switch-project)
  :bind (("C-c p p" . projectile-switch-project))
  :config
  (progn
    (setq projectile-remember-window-configs t)
    (projectile-global-mode)))

(quelpa '(rotate-text :fetcher github :repo "nschum/rotate-text.el"))
(use-package rotate-text
  :commands (rotate-text)
  :bind (("C-x C-t" . rotate-text))
  :config
  (progn
    (setq rotate-text-words '(("width" "height") ("left" "right" "top" "bottom") ("true" "false") ("assert" "refute")))))

(quelpa 'ruby-mode)
(use-package ruby-mode
  :commands (ruby-mode)
  :mode "\\.rb\\'"
  :config
  (progn
    (use-package robe
      :config (push 'company-robe company-backends))

    (use-package ruby-additional)

    (use-package ruby-block
      :config (ruby-block-mode))

    (use-package ruby-electric)

    (use-package ruby-hash-syntax
      :config (bind-key (kbd "C-c h")  'antonio-ruby-toggle-hash-syntax ruby-mode-map))

    (use-package ruby-test-mode)

    (use-package ruby-tools
      :config
      (progn
        (bind-key (kbd "C-c :")  'ruby-tools-to-symbol ruby-mode-map)
        (bind-key (kbd "C-c '")  'ruby-tools-to-single-quote-string ruby-mode-map)
        (bind-key (kbd "C-c \"") 'ruby-tools-to-double-quote-string ruby-mode-map)
        ))

    (bind-key (kbd "C-x l") 'antonio-ruby-spec-var-to-let ruby-mode-map)
    (evil-define-key 'insert ruby-mode-map (kbd "C-.") 'antonio-insert-hashrocket)
    (add-hook 'ruby-mode-hook 'ruby-electric-mode)
    (add-hook 'ruby-mode-hook (lambda () (setq require-final-newline nil))))
  :mode (("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
         ("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(quelpa 'scss-mode)
(use-package scss-mode
  :mode "\\.scss\\'"
  :init
  (progn
    (setq scss-compile-at-save nil)
    (setq css-indent-offset 2)))

(quelpa 'smart-mode-line)
(use-package smart-mode-line
  :commands (sml/setup)
  :idle (progn
            (sml/setup)
            (sml/apply-theme 'respectful)))

(quelpa 'smartparens)
(use-package smartparens
  :idle-priority 2
  :idle (smartparens-global-mode)
  :commands (smartparens-global-mode)
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode)
    (show-smartparens-global-mode)
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
    (sp-pair "{" "}" :post-handlers '(("||\n[i]" "RET"
                                       "| " "SPC")))
    (sp-pair "(" ")" :post-handlers '(("| " "SPC")))))

(quelpa 'undo-tree)
(use-package undo-tree
  :commands (undo-tree-visualize undo-tree-undo undo-tree-redo)
  :config
  (progn
  (setq undo-tree-history-directory-alist (quote (("." . "~/.undo/"))))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode)))

(quelpa 'uniquify)
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(quelpa 'web-mode)
(use-package web-mode
  :mode "\\.\\(erb\\|html?\\)\\'"
  :commands (web-mode)
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-enable-auto-pairing nil)))

(quelpa 'workgroups2)
(use-package workgroups2
  :commands (workgroups-mode antonio-save-perspective antonio-load-perspective)
  :config
  (progn
    (setq wg-prefix-key (kbd "C-c z"))
    (setq wg-session-load-on-start nil)
    (setq wg-session-file "~/.emacs.d/.emacs_workgroups")
    (setq wg-emacs-exit-save-behavior nil)
    (setq wg-workgroups-mode-exit-save-behavior nil)
    (setq wg-mode-line-display-on nil)
    (setq wg-flag-modified nil))
  :bind (("C-x x S" . antonio-save-perspective)
         ("C-x x l" . antonio-load-perspective)))

(quelpa 'yaml-mode)
(use-package yaml-mode
  :commands (yaml-mode)
  :mode "\\.yml\\'")

(quelpa 'yafolding)
(use-package yafolding
  :commands (yafolding-mode)
  :init (add-hook 'prog-mode-hook 'yafolding-mode))

(quelpa 'yasnippet)
(use-package yasnippet
  :config
  (progn
    (defun check-expansion ()
      (save-excursion
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil)))))

    (defun do-yas-expand ()
      (let ((yas/fallback-behavior 'return-nil))
        (yas/expand)))

    (defun tab-indent-or-complete ()
      (interactive)
      (if (minibufferp)
          (minibuffer-complete)
        (if (or (not yas/minor-mode)
                (null (do-yas-expand)))
            (if (check-expansion)
                (company-complete-common)
              (indent-for-tab-command))))))
  :idle (yas-global-mode)
  :commands (yas-global-mode))

(quelpa '(zoom-window :fetcher github :repo "syohex/emacs-zoom-window"))
(use-package zoom-window
  :commands (zoom-window-zoom)
  :bind (("C-x z" . zoom-window-zoom))
  :config (setq zoom-window-mode-line-color "#202020"))
