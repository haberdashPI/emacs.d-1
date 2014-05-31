(require 'cask "~/.emacs.d/cask.el/cask.el")
(cask-initialize)

(require 'benchmark-init)

(require 'use-package)

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keymap.el")
(load "~/.emacs.d/functions.el")

(use-package bundler
  :commands (bundle-check bundle-console bundle-install bundle-open bundle-update))

(use-package coffee-mode
  :mode "\\.coffee\\'")

(use-package company
  :init
  (progn
    (setq company-idle-delay 0.1)
    (define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
    )
  :idle (global-company-mode)
  )

(use-package dash-at-point
  :commands (dash-at-point dash-at-point-with-docset)
  :init
  (progn
    (bind-key "C-c d" 'dash-at-point)
    (bind-key "C-c e" 'dash-at-point-with-docset))
  :config
  (progn
    (add-to-list 'dash-at-point-mode-alist '(ruby-mode . "ruby"))
    (add-to-list 'dash-at-point-mode-alist '(lisp-mode . "lisp"))
    (add-to-list 'dash-at-point-mode-alist '(emacs-lisp-mode . "elisp"))
    ))

(use-package delight
  :commands (antonio-cleanup-mode-line)
  :init
  (progn
    (add-hook 'after-init-hook 'antonio-cleanup-mode-line)))

(use-package dired+
  :defer t)

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :config (add-hook 'dockerfile-mode-hook (lambda () (setq require-final-newline nil)))
  )

(use-package emmet-mode
  :defer t
  :config
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)
    (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
    ))

(use-package expand-region
  :commands er/expand-region
  :init
  (bind-key "C-=" 'er/expand-region)
  )

(use-package ethan-wspace
  :config (global-ethan-wspace-mode))

(use-package evil
  :pre-load
  (progn
    (set 'evil-want-C-i-jump nil)
    (setq evil-toggle-key "M-V"))
  :config
  (progn
    (evil-mode)
    (setq evil-ex-substitute-global t)
    (evil-define-key 'insert global-map (kbd "RET") 'newline-and-indent)
    (evil-define-key 'normal global-map (kbd "%") 'ck/dispatch-goto-matching)
    (evil-define-key 'normal global-map (kbd "SPC") 'evil-search-forward)

    (use-package evil-leader
      :config
      (progn
        (global-evil-leader-mode)
        (evil-leader/set-leader ",")

        (evil-leader/set-key
          "." 'antonio-helm-files
          "b" 'antonio-helm-buffers
          "c" 'comment-dwim
          "f" 'helm-ag
          "o" 'helm-imenu
          "r" 'ruby-test-run-at-point
          "R" 'ruby-test-run
          "y" 'helm-show-kill-ring
          )))

    (use-package evil-matchit
      :config
      (progn
        (add-hook 'web-mode-hook 'evil-matchit-mode)
        ))

    (use-package evil-numbers
      :init
      (progn
        (define-key evil-normal-state-map (kbd "C-x C-a") 'evil-numbers/inc-at-pt)
        (define-key evil-normal-state-map (kbd "C-x C-x") 'evil-numbers/dec-at-pt)))
    ))

(use-package fic-mode
  :defer t
  :commands fic-mode
  :init (add-hook 'prog-mode-hook 'fic-mode))

(use-package flx-ido
  :config
  (progn
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    (setq ido-use-faces nil)
    ))

(use-package flycheck
  :defer t
  :config (add-hook 'ruby-mode-hook 'flycheck-mode))

(use-package ggtags
  :defer t
  :config
  (add-hook 'prog-mode-hook 'ggtags-mode))

(use-package git-gutter-fringe
  :defer t
  :config (global-git-gutter-mode))

(use-package haml-mode
  :mode ("\\.haml\\'" . haml-mode))

(use-package helm
  :config
  (progn
    (helm-mode)
    (use-package helm-ag)
    (use-package helm-projectile)
    (setq projectile-switch-project-action 'helm-projectile)
    ))

(use-package hrb-mode
  :defer t
  :config (progn
            (setq hrb-delay 0)
            (setq hrb-highlight-keyword-face 'show-paren-match-face)
            (setq hrb-highlight-block-face 'highlight)
            (setq hrb-highlight-mode 'keywords)
            (add-hook 'ruby-mode-hook 'hrb-mode)))

(use-package imenu+
  :defer t
  :config (progn
            (imenup-toggle-sort nil)
            )
  )

(use-package json-mode
  :mode "\\.json\\'")

(use-package magit
  :commands magit-status)

(use-package markdown-mode
             :mode (("\\.md\\'" . markdown-mode)
                    ("\\.markdown\\'" . markdown-mode)))

(use-package mo-git-blame
  :commands mo-git-blame
  :init (evil-set-initial-state 'mo-git-blame-mode 'emacs))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
    (setq org-directory "~/Dropbox/org")
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    (setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/")

    (setq org-todo-keywords
        '((sequence "TODO(t)" "FUTURE(f)" "WAIT(w@)" "|" "CANCELED(c@)" "DONE(d)")))

    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cb" 'org-iswitchb)

    (setq org-default-notes-file (concat org-directory "/plan.org"))
    (define-key global-map "\C-cc" 'org-capture)

    (setq org-archive-location "~/Dropbox/org/archives/%s::datetree/")
    (setq org-agenda-start-on-weekday nil)

    (setq org-capture-templates
          '(("t" "Task" entry (file+headline "~/Dropbox/org/plan.org" "INBOX")
             "* TODO %?\n")))

    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (define-key org-agenda-mode-map "j" 'evil-next-line)
                (define-key org-agenda-mode-map "\C-j" 'org-agenda-goto-date)
                (define-key org-agenda-mode-map "k" 'evil-previous-line)
                ))

    (setq org-tag-alist
          '(("COMPUTER" . ?c)
            (:startgroup . nil)
            ("HOME" . ?h) ("OFFICE" . ?o)
            (:endgroup . nil)
            ("READING" . ?r)
            ("FUTURE" . ?f)
            ))

    (setq org-completion-use-ido t)

    (setq org-refile-targets '((nil :maxlevel . 9)))

    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)

    (add-hook 'org-mode-hook
              (lambda ()
                (setq evil-auto-indent nil)
                ))
    ))

(use-package popwin
  :config
  (progn
    (popwin-mode)
    (setq popwin:popup-window-height 30)
    (push '("*" :regexp t :height 30) popwin:special-display-config)
    (push '("*helm" :regexp t :height 30) popwin:special-display-config)
    (push '("*compilation" :regexp t :height 50) popwin:special-display-config)
    (push '("*Bundler" :regexp t :height 50) popwin:special-display-config)))

(use-package powerline
  :config (powerline-center-theme))

(use-package puppet-mode
  :mode "\\.pp\\'")

(use-package projectile
  :config
  (progn
    (setq projectile-remember-window-configs t)
    (use-package projectile-rails)
    (add-hook 'projectile-mode-hook 'projectile-rails-on)
    (projectile-global-mode)
    ))

(use-package rainbow-delimiters
  :defer t
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    ))

(use-package robe
  :mode ("\\.rb\\'" . robe-mode)
  :init
  (progn
    (push 'company-robe company-backends)
    ))

(use-package rotate-text
  :defer t
  :config
  (progn
    (setq rotate-text-words '(("width" "height") ("left" "right" "top" "bottom") ("true" "false")))
    (bind-key (kbd "C-x C-t") 'rotate-text)))

(use-package ruby-additional)

(use-package ruby-block
  :mode ("\\.rb\\'" . ruby-block-mode))

(use-package ruby-electric
  :config
  (add-hook 'ruby-mode-hook 'ruby-electric-mode))

(use-package ruby-mode
  :config
  (progn
    (bind-key (kbd "C-x l") 'antonio-ruby-spec-var-to-let ruby-mode-map)
    (add-hook 'ruby-mode-hook (lambda () (setq require-final-newline nil))))
  :mode (("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
         ("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package ruby-hash-syntax
  :defer t
  :init
  (progn
    (bind-key (kbd "C-c h")  'antonio-ruby-toggle-hash-syntax ruby-mode-map)
    ))

(use-package ruby-test-mode
  :defer t
  )

(use-package ruby-tools
  :config
  (progn
    (bind-key (kbd "C-c :")  'ruby-tools-to-symbol ruby-mode-map)
    (bind-key (kbd "C-c '")  'ruby-tools-to-single-quote-string ruby-mode-map)
    (bind-key (kbd "C-c \"") 'ruby-tools-to-double-quote-string ruby-mode-map)
    ))

(use-package smex
  :idle (smex-initialize)
  :config (bind-key (kbd "M-x") 'smex))

(use-package surround
  :config (global-surround-mode))

(use-package undo-tree
  :config
  (progn
  (setq undo-tree-history-directory-alist (quote (("." . "~/.undo/"))))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package web-mode
  :mode "\\.\\(erb\\|html?\\|css\\|scss\\)\\'"
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-enable-css-colorization t)
    )
  )

(use-package yaml-mode
  :mode "\\.yml\\'"
  )

(use-package yasnippet
  :defer t
  :idle (yas-global-mode))

(global-set-key (kbd "M-x") 'helm-M-x)
