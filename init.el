(require 'cask "~/.emacs.d/cask.el/cask.el")
(cask-initialize)

(require 'use-package)

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keymap.el")
(load "~/.emacs.d/functions.el")

(use-package bookmark+)

(use-package bundler
  :defer t)

(use-package coffee-mode
  :mode ("\\.coffee\\'" . coffee-mode))

(use-package company
  :config
  (progn
    (global-company-mode)
    (setq company-idle-delay 0.1)
    (define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
    ))

(use-package dash-at-point
  :init
  (progn
    (bind-key "C-c d" 'dash-at-point)
    (bind-key "C-c e" 'dash-at-point-with-docset)
    (add-to-list 'dash-at-point-mode-alist '(ruby-mode . "ruby"))
    (add-to-list 'dash-at-point-mode-alist '(lisp-mode . "lisp"))
    (add-to-list 'dash-at-point-mode-alist '(emacs-lisp-mode . "elisp"))
    ))

(use-package delight
  :config
  (progn
    (add-hook 'after-init-hook 'antonio/cleanup-mode-line)))

(use-package dired+
  :init
  (progn
    (bind-key "C-x d" 'diredp-dired-files)))

(use-package emmet-mode
  :config
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook  'emmet-mode)
    (add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2)))
    ))

(use-package expand-region
  :config (global-set-key (kbd "C-=") 'er/expand-region))

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
    (evil-define-key 'normal global-map (kbd "SPC") 'evil-search-forward)))

(use-package evil-leader
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")

    (evil-leader/set-key
     "." 'antonio/helm-files
     "b" 'antonio/helm-buffers
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

(use-package fic-mode
  :diminish fic-mode
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
  :config (add-hook 'ruby-mode-hook 'flycheck-mode))

(use-package ggtags
  :config
  (add-hook 'prog-mode-hook 'ggtags-mode))

(use-package git-gutter-fringe
  :config (global-git-gutter-mode))

(use-package haml-mode
  :mode ("\\.haml\\'" . haml-mode))

(use-package helm
  :config
  (progn
    (helm-mode)
    ))

(use-package helm-ag)

(use-package helm-projectile
  :config
  (progn
    (setq projectile-switch-project-action 'helm-projectile)
    ))

(use-package hrb-mode
  :config (progn
            (setq hrb-delay 0)
            (setq hrb-highlight-keyword-face 'show-paren-match-face)
            (setq hrb-highlight-block-face 'highlight)
            (setq hrb-highlight-mode 'keywords)
            (add-hook 'ruby-mode-hook 'hrb-mode)))

(use-package json-mode)

(use-package magit
  :commands magit-status)

(use-package markdown-mode
             :mode (("\\.md\\'" . markdown-mode)
                    ("\\.markdown\\'" . markdown-mode)))

(use-package mo-git-blame
  :init (evil-set-initial-state 'mo-git-blame-mode 'emacs))

(use-package org
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

(use-package puppet-mode)

(use-package projectile
  :config
  (progn
    (setq projectile-remember-window-configs t)
    (projectile-global-mode)
    ))

(use-package projectile-rails
  :config
  (progn
    (add-hook 'projectile-mode-hook 'projectile-rails-on)
    ))

(use-package rainbow-delimiters
  :config
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    ))

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package robe
  :config
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (push 'company-robe company-backends)
    ))

(use-package rotate-text
  :config
  (progn
    (setq rotate-text-words '(("width" "height") ("left" "right" "top" "bottom") ("true" "false")))
    (bind-key (kbd "C-x C-t") 'rotate-text)))

(use-package ruby-additional)

(use-package ruby-block
  :config
    (add-hook 'ruby-mode-hook 'ruby-block-mode))

(use-package ruby-electric
  :config
    (add-hook 'ruby-mode-hook 'ruby-electric-mode))

(use-package ruby-mode
  :mode (("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
         ("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package ruby-hash-syntax
  :init
  (progn
    (bind-key (kbd "C-c h")  'antonio/ruby-toggle-hash-syntax ruby-mode-map)
    ))

(use-package ruby-test-mode)

(use-package ruby-tools
  :init
  (progn
    (bind-key (kbd "C-c :")  'ruby-tools-to-symbol ruby-mode-map)
    (bind-key (kbd "C-c '")  'ruby-tools-to-single-quote-string ruby-mode-map)
    (bind-key (kbd "C-c \"") 'ruby-tools-to-double-quote-string ruby-mode-map)
    ))

(use-package scss-mode
  :mode "\\.scss\\'"
  :config
  (progn
    (setq scss-compile-at-save nil)
    (setq css-indent-offset 2)))

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
  :mode (("\\.erb\\'" . web-mode) ("\\.html?\\'" . web-mode)))

(use-package workgroups2
  :config
  (progn
    (setq wg-use-default-session-file nil)
    (setq wg-prefix-key (kbd "C-c w"))
    (setq wg-default-session-file "~/.emacs.d/sessions")
    (workgroups-mode 1)
    )
  )

(use-package yaml-mode)

(use-package yasnippet
  :idle (yas-global-mode))

(global-set-key (kbd "M-x") 'helm-M-x)
