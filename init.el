(require 'cask "~/.emacs.d/cask.el/cask.el")
(cask-initialize)

(require 'use-package)

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keymap.el")

(use-package ac-emmet)

(use-package auto-complete-config
  :config
  (progn
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    (ac-config-default)
    (auto-complete-mode)))

(use-package bookmark+)

(use-package browse-kill-ring
  :commands browse-kill-ring)

(use-package bundler
  :defer t)

(use-package coffee-mode
  :mode ("\\.coffee\\'" . coffee-mode))

(use-package dash-at-point
  :init
  (progn
    (bind-key "C-c d" 'dash-at-point)
    (bind-key "C-c e" 'dash-at-point-with-docset)
    (add-to-list 'dash-at-point-mode-alist '(ruby-mode . "ruby"))
    (add-to-list 'dash-at-point-mode-alist '(lisp-mode . "lisp"))
    (add-to-list 'dash-at-point-mode-alist '(emacs-lisp-mode . "elisp"))
    ))

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
    (add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
    (add-hook 'web-mode-hook 'ac-emmet-html-setup)
    (add-hook 'css-mode-hook 'ac-emmet-css-setup)
    ))

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
    (evil-define-key 'normal global-map "\C-j" 'evil-next-buffer)
    (evil-define-key 'normal global-map "\C-k" 'evil-prev-buffer)
    (evil-define-key 'normal global-map (kbd "SPC") 'evil-search-forward)))

(use-package evil-leader
  :config
  (progn
    (global-evil-leader-mode)
    (evil-leader/set-leader ",")

    (evil-leader/set-key
     "." 'helm-projectile
     "b" 'helm-buffers-list
     "c" 'evilnc-comment-or-uncomment-lines
     "f" 'helm-ag
     "o" 'helm-imenu
     "r" 'ruby-test-run-at-point
     "R" 'ruby-test-run
     "y" 'browse-kill-ring
     )))

(use-package evil-nerd-commenter
  :commands evilnc-comment-or-uncomment-lines)

(use-package evil-numbers
  :init
  (progn
    (define-key evil-normal-state-map (kbd "C-x C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x C-x") 'evil-numbers/dec-at-pt)))

(use-package fic-mode
  :diminish fic-mode
  :commands fic-mode
  :init (add-hook 'prog-mode-hook 'fic-mode))

(use-package flycheck
  :config (add-hook 'ruby-mode-hook 'flycheck-mode))

(use-package git-blame)

(use-package git-gutter-fringe)

(use-package haml-mode
  :mode ("\\.haml\\'" . haml-mode))

(use-package helm)

(use-package helm-ag)

(use-package helm-projectile)

(use-package json-mode)

(use-package markdown-mode
             :mode (("\\.md\\'" . markdown-mode)
                    ("\\.markdown\\'" . markdown-mode)))

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

    (setq org-default-notes-file (concat org-directory "/inbox.org"))
    (define-key global-map "\C-cc" 'org-capture)
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

(use-package projectile)

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
    (add-hook 'robe-mode-hook 'robe-ac-setup)
    ))

(use-package ruby-mode
  :mode (("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
         ("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package ruby-hash-syntax
  :init
  (progn
    (bind-key (kbd "C-c h")  'ruby-toggle-hash-syntax ruby-mode-map)
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

(use-package smartparens-config
  :config
  (progn
    (require 'smartparens-ruby)
    (smartparens-global-strict-mode t)
    (show-smartparens-global-mode t)
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
  :mode (("\\.erb\\'" . web-mode) ("\\.html?\\'" . web-mode)))

(use-package workgroups2
  :config
  (progn
    (setq wg-use-default-session-file nil)
    (setq wg-prefix-key (kbd "C-c z"))
    (setq wg-default-session-file "~/.emacs.d/sessions")
    (workgroups-mode 1)
    )
  )

(use-package yaml-mode)

(use-package yasnippet
  :idle (yas-global-mode))
