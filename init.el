(require 'cask "~/.emacs.d/cask.el/cask.el")
(cask-initialize)

(require 'use-package)

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keymap.el")

(use-package auto-complete-config
  :config
  (progn
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    (ac-config-default)
    (auto-complete-mode)))

(use-package browse-kill-ring
  :commands browse-kill-ring)

(use-package bundler
  :defer t)

(use-package coffee-mode
  :mode ("\\.coffee\\'" . coffee-mode))

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

(use-package git-blame
  :commands git-blame-mode)

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

    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)
    (global-set-key "\C-cb" 'org-iswitchb)))

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

(use-package projectile)

(use-package rainbow-mode
  :commands rainbow-mode)

(use-package robe
  :config
  (progn
    (add-hook 'ruby-mode-hook 'robe-mode)
    (add-hook 'robe-mode-hook 'robe-ac-setup)
    ))

(use-package scss-mode
  :mode "\\.scss\\'"
  :config
  (progn
    ((setq scss-compile-at-save nil)
     (setq css-indent-offset 2))))

(use-package smartparens-config
  :config
  (progn
    (require 'smartparens-ruby)
    (smartparens-global-strict-mode t)
    (show-smartparens-global-mode t)
    ))

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

(use-package yaml-mode)

(use-package yasnippet
  :config (yas-global-mode))
