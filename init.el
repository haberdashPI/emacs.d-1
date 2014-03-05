(require 'cask "~/.emacs.d/cask.el/cask.el")
(cask-initialize)

(require 'use-package)

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keymap.el")

(use-package auto-complete-config
  :defer t
  :config
  (progn
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    (ac-config-default)))

(use-package browse-kill-ring
  :defer t)

(use-package bundler
  :defer t)

(use-package coffee-mode
  :mode ("\\.coffee\\'" . coffee-mode))

(use-package ethan-wspace
  :defer t
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
     )))

(use-package evil-nerd-commenter
  :commands evilnc-comment-or-uncomment-lines)

(use-package evil-numbers
  :init
  (progn
    (define-key evil-normal-state-map (kbd "C-x C-a") 'evil-numbers/inc-at-pt)
    (define-key evil-normal-state-map (kbd "C-x C-x") 'evil-numbers/dec-at-pt)))

(use-package surround
  :config (global-surround-mode))
