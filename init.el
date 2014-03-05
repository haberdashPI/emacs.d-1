(require 'cask "~/.emacs.d/cask.el/cask.el")
(cask-initialize)

(require 'pallet)
(require 'use-package)

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keymap.el")

(use-package auto-complete
  :init
  (progn
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    (ac-config-default)))

(use-package evil
  :pre-load
  (progn
    (set 'evil-want-C-i-jump nil)
    (setq evil-toggle-key "M-V"))
  :init
  (progn
    (evil-mode)
    (setq evil-ex-substitute-global t)
    (evil-define-key 'normal global-map "\C-j" 'evil-next-buffer)
    (evil-define-key 'normal global-map "\C-k" 'evil-prev-buffer)
    (evil-define-key 'normal global-map (kbd "SPC") 'evil-search-forward)))