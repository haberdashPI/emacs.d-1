(require 'cask "~/.emacs.d/cask.el/cask.el")
(cask-initialize)

(require 'pallet)
(require 'use-package)

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keymap.el")

(use-package evil
  :init (evil-mode))
