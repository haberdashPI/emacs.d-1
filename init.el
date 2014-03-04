(require 'cask "~/.emacs.d/cask.el/cask.el")
(cask-initialize)
(require 'pallet)

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keymap.el")
(setq custom-file "~/.emacs.d/customize.el")
(load custom-file)
