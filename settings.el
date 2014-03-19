;; Customizations
(setq custom-file "~/.emacs.d/customize.el")
(load custom-file)

;; Column/Row numbering
(setq column-number-mode t)
(require 'linum)
(global-linum-mode t)
(setq linum-format "%3d ")

;; substitute the yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; clipboard
(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(if (eq system-type 'darwin)
    (progn (setq interprogram-cut-function 'paste-to-osx))
    (setq x-select-enable-clipboard t)
)

;; no menubar, no toolbar, no scrollbar, no splash
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq inhibit-splash-screen t)

;; disable auto-save and auto-backup
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; follow symlinks
(setq vc-follow-symlinks t)

;; indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(setq js-indent-level 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(electric-indent-mode t)

;; themes
(setq custom-theme-directory "~/.emacs.d/themes/")
(load-theme 'base16-tomorrow-dark t)

;; automatically refresh files
(global-auto-revert-mode t)

;; window movement
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)

;; go back and forth the last two buffers
(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key (kbd "M-SPC") 'switch-to-previous-buffer)

;; don't add a new line at the end of the file
(setq-default mode-require-final-newline nil)

;; minibuffer history
(savehist-mode 1)

;; alias and functions
(defalias 'ar #'align-regexp)
(defalias 'K #'kill-this-buffer)

(require 'uniquify)

;; Cask is lisp-mode
(add-to-list 'auto-mode-alist '("Cask" . lisp-mode))

;; encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)
