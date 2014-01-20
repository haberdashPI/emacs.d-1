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

;; autosave
(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))
(make-directory user-temporary-file-directory t)
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))

;; follow symlinks
(setq vc-follow-symlinks t)

;; indentation
(setq-default indent-tabs-mode nil)
(setq tab-width 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(electric-indent-mode t)

;; themes
(setq custom-theme-directory "~/.emacs.d/themes/")
(load-theme 'base16-tomorrow-dark t)

;; stay in current directory
(add-hook 'find-file-hook
          (lambda ()
            (setq default-directory command-line-default-directory)))

;; highlight current line
(global-hl-line-mode)

;; automatically close brackets
(electric-pair-mode t)

;; automatically refresh files
(global-auto-revert-mode t)
