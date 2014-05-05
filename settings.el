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

(unless (display-graphic-p)
  (if (eq system-type 'darwin)
      (setq interprogram-cut-function 'paste-to-osx)
      (setq x-select-enable-clipboard t)))

(if (eq system-type 'darwin)
    (setq ns-use-srgb-colorspace t))

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
(defadvice switch-to-buffer (before save-current-buffer activate)
  (setq antonio-last-visited-buffer (buffer-name))
)

(global-set-key (kbd "M-SPC") 'antonio-switch-to-previous-buffer)

;; don't add a new line at the end of the file
(setq-default mode-require-final-newline nil)

;; minibuffer history
(setq savehist-additional-variables '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
(savehist-mode 1)

;; alias and functions
(defalias 'ar #'align-regexp)
(defalias 'K #'kill-this-buffer)
(defalias 'blame #'mo-git-blame-current)
(defalias 'bundle #'bundle-install)

(require 'uniquify)

;; Cask files are lisp
(add-to-list 'auto-mode-alist '("Cask" . lisp-mode))

;; sh-mode
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(setq sh-basic-offset 2
      sh-indentation 2)

;; encoding
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

;; calendar
(setq calendar-week-start-day 1)

;; parenthesis
(add-hook 'prog-mode-hook 'electric-pair-mode)
(show-paren-mode t)

;; mode-line
(setq-default mode-line-buffer-identification
              (list 'buffer-file-name
                    (propertized-buffer-identification "%12f")
                    (propertized-buffer-identification "%12b")))
