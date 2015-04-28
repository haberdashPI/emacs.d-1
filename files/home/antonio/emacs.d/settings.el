;; Customizations
(setq custom-file "~/.emacs.d/customize.el")
(load custom-file)

;; Column/Row numbering
(setq column-number-mode t)
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
(setq coffee-tab-width 2)
(setq js-indent-level 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)
(electric-indent-mode t)

;; themes
(setq custom-theme-directory "~/.emacs.d/themes/")
(load-theme 'base16-tomorrow-dark t)
(set-face-attribute 'region nil :background "#444")

;; automatically refresh files
(global-auto-revert-mode t)

;; window management
(defadvice windmove-do-window-select (after windmove-blink-mode-line activate)
  (antonio-blink-mode-line))
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-H") 'move-border-left)
(global-set-key (kbd "M-L") 'move-border-right)
(global-set-key (kbd "M-K") 'move-border-up)
(global-set-key (kbd "M-J") 'move-border-down)

;; go back and forth the last two buffers
(defadvice switch-to-buffer (before save-current-buffer activate)
  (setq antonio-last-visited-buffer (buffer-name))
)

(global-set-key (kbd "M-SPC") 'antonio-switch-to-previous-buffer)

;; don't add a new line at the end of the file
(setq-default mode-require-final-newline nil)
(setq-default require-final-newline nil)
(setq mode-require-final-newline nil)
(setq require-final-newline nil)

;; minibuffer history
(setq savehist-additional-variables '(kill-ring mark-ring global-mark-ring search-ring regexp-search-ring extended-command-history))
(savehist-mode 1)

;; alias and functions
(defalias 'ar #'align-regexp)
(defalias 'K #'kill-this-buffer)
(defalias 'blame #'magit-blame-mode)
(defalias 'bundle #'bundle-install)

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

;; mode-line
(setq-default mode-line-buffer-identification
              (list 'buffer-file-name
                    (propertized-buffer-identification "%12f")
                    (propertized-buffer-identification "%12b")))

;; scratch buffer
(setq initial-scratch-message nil)

;; tramp configuration
(setq tramp-default-method "ssh")

;; suppress warnings
(setq warning-minimum-level :error)

;; show matching parenthesis
(show-paren-mode)

;; keybinds
(global-set-key (kbd "C-x C-e") 'eval-region)
(global-set-key (kbd "C-c C-o") 'browse-url-at-point)

;; ediff
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; automatically create directories
(add-to-list 'find-file-not-found-functions #'antonio-create-non-existent-directory)

;; use right alt to enter international characters
(setq ns-right-alternate-modifier nil)

;; emacs-mac-port tweaking
(defun antonio-swap-alts ()
  "Swap the alt keys"
  (interactive)
  (let ((current-mac-option-modifier mac-option-modifier))
    (setq mac-option-modifier mac-right-option-modifier)
    (setq mac-right-option-modifier current-mac-option-modifier)))
(global-set-key (kbd "C-x t") 'antonio-swap-alts)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'hyper)
(setq mac-right-option-modifier nil)
(global-set-key [(hyper v)] 'yank)
(global-set-key [(hyper c)] 'kill-ring-save)

(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter nil 'fullscreen
                       (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))
(global-set-key [(hyper f)] 'toggle-fullscreen)

;; ido stop autocorrecting
(setq ido-auto-merge-work-directories-length -1)

;; modeline
(defun antonio-mode-line ()
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let*
                       ((active
                         (powerline-selected-window-active))
                        (mode-line
                         (if active 'mode-line 'mode-line-inactive))
                        (face1
                         (if active 'powerline-active1 'powerline-inactive1))
                        (face2
                         (if active 'powerline-active2 'powerline-inactive2))
                        (separator-left
                         (intern
                          (format "powerline-%s-%s"
                                  (powerline-current-separator)
                                  (car powerline-default-separator-dir))))
                        (separator-right
                         (intern
                          (format "powerline-%s-%s"
                                  (powerline-current-separator)
                                  (cdr powerline-default-separator-dir))))
                        (lhs
                         (list
                          (powerline-raw "%*" nil 'l)
                          (powerline-buffer-size nil 'l)
                          (powerline-buffer-id nil 'l)
                          (powerline-raw " ")
                          (funcall separator-left mode-line face1)
                          (powerline-narrow face1 'l)
                          (powerline-vc face1)))
                        (rhs
                         (list
                          (powerline-raw global-mode-string face1 'r)
                          (powerline-raw "%4l" face1 'r)
                          (powerline-raw ":" face1)
                          (powerline-raw "%3c" face1 'r)
                          (funcall separator-right face1 mode-line)
                          (powerline-raw " ")
                          (powerline-raw "%6p" nil 'r)
                          (powerline-hud face2 face1)))
                        (center
                         (append
                          (list
                           (powerline-raw " " face1)
                           (funcall separator-left face1 face2)
                           (when
                               (boundp 'erc-modified-channels-object)
                             (powerline-raw erc-modified-channels-object face2 'l))
                           (powerline-major-mode face2 'l)
                           (powerline-process face2)
                           (powerline-raw " " face2))
                          (append
                           (list
                            (powerline-raw evil-mode-line-tag face2)
                            (funcall separator-right face2 face1))))))
                     (concat
                      (powerline-render lhs)
                      (powerline-fill-center face1
                                             (/
                                              (powerline-width center)
                                              2.0))
                      (powerline-render center)
                      (powerline-fill face1
                                      (powerline-width rhs))
                      (powerline-render rhs)))))))
