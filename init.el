;; do not load outdated compiled files
(setq load-prefer-newer t)

(require 'cask "~/.emacs.d/cask.el/cask.el")
(cask-initialize)

(require 'benchmark-init)

(require 'use-package)

(load "~/.emacs.d/settings.el")
(load "~/.emacs.d/keymap.el")
(load "~/.emacs.d/functions.el")

(use-package buffer-move
  :bind (
         ("C-M-h" . buf-move-left)
         ("C-M-j" . buf-move-down)
         ("C-M-k" . buf-move-up)
         ("C-M-l" . buf-move-right)
         )
  )

(use-package bundler
  :commands (bundle-check bundle-console bundle-install bundle-open bundle-update))

(use-package coffee-mode
  :mode "\\.coffee\\'")

(use-package company
  :init
  (progn
    (setq company-idle-delay 0.1)
    (define-key company-active-map (kbd "C-h") 'company-show-doc-buffer)
    (global-company-mode)
    )
  )

(use-package dash-at-point
  :commands (dash-at-point dash-at-point-with-docset)
  :init
  (progn
    (bind-key "C-c d" 'dash-at-point)
    (bind-key "C-c e" 'dash-at-point-with-docset))
  :config
  (progn
    (add-to-list 'dash-at-point-mode-alist '(ruby-mode . "ruby"))
    (add-to-list 'dash-at-point-mode-alist '(lisp-mode . "lisp"))
    (add-to-list 'dash-at-point-mode-alist '(emacs-lisp-mode . "elisp"))
    ))

(use-package dired+
  :defer t)

(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :config (add-hook 'dockerfile-mode-hook (lambda () (setq require-final-newline nil)))
  )

(use-package eldoc
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
    )
  )

(use-package emmet-mode
  :commands (emmet-mode)
  :config (add-hook 'emmet-mode-hook (lambda ()
                                       (setq emmet-preview-default nil)
                                       (setq emmet-indentation 2)
                                       ))
  :init
  (progn
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode)
    ))

(use-package expand-region
  :commands er/expand-region
  :init
  (bind-key "C-=" 'er/expand-region)
  )

(use-package ethan-wspace
  :commands (global-ethan-wspace-mode)
  :init (add-hook 'after-init-hook 'global-ethan-wspace-mode)
  )

(use-package evil
  :pre-load
  (progn
    (setq evil-toggle-key "M-V"))
  :init
  (evil-mode 1)
  :config
  (progn
    (setq evil-ex-substitute-global t)

    (define-key evil-insert-state-map [remap newline] 'newline)
    (define-key evil-insert-state-map [remap newline-and-indent] 'newline-and-indent)
    (evil-define-key 'normal global-map (kbd "%") 'ck/dispatch-goto-matching)
    (evil-define-key 'normal global-map (kbd "SPC") 'evil-search-forward)
    (evil-define-key 'normal global-map (kbd "j") 'evil-next-visual-line)
    (evil-define-key 'normal global-map (kbd "k") 'evil-previous-visual-line)
    (evil-define-key 'insert global-map (kbd "TAB") 'tab-indent-or-complete)

    (add-to-list 'evil-emacs-state-modes 'git-rebase-mode)

    (use-package evil-jumper)

    (use-package evil-leader
      :config
      (progn
        (global-evil-leader-mode)
        (evil-leader/set-leader ",")

        (evil-leader/set-key
          "." 'helm-projectile-find-file
          "b" 'helm-projectile-switch-to-buffer
          "c" 'comment-dwim
          "f" 'antonio-helm-ag
          "o" 'helm-imenu
          "s" 'helm-swoop
          "S" 'helm-multi-swoop
          "r" 'ruby-test-run-at-point
          "R" 'ruby-test-run
          "p" 'helm-show-kill-ring
          "t" 'helm-gtags-select)
        ))

    (use-package evil-matchit
      :commands (evil-matchit-mode)
      :config
      (progn
        (add-hook 'web-mode-hook 'evil-matchit-mode)
        ))

    (use-package evil-numbers
      :commands (evil-numbers/inc-at-pt evil-numbers/dec-at-pt)
      :init
      (progn
        (define-key evil-normal-state-map (kbd "C-x C-a") 'evil-numbers/inc-at-pt)
        (define-key evil-normal-state-map (kbd "C-x C-x") 'evil-numbers/dec-at-pt)))

    (use-package evil-surround
      :config (global-evil-surround-mode 1))
    ))

(use-package fic-mode
  :commands fic-mode
  :init (add-hook 'prog-mode-hook 'fic-mode))

(use-package flycheck
  :init (add-hook 'ruby-mode-hook 'flycheck-mode)
  :commands (flycheck-mode))

(use-package haml-mode
  :mode ("\\.haml\\'" . haml-mode))

(use-package helm
  :config
  (bind-key (kbd "M-x") 'helm-M-x)
  (progn
    (use-package helm-ag
      :commands (helm-ag))

    (use-package helm-gtags
      :commands (helm-gtags-select helm-gtags-find-pattern helm-gtags-find-tag))

    (use-package helm-projectile
      :config (setq projectile-switch-project-action 'helm-projectile)
      )

    (use-package helm-swoop
      :commands (helm-swoop helm-multi-swoop)
      )
    ))

(use-package json-mode
  :mode "\\.json\\'")

(use-package linum
  :init (global-linum-mode t)
  )

(use-package lua-mode
  :config (setq lua-indent-level 2)
  :mode "\\.lua\\'")

(use-package magit
  :commands magit-status
  :config
  (progn
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))

    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
    )
  )

(use-package markdown-mode
             :mode (("\\.md\\'" . markdown-mode)
                    ("\\.markdown\\'" . markdown-mode)))

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :config
  (progn
    (auto-fill-mode)

    (setq org-todo-keywords
        '((sequence "TODO(t)" "FUTURE(f)" "WAIT(w@)" "|" "CANCELED(c@)" "DONE(d)")))

    (global-set-key "\C-cl" 'org-store-link)
    (global-set-key "\C-ca" 'org-agenda)

    (setq org-agenda-window-setup 'current-window)
    (setq org-default-notes-file (concat org-directory "/plan.org"))
    (define-key global-map "\C-cc" 'org-capture)

    (setq org-archive-location "~/Dropbox/org/archives/%s::datetree/")
    (setq org-agenda-start-on-weekday nil)

    (setq org-capture-templates
          '(("t" "Task" entry (file+headline "~/Dropbox/org/plan.org" "INBOX")
             "* TODO %?\n")))

    (add-hook 'org-agenda-mode-hook
              (lambda ()
                (define-key org-agenda-mode-map "j" 'evil-next-line)
                (define-key org-agenda-mode-map "\C-j" 'org-agenda-goto-date)
                (define-key org-agenda-mode-map "k" 'evil-previous-line)
                ))

    (setq org-tag-alist
          '(("COMPUTER" . ?c)
            (:startgroup . nil)
            ("HOME" . ?h) ("OFFICE" . ?o)
            (:endgroup . nil)
            ("READING" . ?r)
            ("FUTURE" . ?f)
            ))

    (setq org-completion-use-ido t)

    (setq org-refile-targets '((nil :maxlevel . 9)))

    (setq org-clock-persist 'history)
    (org-clock-persistence-insinuate)

    (add-hook 'org-mode-hook
              (lambda ()
                (setq evil-auto-indent nil)
                ))
    )
  :init
  (progn
    (setq org-directory "~/Dropbox/org")
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    (setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/")

    ;; (add-hook 'after-save-hook
    ;;           (lambda ()
    ;;             (if (eq major-mode 'org-mode)
    ;;                 (org-mobile-push))
    ;;             ))

    ;; ;; watch mobileorg.org for changes, and then call org-mobile-pull
    ;; ;; http://stackoverflow.com/questions/3456782/emacs-lisp-how-to-monitor-changes-of-a-file-directory
    (defun install-monitor (file secs)
      (run-with-timer
       0 secs
       (lambda (f p)
         (unless (< p (second (time-since (elt (file-attributes f) 5))))
           (org-mobile-pull)))
       file secs))
    (defvar monitor-timer (install-monitor (concat org-mobile-directory "/mobileorg.org") 30)
      "Check if file changed every 30 s.")
    )
  )

(use-package popwin
  :config
  (progn
    (popwin-mode)
    (setq popwin:popup-window-height 30)
    (push '("*" :regexp t :height 30) popwin:special-display-config)
    (push '("*helm" :regexp t :height 30) popwin:special-display-config)
    (push '("*compilation" :regexp t :height 50) popwin:special-display-config)
    (push '("*Bundler" :regexp t :height 50) popwin:special-display-config)))

(use-package puppet-mode
  :mode "\\.pp\\'")

(use-package perspective
  :init (persp-mode)
  )

(use-package projectile
  :commands (projectile-switch-project projectile-persp-switch-project)
  :bind (("C-c p p" . projectile-switch-project))
  :config
  (progn
    (setq projectile-remember-window-configs t)
    (projectile-global-mode)
    ))

(use-package rotate-text
  :commands (rotate-text)
  :bind (("C-x C-t" . rotate-text))
  :config
  (progn
    (setq rotate-text-words '(("width" "height") ("left" "right" "top" "bottom") ("true" "false") ("assert" "refute")))
    ))

(use-package ruby-mode
  :config
  (progn
    (use-package robe
      :config (push 'company-robe company-backends))

    (use-package ruby-additional)

    (use-package ruby-block
      :config (ruby-block-mode))

    (use-package ruby-electric)

    (use-package ruby-hash-syntax
      :config (bind-key (kbd "C-c h")  'antonio-ruby-toggle-hash-syntax ruby-mode-map))

    (use-package ruby-test-mode)

    (use-package ruby-tools
      :config
      (progn
        (bind-key (kbd "C-c :")  'ruby-tools-to-symbol ruby-mode-map)
        (bind-key (kbd "C-c '")  'ruby-tools-to-single-quote-string ruby-mode-map)
        (bind-key (kbd "C-c \"") 'ruby-tools-to-double-quote-string ruby-mode-map)
        ))

    (bind-key (kbd "C-x l") 'antonio-ruby-spec-var-to-let ruby-mode-map)
    (evil-define-key 'insert ruby-mode-map (kbd "C-.") 'antonio-insert-hashrocket)
    (add-hook 'ruby-mode-hook 'ruby-electric-mode)
    (add-hook 'ruby-mode-hook (lambda () (setq require-final-newline nil))))
  :mode (("\\.\\(?:gemspec\\|irbrc\\|gemrc\\|rake\\|rb\\|ru\\|thor\\)\\'" . ruby-mode)
         ("\\(Capfile\\|Gemfile\\(?:\\.[a-zA-Z0-9._-]+\\)?\\|[rR]akefile\\)\\'" . ruby-mode)))

(use-package scss-mode
  :mode "\\.scss\\'"
  :init
  (progn
    (setq scss-compile-at-save nil)
    (setq css-indent-offset 2)
    )
  )

(use-package smart-mode-line
  :config (progn
            (sml/setup)
            (sml/apply-theme 'respectful)

            (setq rm-blacklist '(" FIC" " ew:mnlt" " company" " ElDoc"
                                 " Undo-Tree" " yas" " SP" " rt"
                                 " Ruby-Test" " REl" " RBlock"))
            ))

(use-package smartparens
  :init (require 'smartparens-config)
  :config
  (progn
    (smartparens-global-mode)
    (show-smartparens-global-mode)
    (setq sp-autoescape-string-quote nil)

    (defun space-and-space-on-each-side (&rest _ignored)
      (save-excursion
        (insert "  ")))

    (defun space-on-each-side (&rest _ignored)
      (when (or (looking-back "=")
                (looking-back "#"))
        (insert " ")
        (save-excursion
          (insert " "))))

    (sp-local-pair 'web-mode "%" "%"
                   :unless '(sp-in-string-or-word-p)
                   :post-handlers '(
                                    (space-and-space-on-each-side "SPC")
                                    (space-on-each-side "=" "#")
                                    ))
    )
  )

(use-package undo-tree
  :config
  (progn
  (setq undo-tree-history-directory-alist (quote (("." . "~/.undo/"))))
  (setq undo-tree-auto-save-history t)
  (global-undo-tree-mode)))

(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))

(use-package web-mode
  :mode "\\.\\(erb\\|html?\\)\\'"
  :config
  (progn
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-enable-css-colorization t)
    (setq web-mode-enable-auto-pairing nil)
    )
  )

(use-package yaml-mode
  :mode "\\.yml\\'"
  )

(use-package yasnippet
  :config
  (progn
    (defun check-expansion ()
      (save-excursion
        (if (looking-at "\\_>") t
          (backward-char 1)
          (if (looking-at "\\.") t
            (backward-char 1)
            (if (looking-at "->") t nil)))))

    (defun do-yas-expand ()
      (let ((yas/fallback-behavior 'return-nil))
        (yas/expand)))

    (defun tab-indent-or-complete ()
      (interactive)
      (if (minibufferp)
          (minibuffer-complete)
        (if (or (not yas/minor-mode)
                (null (do-yas-expand)))
            (if (check-expansion)
                (company-complete-common)
              (indent-for-tab-command)))))
    )
  :init (yas-global-mode)
  )
