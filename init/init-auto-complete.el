(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

(defun ac-emacs-lisp-mode ()
  (setq ac-sources
        '(ac-source-features ac-source-functions ac-source-yasnippet ac-source-variables ac-source-symbols ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)))

(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode)

(defun ac-css-mode ()
  (setq ac-sources
        '(ac-source-css-property ac-source-yasnippet ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers)))

(add-hook 'css-mode-hook 'ac-css-mode)
(add-hook 'scss-mode-hook 'ac-css-mode)

(defun ac-ruby-mode ()
  (setq ac-sources
        '(ac-source-yasnippet ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers ac-source-imenu)))

(add-hook 'ruby-mode-hook 'ac-ruby-mode)

(auto-complete-mode)
