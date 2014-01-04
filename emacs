;; package
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
(el-get 'sync)

;; Column/Row numbering
(setq column-number-mode t)
(require 'linum)
(global-linum-mode t)
(setq linum-format "%d ")

;; substitute the yes/no to y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; use clipboard
(setq x-select-enable-clipboard t)

;; Matching parenthesis
(global-set-key "%" 'match-paren)
(defun match-paren (arg)
  "Go to the matching paren if on a parent otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
  ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
  (t (self-insert-command (or arg 1)))))

;; no menubar, no toolbar, no scrollbar, no splash
;; (if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
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
(set 'tab-width 2)
(set 'standard-indent 2)
(set 'c-basic-offset 2)
(set 'indent-tabs-mode nil)
(c-set-offset 'case-label '+)
(c-set-offset 'arglist-intro '+) ;; for FAPI arrays and DBTNG
(c-set-offset 'arglist-cont-nonempty 'c-lineup-math) ;; for DBTNG fields and values

;; browser
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program (getenv "BROWSER"))

;; delete trailing whitespaces when saving buffer
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; comment or uncomment region
(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments current current line or whole lines in region."
  (interactive)
  (save-excursion
    (let (min max)
      (if (region-active-p)
          (setq min (region-beginning) max (region-end))
        (setq min (point) max (point)))
      (comment-or-uncomment-region
       (progn (goto-char min) (line-beginning-position))
       (progn (goto-char max) (line-end-position))))))
(global-set-key (kbd "C-c C-SPC") 'comment-or-uncomment-line-or-region)

(load-theme 'wombat)

(set 'evil-want-C-i-jump nil)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
