(load "~/.emacs.d/settings.el")

(mapc 'load (directory-files
	     (expand-file-name "~/.emacs.d/pre-init") t ".*\.el"))

(require 'package)
(setq package-archives '(
                         ("gnu" . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
			))

(setq el-get-user-package-directory "~/.emacs.d/init/")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))

(add-to-list 'el-get-recipe-path "~/.emacs.d/recipes")
(el-get 'sync)

(setq el-get-packages '(evil
                        evil-leader
                        evil-surround
                        evil-numbers
                        rainbow-mode
                        org))

(el-get 'sync el-get-packages)
