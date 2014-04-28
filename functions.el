(defun antonio/ruby-toggle-hash-syntax ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (push-mark (point) t t)
    (forward-list)
    (ruby-toggle-hash-syntax (region-beginning) (region-end))))

(defun antonio/helm-files ()
  "Custom function to list files."
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources '(helm-source-projectile-files-list)
          :buffer "*helm*"
          :prompt (projectile-prepend-project-name "pattern: "))))

(defun antonio/helm-buffers ()
  "Custom function to list buffers."
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources '(helm-source-projectile-buffers-list)
          :buffer "*helm*"
          :prompt (projectile-prepend-project-name "pattern: "))))

;; http://ck.kennt-wayne.de/2013/may/emacs%3A-jump-to-matching-paren-beginning-of-block

(defun ck/goto-match-paren (arg)
  "Go to the matching  if on (){}[], similar to vi style of % "
  (interactive "p")
  (cond ((looking-at "[\[\(\{]") (forward-sexp))
        ((looking-back "[\]\)\}]" 1) (backward-sexp))
        ;; try to also succeed from inside of a bracket
        ((looking-at "[\]\)\}]") (forward-char) (backward-sexp))
        ((looking-back "[\[\(\{]" 1) (backward-char) (forward-sexp))
        (t nil)
        )
  )

(defun ck/goto-matching-ruby-block (arg)
  (cond
   ((equal (current-word) "end")
    (ruby-beginning-of-block))
   ((string-match (current-word) "\\(for\\|while\\|until\\|if\\|class\\|module\\|case\\|unless\\|def\\|begin\\|do\\)")
    (ruby-end-of-block)
    )
   )
  )

(defun ck/dispatch-goto-matching (arg)
  (interactive "p")

  (if (or
       (looking-at "[\[\(\{]")
       (looking-at "[\]\)\}]")
       (looking-back "[\[\(\{]" 1)
       (looking-back "[\]\)\}]" 1))

      (ck/goto-match-paren arg)

    (when (eq major-mode 'ruby-mode)
      (ck/goto-matching-ruby-block arg)
      )
    )
  )

(defun antonio/helm-ag ()
  (with-helm-default-directory (projectile-project-root) (helm-ag)))

(defun antonio/cleanup-mode-line ()
  (delight '(
             (company-mode nil "company")
             (ethan-wspace-mode nil "ethan-wspace")
             (helm-mode nil "helm")
             (hrb-mode)
             (ggtags-mode nil "ggtags")
             (git-gutter-mode nil "git-gutter-fringe")
             (ruby-block-mode nil "ruby-block")
             (undo-tree-mode nil "undo-tree")
             (workgroups-mode nil "workgroups2")
             (yas-minor-mode nil "yasnippet")
             )))
