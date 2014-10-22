(defun antonio-ruby-toggle-hash-syntax ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (push-mark (point) t t)
    (forward-list)
    (ruby-toggle-hash-syntax (region-beginning) (region-end))))

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

(defun antonio-helm-ag ()
  (interactive)
  (helm-ag (projectile-project-root)))

(defun antonio-ruby-spec-var-to-let ()
  (interactive)
  (perform-replace "\\(\\w+\\) *= \\(.*\\)" "let(:\\1) { \\2 }" nil t nil nil nil (line-beginning-position) (line-end-position))
  )

(defun antonio-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer antonio-last-visited-buffer))

(defun antonio-insert-hashrocket ()
    (interactive)
    (insert "=> "))
