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

;; intuitive window resizing
(defun xor (b1 b2)
  (or (and b1 b2)
      (and (not b1) (not b2))))

(defun move-border-left-or-right (arg dir)
  "General function covering move-border-left and move-border-right. If DIR is
     t, then move left, otherwise move right."
  (interactive)
  (if (null arg) (setq arg 3))
  (let ((left-edge (nth 0 (window-edges))))
    (if (xor (= left-edge 0) dir)
        (shrink-window arg t)
      (enlarge-window arg t))))

(defun move-border-up-or-down (arg dir)
  "General function covering move-border-up and move-border-down. If DIR is
     t, then move up, otherwise move down."
  (interactive)
  (if (null arg) (setq arg 3))
  (let ((top-edge (nth 1 (window-edges))))
    (if (xor (= top-edge 0) dir)
        (shrink-window arg nil)
      (enlarge-window arg nil))))

(defun move-border-left (arg)
  (interactive "P")
  (move-border-left-or-right arg t))

(defun move-border-right (arg)
  (interactive "P")
  (move-border-left-or-right arg nil))

(defun move-border-up (arg)
  (interactive "P")
  (move-border-up-or-down arg t))

(defun move-border-down (arg)
  (interactive "P")
  (move-border-up-or-down arg nil))

(defun antonio-current-perspective-name ()
  (interactive)
  (nth (persp-curr-position) (persp-all-names))
  )

(defun antonio-current-perspective-file ()
  (interactive)
  (format "~/.emacs.d/perspectives/%s" (antonio-current-perspective-name)))

(defun antonio-save-perspective ()
  (interactive)
  (wg-create-workgroup (antonio-current-perspective-name))
  (wg-save-session-as (antonio-current-perspective-file))
  )

(defun antonio-load-perspective ()
  (interactive)
  (wg-open-session (antonio-current-perspective-file))
  )
