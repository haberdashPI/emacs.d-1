(defun antonio-ruby-toggle-hash-syntax ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (push-mark (point) t t)
    (forward-list)
    (ruby-toggle-hash-syntax (region-beginning) (region-end))))

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

(defun antonio-open-newline ()
  (interactive)
  (if (s-blank? (s-trim (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
      (next-line)
    (progn
      (end-of-line)
      (newline-and-indent)
      )))

(defun antonio-invert-mode-line ()
  (interactive)
  (invert-face 'mode-line))

(defun antonio-blink-mode-line ()
  (interactive)
  (antonio-invert-mode-line)
  (run-at-time "0.3 sec" nil 'antonio-invert-mode-line))

(defun antonio-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (if (not (file-exists-p parent-directory))
        (make-directory parent-directory t))))

(defun antonio-ag-in-project ()
  (interactive)
  (helm-do-ag (projectile-project-root)))
