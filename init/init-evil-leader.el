(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-leader/set-key
  "." 'helm-projectile
  "b" 'helm-buffers-list
  "c" 'evilnc-comment-or-uncomment-lines
  "f" 'helm-ag
  "o" 'helm-imenu
  "r" 'ruby-test-run-at-point
  "R" 'ruby-test-run
)
