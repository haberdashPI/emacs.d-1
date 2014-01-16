(global-evil-leader-mode)
(evil-leader/set-leader ",")

(evil-leader/set-key
  "." 'helm-find-files
  "b" 'helm-buffers-list
  "c" 'evilnc-comment-or-uncomment-lines
  "f" 'helm-ag
)
