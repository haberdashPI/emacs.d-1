;; http://stackoverflow.com/questions/683425/globally-override-key-binding-in-emacs/1758638

(defvar custom-minor-mode-map (make-keymap) "custom-minor-mode keymap.")

(define-key custom-minor-mode-map (kbd "M-h") 'windmove-left)
(define-key custom-minor-mode-map (kbd "M-l") 'windmove-right)
(define-key custom-minor-mode-map (kbd "M-k") 'windmove-up)
(define-key custom-minor-mode-map (kbd "M-j") 'windmove-down)

(define-minor-mode custom-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " custom-keymap" 'custom-minor-mode-map)

(defun custom-minibuffer-setup-hook ()
  (custom-minor-mode 0))

(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'custom-minor-mode))
      (let ((mykeys (assq 'custom-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'custom-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(add-hook 'minibuffer-setup-hook 'custom-minibuffer-setup-hook)

(custom-minor-mode 1)
