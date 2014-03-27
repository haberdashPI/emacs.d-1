(defun antonio/ruby-toggle-hash-syntax ()
  (interactive)
  (save-excursion
    (re-search-backward "[({]")
    (push-mark (point) t t)
    (evil-jump-item)
    (ruby-toggle-hash-syntax (region-beginning) (region-end))))
