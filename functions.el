(defun antonio/ruby-toggle-hash-syntax ()
  (interactive)
  (save-excursion
    (re-search-backward "[({]")
    (push-mark (point) t t)
    (forward-list)
    (ruby-toggle-hash-syntax (region-beginning) (region-end))))
