(defun antonio/ruby-toggle-hash-syntax ()
  (interactive)
  (save-excursion
    (backward-up-list)
    (push-mark (point) t t)
    (forward-list)
    (ruby-toggle-hash-syntax (region-beginning) (region-end))))

(defun antonio/helm-projectile ()
  "Use projectile with Helm instead of ido."
  (interactive)
  (let ((helm-ff-transformer-show-only-basename nil))
    (helm :sources '(helm-source-projectile-buffers-list
                     helm-source-projectile-files-list)
          :buffer "*helm projectile*"
          :prompt (projectile-prepend-project-name "pattern: "))))
