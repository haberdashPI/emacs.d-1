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
