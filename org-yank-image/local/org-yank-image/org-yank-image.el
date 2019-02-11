;;;###autoload
(defun org-yank-image/default-dir ()
  (format "%s/media" (file-name-base (buffer-file-name))))

;;;###autoload
(defun org-yank-image/default-file (dir)
  (let ((last 0))
    (format "image%d.png" (1+
                           (dolist (f (file-expand-wildcards (format "%s/*" dir)) last)
                             (when (string-match "/image\\([0-9]+\\)\\.png$" f)
                               (let ((n (string-to-number (match-string 1 f))))
                                 (when (> n last)
                                   (setq last n)))))))))

;;;###autoload
(defun org-yank-image/get-file ()
  (let* ((default-dir (org-yank-image/default-dir))
         (default-file (org-yank-image/default-file default-dir)))
    (format "./%s" (file-relative-name (read-file-name "Save to file: " default-dir nil nil default-file) default-directory))))

;;;###autoload
(defun org-yank-image/write-to-file (path)
  (make-directory (file-name-directory path) t)
  (let* ((temp-file (make-temp-file "org-yank-image"))
         (cmd (format "convert -resize '50%%x50%%!' %s %s" temp-file  (shell-quote-argument (expand-file-name path)))))
         ;; (cmd (format "cp %s %s" temp-file  (shell-quote-argument (expand-file-name path)))))
    (shell-command (format "pngpaste %s" temp-file))
    (call-process-shell-command cmd)
    (delete-file temp-file)))

;;;###autoload
(defun org-yank-image/insert-link (file-path)
  (insert (format "[[file:%s]]" file-path))
  )

;;;###autoload
(defun org-yank-image/write-and-insert ()
  (let ((file-path (org-yank-image/get-file)))
    (org-yank-image/write-to-file file-path)
    (org-yank-image/insert-link file-path)
    (org-display-inline-images nil)))

;;;###autoload
(defun org-yank-image/yank ()
  (interactive)
  (org-yank-image/write-and-insert))

(provide 'org-yank-image)
