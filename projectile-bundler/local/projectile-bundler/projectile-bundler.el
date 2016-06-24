(require 'projectile-rails)
(require 'inf-ruby)
(require 'f)
(require 'rake)
(require 'bundler)

(defun projectile-bundler-root ()
  "Returns bundler project root directory if this file is a part of a bundler application else nil"
  (ignore-errors
    (projectile-locate-dominating-file default-directory "Gemfile")))

(defun bundle-console ()
  "Run an inferior Ruby process in the context of the current bundle."
  (interactive)
  (run-ruby "bundle console" (concat "*" (projectile-project-name)  "bundleconsole*")))

(defun projectile-bundler-console()
  (interactive)
  (call-interactively
   (if (projectile-rails-root)
       'projectile-rails-console
     'bundle-console)))

(define-minor-mode projectile-bundler-mode "Bundler mode based on projectile"
  :init-value nil
  :lighter " Bundler")

;;;###autoload
(defun projectile-bundler-on ()
  "Enable `projectile-bundler-mode' minor mode if this is a bundler project."
  (when (and
         (projectile-project-p)
         (projectile-bundler-root))
    (projectile-bundler-mode +1)))

(provide 'projectile-bundler)

