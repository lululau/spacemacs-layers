;;; packages.el --- ob-rails layer packages file for Spacemacs.
;;
;; Author: 刘向 <liuxiang@ktjr.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Code:
(defconst ob-rails-packages
  '((ob-rails :location local)))

(defun ob-rails/init-ob-rails ()
  (use-package ob-rails
    :defer t
    :init
    (progn
      (autoload 'org-babel-default-header-args:rails "ob-rails")
      (autoload 'org-babel-rails-command "ob-rails")
      (autoload 'org-babel-rails-hline-to "ob-rails")
      (autoload 'org-babel-rails-nil-to "ob-rails")
      (autoload 'org-babel-execute:rails "ob-rails")
      (autoload 'org-babel-prep-session:rails "ob-rails")
      (autoload 'org-babel-load-session:rails "ob-rails")
      (autoload 'org-babel-variable-assignments:rails "ob-rails")
      (autoload 'org-babel-rails-var-to-rails "ob-rails")
      (autoload 'org-babel-rails-table-or-string "ob-rails")
      (autoload 'org-babel-rails-initiate-session "ob-rails")
      (autoload 'org-babel-rails-eoe-indicator "ob-rails")
      (autoload 'org-babel-rails-f-write "ob-rails")
      (autoload 'org-babel-rails-pp-f-write "ob-rails")
      (autoload 'org-babel-rails-wrapper-method "ob-rails")
      (autoload 'org-babel-rails-pp-wrapper-method "ob-rails")
      (autoload 'org-babel-rails-evaluate "ob-rails"))))

;;; packages.el ends here
