;;; packages.el --- ob-arql layer packages file for Spacemacs.
;;
;; Author: 刘向 <liuxiang@ktjr.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Code:
(defconst ob-arql-packages
  '((ob-arql :location local)))

(defun ob-arql/init-ob-arql ()
  (use-package ob-arql
    :defer t
    :init
    (progn
      (autoload 'org-babel-default-header-args:arql "ob-arql")
      (autoload 'org-babel-arql-command "ob-arql")
      (autoload 'org-babel-arql-hline-to "ob-arql")
      (autoload 'org-babel-arql-nil-to "ob-arql")
      (autoload 'org-babel-execute:arql "ob-arql")
      (autoload 'org-babel-prep-session:arql "ob-arql")
      (autoload 'org-babel-load-session:arql "ob-arql")
      (autoload 'org-babel-variable-assignments:arql "ob-arql")
      (autoload 'org-babel-arql-var-to-arql "ob-arql")
      (autoload 'org-babel-arql-table-or-string "ob-arql")
      (autoload 'org-babel-arql-initiate-session "ob-arql")
      (autoload 'org-babel-arql-eoe-indicator "ob-arql")
      (autoload 'org-babel-arql-f-write "ob-arql")
      (autoload 'org-babel-arql-pp-f-write "ob-arql")
      (autoload 'org-babel-arql-wrapper-method "ob-arql")
      (autoload 'org-babel-arql-pp-wrapper-method "ob-arql")
      (autoload 'org-babel-arql-evaluate "ob-arql"))))

;;; packages.el ends here
