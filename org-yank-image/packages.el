;;; packages.el --- org-yank-image layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: 刘向 <liuxiang@liuxiangdeMacBookAir>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `org-yank-image-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `org-yank-image/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `org-yank-image/pre-init-PACKAGE' and/or
;;   `org-yank-image/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst org-yank-image-packages
  '((org-yank-image :location local) org)
  "The list of Lisp packages required by the org-yank-image layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun org-yank-image/init-org-yank-image ()
  ;; (add-hook 'org-mode-hook #'(lambda ()
  ;;                              (define-key org-mode-map (kbd "s-v") #'org-yank-image/yank)))
  (use-package org-yank-image
    :defer t
    :init
    (autoload 'org-yank-image/yank "org-yank-image")))

(defun org-yank-image/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config
    (define-key org-mode-map (kbd "s-v") #'org-yank-image/yank)))

;;; packages.el ends here
