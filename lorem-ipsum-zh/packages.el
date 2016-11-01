(defconst lorem-ipsum-zh-packages
  '((lorem-ipsum-zh :location local))
  "The list of Lisp packages required by the lorem-ipsum-zh layer.

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

  (defun lorem-ipsum-zh/init-lorem-ipsum-zh ()
  (use-package lorem-ipsum-zh
    :commands (lorem-ipsum-zh-insert-list
               lorem-ipsum-zh-insert-paragraphs
               lorem-ipsum-zh-insert-sentences)
    :init
    (progn
      (spacemacs/declare-prefix "il" "lorem ipsum")
      (spacemacs/set-leader-keys
        "ilL" 'lorem-ipsum-zh-insert-list
        "ilP" 'lorem-ipsum-zh-insert-paragraphs
        "ilS" 'lorem-ipsum-zh-insert-sentences))))
;;; packages.el ends here
