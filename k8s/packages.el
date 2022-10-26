;;; packages.el --- k8s layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: 刘向 <liuxiang@ktjr.com>
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
;; added to `k8s-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `k8s/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `k8s/pre-init-PACKAGE' and/or
;;   `k8s/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst k8s-packages
  '(k8s-mode kubernetes kubernetes-evil kubernetes-tramp))

(defun k8s/init-k8s-mode ()
  (use-package k8s-mode))

(defun k8s/init-kubernetes ()
  (use-package kubernetes
    :defer t
    :commands (kubernetes-overview)
    :init (progn (spacemacs/set-leader-keys "aK" 'kubernetes-overview)))

  (with-eval-after-load 'kubernetes-overview
    (evilified-state-evilify-map kubernetes-overview-mode-map :mode kubernetes-overview-mode :bindings
      (kbd "v") 'kubernetes-overview-set-sections
      (kbd "r") 'kubernetes-refresh
      (kbd "l") 'kubernetes-logs-popup))

  (with-eval-after-load 'kubernetes-commands
    (defun kubernetes--display-window-action (buffer alist)
      (when-let (window (or (display-buffer-reuse-window buffer alist)
                            (display-buffer-same-window buffer alist)
                            (display-buffer-pop-up-window buffer alist)
                            (display-buffer-use-some-window buffer alist)))
        (delete-other-windows window)
        window))
    (defun kubernetes-commands-display-buffer-fullframe (buffer)
      (display-buffer buffer (list #'kubernetes--display-window-action)))))

(defun k8s/init-kubernetes-evil ()
  (use-package kubernetes-evil
    :after kubernetes-overview))

(defun k8s/init-kubernetes-tramp ()
  (use-package kubernetes-tramp
    :defer t))

;;; packages.el ends here
