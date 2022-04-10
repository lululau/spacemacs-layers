;;; Code:

;;; 声明tabnine这个layer，并安装compnay-tabnine这个库
(defconst tabnine-packages
  '(company-tabnine)
  )

;;; 初始化company-tabnine这个库
(defun tabnine/init-company-tabnine()
  (use-package company-tabnine
    :ensure t
    :defer t
    :init
    :config)
  )

;;; 配置company-tabnine作为company的后端
(defun tabnine/post-init-company-tabnine()
  (with-eval-after-load 'company
    (add-to-list 'company-backends '(company-tabnine :with company-yasnippet))))

;;; packages.el ends here
