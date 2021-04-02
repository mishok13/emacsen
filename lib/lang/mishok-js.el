;;; mishok-js --- Javascript/Typescript setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package typescript-mode
  :straight t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'company-mode)
  (add-hook 'typescript-mode-hook 'smartparens-mode))

(use-package json-mode
  :straight t
  :config
  (setq js-indent-level 2))

(use-package json-reformat
  :straight t
  :config
  (setq json-reformat:indent-width 2))

(use-package web-mode
  :straight t)

(use-package tide
  :straight t
  :config
  (require 'web-mode)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-hook 'typescript-mode-hook 'smartparens-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(provide 'mishok-js)
;;; mishok-js ends here
