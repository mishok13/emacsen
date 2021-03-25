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

(use-package tide
  :straight t
  :config
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-hook 'typescript-mode-hook 'smartparens-mode))

(provide 'mishok-js)
;;; mishok-js ends here
