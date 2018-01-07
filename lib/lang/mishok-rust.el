;;; mishok-rust --- Rust specific setup
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'smartparens-mode))

(use-package racer
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode))

(use-package company
  :ensure t
  :bind ("TAB" . company-indent-or-complete-common)
  :config
  (add-hook 'racer-mode-hook #'company-mode)
  (setq company-tooltip-align-annotations t))

(provide 'mishok-rust)
;;; mishok-rust ends here
