;;; mishok-rust --- Rust specific setup
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package racer
  :ensure t
  :hook (rust-mode . racer-mode))

(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook 'smartparens-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode))

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
