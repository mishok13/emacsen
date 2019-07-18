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

(provide 'mishok-rust)
;;; mishok-rust ends here
