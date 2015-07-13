;;; mishok-rust --- Rust specific setup
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package rust-mode
  :ensure t
  :config
  (add-hook 'rust-mode-hook 'smartparens-mode))

(provide 'mishok-rust)
;;; mishok-rust ends here
