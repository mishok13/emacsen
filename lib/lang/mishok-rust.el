;;; mishok-rust --- Rust specific setup
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package rust-mode
  :ensure t
  :init
  :config
  (setq rust-mode-hook nil)
  (add-hook 'rust-mode-hook 'smartparens-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook (lambda () (flycheck-rust-setup))))

(provide 'mishok-rust)
;;; mishok-rust ends here
