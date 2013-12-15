;;; mishok-rust --- Rust specific setup
;;; Commentary:
;;; Code:

(require 'rust-mode)
(require 'smartparens)

(add-hook 'rust-mode-hook 'smartparens-mode)

(provide 'mishok-rust)
;;; mishok-rust ends here
