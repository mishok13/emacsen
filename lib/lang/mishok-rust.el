;;; mishok-rust --- Rust specific setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package eglot)

(use-package rustic
  :straight t
  :after eglot
  :bind (:map rustic-mode-map
              ;; ("M-j" . lsp-ui-imenu)
              ;; ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . eglot-code-actions)
              ("C-c C-c r" . eglot-rename)
              ;; ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . eglot-shutdown-all)
              ;; ("C-c C-c s" . lsp-rust-analyzer-status)
              )
  :config
  (setq rustic-format-on-save t)
  (setq rustic-lsp-client 'eglot)
  (add-hook 'rustic-mode-hook 'eglot-ensure))

(provide 'mishok-rust)
;;; mishok-rust ends here
