;;; mishok-haskell --- Haskell support
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package haskell-mode
  :straight t)

(use-package shm
  :straight t
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'structured-haskell-mode))

(provide 'mishok-haskell)
;;; mishok-haskell.el ends here
