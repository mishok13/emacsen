;;; mishok-haskell --- Haskell support
;;; Commentary:
;;; Code:
(require 'haskell-mode)
(require 'shm)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'structured-haskell-mode)

(provide 'mishok-haskell)
;;; mishok-haskell.el ends here
