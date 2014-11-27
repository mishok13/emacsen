;;; mishok-haskell --- Haskell support
;;; Commentary:
;;; Code:
(require 'haskell-mode)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

(provide 'mishok-haskell)
;;; mishok-haskell.el ends here
