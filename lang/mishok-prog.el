;;; mishok-prog --- All programming-global setups

;;; Commentary:

;;; Code:

(require 'prog-mode)
(require 'fill-column-indicator)

(add-hook 'prog-mode-hook 'fci-mode)
(setq fci-rule-column 80)
(which-function-mode t)

(provide 'mishok-prog)
;;; mishok-prog ends here
