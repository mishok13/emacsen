;;; mishok-prog --- All programming-global setups
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package fill-column-indicator
  :straight t
  :hook (prog-mode . fci-mode)
  :init
  (setq-default fci-rule-column 80))

(use-package which-func
  :config
  (which-function-mode t))

(use-package flycheck
  :straight t
  :bind (("<f4>" . flycheck-previous-error)
         ("<f5>" . flycheck-next-error)))

(use-package yaml-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)

(provide 'mishok-prog)
;;; mishok-prog ends here
