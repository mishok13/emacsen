;;; mishok-prog --- All programming-global setups
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package fill-column-indicator
  :ensure t
  :init
  (setq-default fci-rule-column 80))

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'fci-mode))

(use-package which-func
  :config
  (which-function-mode t))

(use-package flycheck
  :ensure t
  :bind (("<f4>" . flycheck-previous-error)
         ("<f5>" . flycheck-next-error))
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(provide 'mishok-prog)
;;; mishok-prog ends here
