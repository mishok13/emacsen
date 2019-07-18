;;; mishok-py --- Python setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package smartparens
  :ensure t
  :hook ((python-mode . smartparens-mode)
         (rust-mode . smartparens-mode)))

(use-package jedi
  :ensure t)

(use-package flycheck
  :ensure t
  :hook (python-mode . flycheck-mode))

(use-package pipenv
  :ensure t
  :hook (python-mode . pipenv-mode))

(use-package python
  :bind ("C-j" . newline-and-indent)
  :init
  (add-hook 'python-mode-hook (lambda () (yas-reload-all))))

(provide 'mishok-py)
;;; mishok-py ends here
