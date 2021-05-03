;;; mishok-py --- Python setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package smartparens
  :straight t
  :hook ((python-mode . smartparens-mode)
         (rust-mode . smartparens-mode)))

(use-package jedi
  :straight t)

(use-package elpy
  :straight t
  :init
  (elpy-enable))

(use-package flycheck
  :straight t
  :hook (python-mode . flycheck-mode))

(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode))

(use-package python
  :bind ("C-j" . newline-and-indent)
  :init
  (add-hook 'python-mode-hook (lambda () (yas-reload-all))))

(provide 'mishok-py)
;;; mishok-py ends here
