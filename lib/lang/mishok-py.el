;;; mishok-py --- Python setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package smartparens
  :ensure t
  :config
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook 'electric-indent-mode))

(use-package python
  :bind ("C-j" . newline-and-indent))

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell)
  (setq venv-location (expand-file-name "~/.venv"))
  (add-hook 'python-mode-hook (lambda ()
                                (hack-local-variables)
                                (when (boundp 'project-venv-name)
                                  (venv-workon project-venv-name)))))


(provide 'mishok-py)
;;; mishok-py ends here
