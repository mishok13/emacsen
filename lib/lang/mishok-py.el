;;; mishok-py --- Python setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package smartparens
  :ensure t
  :config
  (add-hook 'python-mode-hook 'smartparens-mode)
  (add-hook 'python-mode-hook 'electric-indent-mode))

(defun mishok/python-flycheck-setup ()
  (let ((exec-path (python-shell-calculate-exec-path)))
    (setq-local flycheck-python-pylint-executable (executable-find "pylint"))
    (setq-local flycheck-python-flake8-executable (executable-find "flake8"))))

(defun mishok/workon (&optional VENV-NAME)
  (interactive)
  (venv-workon VENV-NAME)
  (mishok/python-flycheck-setup)
  (flycheck-mode nil)
  (flycheck-add-next-checker 'python-flake8 'python-pylint)
  (flycheck-mode t))

(use-package python
  :bind ("C-j" . newline-and-indent)
  :init
  (add-hook 'hack-local-variables-hook 'mishok/python-flycheck-setup nil 'local))

(use-package jedi
  :ensure t)

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
