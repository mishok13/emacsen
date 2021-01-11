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
  :bind ("C-c C-f" . elpy-black-fix-code)
  :config
  (if (executable-find "python3")
      (progn
        (setq elpy-rpc-python-command "python3")
        (setq python-shell-interpreter "python3")))
  (use-package jedi
    :ensure t)
  ;; (add-hook 'elpy-mode-hook
  ;;           '(lambda ()
  ;;              (when (eq major-mode 'python-mode)
  ;;                (add-hook 'after-save-hook 'elpy-black-fix-code))))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (elpy-enable))

(use-package flycheck
  :straight t
  :hook (python-mode . flycheck-mode))

(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode))

(use-package poetry
  :ensure t
  :straight t)

(use-package python
  :bind ("C-j" . newline-and-indent)
  :init
  (add-hook 'python-mode-hook (lambda () (yas-reload-all))))

(provide 'mishok-py)
;;; mishok-py ends here
