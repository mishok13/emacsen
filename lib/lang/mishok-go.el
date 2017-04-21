;;; mishok-go --- Golang setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package go-mode
  :ensure t
  :bind (("C-c C-r" . go-remove-unused-imports)
         ("C-c C-i" . go-goto-imports)
         ("M-." . godef-jump))
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'go-mode-hook 'company-mode)
  (add-hook 'go-mode-hook 'smartparens-mode)
  (setenv "GOPATH" (concat (getenv "HOME") "/work")))

(provide 'mishok-go)
;;; mishok-py ends here
