;;; mishok-lisp --- ELisp setup
;;; Commentary:
;;; Code:

(use-package paredit
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package aggressive-indent
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(provide 'mishok-lisp)
;;; mishok-lisp ends here
