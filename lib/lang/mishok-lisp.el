;;; mishok-lisp --- ELisp setup
;;; Commentary:
;;; Code:

(use-package paredit
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package aggressive-indent
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))

(provide 'mishok-lisp)
;;; mishok-lisp ends here
