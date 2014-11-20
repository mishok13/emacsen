;;; mishok-prog --- All programming-global setups

;;; Commentary:

;;; Code:

(require 'prog-mode)
(require 'fill-column-indicator)
(require 'flycheck)
(require 'aggressive-indent)
(require 'flyspell)

(add-hook 'prog-mode-hook #'aggressive-indent-mode)
(setq-default fci-rule-column 80)
(add-hook 'prog-mode-hook 'fci-mode)
(which-function-mode t)

(add-hook 'after-init-hook #'global-flycheck-mode)
(defun flymake-xml-init ())
(defun flymake-java-init ())

(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(provide 'mishok-prog)
;;; mishok-prog ends here
