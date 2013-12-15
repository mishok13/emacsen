;;; mishok-prog --- All programming-global setups

;;; Commentary:

;;; Code:

(require 'prog-mode)
(require 'fill-column-indicator)
(require 'flycheck)

(setq-default fci-rule-column 80)
(add-hook 'prog-mode-hook 'fci-mode)
(which-function-mode t)

(add-hook 'after-init-hook #'global-flycheck-mode)
(defun flymake-xml-init ())
(defun flymake-java-init ())

(require 'flyspell)

(add-hook 'prog-mode-hook 'flyspell-prog-mode)

(provide 'mishok-prog)
;;; mishok-prog ends here
