;;; mishok-c -- C/C++ setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package cc-mode
  :ensure t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-style "k&r")
              (setq indent-tabs-mode nil))))

(provide 'mishok-c)
;;; mishok-c.el ends here
