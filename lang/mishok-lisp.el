;;; mishok-lisp --- ELisp setup
;;; Commentary:
;;; Code:

(require 'lisp-mode)
(require 'paredit)

;; (define-globalized-minor-mode global-highlight-parentheses-mode
;;  highlight-parentheses-mode
;;  (lambda () (highlight-parentheses-mode t)))
;; (global-highlight-parentheses-mode t)

(add-hook 'lisp-mode-hook 'enable-paredit-mode)

(provide 'mishok-lisp)
;;; mishok-lisp ends here
