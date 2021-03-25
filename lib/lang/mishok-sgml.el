;;; mishok-sgml --- SGML and children handling
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package emmet-mode
  :straight t
  :init
  ;; Disable preview before expanding
  (setq emmet-preview-default nil)
  ;; Move the cursor to next edit point
  (setq emmet-move-cursor-between-quotes t)
  :config
  (define-key emmet-mode-keymap (kbd "C-m") 'emmet-expand-line)
  (define-key emmet-mode-keymap (kbd "C-j") 'newline-and-indent))

;; (use-package web-mode
;;   :straight t
;;   :mode "\\.html\\'"
;;   :config
;;   (add-hook 'web-mode-hook 'emmet-mode))

;; (defun web-mode-django-setup ()
;;   "Hooks and general setup for Django templating support."
;;   (setq web-mode-markup-indent-offset 4)
;;   (setq web-mode-code-indent-offset 4)
;;   (setq web-mode-enable-control-block-indentation t)
;;   (web-mode-set-engine "django")
;;   (setq indent-tabs-mode nil))

(provide 'mishok-sgml)
;;; mishok-sgml.el ends here
