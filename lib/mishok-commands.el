;;; mishok-commands -- All the Emacs commands improvements
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t)
  ;; Enable fuzzy matching for recent files
  (setq helm-recentf-fuzzy-match t)
  ;; Prevent helm from annoying message pop up when fuzzy-completing
  ;; https://github.com/emacs-helm/helm/issues/550
  (setq helm-exit-idle-delay 0)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)))

(provide 'mishok-commands)
;;; mishok-commands.el ends here
