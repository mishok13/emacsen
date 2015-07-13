;;; mishok-commands -- All the Emacs commands improvements
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package helm
  :init
  (setq helm-M-x-fuzzy-match t)
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)))

(provide 'mishok-commands)
;;; mishok-commands.el ends here
