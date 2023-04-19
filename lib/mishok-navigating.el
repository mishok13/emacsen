;;; mishok-navigating --- Global key-bindings
;;; Commentary:
;;; All other bindings are set per major mode.
;;; Code:
(require 'use-package)
(use-package expand-region
  :straight t
  :bind (("M-@" . er/expand-region)))

(use-package windmove
  :straight t
  :init
  (global-unset-key (kbd "<right>"))
  (global-unset-key (kbd "<left>"))
  (global-unset-key (kbd "<up>"))
  (global-unset-key (kbd "<down>"))
  :bind (("<left>" . windmove-left)
         ("<right>" . windmove-right)
         ("<up>" . windmove-up)
         ("<down>" . windmove-down)))

(use-package undo-tree
  :straight t
  :bind (("C-z" . undo-tree-undo)
         ("C-M-z" . undo-tree-redo))
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.undo")))
  (global-undo-tree-mode))


(provide 'mishok-navigating)
;;; mishok-navigating ends here
