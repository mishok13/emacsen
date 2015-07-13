;;; mishok-keybindings --- Global key-bindings
;;; Commentary:
;;; All other bindings are set per major mode.
;;; Code:
(require 'use-package)
(require 'cc-mode)
(require 'expand-region)
(require 'smex)
(require 'undo-tree)
(require 'windmove)
(require 'magit)


(use-package expand-region
  :ensure t
  :bind (("M-@" . er/expand-region)))

;; Activate windmove
;; Temporary workaround for windmove bug: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16017#11
;; (setq windmove-window-distance-delta 2)
(use-package windmove
  :ensure t
  :init
  (global-unset-key (kbd "<right>"))
  (global-unset-key (kbd "<left>"))
  (global-unset-key (kbd "<up>"))
  (global-unset-key (kbd "<down>"))
  :bind (("<left>" . windmove-left)
         ("<right>" . windmove-right)
         ("<up>" . windmove-up)
         ("<down>" . windmove-down))
  :config
  (windmove-default-keybindings))

(use-package hungry-delete
  :bind (("M-c" . c-hungry-delete-forward)))

(use-package undo-tree
  :ensure t
  :bind (("C-z" . undo-tree-undo)
         ("C-M-z" . undo-tree-redo))
  :config
  (global-undo-tree-mode))

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("M-X" . smex-major-mode-commands)))

(provide 'mishok-keybindings)
;;; mishok-keybindings ends here
