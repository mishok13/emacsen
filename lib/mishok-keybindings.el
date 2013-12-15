;;; mishok-keybindings --- Globally set keys
;;; Commentary:
;;; Code:

(require 'cc-mode)
(require 'expand-region)
(require 'smex)
(require 'undo-tree)

(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

;; Activate windmove
(windmove-default-keybindings)

(global-set-key (kbd "<right>") 'next-buffer)
(global-set-key (kbd "<left>") 'previous-buffer)
(global-set-key (kbd "<up>") 'other-window)
(global-set-key (kbd "<down>") 'other-window)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-o") 'ido-switch-buffer)

(global-set-key (kbd "<f4>") 'flycheck-previous-error)
(global-set-key (kbd "<f5>") 'flycheck-next-error)

(global-set-key (kbd "<f8>") 'org-capture)

(global-set-key (kbd "C-@") 'er/expand-region)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "M-c") 'c-hungry-delete-forward)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-_"))
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

(provide 'mishok-keybindings)
;;; mishok-keybindings ends here
