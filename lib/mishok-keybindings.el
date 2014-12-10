;;; mishok-keybindings --- Global key-bindings
;;; Commentary:
;;; All other bindings are set per major mode.
;;; Code:

(require 'cc-mode)
(require 'expand-region)
(require 'smex)
(require 'undo-tree)
(require 'windmove)
(require 'magit)

(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))

(global-set-key (kbd "<f7>") 'magit-status)

;; Activate windmove
(windmove-default-keybindings)
;; Temporary workaround for windmove bug: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16017#11
;; (setq windmove-window-distance-delta 2)

(global-set-key (kbd "<left>")  'windmove-left)
(global-set-key (kbd "<right>") 'windmove-right)
(global-set-key (kbd "<up>")    'windmove-up)
(global-set-key (kbd "<down>")  'windmove-down)
;; (global-set-key (kbd "<right>") 'next-buffer)
;; (global-set-key (kbd "<left>") 'previous-buffer)
;; (global-set-key (kbd "<up>") 'other-window)
;; (global-set-key (kbd "<down>") 'other-window)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-o") 'ido-switch-buffer)

;; This should probably go to prog-mode setup
(global-set-key (kbd "<f4>") 'flycheck-previous-error)
(global-set-key (kbd "<f5>") 'flycheck-next-error)

(global-set-key (kbd "<f8>") 'org-capture)
(global-set-key (kbd "<f10>") 'org-agenda)

;; Expand region support, enables nice selection when in programming
;; modes, works great for markdown and latex as well
(global-set-key (kbd "M-@") 'er/expand-region)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

(global-set-key (kbd "M-c") 'c-hungry-delete-forward)
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-_"))
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-M-z") 'undo-tree-redo)

(provide 'mishok-keybindings)
;;; mishok-keybindings ends here
