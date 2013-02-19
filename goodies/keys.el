;; Globally set keys

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


(global-set-key (kbd "<f8>") 'org-capture)

(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(global-set-key (kbd "M-j")
            (lambda ()
              (interactive)
              (join-line -1)))
