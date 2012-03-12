;; Globally set keys

(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-set-key (kbd "<right>") 'next-buffer)
(global-set-key (kbd "<left>") 'previous-buffer)
(global-set-key (kbd "<up>") 'other-window)
(global-set-key (kbd "<down>") 'other-window)
(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "M-o") 'ido-switch-buffer)
