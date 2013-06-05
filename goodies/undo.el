(require 'undo-tree)
(global-undo-tree-mode)

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-_"))
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)
