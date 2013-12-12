(require 'emmet-mode)
(setq emmet-preview-default nil)
(define-key emmet-mode-keymap (kbd "C-j") 'newline-and-indent)
(define-key emmet-mode-keymap (kbd "C-m") 'emmet-expand-line)

(provide 'mishok-sgml)
