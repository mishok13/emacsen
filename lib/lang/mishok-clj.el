;;; mishok-clj --- Clojure setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package company
  :straight t
  :config
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common))

(use-package clojure-mode
  :straight t
  :config
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode))

(use-package clj-refactor
  :straight t
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-b"))))

(use-package cider
  :straight t
  :config
  (setq cider-show-error-buffer 'only-in-repl)
  (setq cider-auto-select-error-buffer nil)
  (setq nrepl-hide-special-buffers t)
  ;; Wrap stacktraces at whatever fill-column is set to
  (setq cider-stacktrace-fill-column t)
  ;; Don't prompt for symbol names when jumping to definitions
  (setq cider-prompt-for-symbol nil)
  ;; Write REPL history to file
  (setq cider-repl-history-file "/tmp/replhistory")
  (setq cider-auto-select-error-buffer nil)
  ;; Enable paredit in REPL
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  ;; Enable eldoc in REPL
  (add-hook 'cider-mode-hook 'eldoc-mode))

(use-package company
  :straight t
  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  (add-hook 'cider-mode-hook #'company-mode))

(provide 'mishok-clj)
;;; mishok-clj ends here
