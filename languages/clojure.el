(require 'clojure-mode)
(require 'kibit-mode)

(add-hook 'nrepl-interaction-mode-hook 'my-nrepl-mode-setup)
(defun my-nrepl-mode-setup ()
  (require 'nrepl-ritz))

(add-hook 'clojure-mode-hook 'fci-mode)
(add-hook 'clojure-mode-hook 'flycheck-mode)
(add-hook 'clojure-mode-hook 'kibit-mode)
