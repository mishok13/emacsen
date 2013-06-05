(require 'nrepl)
(require 'paredit)

(add-hook 'nrepl-connected-hook 'bury-buffer) ;;; don't send me to the repl on connect
(add-hook 'nrepl-connected-hook 'reset-nrepl-connection-to-default) ;;; always default to first connection
(add-hook 'nrepl-mode-hook 'paredit-mode)
