(require 'cider)

(require 'paredit)
(require 'clojure-mode)

;; (add-to-list 'same-window-buffer-names "*nrepl*")

(add-hook 'nrepl-connected-hook 'bury-buffer) ;;; don't send me to the repl on connect
;; (add-hook 'nrepl-connected-hook 'reset-nrepl-connection-to-default)
;;; always default to first connection

;; (add-hook 'nrepl-interaction-mode-hook 'my-nrepl-mode-setup)
;; (defun my-nrepl-mode-setup ()
;;   (require 'nrepl-ritz))


(defun mishok-pretty-partial ()
  (font-lock-add-keywords nil
                          `(("(\\(partial\\)[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1)
                                                       "\u03a0"
                                                       'decompose-region)))))))

(defun mishok-pretty-comp ()
  (font-lock-add-keywords nil
                          `(("(\\(comp\\)[[:space:]]"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1)
                                                       "\u2218"
                                                       'decompose-region)))))))

(defun mishok-pretty-fn ()
  (font-lock-add-keywords nil
                          `(("(\\(\\<fn\\>\\)"
                             (0 (progn (compose-region (match-beginning 1)
                                                       (match-end 1)
                                                       "\u0192"
                                                       'decompose-region)))))))

(add-hook 'clojure-mode-hook 'mishok-pretty-fn)
(add-hook 'clojure-mode-hook 'mishok-pretty-partial)
(add-hook 'clojure-mode-hook 'mishok-pretty-comp)

;; (add-hook 'clojure-mode-hook 'kibit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
;; (add-hook 'nrepl-mode-hook 'paredit-mode)
;; (add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-history-file "/tmp/replhistory")
(add-hook 'cider-repl-mode-hook 'paredit-mode)
