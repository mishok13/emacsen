;;; mishok-clj --- Summary
;;; Commentary:
;;; Code:
(require 'clojure-mode)
(require 'cider)
(require 'paredit)

;; Enable Paredit for Clojure source code
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; Cider (formerly nrepl.el) setup
(setq cider-popup-stacktraces nil)
(setq cider-repl-popup-stacktraces t)
(setq cider-repl-history-file "/tmp/replhistory")
(add-hook 'cider-repl-mode-hook 'paredit-mode)
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; Pretty printing for partial, comp and fn form
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

(provide 'mishok-clj)
;;; mishok-clj ends here
