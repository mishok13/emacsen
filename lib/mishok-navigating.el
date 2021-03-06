;;; mishok-navigating --- Global key-bindings
;;; Commentary:
;;; All other bindings are set per major mode.
;;; Code:
(require 'use-package)

(use-package expand-region
  :straight t
  :bind (("M-@" . er/expand-region)))

;; Activate windmove
;; Temporary workaround for windmove bug: http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16017#11
;; (setq windmove-window-distance-delta 2)
(use-package windmove
  :straight t
  :init
  (global-unset-key (kbd "<right>"))
  (global-unset-key (kbd "<left>"))
  (global-unset-key (kbd "<up>"))
  (global-unset-key (kbd "<down>"))
  :bind (("<left>" . windmove-left)
         ("<right>" . windmove-right)
         ("<up>" . windmove-up)
         ("<down>" . windmove-down)))

(use-package undo-tree
  :straight t
  :bind (("C-z" . undo-tree-undo)
         ("C-M-z" . undo-tree-redo))
  :config
  (global-undo-tree-mode))

(use-package smex
  :straight t
  :bind (("M-X" . smex-major-mode-commands)))

(use-package helm
  :straight t
  :custom (helm-completion-style 'emacs)
  :init
  (setq helm-M-x-fuzzy-match t)
  ;; Enable fuzzy matching for recent files
  (setq helm-recentf-fuzzy-match t)
  ;; Prevent helm from annoying message pop up when fuzzy-completing
  ;; https://github.com/emacs-helm/helm/issues/550
  (setq helm-exit-idle-delay 0)
  (setq completion-styles
        `(basic partial-completion emacs22 initials
                ,(if (version<= emacs-version "27.0") 'helm-flex 'flex)))
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)))

(provide 'mishok-navigating)
;;; mishok-navigating ends here
