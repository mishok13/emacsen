;;; init.el --- Initialization code for Emacs
;;; Commentary:
;;; License: See LICENSE file

;;; Code:

(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package zenburn-theme
  :straight t
  :init (load-theme 'zenburn t))


(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/lib/lang")

(when (eq system-type 'darwin)
  (load-file "~/.emacs.d/osx.el"))

(use-package undo-tree
  :straight t)

(use-package mishok-prog)
(use-package mishok-lisp)
(use-package mishok-sgml)
(use-package mishok-py)
(use-package mishok-go)
(use-package mishok-js)
(use-package mishok-clj)
(use-package mishok-c)
(use-package mishok-rust)
(use-package mishok-haskell)
(use-package mishok-typescript)


(use-package mishok-navigating)
(use-package mishok-editing)
(use-package mishok-viewing)
(use-package mishok-git)
;; (use-package mishok-twit)
(use-package mishok-utils)
(use-package mishok-org)
(use-package mishok-display)

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; ;;; init.el ends here
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(helm-completion-style (quote emacs))
;;  '(initial-frame-alist (quote ((fullscreen . maximized))))
;;  '(package-selected-packages
;;    (quote
;;     (web-mode blacken feature-mode company-terraform terraform-mode smart-mode-line twittering-mode magit-gitflow travis magit transpose-frame yasnippet-snippets projectile-ripgrep protobuf-mode neotree helm-projectile helm-rg helm flx-ido markdown-mode shm haskell-mode racer clj-refactor clojure-mode company tide json-mode typescript-mode go-mode pipenv jedi smartparens emmet-mode aggressive-indent rainbow-delimiters paredit dockerfile-mode yaml-mode flycheck fill-column-indicator smex expand-region undo-tree exec-path-from-shell use-package))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
