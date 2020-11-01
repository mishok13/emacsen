;;; init.el --- Initialization code for Emacs
;;; Commentary:
;;; License: See LICENSE file

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/lib/lang")

(when (eq system-type 'darwin)
  (load-file "~/.emacs.d/osx.el"))

(use-package undo-tree
  :ensure t)

(use-package expand-region
  :ensure t)

(use-package smex
  :ensure t)

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
(use-package mishok-twit)
(use-package mishok-utils)
(use-package mishok-org)
(use-package mishok-display)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(helm-completion-style (quote emacs))
 '(initial-frame-alist (quote ((fullscreen . maximized))))
 '(package-selected-packages
   (quote
    (blacken feature-mode company-terraform terraform-mode smart-mode-line twittering-mode magit-gitflow travis magit transpose-frame yasnippet-snippets projectile-ripgrep protobuf-mode neotree helm-projectile helm-rg helm flx-ido markdown-mode shm haskell-mode racer clj-refactor clojure-mode company tide json-mode typescript-mode go-mode pipenv jedi smartparens emmet-mode aggressive-indent rainbow-delimiters paredit dockerfile-mode yaml-mode flycheck fill-column-indicator smex expand-region undo-tree exec-path-from-shell use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
