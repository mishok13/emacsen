;;; init.el --- Initialization code for Emacs
;;; Commentary:
;;; License: See LICENSE file

;;; Code:

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
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

(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs
        '("~/nonwork/yasnippet-snippets"
          "~/nonwork/emacsen/snippets/"))
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package mishok-prog)
(use-package mishok-lisp)
(use-package mishok-sgml)
(use-package mishok-py)
(use-package mishok-go)
(use-package mishok-clj)
(use-package mishok-c)
(use-package mishok-rust)
(use-package mishok-haskell)

(use-package mishok-editing)
(use-package mishok-viewing)
(use-package mishok-keybindings)
(use-package mishok-git)
(use-package mishok-twit)
(use-package mishok-utils)
(use-package mishok-org)
(use-package mishok-display)
(use-package mishok-commands)
;;; init.el ends here
