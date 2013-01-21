;; .emacs
;; Andrii V. Mishkovskyi

(add-to-list 'load-path "~/.emacs.d/")

(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; This should be moved to another module, to speedup startup time
;; (defvar prelude-packages
;;   '(ack-and-a-half auctex clojure-mode expand-region
;;                    autopair fill-column-indicator flymake-cursor
;;                    helm highlight-parentheses
;;                    magit markdown-mode paredit projectile
;;                    nrepl nrepl-ritz org php-mode powerline
;;                    rainbow-mode python-mode python-pep8 smex
;;                    twittering-mode virtualenv yasnippet zencoding-mode)
;;   "A list of packages to ensure are installed at launch.")

;; (defun prelude-packages-installed-p ()
;;   (loop for p in prelude-packages
;;         when (not (package-installed-p p)) do (return nil)
;;         finally (return t)))

;; (unless (prelude-packages-installed-p)
;;   ;; check for new packages (package versions)
;;   (message "%s" "Emacs Prelude is now refreshing its package database...")
;;   (package-refresh-contents)
;;   (message "%s" " done.")
;;   ;; install the missing packages
;;   (dolist (p prelude-packages)
;;     (when (not (package-installed-p p))
;;       (package-install p))))

;; (provide 'prelude-packages)




(global-font-lock-mode t)
(set-face-attribute 'default nil :font "Consolas-16")

;; I HATE ANNOYING SPLASH SCREEN
(setq inhibit-splash-screen t)

;; 1-2 letters shorter to type!
(fset 'yes-or-no-p 'y-or-n-p)


(load-theme 'mishok-dark t)

(require 'yasnippet)
(yas/global-mode t)

(require 'autopair)
(autopair-global-mode t)

(require 'smex)
(smex-initialize)

(require 'twittering-mode)
(setq twittering-use-master-password t)

(require 'fill-column-indicator)
(setq-default fill-column 80)


(define-globalized-minor-mode global-highlight-parentheses-mode
 highlight-parentheses-mode
 (lambda () (highlight-parentheses-mode t)))
(global-highlight-parentheses-mode t)

(require 'powerline)
(powerline-default)

(require 'paredit)
(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'clojure-mode-hook 'enable-paredit-mode)

(tool-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode 0)
(setq initial-scratch-message nil)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(line-move-visual 0)

(global-linum-mode t)
(column-number-mode t)
(ido-mode t)
(global-hl-line-mode t)
(which-func-mode t)
(setq-default indent-tabs-mode nil)


(when (and window-system (eq system-type 'darwin))
  (load "osx.el")) ;; OS X-specific init

(load "stupids.el") ;; stupid utilities

;; Different languages support
(load "languages/c.el")
(load "languages/python.el")
(load "languages/clojure.el")

(load "goodies/flymake-init.el")
(load "goodies/clean-buffers.el")
(load "goodies/uniquify-buffer-names.el")
(load "goodies/remove-trailing-whitespace.el")
(load "goodies/backups.el")
(load "goodies/scrolling.el")
(load "goodies/copying.el")
(load "goodies/keys.el")
(load "goodies/save-history.el")
(load "goodies/open-files.el")
(load "goodies/org.el")
(load "goodies/git.el")

(set-face-attribute 'default (not 'this-frame-only)
                    :font "Consolas")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("3881005eae910bee61cc5a86bb4ddfc4e416076b32a458f5f227c2f7c1e90cec" "1f824669122549e2943dfc4f90948ddfd48e6a617e55ebf1cc9cba4efd24aed1" "638f6dca11a3f236f8222c494835c7f2cf59245dc96564d76c8d7bcd1cdeb562" "2de1f80d1b314e07ae8626efc79fb0190ee75936a99668adc1d57dfbd1c3e3d5" "319b4580ad043aa536e6b388b8fb46eb5c6f2285cc43aa5b7cab3c7c16c29cee" "a16ad1c472ed397848d93f61b382db7ae5344efe285014d06b4839b7c1b32ccf" "b3b20e6258dbbd3d7f08af20cad0273eeb8ee743da8c8982fc34cc3a2fd86991" "7a2acf3ed00ebe28cf66efd3675133e07b69f363eef6aeb439ec3f30752a9bf9" "8f0dc5b95e2d7b423a56f4434e26f2b5a5c4213608ab3c0339ca33f27666487a" "3b615b0370e3cb2c3e622e08a2f9e0b1450711043a71350694812907aa2550d3" "56cd68df8602069e976064102b0fe4bce5b891bebc23fe4c9b2dae4477f9aa91" "620a2eda00c00711860d5d09e109f8de57a59b6a634bfb2d3cd3952ce4e91212" "74abb7c26b98b385be251de260f5eea71865cc5c244f929b77345b991c1eec2b" "222b8742907277c6c57542cc3668d3bfd52929993a1309be2366d00a3f9d742e" "eed5edb4a621615feb95481f7536b5fa4f633a0f924a159c40ed8457d7e79dbf" "3d331f26183d402b1c79b3cef7ebd5f950e9f3fb24eee0f6c637158e6497619b" "b061c6de0500271fb7e8863fae18f0e68e072c11a315058472e4864925b04870" "a73835a9bb3f74daeda3e3b987994a03d624e62085ae703d50ba0e3b3273dce6" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "27470eddcaeb3507eca2760710cc7c43f1b53854372592a3afa008268bcf7a75" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(safe-local-variable-values (quote ((sql-product quote postgres)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
