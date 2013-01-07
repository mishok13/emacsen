;; .emacs
;; Andrii V. Mishkovskyi

(add-to-list 'load-path "~/.emacs.d/")
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")

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
;; (defvar my-packages '(clojure-mode
;;                       nrepl
;;                       nrepl-ritz))
;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))


(global-font-lock-mode t)
(set-face-attribute 'default nil :font "Consolas-16")

;; I HATE ANNOYING SPLASH SCREEN
(setq inhibit-splash-screen t)

;; 1-2 letters shorter to type!
(fset 'yes-or-no-p 'y-or-n-p)


(load-theme 'tango-dark t)

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
