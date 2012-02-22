;; .emacs
;; Andrii V. Mishkovskyi

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(global-font-lock-mode t)
(set-face-attribute 'default nil :font "Consolas-16")

;; I HATE ANNOYING SPLASH SCREEN
(setq inhibit-splash-screen t)

;; 1-2 letters shorter to type!
(fset 'yes-or-no-p 'y-or-n-p)

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

;; local sources
(setq el-get-sources
      '((:name magit
               :after (lambda ()
			(global-set-key (kbd "<f7>") 'magit-status)
			(require 'magit-svn)))
	(:name expand-region
	       :type git
	       :url "https://github.com/magnars/expand-region.el.git"
	       :description "Increase the selected region by semantic units"
	       :website "https://github.com/magnars/expand-region.el#readme"
	       :after (lambda () (global-set-key (kbd "C-@") 'er/expand-region)))
	(:name yasnippet
	       :after (lambda () (yas/global-mode 1)))
	(:name flymake-cursor
	       :description "Flymake Cursor minor mode"
	       :website "http://www.emacswiki.org/emacs/flymake-cursor.el"
	       :type emacswiki
	       :features flymake-cursor
	       :load "flymake-cursor.el"
	       :after (lambda ()
			(global-set-key (kbd "<f3>") 'flymake-goto-next-error)
			(global-set-key (kbd "<f4>") 'flymake-goto-prev-error)
			(global-set-key (kbd "<f5>") 'flymake-display-err-menu-for-current-line)))
	(:name color-theme-solarized
	       :after (lambda ()
			(color-theme-initialize)
			(color-theme-solarized-dark)))
	(:name autopair
	       :after (lambda () (autopair-global-mode 1)))
	(:name highlight-parentheses
	       :after (lambda ()
			(define-globalized-minor-mode global-highlight-parentheses-mode
			  highlight-parentheses-mode
			  (lambda () (highlight-parentheses-mode t)))
			(global-highlight-parentheses-mode t)))))

(setq my-packages
      (append
       '(el-get color-theme python-mode vkill
	 yaml-mode clojure-mode twittering-mode
	 ;; fill-column-indicator
	 ;; expand-region
	 ;; rainbow-mode
	 )
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
(el-get 'wait)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(global-linum-mode 1)
(column-number-mode 1)
(ido-mode t)
(global-hl-line-mode t)
(which-func-mode t)
(setq-default indent-tabs-mode nil)


(when (and window-system (eq system-type 'darwin))
  (load "osx.el")) ;; OS X-specific init

(load "stupids.el") ;; stupid utilities
(load "languages/c.el")
(load "languages/python.el")
(load "languages/clojure.el")

(load "goodies/clean-buffers.el")
(load "goodies/save-history.el")
(load "goodies/uniquify-buffer-names.el")
(load "goodies/remove-trailing-whitespace.el")
(load "goodies/backups.el")
(load "goodies/scrolling.el")
(load "goodies/copying.el")
(load "goodies/keys.el")
