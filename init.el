;; .emacs
;; Andrii V. Mishkovskyi


(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(global-font-lock-mode t)
(set-face-attribute 'default nil :font "Consolas-16")

(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-set-key (kbd "<right>") 'next-buffer)
(global-set-key (kbd "<left>") 'previous-buffer)
(global-set-key (kbd "<up>") 'other-window)
(global-set-key (kbd "<down>") 'other-window)
(global-set-key (kbd "C-z") 'undo)

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
               :after (lambda () (global-set-key (kbd "<f7>") 'magit-status)))
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
	(:name highlight-parentheses
	       :after (lambda () (autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
			(dolist (hook '(python-mode-hook emacs-lisp-mode-hook))
			  (add-hook hook 'highlight-parentheses-mode))))))

(setq my-packages
      (append
       '(el-get color-theme python-mode vkill
	 yaml-mode clojure-mode twittering-mode
	 ;; fill-column-indicator
	 )
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
(el-get 'wait)

(require 'midnight)
(require 'autopair)
(require 'magit)
(require 'magit-svn)
(require 'column-marker)
(require 'linum)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)
(global-linum-mode 1)
(column-number-mode 1)
(ido-mode t)
(global-hl-line-mode t)
;; (show-paren-mode t)
(autopair-global-mode)
(which-func-mode t)


(when (and window-system (eq system-type 'darwin))
  (load "osx.el")) ;; OS X-specific init

(load "stupids.el") ;; stupid utilities
(load "languages/c.el")
(load "languages/python.el")

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

;; clear up files before saving them
(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file and leaves single newline character."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (newline)              ;; ensures that there is at least one
    (delete-blank-lines))) ;; leaves at most one

;; Don't leave garbage when saving files
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-blank-lines)


;; interesting mode for highlighting parens in different colors
;; (require 'highlight-parentheses)


(push '("." . "~/.emacs-backups") backup-directory-alist)

;; flymake special
;; (load-library "flymake-cursor")


;; this should enable copy from emacs to any other X frame
(setq x-select-enable-clipboard t)

;; make scroll behave more like notepad, he-he
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)

(load "goodies/clean-buffers.el")

(setq inferior-lisp-program "java -cp /home/mishok/.clojure/clojure.jar:/home/mishok/.clojure/clojure-contrib.jar clojure.main")
