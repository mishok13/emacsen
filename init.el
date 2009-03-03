;; .emacs
;; Andrii V. Mishkovskyi

(add-to-list 'load-path "~/.emacs.d/")

(global-font-lock-mode t)

;; XXX: add normal color theme

;; interesting mode for highlighting parens in different colors
(require 'highlight-parentheses)

;; this should highlight any line longer than 80 symbols
(require 'highlight-80+)

(require 'pymacs)
(require 'linum)

(which-func-mode t)


;; Pymacs special
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)

(define-globalized-minor-mode
  global-highlight-parentheses-mode
  highlight-parentheses-mode 
  highlight-parentheses-mode)
(global-highlight-parentheses-mode)

;; flymake special
(require 'flymake)
(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)
(global-set-key [f5] 'flymake-goto-prev-error)

(defun flymake-get-tex-args (file-name)
  (list "pdflatex" (list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

;; pylint checking
(when (load "flymake" t)
  (defun flymake-pylint-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		       'flymake-create-temp-inplace))
           (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
      (list "epylint" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py\\'" flymake-pylint-init)))

(load-library "flymake-cursor")
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Loading ropemacs
(pymacs-load "ropemacs" "rope-")

;; show line number on the left pane
(global-linum-mode 1)

;; show column number
(column-number-mode 1)

(setq-default fill-column 72)

;; this should enable copy from emacs to any other X frame
(setq x-select-enable-clipboard t)

;; make scroll behave more like notepad, he-he
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)

;; python related stuff
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-variable-buffer-local 'beginning-of-defun-function)
		 'py-beginning-of-def-or-class)
	    (setq outline-regexp "def\\|class ")))

;; IPython
(setq ipython-command "/usr/bin/ipython")
(require 'ipython)


(load-file "~/.emacs.d/cedet/common/cedet.el")
(add-to-list 'load-path "~/.emacs.d/cedet/eieio/")
(add-to-list 'load-path "~/.emacs.d/cedet/semantic/")

(global-ede-mode t)
;; (semantic-load-enable-code-helpers)
(add-to-list 'load-path "~/.emacs.d/cedet/speedbar/")
(require 'sr-speedbar)
(require 'semantic-ia)
(require 'semantic-gcc)
(require 'semanticdb)
(global-semanticdb-minor-mode 1)

(load-file "~/.emacs.d/twit.el")


;; TinyURL
(require 'mm-url)
(defun get-tinyurl ()
  "Grabs the url at point and echos the equivalent tinyurl in the
minibuffer to ease cutting and pasting."
  (interactive)
  (let* ((long-url (thing-at-point 'url))
	 (tinyurl
	  (save-excursion
	    (with-temp-buffer
	      (mm-url-insert
	       (concat "http://tinyurl.com/api-create.php?url=" long-url))
	      (kill-ring-save (point-min) (point-max))
              (buffer-string)))))
    (message tinyurl)))


(setq-mode-local c-mode semanticdb-find-default-throttle
                 '(project unloaded system recursive))
(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS"))
(add-hook 'semantic-init-hooks 'my-semantic-hook)

(ido-mode t)

;; (require 'semantic)
;; (require 'eieio)

(global-set-key [f8] 'sr-speedbar-toggle)
(global-set-key [f7] 'other-window)

;; (add-to-list 'load-path "~/.emacs.d/ecb/")
;; (require 'ecb)


(add-to-list 'load-path "~/.emacs.d/color-theme/")
(add-to-list 'load-path "~/.emacs.d/color-theme/themes/")
(require 'color-theme)
(color-theme-initialize)
(color-theme-mishok)
;; used to edit debian/control files

(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
;;(setq desktop-buffers-not-to-save
;;      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;;	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb" 
;;	      "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; (global-set-key [(control #)] 'comment-region)
;; (global-set-key [(control @)] 'uncomment-region)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(ecb-options-version "2.33beta2")
 '(global-hl-line-mode t)
 '(kill-whole-line t)
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar))
 '(twit-mode t)
 '(twit-pass "hujhujhuj")
 '(twit-user "mishok13"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#1a1a1a" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 160 :width normal :foundry "unknown" :family "Inconsolata"))))
 '(flymake-errline ((((class color)) (:background "Grey25" :underline "Firebrick"))))
 '(flymake-warnline ((((class color)) (:underline "Grey50"))))
 '(hl-line ((t (:inherit highlight)))))

(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

(setq ring-bell-function 'ignore)
