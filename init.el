;; .emacs
;; Andrii V. Mishkovskyi

(add-to-list 'load-path "~/.emacs.d/")

(global-font-lock-mode t)


(defun donuts ()
  "For the love of God"
  (interactive)
  (print "Mmmm, donuts."))

;; XXX: add normal color theme

;; interesting mode for highlighting parens in different colors
(require 'highlight-parentheses)

;; this should highlight any line longer than 80 symbols
(require 'highlight-80+)

(require 'pymacs)
(require 'linum)

(which-func-mode t)

;;'(flymake-allowed-file-name-masks (quote nil))
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


(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

(push '("." . "~/.emacs-backups") backup-directory-alist)

;; flymake special
(require 'flymake)
(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
(global-set-key [f4] 'flymake-goto-next-error)
(global-set-key [f5] 'flymake-goto-prev-error)

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

(load-library "pyrex-mode")

(load-library "flymake-cursor")
(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Loading ropemacs
(pymacs-load "ropemacs" "rope-")

;; show line number on the left pane
(global-linum-mode 1)

;; show column number
(column-number-mode 1)

(scroll-bar-mode -1)

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


(ido-mode t)

;; (require 'semantic)
;; (require 'eieio)

(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map [f6] 'org-store-link)
(define-key global-map [f7] 'org-agenda)
;; (global-set-key (kbd "<f12>") 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files (list "~/.emacs.d/orgfiles/work.org"
			     "~/.emacs.d/orgfiles/blog.org"
			     "~/.emacs.d/orgfiles/tilman.org"
			     "~/.emacs.d/orgfiles/triton.org"
			     "~/.emacs.d/orgfiles/vectormaps.org" 
			     "~/.emacs.d/orgfiles/render.org"
			    ))



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

;; midnight mode
(require 'midnight)

;;kill buffers if they were last disabled more than this seconds ago
(setq clean-buffer-list-delay-special 1800)

(defvar clean-buffer-list-timer nil
  "Stores clean-buffer-list timer if there is one. You can disable clean-buffer-list by (cancel-timer clean-buffer-list-timer).")

;; run clean-buffer-list every 2 hours
(setq clean-buffer-list-timer (run-at-time t 3600 'clean-buffer-list))

;; kill everything, clean-buffer-list is very intelligent at not killing
;; unsaved buffer.
(setq clean-buffer-list-kill-regexps
      '("^.*$"))

;; keep these buffer untouched
;; prevent append multiple times
(defvar clean-buffer-list-kill-never-buffer-names-init
  clean-buffer-list-kill-never-buffer-names
  "Init value for clean-buffer-list-kill-never-buffer-names")
(setq clean-buffer-list-kill-never-buffer-names
      (append
       '("*Messages*" "*scratch*" "*Pymacs*")
       clean-buffer-list-kill-never-buffer-names-init))

;; prevent append multiple times
(defvar clean-buffer-list-kill-never-regexps-init
  clean-buffer-list-kill-never-regexps
  "Init value for clean-buffer-list-kill-never-regexps")
;; append to *-init instead of itself
(setq clean-buffer-list-kill-never-regexps
      (append '("^\\*Org Agenda\\*.*$")
	      clean-buffer-list-kill-never-regexps-init))

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
;; '(org-agenda-files (quote ("~/.emacs.d/orgfiles/work.org" "~/.emacs.d/orgfiles/blog.org" "~/.emacs.d/orgfiles/tilman.org" "~/.emacs.d/orgfiles/triton.org" "~/.emacs.d/orgfiles/vectormaps.org" "~/.emacs.d/orgfiles/render.org")))
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#ffffff" :foreground "#1a1a1a" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "unknown" :family "Inconsolata"))))
 '(flymake-errline ((((class color)) (:background "Grey25" :underline "Firebrick"))))
 '(flymake-warnline ((((class color)) (:underline "Grey50"))))
 '(hl-line ((t (:inherit highlight)))))

(require 'maxframe)
(add-hook 'window-setup-hook 'maximize-frame t)

(setq ring-bell-function 'ignore)
