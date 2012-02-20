;; .emacs
;; Andrii V. Mishkovskyi

(set-face-attribute 'default nil :font "Consolas-16")

(add-to-list 'load-path "~/.emacs.d/")
;; (add-to-list 'load-path "~/.emacs.d/color-theme")
;; (add-to-list 'load-path "~/.emacs.d/color-theme/solarized/")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(add-to-list 'load-path "~/.emacs.d/twittering-mode/")

(global-font-lock-mode t)
(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-set-key [right] 'next-buffer)
(global-set-key [left] 'previous-buffer)
(global-set-key [up] 'other-window)
;; TODO: turn this into previous window
(global-set-key [down] 'other-window)
(global-linum-mode 1)
(column-number-mode 1)
(scroll-bar-mode -1)
(desktop-save-mode 1)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)
(ido-mode t)


;; 1-2 letters shorter to type!
(fset 'yes-or-no-p 'y-or-n-p)

(global-set-key (kbd "C-z") 'undo)
;; (global-set-key [f7] 'magit-status)

(require 'el-get)

;; local sources
(setq el-get-sources
      '((:name magit
               :after (lambda () (global-set-key (kbd "<f7>") 'magit-status)))
	(:name highlight-parentheses
	       :after (lambda () (autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
			(dolist (hook '(python-mode-hook emacs-lisp-mode-hook))
			  (add-hook hook 'highlight-parentheses-mode))))))

(setq my-packages
      (append
       '(el-get yasnippet color-theme color-theme-solarized python-mode)
       (mapcar 'el-get-source-name el-get-sources)))

(el-get 'sync my-packages)
(el-get 'wait)
(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))
(el-get 'sync)

(require 'el-get)
(require 'twittering-mode)
(require 'midnight)
(require 'autopair)
(require 'magit)
(require 'magit-svn)

(require 'color-theme)
(require 'color-theme-solarized)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-solarized-dark)))

(tool-bar-mode -1)
(menu-bar-mode -1)
(global-hl-line-mode t)
(show-paren-mode t)
(blink-cursor-mode nil)
(autopair-global-mode)
(yas/global-mode 1)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

(when (and window-system (eq system-type 'darwin))
  ;; When started from Emacs.app or similar, ensure $PATH
  ;; is the same the user would see in Terminal.app
  (set-exec-path-from-shell-PATH))


; magit
; highlight-parenthesis
; yasnippet
; color-theme: solarized






(require 'yasnippet)
;; (setq yas/snippet-dirs "~/.emacs.d/snippets")
(yas/global-mode 1)


(defun donuts ()
  "For the love of God"
  (interactive)
  (print "Mmmm, donuts."))

(defun look-of-disapproval ()
  "Just in case we need this"
  (interactive)
  (insert "ಠ_ಠ"))

;; (global-set-key [f7] 'magit-status)

(require 'uniquify)
(setq
  uniquify-buffer-name-style 'post-forward
  uniquify-separator ":")

(require 'cc-mode)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "k&r")
            (setq c-basic-offset 4)
	    (setq indent-tabs-mode nil)))

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


;; this should highlight any line longer than 80 symbols
(require 'highlight-80+)
(require 'column-marker)
(require 'linum)

(which-func-mode t)

;;'(flymake-allowed-file-name-masks (quote nil))
;; Pymacs special
;; (autoload 'pymacs-apply "pymacs")
;; (autoload 'pymacs-call "pymacs")
;; (autoload 'pymacs-eval "pymacs" nil t)
;; (autoload 'pymacs-exec "pymacs" nil t)
;; (autoload 'pymacs-load "pymacs" nil t)

;; (define-globalized-minor-mode
;;   global-highlight-parentheses-mode
;;   highlight-parentheses-mode
;;   highlight-parentheses-mode)
;; (global-highlight-parentheses-mode)


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
(global-set-key [f2] 'other-frame)

;; pylint checking
(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "/usr/local/bin/epylint" (list local-file))))


;; If I'm ever to change pylint to something else, I should just change init functions
(defconst flymake-allowed-python-file-name-masks
  '(("\\.py$" flymake-pylint-init)
    (".*$" flymake-pylint-init)))

;; Simple hook for python-mode + flymake
;; (add-to-list 'load-path "~/.emacs.d/python-mode")
;; python related stuff
;; (autoload 'python-mode "python-mode" "Python Mode." t)
;; (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;; (add-to-list 'interpreter-mode-alist '("python" . python-mode))
(add-hook 'python-mode-hook
	  (lambda ()
	    (set (make-variable-buffer-local 'beginning-of-defun-function)
		 'py-beginning-of-def-or-class)
	    (setq outline-regexp "def\\|class ")))

(defun flymake-python-load ()
  (setq flymake-allowed-file-name-masks
	(append flymake-allowed-file-name-masks
		flymake-allowed-python-file-name-masks))
  (flymake-mode t))
(add-hook 'python-mode-hook 'flymake-python-load)
(load-library "flymake-cursor")


;; this should enable copy from emacs to any other X frame
(setq x-select-enable-clipboard t)

;; make scroll behave more like notepad, he-he
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)

;;(setq desktop-buffers-not-to-save
;;      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
;;	      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
;;	      "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

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
       '("*Messages*" "*scratch*")
       clean-buffer-list-kill-never-buffer-names-init))

;; prevent append multiple times
(defvar clean-buffer-list-kill-never-regexps-init
  clean-buffer-list-kill-never-regexps
  "Init value for clean-buffer-list-kill-never-regexps")

(setq clean-buffer-list-kill-never-regexps
      (append '("^.*\\.org$")
	      clean-buffer-list-kill-never-regexps-init))

(setq inferior-lisp-program "java -cp /home/mishok/.clojure/clojure.jar:/home/mishok/.clojure/clojure-contrib.jar clojure.main")
