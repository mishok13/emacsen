;; .emacs
;; Andrii V. Mishkovskyi

(add-to-list 'load-path "~/.emacs.d/")

(global-unset-key (kbd "<right>"))
(global-unset-key (kbd "<left>"))
(global-unset-key (kbd "<up>"))
(global-unset-key (kbd "<down>"))
(global-set-key [f7] 'magit-status)

(global-font-lock-mode t)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(require 'el-get)

(defmacro el-get-add (item)
  `(add-to-list 'el-get-sources ',item))


(el-get-add
 (:name nxhtml))

(el-get-add
 (:name ropemacs))

;; (el-get-add
;;  (:name grep+
;; 	:features grep+))

;; (el-get-add
;;  (:name session
;; 	:after (lambda () (autoload 'session-initialize "session" nil t)
;; 		 (add-hook 'after-init-hook 'session-initialize))))

;; (el-get-add
;;  (:name python-mode))

(el-get-add
 (:name magit))

;; el-get's handling of color-theme is broken, just use the
;; preinstalled library one

;; (setq color-theme-is-global t)
;; (el-get-add
;;  (:name color-theme
;; 	:after (lambda () (require 'color-theme nil t)
;; 		 (eval-after-load "color-theme"
;; 		   '(progn
;; 		      (load "~/.emacs.d/color-theme/themes/mishok.el")
;; 		      (color-theme-mishok)
;; 		      (color-theme-initialize))))))

;; (add-to-list 'load-path "~/.emacs.d/color-theme/")
(add-to-list 'load-path "~/.emacs.d/color-theme/")
(load "~/.emacs.d/color-theme/mishok.el")
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-mishok)
(color-theme-initialize)

(el-get-add
 (:name yasnippet
	:type git-svn
	:url "http://yasnippet.googlecode.com/svn/trunk/"))


(el-get-add
 (:name twittering-mode))

;; OH FUCK THIS DOES NOT WORK
;; (el-get-add
;;  (:name yasnippet
;; 	:after (lambda () (require 'yasnippet nil t)
;; 		 (eval-after-load "yasnippet"
;; 		   '(progn
;; 		      (yas/initialize)
;; 		      (yas/load-directory "~/.emacs.d/snippets/"))))))

(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
;; (yas/initialize)
;; (yas/reload-all)

(el-get-add
 (:name highlight-parentheses
	:after (lambda () (autoload 'highlight-parentheses-mode "highlight-parentheses" nil t)
		 (dolist (hook '(python-mode-hook
				 emacs-lisp-mode-hook))
		   (add-hook hook 'highlight-parentheses-mode)))))

;; smex
(el-get-add
 (:name smex
  :features smex
  :after (lambda ()
           (setq smex-save-file "~/.emacs.d/smex.save")
           (smex-initialize)
           (smex-auto-update)
           (global-set-key (kbd "M-x") 'smex)
           (global-set-key (kbd "M-X") 'smex-major-mode-commands)
           (global-set-key (kbd "C-c M-x") 'smex-update-and-run)
           (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))))

;; (el-get-add
;;  (:name breadcrumb
;;   :features breadcrumb
;;   :after (lambda ()
;;            (global-set-key (kbd "C-c M-b" 'bc-set) ;; Shift-SPACE
;;            (global-set-key (kbd "C-c M-n") 'bc-previous)
;;            (global-set-key (kbd "C-c M-p") 'bc-next)
;;            (global-set-key (kbd "C-c M-c") 'bc-goto-current)
;;            (global-set-key (kbd "C-c M-j") 'bc-list))))

(el-get-add
 (:name scala-mode
	:type svn
	:url "http://lampsvn.epfl.ch/svn-repos/scala/scala-tool-support/trunk/src/emacs/"
	:build ("make")
	:load-path (".")
	:features scala-mode-auto
	:after (lambda ()
		 (add-hook 'scala-mode-hook
			   '(lambda ()
			      (yas/minor-mode-on))))))

(el-get)


(global-set-key (kbd "<up>") 'other-frame)

(fset 'yes-or-no-p 'y-or-n-p)

(defun donuts ()
  "For the love of God"
  (interactive)
  (print "Mmmm, donuts."))

(defun look-of-disapproval ()
  "Just in case we need this"
  (interactive)
  (insert "ಠ_ಠ"))

;; (require 'magit)
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
    (list "epylint" (list local-file))))


;; If I'm ever to change pylint to something else, I should just change init functions
(defconst flymake-allowed-python-file-name-masks
  '(("\\.py$" flymake-pylint-init)
    (".*$" flymake-pylint-init)))

;; Simple hook for python-mode + flymake
(add-to-list 'load-path "~/.emacs.d/python-mode")
;; python related stuff
(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))
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

;; (load-library "pyrex-mode")

;; Loading ropemacs
(pymacs-load "ropemacs" "rope-")

;; show line number on the left pane
(global-linum-mode 1)

;; show column number
(column-number-mode 1)

;; Scroll bar is useless
(scroll-bar-mode -1)

(setq-default fill-column 72)

;; this should enable copy from emacs to any other X frame
(setq x-select-enable-clipboard t)

;; make scroll behave more like notepad, he-he
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position 't)


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


;; org-mode is a powerfull thingy
;; I'm yet to understand all of its power, but I'm learning :)
;; most of this code is taken (stolen, if you prefer) from here:
;; http://doc.norang.ca/org-mode.html
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map [f6] 'org-store-link)
;; (global-set-key (kbd "<f7>") 'org-agenda)
;; (global-set-key (kbd "C-M-r") 'org-remember)
;; Enabling flyspell for org-mode
(add-hook 'org-mode-hook
          (lambda () (flyspell-mode 1)))

(setq org-log-done t)
(setq org-agenda-files (file-expand-wildcards "~/.emacs.d/orgfiles/*.org"))
(setq org-default-notes-file "~/.emacs.d/orgfiles/refile.org")

(setq org-agenda-custom-commands
      (quote (("P" "Projects" tags "/!PROJECT" ((org-use-tag-inheritance nil)))
              ("w" "Tasks I'm working on right now" todo "WORKING" ((org-agenda-todo-ignore-with-date nil)))
              ("p" "Paused tasks -- probably waiting for something" tags "PAUSED" ((org-use-tag-inheritance nil)))
              ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ("n" "Notes" tags "NOTE" nil))))

(setq org-todo-keyword-faces
      (quote
       (("TODO" :foreground "red" :weight bold)
	("STARTED" :foreground "blue" :weight bold)
	("DONE" :foreground "forest green" :weight bold)
	("WAITING" :foreground "orange" :weight bold)
	("SOMEDAY" :foreground "magenta" :weight bold)
	("CANCELLED" :foreground "forest green" :weight bold)
	("QUOTE" :foreground "red" :weight bold)
	("QUOTED" :foreground "magenta" :weight bold)
	("APPROVED" :foreground "forest green" :weight bold)
	("EXPIRED" :foreground "forest green" :weight bold)
	("REJECTED" :foreground "forest green" :weight bold)
	("OPEN" :foreground "blue" :weight bold))))

;;;  Load Org Remember Stuff
(require 'remember)
(org-remember-insinuate)

;; Start clock if a remember buffer includes :CLOCK-IN:
(add-hook 'remember-mode-hook 'my-start-clock-if-needed 'append)

(defun my-start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward " *:CLOCK-IN: *" nil t)
      (replace-match "")
      (org-clock-in))))

;; I use C-M-r to start org-remember

;; Keep clocks running
(setq org-remember-clock-out-on-exit nil)

;; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)

;; I don't use this -- but set it in case I forget to specify a location in a future template
(setq org-remember-default-headline "Tasks")

;; 3 remember templates for TODO tasks, Notes, and Phone calls
(setq org-remember-templates (quote (("todo" ?t "* TODO %?
  %u
  %a" nil bottom nil)
                                     ("note" ?n "* %?                                        :NOTE:
  %u
  %a" nil bottom nil)
                                     ("phone" ?p "* PHONE %:name - %:company -                :PHONE:
  Contact Info: %a
  %u
  :CLOCK-IN:
  %?" nil bottom nil))))

; Use IDO for target completion
(setq org-completion-use-ido t)
; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))
; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
(setq org-outline-path-complete-in-steps t)


(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "WORKING(w!)" "|" "DONE(d!/!)")
	      (sequence "PAUSED(p@/!)" "DEFERRED(D!)" "|" "CANCELLED(c@/!)"))))

(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("PAUSED" ("PAUSED" . t) ("NEXT"))
              ("DEFERRED" ("PAUSED" . t))
              (done ("NEXT") ("PAUSED"))
              ("TODO" ("PAUSED") ("CANCELLED"))
              ("WORKING" ("PAUSED") ("NEXT" . t)))))
;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "WORKING")


;; allows changing from any task todo state to any other state directly
;;  by selecting the appropriate key from the fast todo selection key menu
;; C-c C-t <KEY>
(setq org-use-fast-todo-selection t)


;; ;; TODO: move my theme to separate file
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

;; (require 'twit)
;; (setq twit-show-user-images 1)
;; (setq twit-user-image-dir "~/.emacs.d/twit-user-images")
;; (global-set-key [f8] 'twit-show-recent-tweets)

;; (add-to-list 'load-path "~/.emacs.d/twittering-mode/")
;; (require 'twittering-mode)


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

(setq clean-buffer-list-kill-never-regexps
      (append '("^.*\\.org$")
	      clean-buffer-list-kill-never-regexps-init))

(global-set-key (kbd "C-z") 'undo)

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$|\\.json$" . js2-mode))

(require 'browse-url)
(setq browse-url-browser-function 'browse-url-firefox
      browse-url-new-window-flag  t
      browse-url-firefox-new-window-is-tab t)

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)


(setq inferior-lisp-program "java -cp /home/mishok/.clojure/clojure.jar:/home/mishok/.clojure/clojure-contrib.jar clojure.main")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(ecb-options-version "2.33beta2")
 '(global-hl-line-mode t)
 '(kill-whole-line t)
 '(org-agenda-files (quote ("~/.emacs.d/orgfiles/auth.org" "~/.emacs.d/orgfiles/blog.org" "~/.emacs.d/orgfiles/main.org" "~/.emacs.d/orgfiles/notes.org" "~/.emacs.d/orgfiles/openmapsua.org" "~/.emacs.d/orgfiles/python-api.org" "~/.emacs.d/orgfiles/refile.org" "~/.emacs.d/orgfiles/render.org" "~/.emacs.d/orgfiles/tiles.org" "~/.emacs.d/orgfiles/tilman.org" "~/.emacs.d/orgfiles/triton.org" "~/.emacs.d/orgfiles/vectormaps.org" "~/.emacs.d/orgfiles/work.org")))
 '(safe-local-variable-values (quote ((test-case-name . twisted\.trial\.test\.test_script) (test-case-name . twisted\.trial\.test\.test_runner) (test-case-name . twisted\.trial\.test\.test_tests))))
 '(show-paren-mode t)
 '(tool-bar-mode nil nil (tool-bar)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "unknown" :family "Consolas"))))
 '(hl-line ((t (:inherit highlight)))))

(setq ring-bell-function 'ignore)
