;;; init.el --- Initialization code for Emacs
;;; Commentary:
;;; License: See LICENSE file

;;; Code:

(setq debug-on-error 1)

(setq package-enable-at-startup nil)
(defvar bootstrap-version)
(setq straight-repository-branch "develop")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(use-package zenburn-theme
  :straight t
  :init (load-theme 'zenburn t))


(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/lib/lang")

(when (eq system-type 'darwin)
  (load-file "~/.emacs.d/osx.el"))

(use-package treesit
  :config
  (setq treesit-language-source-alist
        '((yaml . ("https://github.com/ikatyang/tree-sitter-yaml" nil nil nil nil))
          (python . ("https://github.com/tree-sitter/tree-sitter-python" nil nil nil nil))))
  (treesit-install-language-grammar 'python)
  (treesit-install-language-grammar 'yaml))

(use-package denote
  :straight t
  :bind (("C-x m" . denote))
  :config
  (setq denote-known-keywords '("emacs" "life" "work" "joy" "rust" "programming" "python")))


(use-package cc-mode
  :straight t
  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-style "k&r")
              (setq indent-tabs-mode nil))))

(use-package clojure-mode
  :straight t
  :config
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'aggressive-indent-mode)
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode))

(use-package clj-refactor
  :straight t
  :config
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-b"))))

(use-package cider
  :straight t
  :config
  (setq cider-show-error-buffer 'only-in-repl)
  (setq cider-auto-select-error-buffer nil)
  (setq nrepl-hide-special-buffers t)
  ;; Wrap stacktraces at whatever fill-column is set to
  (setq cider-stacktrace-fill-column t)
  ;; Don't prompt for symbol names when jumping to definitions
  (setq cider-prompt-for-symbol nil)
  ;; Write REPL history to file
  (setq cider-repl-history-file "/tmp/replhistory")
  (setq cider-auto-select-error-buffer nil)
  ;; Enable paredit in REPL
  (add-hook 'cider-repl-mode-hook 'paredit-mode)
  ;; Enable eldoc in REPL
  (add-hook 'cider-mode-hook 'eldoc-mode))


(use-package go-mode
  :straight t)


(use-package haskell-mode
  :straight t)

(use-package shm
  :straight t
  :config
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'structured-haskell-mode))

;; (add-hook 'java-mode-hook 'smartparens-mode)


(use-package typescript-mode
  :straight t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'company-mode)
  (add-hook 'typescript-mode-hook 'smartparens-mode))

(use-package json-mode
  :straight t
  :config
  (setq js-indent-level 2))

(use-package json-reformat
  :straight t
  :config
  (setq json-reformat:indent-width 2))

(use-package web-mode
  :straight t
  :config
  (setq web-mode-enable-auto-indentation nil))

(use-package tide
  :straight t
  :config
  (require 'web-mode)
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-hook 'typescript-mode-hook 'smartparens-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package tide
  :straight t
  :config
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package paredit
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode))

(use-package rainbow-delimiters
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package aggressive-indent
  :straight t
  :config
  (add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode))


(use-package fill-column-indicator
  :straight t
  :hook (prog-mode . fci-mode)
  :init
  (setq-default fci-rule-column 80))

(use-package which-func
  :config
  (which-function-mode t))

(use-package flycheck
  :straight t
  :bind (("<f4>" . flycheck-previous-error)
         ("<f5>" . flycheck-next-error)))

(use-package yaml-mode
  :straight t)

(use-package dockerfile-mode
  :straight t)


(use-package smartparens
  :straight t
  :hook ((python-mode . smartparens-mode)
         (rust-mode . smartparens-mode)))

(use-package jedi
  :straight t)

(use-package elpy
  :straight t
  :bind ("C-c C-f" . elpy-black-fix-code)
  :config
  (if (executable-find "python3")
      (progn
        (setq elpy-rpc-python-command "python3")
        (setq python-shell-interpreter "python3")))
  ;; (add-hook 'elpy-mode-hook
  ;;           '(lambda ()
  ;;              (when (eq major-mode 'python-mode)
  ;;                (add-hook 'after-save-hook 'elpy-black-fix-code))))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (elpy-enable))

(use-package flycheck
  :straight t
  :hook (python-mode . flycheck-mode))

(use-package pipenv
  :straight t
  :hook (python-mode . pipenv-mode))

(use-package poetry
  :ensure t
  :straight t)

(use-package python
  :bind ("C-j" . newline-and-indent)
  :init)

(use-package eglot)

(use-package rustic
  :straight t
  :after eglot
  :bind (:map rustic-mode-map
              ;; ("M-j" . lsp-ui-imenu)
              ;; ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . eglot-code-actions)
              ("C-c C-c r" . eglot-rename)
              ;; ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . eglot-shutdown-all)
              ;; ("C-c C-c s" . lsp-rust-analyzer-status)
              )
  :config
  (setq rustic-format-on-save t)
  (setq rustic-lsp-client 'eglot)
  (add-hook 'rustic-mode-hook 'eglot-ensure))

(use-package emmet-mode
  :straight t
  :init
  ;; Disable preview before expanding
  (setq emmet-preview-default nil)
  ;; Move the cursor to next edit point
  (setq emmet-move-cursor-between-quotes t)
  :config
  (define-key emmet-mode-keymap (kbd "C-m") 'emmet-expand-line)
  (define-key emmet-mode-keymap (kbd "C-j") 'newline-and-indent))


(use-package web-mode
  :straight t
  :mode (("\\.html\\'" . web-mode)
         ("\\.hbs\\'" . web-mode))
  :init
  (setq web-mode-engines-alist
        '(("ctemplate" . "\\.hbs\\'")))
  :config
  (add-hook 'web-mode-hook 'emmet-mode))

;; (defun web-mode-django-setup ()
;;   "Hooks and general setup for Django templating support."
;;   (setq web-mode-markup-indent-offset 4)
;;   (setq web-mode-code-indent-offset 4)
;;   (setq web-mode-enable-control-block-indentation t)
;;   (web-mode-set-engine "django")
;;   (setq indent-tabs-mode nil))


;; (require 'tex-site)

(use-package web-mode
  :straight t)



(use-package yaml-mode
  :straight t)


;;; mishok-display --- All display specific things
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package smart-mode-line
  :straight t)

(use-package midnight
  :straight t)

(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Switch off splash screen at Emacs startup
(setq inhibit-splash-screen t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode 0)
(setq initial-scratch-message nil)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(line-move-visual 0)
(global-hl-line-mode t)
(column-number-mode t)

;; FIXME: disable line numbers for certain modes
(global-display-line-numbers-mode t)

;; (global-visual-line-mode t)

;; Setup fonts
(global-font-lock-mode t)
(set-face-attribute 'default nil
                    :font "Hack-12")
(set-frame-font "Hack-12")
(setq native-comp-async-report-warnings-errors nil)

;; (seq-filter (lambda (font)
;;               (when-let ((info (font-info font)))
;;                 (string-match-p "spacing=100" (aref info 1))))
;;             (font-family-list))
;; Mode line setup
(setq sml/no-confirm-load-theme t)
(sml/setup)
(sml/apply-theme 'respectful)

;; Make scrolling with C-v work on last page, instead of notifying
;; "end of buffer" error
(setq scroll-error-top-bottom 'true)

;; Don't let minibufer cursor jump into read-only prompt
(setq minibuffer-prompt-properties
      (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt))

;; 1-2 letters shorter to type!
(fset 'yes-or-no-p 'y-or-n-p)

;; Set up cleaning of unused buffers
(setq midnight-period 3600)
(setq clean-buffer-list-delay-general 1)
(add-hook 'midnight-hook 'clean-buffer-list)
;; (run-at-time t 3600 'clean-buffer-list)

;; Make sure every file name has unique name
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
;; Setup scrolling behaviour

;; make scroll behave more like notepad, he-he
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq scroll-preserve-screen-position 't)


(require 'desktop)
(require 'saveplace)

;; https://github.com/emacscollective/no-littering
;; https://github.com/KaratasFurkan/.emacs.d
;; https://github.com/minad/vertico
;; https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:EC68944C-F745-45D8-9905-420E0813DBAF
;; https://github.com/minad/consult/blob/main/README.org#use-package-example

(setq visible-bell       nil
      ring-bell-function #'ignore)

(use-package emacs
  :config
  (setq initial-major-mode 'fundamental-mode)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (setq-default indent-tabs-mode nil)
  (setq backup-by-copying t
        backup-directory-alist '(("." . "~/.emacs.d/.backups"))
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)
  (setq visible-bell       nil
        ring-bell-function #'ignore)
  (recentf-mode t)
  (global-visual-line-mode 1)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)  )


;; Setup kill-buffer and system clipboard
;; this should enable copy from emacs to any other X frame
(setq x-select-enable-clipboard t)

;; Save history
(use-package desktop
  :init
  (setq history-length 500)
  (add-to-list 'desktop-globals-to-save 'file-name-history)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  ;; Save point position between sessions
  (setq-default save-place t)
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  :config
  (desktop-save-mode t))

(use-package flyspell
  ;; Look into using https://github.com/syohex/emacs-ac-ispell
  )

(use-package markdown-mode
  :straight t
  :hook (markdown-mode . flyspell-mode))

(use-package project)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(use-package multiple-cursors
  :straight t
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind (("M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package hungry-delete
  :bind (("M-c" . c-hungry-delete-forward)))

(use-package yasnippet-snippets
  :straight t)

(use-package yasnippet
  :straight t
  :bind ("C-<tab>" . yas-expand)
  :config
  (yas-reload-all))

(use-package terraform-mode
  :straight t
  :init
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package company
  :after yasnippet
  :straight t
  :config
  (setq company-idle-delay 0.5) ;; how long to wait until popup
  (setq company-tooltip-align-annotations t)
  (setq company-backends '(company-bbdb company-semantic company-cmake (company-capf :with company-yasnippet) company-clang company-files
                                        (company-dabbrev-code company-gtags company-etags company-keywords)
                                        company-oddmuse company-dabbrev))
  (global-company-mode)
  :bind
  ("TAB" . company-indent-or-complete-common)
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last)))

(use-package company-terraform
  :straight t)

;; Don't create .#filenames
(setq create-lockfiles nil)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package clipetty
  :straight t
  :hook (after-init . global-clipetty-mode))

;; https://git.sr.ht/~ashton314/emacs-bedrock/tree/main/item/mixins/base.el
;; https://codeberg.org/vifon/emacs-config/src/branch/master/emacs.d/lisp/20-completion-engine.el
(use-package vertico
  :straight t
  :init
  (vertico-mode))

(use-package marginalia
  :straight t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Smart(er) fuzzy completion matching (similar to flex)
(use-package hotfuzz
  :straight t)

;;
(use-package orderless
  :straight t
  :init
  (setq completion-styles '(hotfuzz orderless basic)
        completion-category-defaults nil
        orderless-matching-styles '(orderless-flex)
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :straight t
  :init (progn
          (defvar consult-mode-map (make-sparse-keymap))
          (define-minor-mode consult-mode
            "Provide the `consult' commands in a single keymap."
            :global t
            (if consult-mode
                (define-key minibuffer-local-map
                            [remap previous-matching-history-element]
                            #'consult-history)
              (define-key minibuffer-local-map
                          [remap previous-matching-history-element]
                          nil)))
          (consult-mode 1))
  :config
  (setq consult-narrow-key "<")
  :bind
  (:map consult-mode-map
        ("C-x b" . consult-buffer)
        ("M-s u" . consult-focus-lines)
        ("M-s k" . consult-keep-lines)
        ("M-s e" . consult-isearch-history)
        ("M-s d" . consult-find)
        ;; M-g …
        ("M-g g" . consult-line)
        ("M-g M-g" . consult-goto-line)
        ("M-g o" . consult-outline)
        ("M-g i" . consult-imenu)
        ("M-g I" . consult-info)
        ("M-g r" . consult-ripgrep)
        ("M-g m" . consult-mark)
        ("M-g M" . consult-global-mark)
        ;; Misc.
        ("C-x C-r" . consult-recent-file)))

(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package magit
  :bind (("<f7>" . magit-status))
  :straight t
  :init
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  :config
  (defadvice magit-quit-window (around magit-restore-screen activate)
    (let ((current-mode major-mode))
      ad-do-it
      (when (eq 'magit-status-mode current-mode)
        (jump-to-register :magit-fullscreen)))))

(use-package ediff
  :init
  (setq ediff-setup-windows-plain 'ediff-setup-windows-plain))

(use-package magit-gitflow
  :straight t
  :after magit
  :init
  (add-hook 'magit-mode-hook 'turn-on-magit-gitflow))

(use-package transient
  :straight t)

(use-package yaml
  :straight t)

(use-package forge
  :after magit
  :straight t)


(use-package expand-region
  :straight t
  :bind (("M-@" . er/expand-region)))

(use-package windmove
  :straight t
  :init
  (global-unset-key (kbd "<right>"))
  (global-unset-key (kbd "<left>"))
  (global-unset-key (kbd "<up>"))
  (global-unset-key (kbd "<down>"))
  :bind (("<left>" . windmove-left)
         ("<right>" . windmove-right)
         ("<up>" . windmove-up)
         ("<down>" . windmove-down)))

(use-package undo-tree
  :straight t
  :bind (("C-z" . undo-tree-undo)
         ("C-M-z" . undo-tree-redo))
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.undo")))
  (global-undo-tree-mode))



(use-package org
  :straight t
  :bind (("<f8>" . org-capture)
         ("<f10>" . org-agenda))
  :init
  (defun org-path (tail) (concat org-directory tail))
  (setq org-log-done 'note)
  (org-clock-persistence-insinuate)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-directory (expand-file-name "~/Dropbox/org/"))
  (setq org-default-notes-file (org-path "notes.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (org-path "todo.org") "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("d" "Dutch" entry (file (org-path "dutch.org"))
           "* On %U:\n%?  %i\n")
          ("j" "Journal" entry (file+datetree (org-path "journal.org"))
           "* %?\nEntered on %U\n  %i\n  %a")))

  (setq org-agenda-files (mapcar 'org-path '("work.org" "nonwork.org")))
  :config
  ;; Set idle time to 10 minutes (10 minutes of idling will lead to
  ;; org-clock asking whether clock-out has to be performed)
  (setq org-clock-idle-time 10)
  (setq org-clock-persist 'history))


(defun look-of-disapproval ()
  "Just in case we need this"
  (interactive)
  (insert "ಠ_ಠ"))

(require 'windmove)

(use-package hydra
  :straight t)

;;* Helpers

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(use-package transpose-frame
  :straight t
  :config
  (global-set-key
   (kbd "C-M-g")
   (defhydra hydra-frame-management (:color blue :hint nil)
     "
^Flip^                  ^Rotate^           ^Splitter^
^^^^^^^^-----------------------------------------------
_t_: transpose          _r_: rotate 180°   _w_ Up
_f_: flip-vertically    _c_: rotate  90°   _s_ Down
_F_: flip-horizontally  _C_: rotate -90°   _a_ Left
^ ^                     ^ ^                _d_ Right
"
     ("t" transpose-frame)
     ("f" flip-frame)
     ("F" flop-frame)
     ("r" rotate-frame)
     ("c" rotate-frame-clockwise)
     ("C" rotate-frame-anti-clockwise)
     ("w" hydra-move-splitter-up)
     ("s" hydra-move-splitter-down)
     ("a" hydra-move-splitter-left)
     ("d" hydra-move-splitter-right)
     ("." hydra-repeat))))
