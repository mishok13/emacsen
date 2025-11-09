;;; init.el --- Initialization code for Emacs
;;; Commentary:
;;; License: See LICENSE file

;;; Code:

;; https://github.com/nyyManni/ejira
;; https://sr.ht/%7Eashton314/emacs-bedrock/
;; https://batsov.com/articles/2021/12/19/building-emacs-from-source-with-pgtk/
;; https://github.com/xenodium/dotsies/blob/790465b1824481b81bf5c6e08949128c13d76f95/emacs/features/fe-ui.el#L42
;; https://planet.emacslife.com/
;; https://github.com/PillFall/languagetool.el
;; https://karthinks.com/software/fifteen-ways-to-use-embark/
;; https://github.com/zkry/yaml-pro

(defvar bootstrap-version)
(setq straight-use-package-by-default t)
(setq straight-recipes-gnu-elpa-url "git@github.com:emacsmirror/gnu_elpa.git")
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package straight
  :custom
  ;; add project, xref and flymake to the pseudo-packages variable so straight.el doesn't download a separate
  ;; version than what eglot downloads. Original solution from here
  ;; https://github.com/radian-software/straight.el/issues/1146#issuecomment-2227133737
  (straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake xref)))

(use-package project
  :straight (:type built-in)
  :custom
  (project-vc-extra-root-markers '("pyproject.toml")))

(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1))

(use-package emacs
  :ensure nil
  :config
  (defcustom mk13/org-directory (expand-file-name "~/nonwork/notes/org/")
    "Default location for all Org files"
    :type '(string))
  (defcustom mk13/denote-directory (expand-file-name "~/nonwork/notes/denote")
    "Default location for Denote notes"
    :type '(string))
  (let ((dirs (list mk13/org-directory mk13/denote-directory)))
    (dolist (dir dirs)
      (unless (file-exists-p dir)
        (make-directory dir t))))
  ;; (unless (package-installed-p 'vc-use-package)
  ;;   (package-vc-install "https://github.com/slotThe/vc-use-package"))
  ;; (require 'vc-use-package)
  ;; (setq
  ;;  package-archive-priorities '(("gnu-elpa" . 3)
  ;;                               ("melpa" . 2)
  ;;                               ("nongnu" . 1))
  ;;  package-archives           '(("gnu-elpa" . "https://elpa.gnu.org/packages/")
  ;;                               ("gnu-elpa-devel" . "https://elpa.gnu.org/devel/")
  ;;                               ("nongnu" . "https://elpa.nongnu.org/nongnu/")
  ;;                               ("melpa" . "https://melpa.org/packages/")))
  )

(use-package recentf
  :custom
  (recentf-mode t))

(use-package no-littering)

(use-package emacs
  :ensure nil
  ;; Disables suspend-frame keybindings. Because why does it even exist?
  :bind (("C-z" . nil)
         ("C-x C-z" . nil)
         ("C-x m" . nil))

  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (fill-column 112)
  (frame-resize-pixelwise t)
  (initial-major-mode 'fundamental-mode)
  (visible-bell nil)
  (native-comp-async-report-warnings-errors 'silent)
  (indent-tabs-mode nil)
  (vc-follow-symlinks t)

  :config
  (setopt use-short-answers t)
  ;; Disables suspend-frame keybindings. Because why does it even exist?
  (put 'downcase-region 'disabled nil)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (display-fill-column-indicator-mode t)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (put 'upcase-region 'disabled nil)
  (global-display-line-numbers-mode t)
  (global-visual-line-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq
   auto-save-file-name-transforms`((".*" ,temporary-file-directory t))
   backup-by-copying t
   backup-directory-alist '(("." . "~/.emacs.d/.backups"))
   backup-directory-alist`((".*" . ,temporary-file-directory))
   create-lockfiles nil
   delete-old-versions t
   inhibit-splash-screen t
   initial-major-mode 'fundamental-mode
   initial-scratch-message nil
   kept-new-versions 6
   kept-old-versions 2
   require-final-newline 'visit-save
   ring-bell-function #'ignore
   scroll-error-top-bottom 'true
   version-control t
   visible-bell nil
   x-select-enable-clipboard t
   )

  (pixel-scroll-precision-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (menu-bar-mode 0)
  (tooltip-mode 0)
  (blink-cursor-mode 0)
  (line-move-visual 0)
  (global-hl-line-mode t)
  (column-number-mode t)

  ;; Setup fonts
  (global-font-lock-mode t)
  (set-face-attribute 'default nil
                      :font "Hack Nerd Font Mono-14")
  (set-frame-font "Hack Nerd Font Mono-14")
  (setq native-comp-async-report-warnings-errors nil)

  ;; Don't let minibufer cursor jump into read-only prompt
  (setq minibuffer-prompt-properties
        (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt))

  (defvar undo-tree-directory
    (let ((default-directory user-emacs-directory))
      (file-truename "./undo")))
  ;; Make sure every file name has unique name
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package dash)

(use-package org
  :straight (:type built-in)
  :bind (("<f9>" . org-capture)
         ("<f10>" . org-agenda))

  :custom
  (org-directory mk13/org-directory)
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-agenda-files (list mk13/org-directory))
  (org-clock-idle-time 10)
  (org-clock-persist 'history)
  (org-log-done 'note)

  :init
  (org-clock-persistence-insinuate)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-capture-templates
        '(("t" "todo" entry (file org-default-notes-file)
	   "* TODO %?\n%u\n%a\n" :clock-in t :clock-resume t)
	  ("m" "Meeting" entry (file org-default-notes-file)
	   "* MEETING with %? :MEETING:\n%t" :clock-in t :clock-resume t)
	  ("i" "Idea" entry (file org-default-notes-file)
	   "* %? :IDEA: \n%t" :clock-in t :clock-resume t)
	  ("n" "Next Task" entry (file+headline org-default-notes-file "Tasks")
	   "** NEXT %? \nDEADLINE: %t"))))

(use-package smart-mode-line
  :config   ;; Mode line setup
  (setq sml/no-confirm-load-theme t)
  (sml/setup)
  (sml/apply-theme 'respectful))

(use-package exec-path-from-shell
  :config
  (setenv "LANG" "en_US.UTF-8")
  :init
  (exec-path-from-shell-copy-envs '("LC_ALL" "WORKON_HOME" "RUST_SRC_PATH"))
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize)

  (setq mac-allow-anti-aliasing t))

(use-package emacs
  :if (eq system-type 'darwin)
  :custom
  (mac-command-modifier 'super)
  (mac-option-modifier 'meta))

(use-package jenkinsfile-mode

  :config
  (setq jenkinsfile-mode-indent-offset 2))

(use-package zenburn-theme

  :init (load-theme 'zenburn t))

(use-package denote

  :hook (dired-mode . denote-dired-mode-in-directories)
  :defer t
  :bind (("C-x m n" . denote)
         ("C-x m d". denote-open-directory))
  :custom
  (denote-file-type "markdown-yaml")
  (denote-known-keywords '("emacs" "rust" "python" "tech" "softeng" "work" "life") "Expands known keywords a bit")
  (denote-directory mk13/denote-directory)
  :config
  (defun denote-open-directory ()
    "Open denote's directory with dired"
    (interactive)
    (dired mk13/denote-directory)))

(use-package cc-mode

  :config
  (add-hook 'c-mode-common-hook
            (lambda ()
              (c-set-style "k&r")
              (setq indent-tabs-mode nil))))

(use-package clojure-mode

  :config
  (add-hook 'cider-repl-mode-hook 'company-mode)
  (add-hook 'cider-mode-hook 'company-mode))

;; (use-package clj-refactor
;;
;;   :config
;;   (add-hook 'clojure-mode-hook (lambda ()
;;                                  (clj-refactor-mode 1)
;;                                  (cljr-add-keybindings-with-prefix "C-c C-b"))))

;; (use-package cider
;;
;;   :config
;;   (setq cider-show-error-buffer 'only-in-repl)
;;   (setq cider-auto-select-error-buffer nil)
;;   (setq nrepl-hide-special-buffers t)
;;   ;; Wrap stacktraces at whatever fill-column is set to
;;   (setq cider-stacktrace-fill-column t)
;;   ;; Don't prompt for symbol names when jumping to definitions
;;   (setq cider-prompt-for-symbol nil)
;;   ;; Write REPL history to file
;;   (setq cider-repl-history-file "/tmp/replhistory")
;;   (setq cider-auto-select-error-buffer nil)
;;   ;; Enable eldoc in REPL
;;   (add-hook 'cider-mode-hook 'eldoc-mode))

(use-package go-mode
  )

(use-package typescript-mode

  :custom
  (typescript-indent-level 2)
  :config

  (add-hook 'typescript-mode-hook 'company-mode)
  (add-hook 'typescript-mode-hook 'smartparens-mode))


(use-package so-long)

(use-package paredit
  :hook ((emacs-lisp-mode clojure-mode cider-repl-mode) . paredit-mode))

(use-package rainbow-delimiters

  :hook ((emacs-lisp-mode clojure-mode cider-repl-mode python-mode python-ts-mode) . rainbow-delimiters-mode))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode clojure-mode) . aggressive-indent-mode))

(use-package python-mode
  :defer t
  :config
  (setq company-backends '(company-capf
                           company-yasnippet
                           company-files
                           (company-dabbrev-code company-keywords)
                           company-dabbrev))

  ;; automatically generating pyrightconfig could be done with:
  ;; detecting pyproject.toml
  ;; reading it https://github.com/gongo/emacs-toml and detecting the tool used
  ;; running this for poetry (or can we do that without the call to command line?)
  ;; generating config with json.el https://github.com/emacs-mirror/emacs/blob/master/lisp/json.el#L770
  ;; setting fci to correct value based on tool.black.line-length or tool.ruff.line-length value (and sensible default).
  ;; poetry env info -p | read -r d; printf '{\n  "venvPath": "%s",\n  "venv": "%s"\n}\n' "$(dirname "$d")" "$(basename "$d")" > pyrightconfig.json
  )

(use-package which-func
  :config
  (which-function-mode t))

(use-package yaml-mode
  )

(use-package yaml-pro
  )

(use-package caddyfile-mode
  )

(use-package sql
  :custom
  (sql-dialect 'postgres))

(use-package dockerfile-mode
  )

(use-package smartparens
  :hook ((python-mode python-ts-mode rustic-mode typescript-mode terraform-mode hcl-mode) . smartparens-mode))

(use-package poetry
  )

(use-package eglot
  :hook ((rustic-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-M-h a" . eglot-code-actions)
              ("C-M-h r" . eglot-rename)
              ("C-M-h f" . eglot-format-buffer)
              ("C-M-h Q" . eglot-shutdown-all))
  :config
  (add-to-list 'eglot-server-programs
               `((python-ts-mode python-mode) . ,(eglot-alternatives
                                                  '(("poetry" "run" "pylsp")
                                                    ("hatch" "run" "lsp:run")
                                                    ("uv" "run" "basedpyright-langserver" "--stdio")))))
  (add-to-list 'eglot-server-programs
               `((terraform-mode terraform-ts-mode) . ("tofu-ls" "serve"))))

(use-package rustic
  ;; I would like to make rustic window for compilation narrower and
  ;; shorter if possible, as well as automatically focus into it. It should be possible with https://www.reddit.com/r/emacs/comments/cpdr6m/any_additional_docstutorials_on_displaybuffer_and/
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Zen-of-Buffer-Display.html

  :mode ("\\.rs\\'" . rustic-mode)
  :custom
  (rustic-format-trigger 'on-compile)
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save nil)
  (add-to-list 'display-buffer-alist
               `("^\\*rustic-compilation\\*$"
                 (display-buffer-reuse-window display-buffer-below-selected display-buffer-at-bottom)
                 (inhibit-same-window . t)
                 (window-min-height . 10)
                 (window-height . 0.25)
                 (inhibit-switch-frame . nil))
               t))

(use-package flymake)

(use-package midnight

  :init
  ;; Days are shorter in emacs world
  (setq midnight-period (* 60 60 8))
  ;; Ensures buffers are cleaned up after a single "day"
  (setq clean-buffer-list-delay-general 1)
  (midnight-mode t))

;; https://github.com/emacscollective/no-littering
;; https://github.com/KaratasFurkan/.emacs.d
;; https://github.com/minad/vertico
;; https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:EC68944C-F745-45D8-9905-420E0813DBAF
;; https://github.com/minad/consult/blob/main/README.org#use-package-example
;; https://github.com/caisah/emacs.dz


(use-package saveplace
  :config
  (setq save-place-file (expand-file-name ".places" user-emacs-directory))
  (save-place-mode t))

;; Save buffer state
(use-package desktop
  :config
  (setq history-length 500)
  (add-to-list 'desktop-globals-to-save 'file-name-history)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  ;; Save point position between sessions
  (desktop-save-mode t))

(use-package markdown-mode
  )

(use-package flyspell
  ;; Look into using https://github.com/syohex/emacs-ac-ispell
  :hook (markdown-mode . flyspell-mode))

(use-package multiple-cursors

  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind (("M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package hungry-delete

  :hook (prog-mode . hungry-delete-mode)
  :custom (hungry-delete-join-reluctantly t))

(use-package yasnippet-snippets)

;; https://jdhao.github.io/2021/10/06/yasnippet_setup_emacs/
(use-package yasnippet
  :after yasnippet-snippets
  :config
  (yas-reload-all))

(use-package hcl-mode)

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package lua-mode)

(use-package company
  :after (yasnippet)

  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  (company-tooltip-align-annotations t)
  :config
  (global-company-mode)
  :bind
  ("C-<tab>" . company-yasnippet)
  ("TAB" . company-indent-or-complete-common)
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last)))

(use-package which-key
  :config
  (which-key-mode))

(use-package clipetty
  :defer t
  :custom
  (global-clipetty-mode 1))

;; https://git.sr.ht/~ashton314/emacs-bedrock/tree/main/item/mixins/base.el
;; https://codeberg.org/vifon/emacs-config/src/branch/master/emacs.d/lisp/20-completion-engine.el
(use-package vertico
  :init
  (vertico-mode))

(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;; Smart(er) fuzzy completion matching (similar to flex)
(use-package hotfuzz)

(use-package orderless
  :init
  (setq completion-styles '(hotfuzz orderless basic)
        completion-category-defaults nil
        orderless-matching-styles '(orderless-flex)
        completion-category-overrides '((file (styles partial-completion)))))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  ;; (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc.  You may adjust the Eldoc
  ;; strategy, if you want to see the documentation from multiple providers.
  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package magit
  ;; (:type git :flavor melpa :files ("lisp/magit*.el" "lisp/git-*.el" "docs/magit.texi" "docs/AUTHORS.md" "LICENSE" "magit-pkg.el" (:exclude "lisp/magit-section.el") "magit-pkg.el") :host github :repo "magit/magit")
  :bind (("<f7>" . magit-status))
  :init
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  :custom
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package ediff
  :init
  (setq ediff-setup-windows-plain 'ediff-setup-windows-plain))

(use-package expand-region
  :bind (("M-@" . er/expand-region)))

(use-package windmove
  :bind (("<left>" . windmove-left)
         ("<right>" . windmove-right)
         ("<up>" . windmove-up)
         ("<down>" . windmove-down)))

(defun look-of-disapproval ()
  "Just in case we need this"
  (interactive)
  (insert "ಠ_ಠ"))

(use-package hydra
  :config
  (setq hydra-is-helpful 't)
  :bind
  (("C-M-w" . hydra-window-management/body)
   ("C-M-c" . hydra-flymake/body)))

(use-package major-mode-hydra
  :after hydra
  :init
  (pretty-hydra-define
    hydra-window-management
    (:color red :title "Manage windows" :quit-key "q" :foreign-keys warn)
    ("Flip"
     (("t" transpose-frame "Transpose")
      ("f" flip-frame "Vertically")
      ("F" flop-frame "Horizontally"))
     "Rotate"
     (("r" rotate-frame "180°")
      ("c" rotate-frame-clockwise "90°")
      ("C" rotate-frame-anti-clockwise "-90°"))
     "Window"
     (("w" enlarge-window "Taller")
      ("s" shrink-window "Shorter")
      ("d" enlarge-window-horizontally "Wider")
      ("a" shrink-window-horizontally "Narrower"))))
  (pretty-hydra-define
    hydra-flymake
    (:color red :title "Flymake" :quit-key "q" :foreign-keys warn)
    ("Flymake"
     (("p" flymake-show-project-diagnostics)
      ("b" flymake-show-buffer-diagnostics)
      ("x" flymake-show-diagnostic)))))

(use-package transpose-frame)

(use-package jiralib2
  :after (org)
  :straight (:host github :type git :repo "nyyManni/jiralib2")
  :init
  (setq jiralib2-url              "https://jira.kpn.org"
        jiralib2-auth             'basic
        jiralib2-user-login-name  nil
        jiralib2-token            nil))

(use-package ejira
  :straight (:host github :type git :repo "nyyManni/ejira")
  :after (org)
  :init
  (setq jiralib2-url              "https://jira.kpn.org"
        jiralib2-auth             'basic
        jiralib2-user-login-name  nil
        jiralib2-token            nil
        ;; NOTE, this directory needs to be in `org-agenda-files'`
        ejira-org-directory       (concat mk13/org-directory "jira")
        ejira-projects            '("DEP")
        ejira-priorities-alist    '(("Highest" . ?A)
                                    ("High"    . ?B)
                                    ("Medium"  . ?C)
                                    ("Low"     . ?D)
                                    ("Lowest"  . ?E))
        ejira-todo-states-alist   '(("To Do"       . 1)
                                    ("In Progress" . 2)
                                    ("Done"        . 3)))
  :config
  ;; Tries to auto-set custom fields by looking into /editmeta
  ;; of an issue and an epic.
  (add-hook 'jiralib2-post-login-hook #'ejira-guess-epic-sprint-fields)

  ;; They can also be set manually if autoconfigure is not used.
  ;; (setq ejira-sprint-field       'customfield_10001
  ;;       ejira-epic-field         'customfield_10002
  ;;       ejira-epic-summary-field 'customfield_10004)

  ;; (require 'ejira-agenda)

  ;; Make the issues visisble in your agenda by adding `ejira-org-directory'
  ;; into your `org-agenda-files'.
  ;; (add-to-list 'org-agenda-files ejira-org-directory)

  ;; Add an agenda view to browse the issues that
  ;; (org-add-agenda-custom-command
  ;;  '("j" "My JIRA issues"
  ;;    ((ejira-jql "resolution = unresolved and assignee = currentUser()"
  ;;                ((org-agenda-overriding-header "Assigned to me"))))))
  )

(use-package git-modes
  :init
  (add-to-list 'auto-mode-alist '("/.dockerignore\\'" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("/.ignore\\'" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("/.rgignore\\'" . gitignore-mode))
  (add-to-list 'auto-mode-alist '("/.driftignore\\'" . gitignore-mode)))

(use-package undo-tree

  :bind (("C-z" . undo-tree-undo)
         ("C-M-z" . undo-tree-redo))
  :config
  (add-to-list 'undo-tree-history-directory-alist (cons "." undo-tree-directory))
  (global-undo-tree-mode))

(use-package python
  :custom
  ;;
  (python-indent-guess-indent-offset nil)
  (setq company-backends '(company-capf
                           company-yasnippet
                           company-files
                           (company-dabbrev-code company-keywords)
                           company-dabbrev))
  :hook
  (python-mode . eglot-ensure)
  (python-mode . smartparens-mode))

;; https://github.com/emacscollective/no-littering
;; https://github.com/KaratasFurkan/.emacs.d
;; https://github.com/minad/vertico
;; https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:EC68944C-F745-45D8-9905-420E0813DBAF
;; https://github.com/minad/consult/blob/main/README.org#use-package-example

(use-package project)

(use-package consult

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
  (setq consult-locate-args "mdfind -name")
  :bind
  (:map consult-mode-map
        ("C-x b" . consult-buffer)
        ("M-s u" . consult-focus-lines)
        ("M-s k" . consult-keep-lines)
        ("M-s e" . consult-isearch-history)
        ("M-s d" . consult-find)
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

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package jsonian

  :after so-long
  :custom
  (jsonian-default-indentation 2)
  :custom
  (jsonian-no-so-long-mode))

(use-package json-reformat

  :config
  (setq json-reformat:indent-width 2))

(use-package tide

  :config
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'tide-setup)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)))))


(use-package emmet-mode

  :hook (web-mode)
  :custom
  ;; Disable preview before expanding
  (emmet-preview-default nil)
  ;; Move the cursor to next edit point
  (emmet-move-cursor-between-quotes t)
  :bind (:map emmet-mode-keymap
              ;; ("C-j" . newline-and-indent)
              ("C-m" . emmet-expand-line)))

(use-package web-mode
  :mode  ("\\.tsx\\'" "\\.html\\'" "\\.hbs\\'" "\\.vue\\'")
  :custom
  (web-mode-enable-auto-indentation nil)
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))

(use-package justl)
(use-package just-mode)

(use-package mermaid-mode
        :custom
        (mermaid-mmdc-location "bunx")
        (mermaid-flags "@mermaid-js/mermaid-cli@11.4.0"))

(use-package copilot
  :config
  (add-to-list 'copilot-indentation-alist '(prog-mode 2))
  (add-to-list 'copilot-indentation-alist '(org-mode 2))
  (add-to-list 'copilot-indentation-alist '(text-mode 2))
  (add-to-list 'copilot-indentation-alist '(clojure-mode 2))
  (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))

  :straight (:type git :host github :repo "copilot-emacs/copilot.el")
  :bind (("C-c M-c" . copilot-complete)
         :map copilot-completion-map
         ("C-g" . copilot-clear-overlay)
         ("M-n" . copilot-next-completion)
         ("M-p" . copilot-previous-completion)
         ("M-f" . copilot-accept-completion-by-word)
         ("<tab>" . copilot-accept-completion)
         ("M-<return>" . copilot-accept-completion-by-line)))

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  ;; avoid yaml-ts-mode as it's very broken https://www.reddit.com/r/emacs/comments/17gtxmr/indentation_in_yamltsmode/
  (treesit-auto-langs '(python typescript terraform dockerfile nix))
  :config
  (global-treesit-auto-mode))

(use-package terraform-ts-mode
  :straight (:host github :type git :repo "kgrotel/terraform-ts-mode")
  :custom
  terraform-ts-format-on-save nil)

(use-package golden-ratio)

;; (defun force-debug (func &rest args)
;;   (condition-case e
;;       (apply func args)
;;     ((debug error) (signal (car e) (cdr e)))))

;; (advice-add #'vertico--exhibit :around #'force-debug)

(use-package auth-source-1password
  :straight (:host github :type git :repo "dlobraico/auth-source-1password")
  :init
  (auth-source-1password-enable))

(use-package shell-maker
  :straight (:type git :host github :repo "xenodium/shell-maker" :files ("shell-maker*.el" "markdown-overlays.el")))

(use-package chatgpt-shell
  :straight (:type git :host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el"))
  :custom
  (chatgpt-shell-openai-key (lambda ()
                              (auth-source-pick-first-password :host "OpenAI ChatGPT API Key" :user "credential"))))

(use-package magit-file-icons
  :after magit
  :init
  (magit-file-icons-mode 1))

(use-package cov)

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Hack Nerd Font Mono"))

(use-package fish-mode)

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package acp
  :straight (:host github :type git :repo "xenodium/acp.el"))

(use-package agent-shell
  :straight (:host github :type git :repo "xenodium/agent-shell"))

(use-package mise
  :config
  (global-mise-mode))

(use-package typst-ts-mode
  :straight (:type git :host codeberg :repo "meow_king/typst-ts-mode")
  :mode "\\.typ\\'")
