;;; init.el --- Initialization code for Emacs
;;; Commentary:
;;; License: See LICENSE file

;;; Code:

;; Inspirations and straight up sources

;; https://codeberg.org/vifon/emacs-config/src/branch/master/emacs.d/lisp/20-completion-engine.el
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
  (project-vc-extra-root-markers '("pyproject.toml" "Cargo.toml")))

(use-package emacs
  :ensure nil
  ;; Disables suspend-frame keybindings. Because why does it even exist?
  :bind (("C-x C-z" . nil)
         ("C-x m" . nil))

  :custom
  (custom-file (expand-file-name "custom.el" user-emacs-directory))
  (fill-column 112)
  (initial-major-mode 'fundamental-mode)
  (visible-bell nil)
  (native-comp-async-report-warnings-errors 'silent)
  (indent-tabs-mode nil)
  (vc-follow-symlinks t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (remote-file-name-inhibit-auto-save t)

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
  (setopt use-short-answers t)
  (put 'downcase-region 'disabled nil)
  (global-display-fill-column-indicator-mode 1)
  (put 'upcase-region 'disabled nil)
  (global-display-line-numbers-mode t)
  (global-visual-line-mode t)
  (delete-selection-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (setq
   auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
   backup-by-copying t
   backup-directory-alist '(("." . "~/.emacs.d/.backups"))
   create-lockfiles nil
   delete-old-versions t
   inhibit-splash-screen t
   initial-scratch-message nil
   kept-new-versions 6
   kept-old-versions 2
   require-final-newline 'visit-save
   ring-bell-function #'ignore
   scroll-error-top-bottom t
   version-control t
   select-enable-clipboard t)

  (pixel-scroll-precision-mode t)
  (menu-bar-mode 0)
  (tooltip-mode 0)
  (blink-cursor-mode 0)
  (global-hl-line-mode t)
  (column-number-mode t)

  ;; Setup fonts
  (global-font-lock-mode t)
  (set-face-attribute 'default nil
                      :font "Hack Nerd Font Mono-14")
  (set-frame-font "Hack Nerd Font Mono-14")

  (defvar undo-tree-directory
    (let ((default-directory user-emacs-directory))
      (file-truename "./undo")))
  ;; Make sure every file name has unique name
  (setq uniquify-buffer-name-style 'reverse)
  (setq uniquify-separator "|")
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*")
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package recentf
  :custom
  (recentf-mode t))

(use-package no-littering)

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
  :if (memq system-type '(darwin))
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

(use-package clojure-mode)



(use-package so-long)

(use-package paredit
  :hook ((emacs-lisp-mode clojure-mode cider-repl-mode) . paredit-mode))

(use-package rainbow-delimiters
  :hook ((emacs-lisp-mode clojure-mode cider-repl-mode) . rainbow-delimiters-mode))

(use-package aggressive-indent
  :hook ((emacs-lisp-mode clojure-mode) . aggressive-indent-mode))

(use-package python
  :straight (:type built-in)
  :custom
  (python-indent-guess-indent-offset nil)
  :hook
  (python-ts-mode . eglot-ensure)
  (python-ts-mode . smartparens-mode)
  (python-ts-mode . rainbow-delimiters-mode))

(use-package which-func
  :custom
  (which-func-display 'header)
  :config
  (which-function-mode t))

(use-package yaml-mode
  :defer t)

(use-package yaml-pro
  )

(use-package caddyfile-mode)

(use-package sql
  :custom
  (sql-dialect 'postgres))

(use-package smartparens
  :hook ((typescript-ts-mode terraform-mode hcl-mode) . smartparens-mode))

(use-package electric-pair-mode
  :straight (:type built-in)
  :hook ((rustic-mode) . electric-pair-mode))


(use-package terraform-mode
  :after (eglot)
  :custom
  (terraform-format-on-save-mode nil)
  :config
  ;; Annoyingly both terraform-mode and terraform-ts-mode will proactively inject terraform-ls as the LSP
  ;; provider for Terraform/OpenTofu. Since I almost exclusively use OpenTofu nowadays it stands to reason
  ;; tofu-ls is a better fit.
  (setq eglot-server-programs
        (assoc-delete-all 'terraform-mode eglot-server-programs))
  (add-to-list 'eglot-server-programs
               `(terraform-mode . ("tofu-ls" "serve"))))

;; The package in question does a lot of things that simply break interaction, such as injecting eglot-format
;; into global before-save-hook, modifying eglot-server-programs on activation and such. For now this mode can
;; simply go away, but in the future I should probably just fork it.

;; (use-package terraform-ts-mode
;;   :straight (:host github :type git :repo "kgrotel/terraform-ts-mode")
;;   :after (eglot)
;;   :custom
;;   (terraform-ts-format-on-save 1)
;;   :config
;;   (setq eglot-server-programs
;;         (assoc-delete-all 'terraform-ts-mode eglot-server-programs))
;;   (add-to-list 'eglot-server-programs
;;                `(terraform-ts-mode . ("tofu-ls" "serve"))))


(use-package eglot
  :hook ((rustic-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("<f1>" . mk13/eglot-menu))
  :custom
  (eglot-extend-to-xref t)
  :config
  (fset #'jsonrpc--log-event #'ignore)  ; don't log every event
  (add-to-list 'eglot-server-programs
               `(python-ts-mode . ,(eglot-alternatives
                                    '(("poetry" "run" "pylsp")
                                      ("hatch" "run" "lsp:run")
                                      ("uv" "run" "basedpyright-langserver" "--stdio")))))
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("harper-ls" "--stdio")))
  (transient-define-prefix mk13/eglot-menu ()
    "Eglot LSP commands"
    [["Code"
      ("a" "Code actions" eglot-code-actions)
      ("r" "Rename symbol" eglot-rename)
      ("f" "Format buffer" eglot-format-buffer)]
     ["Navigate"
      ("d" "Definition" xref-find-definitions)
      ("D" "Declaration" eglot-find-declaration)
      ("i" "Implementation" eglot-find-implementation)
      ("t" "Type definition" eglot-find-typeDefinition)
      ("?" "References" xref-find-references)
      ("s" "Symbols" consult-eglot-symbols)]
     ["Server"
      ("q" "Shutdown" eglot-shutdown)
      ("Q" "Shutdown all" eglot-shutdown-all)
      ("R" "Reconnect" eglot-reconnect)
      ("e" "Events" eglot-events-buffer)
      ("E" "Stderr" eglot-stderr-buffer)]]))

(use-package consult-eglot
  :after (consult eglot))

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

(use-package flymake
  :bind ("<f2>" . mk13/flymake-menu)
  :custom
  (flymake-show-diagnostics-at-end-of-line t)
  :config
  (transient-define-prefix mk13/flymake-menu ()
    "Flymake diagnostics"
    [["Flymake"
      ("c" "Consult (live list)" consult-flymake)
      ("p" "Project diagnostics" flymake-show-project-diagnostics)
      ("b" "Buffer diagnostics" flymake-show-buffer-diagnostics)
      ("x" "Diagnostic at point" flymake-show-diagnostic)]]))

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

  :bind (("M-<down-mouse-1>" . nil)
         ("M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package hungry-delete

  :hook (prog-mode . hungry-delete-mode)
  :custom (hungry-delete-join-reluctantly t))

(use-package yasnippet-snippets)

;; https://jdhao.github.io/2021/10/06/yasnippet_setup_emacs/
(use-package yasnippet
  :after yasnippet-snippets
  :config
  (yas-reload-all))

(use-package hcl-mode
  :defer t)


(use-package corfu
  :straight (:tag "2.9")
  :after (yasnippet)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.5)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous))
  :config
  (global-corfu-mode))

(use-package cape
  :config
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file))

(use-package yasnippet-capf
  :after (yasnippet cape)
  :bind ("C-<tab>" . yasnippet-capf)
  :config
  (add-hook 'completion-at-point-functions #'yasnippet-capf))

(use-package which-key
  :straight (:type built-in)
  :config
  (which-key-mode))

(use-package clipetty
  :defer t
  :custom
  (global-clipetty-mode 1))

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
  (("C-." . embark-act)
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)
  :custom
  (embark-indicators '(embark-mixed-indicator embark-highlight-indicator embark-isearch-highlight-indicator))
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

(use-package avy
  :bind
  (("M-g j" . avy-goto-char-timer)
   ("M-g J" . avy-goto-line))
  :custom
  (avy-timeout-seconds 0.7)
  :config
  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?. avy-dispatch-alist) 'avy-action-embark))

(use-package magit
  :bind (("<f7>" . magit-status))
  :init
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  :custom
  (magit-format-file-function #'magit-format-file-nerd-icons)
  (magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package forge
  :after magit
  :config
  (advice-add 'ghub--ident :override (lambda (username package) "credential")))

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

(use-package transpose-frame
  :bind ("C-M-w" . mk13/window-management-menu)
  :config
  (transient-define-prefix mk13/window-management-menu ()
    [["Flip"
      ("t" "Transpose" transpose-frame)
      ("f" "Flip vertically" flip-frame)
      ("F" "Flop horizontally" flop-frame)]
     ["Rotate"
      ("r" "180°" rotate-frame )
      ("c" "90° clockwise" rotate-frame-clockwise)
      ("C" "-90°" rotate-frame-anticlockwise)]
     ["Window"
      ("w" "Taller" enlarge-window :transient t)
      ("s" "Shorter" shrink-window :transient t)
      ("d" "Wider" enlarge-window-horizontally :transient t)
      ("a" "Narrower" shrink-window-horizontally :transient t)]]))

(use-package jiralib2
  :after (org)
  :straight (:host github :type git :repo "nyyManni/jiralib2")
  :init
  (setq jiralib2-url              "https://jira.kpn.org"
        jiralib2-auth             'basic
        jiralib2-user-login-name  nil
        jiralib2-token            nil))

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


;; https://github.com/emacscollective/no-littering
;; https://github.com/KaratasFurkan/.emacs.d
;; https://github.com/minad/vertico
;; https://www.lucacambiaghi.com/vanilla-emacs/readme.html#h:EC68944C-F745-45D8-9905-420E0813DBAF
;; https://github.com/minad/consult/blob/main/README.org#use-package-example

(use-package consult
  :custom
  (consult-narrow-key "<")
  :bind
  (("C-x b"   . consult-buffer)
   ("C-x C-r" . consult-recent-file)
   ("M-g g"   . consult-line)
   ("M-g M-g" . consult-goto-line)
   ("M-g r"   . consult-ripgrep)
   ("M-g t"   . mk13/consult-menu)
   :map minibuffer-local-map
   ([remap previous-matching-history-element] . consult-history))
  :config
  (transient-define-prefix mk13/consult-menu ()
    "Consult search and navigation"
    [["Search"
      ("l" "Line" consult-line)
      ("L" "Line (all buffers)" consult-line-multi)
      ("r" "Ripgrep" consult-ripgrep)
      ("g" "Grep" consult-grep)
      ("G" "Git grep" consult-git-grep)
      ("f" "Find file" consult-find)]
     ["Navigate"
      ("o" "Outline" consult-outline)
      ("i" "Imenu" consult-imenu)
      ("I" "Imenu (project)" consult-imenu-multi)
      ("m" "Mark ring" consult-mark)
      ("M" "Global mark ring" consult-global-mark)
      ("b" "Bookmarks" consult-bookmark)]
     ["Filter"
      ("k" "Keep lines" consult-keep-lines)
      ("u" "Focus lines" consult-focus-lines)
      ("e" "Isearch history" consult-isearch-history)]
     ["Misc"
      ("h" "Man page" consult-man)
      ("H" "Info" consult-info)
      ("c" "Command history" consult-complex-command)]]))

(use-package re-builder
  :custom
  (reb-re-syntax 'string))

(use-package jsonian
  :after so-long
  :custom
  (jsonian-default-indentation 2)
  (jsonian-no-so-long-mode))

(use-package json-reformat
  :config
  (setq json-reformat:indent-width 2))

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
  (treesit-auto-langs '(python typescript terraform dockerfile nix go lua))
  :config
  (global-treesit-auto-mode))

(use-package golden-ratio
  :config
  (golden-ratio-mode 1))

(use-package auth-source-1password
  :straight (:host github :type git :repo "dlobraico/auth-source-1password")
  :init
  (auth-source-1password-enable))

(use-package chatgpt-shell
  :straight (:type git :host github :repo "xenodium/chatgpt-shell" :files ("chatgpt-shell*.el"))
  :custom
  (chatgpt-shell-openai-key (lambda ()
                              (auth-source-pick-first-password :host "OpenAI ChatGPT API Key" :user "credential"))))

(use-package cov)

(use-package nerd-icons
  :custom
  (nerd-icons-font-family "Hack Nerd Font Mono"))

(use-package fish-mode)

(use-package nix-ts-mode
  :mode "\\.nix\\'")

(use-package agent-shell
  :after (mise)
  :init
  ;; The way mise works is by adding advice for a predefined set of commands
  ;; https://github.com/eki3z/mise.el/blob/60ef63466d07417a9ef956af363baae39c9ddf34/mise.el#L368-L369
  ;; One could simply update that list to have agent-shell included, but this feels cleaner somehow.
  (advice-add 'agent-shell :around #'mise-propagate-env))

(use-package mise
  :config
  (global-mise-mode))

(use-package typst-ts-mode
  :straight (:type git :host codeberg :repo "meow_king/typst-ts-mode")
  :mode "\\.typ\\'")

(use-package mastodon
  :custom
  (mastodon-instance-url "https://hachyderm.io")
  (mastodon-active-user "mishok13")
  (mastodon-auth-use-auth-source nil))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))
