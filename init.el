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

(use-package emacs
  :config
  ;; Disables suspend-frame keybindings. Because why does it even exist?
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (setq initial-major-mode 'fundamental-mode)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (setq-default indent-tabs-mode nil)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq backup-by-copying t
        backup-directory-alist`((".*" . ,temporary-file-directory))
        auto-save-file-name-transforms`((".*" ,temporary-file-directory t))
        scroll-error-top-bottom 'true
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t
        visible-bell nil
        x-select-enable-clipboard t
        ring-bell-function #'ignore
        create-lockfiles nil)
  (recentf-mode t)
  (global-visual-line-mode t)
  (pixel-scroll-precision-mode t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package zenburn-theme
  :straight t
  :init (load-theme 'zenburn t))

(use-package dash
  :straight t)

(use-package treesit
  :defer t
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
  ;; Enable eldoc in REPL
  (add-hook 'cider-mode-hook 'eldoc-mode))

(use-package go-mode
  :straight t)

(use-package typescript-mode
  :straight t
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook 'company-mode)
  (add-hook 'typescript-mode-hook 'smartparens-mode))

(use-package jsonian
  :straight t
  :after (so-long)
  :custom
  (jsonian-no-so-long-mode))

(use-package paredit
  :straight t
  :hook (emacs-lisp-mode clojure-mode cider-repl-mode))

(use-package rainbow-delimiters
  :straight t
  :hook (emacs-lisp-mode clojure-mode cider-repl-mode python-mode python-ts-mode))

(use-package aggressive-indent
  :straight t
  :hook (emacs-lisp-mode clojure-mode))

(use-package fill-column-indicator
  :straight t
  :hook (prog-mode . fci-mode)
  :init
  (setq-default fci-rule-column 80))

(use-package python-ts-mode
  :defer t
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
  :straight t)

(use-package dockerfile-mode
  :straight t)


(use-package smartparens
  :straight t
  :hook (python-mode python-ts-mode rustic-mode typescript-mode))

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
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (elpy-enable))

(use-package poetry
  :ensure t
  :straight t)

(use-package eglot
  :hook ((rustic-mode . eglot-ensure)
         (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("C-M-h a" . eglot-code-actions)
              ("C-M-h r" . eglot-rename)
              ("C-M-h Q" . eglot-shutdown-all))  )

(use-package rustic
  ;; I would like to make rustic window for compilation narrower and
  ;; shorter if possible, as well as automatically focus into it. It should be possible with https://www.reddit.com/r/emacs/comments/cpdr6m/any_additional_docstutorials_on_displaybuffer_and/
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/The-Zen-of-Buffer-Display.html
  :straight t
  ;; :mode ("\\.rs\\'" . rustic-mode)
  :config
  (setq rustic-format-on-save t)
  (setq rustic-lsp-client 'eglot)
  (add-to-list 'display-buffer-alist
               `("^\\*rustic-compilation\\*$"
                 (display-buffer-reuse-window display-buffer-below-selected display-buffer-at-bottom)
                 (inhibit-same-window . t)
                 (window-min-height . 10)
                 (window-height . 0.25)
                 (inhibit-switch-frame . nil))
               t
               ))

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
  :custom
  (web-mode-enable-auto-indentation nil)
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

(use-package flymake
  )

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


;; Don't let minibufer cursor jump into read-only prompt
(setq minibuffer-prompt-properties
      (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt))


;; Make sure every file name has unique name
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")

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
  :straight t)

(use-package flyspell
  ;; Look into using https://github.com/syohex/emacs-ac-ispell
  :hook (markdown-mode)
  )

(use-package project)

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
  :after (yasnippet)
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

        ("M-g g" . consult-line)
        ("M-g M-g" . consult-goto-line)
        ("M-g o" . consult-outline)
        ("M-g i" . consult-imenu)
        ("M-g I" . consult-info)
        ("M-g r" . consult-ripgrep)
        ("M-g m" . consult-mark)
        ("M-g M" . consult-global-mark)

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

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package magit
  :straight t
  :bind (("<f7>" . magit-status))
  :init
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  :config
  (setq magit-bury-buffer-function 'magit-restore-window-configuration))

(use-package ediff
  :init
  (setq ediff-setup-windows-plain 'ediff-setup-windows-plain))

(use-package transient
  :straight t)

(use-package forge
  :after (magit)
  :straight t)

(use-package expand-region
  :straight t
  :bind (("M-@" . er/expand-region)))

(use-package windmove
  :straight t
  :bind (("<left>" . windmove-left)
         ("<right>" . windmove-right)
         ("<up>" . windmove-up)
         ("<down>" . windmove-down)))

(use-package org
  :ensure t
  :bind (("<f8>" . org-capture)
         ("<f10>" . org-agenda))
  :custom
  (org-directory (expand-file-name "~/nonwork/kitchensink/notes/org/"))
  (org-default-notes-file (expand-file-name "notes.org" org-directory))
  (org-clock-idle-time 10)
  (org-clock-persist 'history)
  (org-log-done 'note)
  :init
  (org-clock-persistence-insinuate)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-agenda-files (-map (-cut expand-file-name <> org-directory) '("work.org" "nonwork.org"))))

(defun look-of-disapproval ()
  "Just in case we need this"
  (interactive)
  (insert "ಠ_ಠ"))

(use-package hydra
  :straight t
  :config
  (setq hydra-is-helpful 't)
  :bind
  (("C-M-g" . hydra-window-management/body)
   ("C-M-c" . hydra-flymake/body)))

(use-package major-mode-hydra
  :straight t
  :after hydra)

(use-package transpose-frame
  :straight t)

(pretty-hydra-define hydra-window-management
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

(pretty-hydra-define hydra-flymake
  (:color red :title "Flymake" :quit-key "q" :foreign-keys warn)
  ("Flymake"
   (("p" flymake-show-project-diagnostics)
    ("b" flymake-show-buffer-diagnostics)
    ("x" flymake-show-diagnostic))))
