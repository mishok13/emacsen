;;; mishok-editing --- Everything that relates to being in global Emacs environment
;;; Commentary:
;;; Code:
(require 'use-package)
(require 'desktop)
(require 'saveplace)

;; https://github.com/emacscollective/no-littering
;; https://github.com/KaratasFurkan/.emacs.d
;; https://github.com/minad/vertico

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
  (global-visual-line-mode 1)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)  )


;; Setup kill-buffer and system clipboard
;; this should enable copy from emacs to any other X frame
(setq x-select-enable-clipboard t)

;; Save history
(use-package desktop
  :straight t
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
  :straight t)

(use-package markdown-mode
  :straight t
  :hook (markdown-mode-hook . flyspell-mode))

(use-package project
  :straight t)

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
  :hook prog-mode
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
  :straight t)

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
  :bind
  (:map consult-mode-map
         ("M-s u" . consult-focus-lines)
         ("M-s k" . consult-keep-lines)
         ("M-s e" . consult-isearch-history)
         ("M-s d" . consult-find)
         ;; M-g …
         ("M-g g" . consult-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-info)
         ("M-g r" . consult-ripgrep)
         ("M-g m" . consult-mark)
         ("M-g M" . consult-global-mark)
         ;; Misc.
         ("C-x C-r" . consult-recent-file)))

(use-package embark
  :straight t

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
  :straight t
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'mishok-editing)
;;; mishok-editing ends here
