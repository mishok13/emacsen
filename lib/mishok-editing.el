;;; mishok-editing --- Everything that relates to being in global Emacs environment
;;; Commentary:
;;; Code:
(require 'use-package)
(require 'desktop)
(require 'saveplace)
(require 'undo-tree)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq initial-major-mode 'fundamental-mode)

(use-package ido
  :straight t
  :bind (("M-o" . ido-switch-buffer)))

(setq-default indent-tabs-mode nil)

;; Set up backups
(setq backup-by-copying t
      backup-directory-alist '(("." . "~/.emacs.d/.backups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(visual-line-mode t)

;; clear up files before saving them
(defun delete-trailing-blank-lines ()
  "Delete all blank lines at the end of the file and leave single newline character."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (newline)              ;; ensures that there is at least one
    (delete-blank-lines))) ;; leaves at most one

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'before-save-hook 'delete-trailing-blank-lines)

;; Setup kill-buffer and system clipboard
;; this should enable copy from emacs to any other X frame
(setq x-select-enable-clipboard t)

;; Save history
(use-package desktop
  :straight t
  :init
  (setq history-length 250)
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

(use-package recentf
  :straight t
  :bind (("C-x C-r" . helm-recentf))
  :init
  (setq recentf-max-saved-items 100)
  (setq helm-recentf-fuzzy-match t)
  :config
  (dolist (path '("/.emacs.d/el-get/" "~$" "/.autosaves/" "/emacs.d/elpa/" "/emacs.d/url/"))
    (add-to-list 'recentf-exclude path)))

(use-package flyspell
  ;; Look into using https://github.com/syohex/emacs-ac-ispell
  :straight t)

(use-package markdown-mode
  :straight t
  :hook (markdown-mode-hook . flyspell-mode))

(use-package flx-ido
  :straight t
  :config
  (flx-ido-mode 1))

(use-package helm
  :straight t
  :bind ("C-M-y" . helm-show-kill-ring))

(use-package helm-rg
  :straight t)

(use-package helm-projectile
  :straight t
  :requires projectile)

(use-package projectile
  :straight t
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-enable-caching t)
  :config
  (setq projectile-switch-project-action 'helm-projectile)
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))

(use-package undo-tree
  :straight t
  :config
  (global-undo-tree-mode))

(use-package neotree
  :straight t
  :bind ("<f6>" . neotree-project-dir)
  :config
  (setq neo-smart-open t)
  (defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root.")))))

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; uncamelcase
(defun mishok/uncamelcase-string (s &optional sep start)
  "Convert CamelCase string S to lower case with word separator SEP.
Default for SEP is a hyphen \"-\".

If third argument START is non-nil, convert words after that
index in STRING."
  (let ((case-fold-search nil))
    (while (string-match "[A-Z]" s (or start 1))
      (setq s (replace-match (concat (or sep "-")
                                     (downcase (match-string 0 s)))
                             t nil s)))
    (downcase s)))

(defun mishok/uncamelcase-word-at-point ()
  "Translate camelCase into camel-case."
  (interactive)
  (let* ((case-fold-search nil)
         (start-point (point))
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (uncamelcase-string txt "-")) )
    (if cml
        (progn
          (delete-region beg end)
          (insert cml)))
    (goto-char start-point)))

(defun mishok/update-all-packages ()
  "Update all current packages."
  (interactive)
  (save-window-excursion
    (list-packages)
    (package-menu-mark-upgrades)
    (package-menu-execute 'noquery)))

(use-package protobuf-mode
  :straight t
  :init
  (setq protobuf-c-style
        '((c-basic-offset . 4)))
  :config
  (add-hook 'protobuf-mode-hook (lambda () (c-add-style "protobuf-style" protobuf-c-style t))))

(use-package projectile-ripgrep
  :straight t
  :init
  (setq ripgrep-arguments '("--smart-case")))

(use-package multiple-cursors
  :straight t
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind (("M-<mouse-1>" . mc/add-cursor-on-click)))

(use-package hungry-delete
  :bind (("M-c" . c-hungry-delete-forward)))

(use-package yasnippet
  :straight t
  :bind ("C-<tab>" . yas-expand)
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package yasnippet-snippets
  :straight t)

(use-package terraform-mode
  :straight t
  :init
  (add-hook 'terraform-mode-hook #'terraform-format-on-save-mode))

(use-package company-terraform
  :straight t)

;; Don't create .#filenames
(setq create-lockfiles nil)

(provide 'mishok-editing)
;;; mishok-editing ends here
