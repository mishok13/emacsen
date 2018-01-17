;;; mishok-editing --- Everything that relates to being in global Emacs environment
;;; Commentary:
;;; Code:
(require 'use-package)
(require 'desktop)
(require 'saveplace)
(require 'undo-tree)

(setq initial-major-mode 'fundamental-mode)

(use-package ido
  :ensure t
  :bind (("C-x b" . ido-switch-buffer)
         ("M-o" . ido-switch-buffer))
  ;; :config
  ;; (ido-mode t)
  ;; (ido-everywhere t)
  )

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
  :ensure t
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
  :ensure t
  :bind (("C-x C-r" . helm-recentf))
  :init
  (setq recentf-max-saved-items 100)
  (setq helm-recentf-fuzzy-match t)
  :config
  (dolist (path '("/.emacs.d/el-get/" "~$" "/.autosaves/" "/emacs.d/elpa/" "/emacs.d/url/"))
    (add-to-list 'recentf-exclude path)))

(use-package flyspell
  ;; Look into using https://github.com/syohex/emacs-ac-ispell
  :ensure t
  :init
  (defun flyspell-check-next-highlighted-word ()
    "Custom function to spell check next highlighted word."
    (interactive)
    (flyspell-goto-next-error)
    (ispell-word))
  :bind (("<f2>" . ispell-word)
         ("<f3>" . flyspell-buffer)
         ("<f4>" . flyspell-check-previous-highlighted-word)
         ("<f5>" . flyspell-check-next-highlighted-word)))

(use-package markdown-mode
  :ensure t
  :init
  (add-hook 'markdown-mode-hook 'flyspell-mode))

(use-package flx-ido
  :ensure t
  :config
  (flx-ido-mode 1))

(use-package helm-projectile
  :ensure t)

(use-package helm
  :ensure t
  :bind ("C-M-y" . helm-show-kill-ring))

(use-package projectile
  :ensure t
  :init
  (define-key projectile-mode-map projectile-keymap-prefix nil)
  (define-key projectile-mode-map (kbd "C-c p") #'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p s") #'projectile-ripgrep)
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode))

(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode))

(use-package neotree
  :ensure t
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
  :ensure t
  :init
  (setq protobuf-c-style
        '((c-basic-offset . 4)))
  :config
  (add-hook 'protobuf-mode-hook (lambda () (c-add-style "protobuf-style" protobuf-c-style t))))

(use-package projectile-ripgrep
  :ensure t
  :init
  (setq ripgrep-arguments '("--smart-case")))

;; Don't create .#filenames
(setq create-lockfiles nil)

(provide 'mishok-editing)
;;; mishok-editing ends here
