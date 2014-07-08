;;; mishok-editing --- Everything that relates to being in global Emacs environment
;;; Commentary:
;;; Code:

(require 'desktop)
(require 'saveplace)
(require 'ido)
(require 'flx-ido)
(require 'recentf)
(require 'flyspell)
(require 'undo-tree)

(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)

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
(desktop-save-mode t)
(setq history-length 250)

(add-to-list 'desktop-globals-to-save 'file-name-history)

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)


;; Save point position between sessions
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


(setq recentf-exclude
      (append recentf-exclude
              '("/.emacs.d/el-get/" "~$" "/.autosaves/"
                "/emacs.d/elpa/" "/emacs.d/url/"))
      recentf-max-saved-items 50)

(recentf-mode t)

;; Get ido to handle recentf results
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(add-hook 'markdown-mode-hook 'flyspell-mode)
(define-key flyspell-mode-map (kbd "<f2>") 'ispell-word)
(define-key flyspell-mode-map (kbd "<f3>") 'flyspell-buffer)
(define-key flyspell-mode-map (kbd "<f4>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word."
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(define-key flyspell-mode-map (kbd "<f5>") 'flyspell-check-next-highlighted-word)

(projectile-global-mode)
(global-undo-tree-mode)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; uncamelcase
(defun un-camelcase-string (s &optional sep start)
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

(defun uncamelcase-word-at-point ()
  (interactive)
  (let* ((case-fold-search nil)
         (start-point (point))
         (beg (and (skip-chars-backward "[:alnum:]:_-") (point)))
         (end (and (skip-chars-forward "[:alnum:]:_-") (point)))
         (txt (buffer-substring beg end))
         (cml (un-camelcase-string txt "-")) )
    (if cml (progn (delete-region beg end) (insert cml)))
    (goto-char start-point)))

;; Don't create .#filenames
(setq create-lockfiles nil)

(provide 'mishok-editing)
;;; mishok-editing ends here
