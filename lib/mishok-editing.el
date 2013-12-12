;;; mishok-editing --- Everything that relates to being in global Emacs environment
;;; Commentary:
;;; Code:

;; Set up backups
(ido-mode t)
(setq-default indent-tabs-mode nil)


(setq
   backup-by-copying t      ; don't clobber symlinks
   backup-directory-alist
    '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)       ; use versioned backups


;; clear up files before saving them

(defun delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file and leaves single newline character."
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

(require 'desktop)

(desktop-save-mode 1)
(setq history-length 250)

(add-to-list 'desktop-globals-to-save 'file-name-history)

(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)


;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

(require 'ido)
(require 'recentf)

(setq recentf-exclude
      (append recentf-exclude
              '("/.emacs.d/el-get/" "~$" "/.autosaves/"
                "/emacs.d/elpa/" "/emacs.d/url/"))
      recentf-max-saved-items 50)

(recentf-mode t)

;; Get ido to handle recentf results
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

(require 'flyspell)
(add-hook 'markdown-mode-hook 'flyspell-mode)
(define-key flyspell-mode-map (kbd "<f2>") 'ispell-word)
(define-key flyspell-mode-map (kbd "<f3>") 'flyspell-buffer)
(define-key flyspell-mode-map (kbd "<f4>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))
(define-key flyspell-mode-map (kbd "<f5>") 'flyspell-check-next-highlighted-word)

(provide 'mishok-editing)
;;; mishok-editing ends here
