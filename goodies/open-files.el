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
