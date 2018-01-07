;;; mishok-git --- Magit setup
;;; Commentary:
;;; Code:

(defun insert-jira-issue-key-into-commit ()
  (let ((ISSUEKEY "[[:upper:]]+-[[:digit:]]+"))
    (when (string-match-p ISSUEKEY (magit-get-current-branch))
      (insert
       (replace-regexp-in-string
        (concat ".*?\\(" ISSUEKEY "\\).*")
        "\n\nRefs \\1"
        (magit-get-current-branch)))
      (beginning-of-buffer))))

(use-package magit
  :bind (("<f7>" . magit-status))
  :straight t
  :init
  (add-hook 'magit-mode-hook 'helm-mode)
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  (add-hook 'git-commit-setup-hook 'insert-jira-issue-key-into-commit)
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

(provide 'mishok-git)
;;; mishok-git.el ends here
