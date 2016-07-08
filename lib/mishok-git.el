;;; mishok-git --- Magit setup
;;; Commentary:
;;; Code:

(use-package magit
  :bind (("<f7>" . magit-status))
  :ensure t
  :init
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  (add-hook 'git-commit-mode-hook 'flyspell-mode)
  :config
  (defadvice magit-quit-window (around magit-restore-screen activate)
    (let ((current-mode major-mode))
      ad-do-it
      (when (eq 'magit-status-mode current-mode)
        (jump-to-register :magit-fullscreen)))))

(use-package travis
  :ensure t)

(provide 'mishok-git)
;;; mishok-git.el ends here
