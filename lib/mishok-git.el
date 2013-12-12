;; full screen magit-status

(require 'magit)
(require 'magit-svn)


(global-set-key (kbd "<f7>") 'magit-status)

(add-hook 'magit-mode-hook 'magit-load-config-extensions)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(provide 'mishok-git)
