;;; osx -- OS X related setup
;;; Commentary:
;;; Code:
(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize))

;; (setq test (shell-command-to-string "source ~/.bashrc; echo -n $PATH"))

(setq mac-allow-anti-aliasing t)

(provide 'osx)
;;; osx.el ends here
