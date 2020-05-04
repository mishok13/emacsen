;;; osx -- OS X related setup
;;; Commentary:
;;; Code:
(use-package exec-path-from-shell
  :ensure t
  :config
  (setenv "LANG" "en_US.UTF-8")
  :init
  (exec-path-from-shell-copy-envs '("LC_ALL" "WORKON_HOME"))
  (setq exec-path-from-shell-check-startup-files nil)
  (exec-path-from-shell-initialize))

;; (setq test (shell-command-to-string "source ~/.bashrc; echo -n $PATH"))

(setq mac-allow-anti-aliasing t)

(provide 'osx)
;;; osx.el ends here
