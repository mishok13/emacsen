;;; mishok-twit -- Twittering-mode setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package twittering-mode
  :ensure t
  :init
  ;; Use password for accessing private key
  (setq twittering-use-master-password t)
  ;; Don't put message in echo area
  (setq twittering-url-show-status nil)
  ;; Open links from twits in EWW
  (setq browse-url-browser-function 'osx-browse-url-firefox))

(provide 'mishok-twit)
;;; mishok-twit.el ends here
