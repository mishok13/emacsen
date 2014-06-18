;;; init.el --- Initialization code for Emacs
;;; Commentary:
;;; License: See LICENSE file

;;; Code:

;; (add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/lib")
(add-to-list 'load-path "~/.emacs.d/lib/lang")

(require 'mishok-packages)

(require 'mishok-prog)
(require 'mishok-lisp)
(require 'mishok-sgml)
(require 'mishok-py)
(require 'mishok-clj)
(require 'mishok-c)
(require 'mishok-rust)

(require 'mishok-editing)
(require 'mishok-keybindings)
(require 'mishok-git)
(require 'mishok-twit)
(require 'mishok-utils)
(require 'mishok-org)
(require 'mishok-display)

;;; init.el ends here
