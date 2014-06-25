;;; mishok-packages --- Dependencies downloaded and packages setup
;;; Commentary:
;;; Code:

(require 'package)
(require 'cl)

(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(defvar prelude-packages
  '(ack-and-a-half
    auctex
    autopair
    clojure-mode
    cider
    dash
    emmet-mode
    expand-region
    fill-column-indicator
    flycheck
    flymake
    flymake-cursor
    flx-ido
    fringe-helper
    helm
    highlight-parentheses
    js2-mode
    js3-mode
    magit
    magit-svn
    markdown-mode
    org
    paredit
    pivotal-tracker
    powerline
    projectile
    pymacs
    python-pep8
    rainbow-mode
    rect-mark
    rust-mode
    s
    scala-mode2
    smart-tabs-mode
    smartparens
    smex
    twittering-mode
    undo-tree
    virtualenv
    yaml-mode
    rainbow-delimiters
    zencoding-mode)
  "A list of packages to ensure are installed at launch.")

(defun prelude-packages-installed-p ()
  "Check if all packages in `prelude-packages' are installed."
  (every #'package-installed-p prelude-packages))

(defun prelude-install-packages ()
  "Install all packages listed in `prelude-packages'."
  (unless (prelude-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Emacs Prelude is now refreshing its package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (mapc #'package-install
     (remove-if #'package-installed-p prelude-packages))))

(prelude-install-packages)

(provide 'mishok-packages)
;;; mishok-packages ends here
