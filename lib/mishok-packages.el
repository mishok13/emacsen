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
    aggressive-indent
    ample-theme
    async
    auctex
    autopair
    bubbleberry-theme
    cider
    clj-refactor
    clojure-mode
    dash
    emmet-mode
    epl
    expand-region
    f
    fill-column-ind...
    firebelly-theme
    flatui-theme
    flx
    flx-ido
    flycheck
    flymake-cursor
    fringe-helper
    git-commit-mode
    git-rebase-mode
    helm
    helm-projectile
    highlight-paren...
    js2-mode
    js3-mode
    magit
    magit-svn
    markdown-mode
    monokai-theme
    multiple-cursors
    nginx-mode
    org
    paredit
    pivotal-tracker
    pkg-info
    powerline
    projectile
    puppet-mode
    pymacs
    python-pep8
    queue
    rainbow-delimiters
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
    yasnippet
    zenburn-theme
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
