;;; mishok-packages --- Dependencies downloaded and packages setup
;;; Commentary:
;;; Code:
(require 'cl)

(defvar prelude-packages
  '(ack-and-a-half
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
    firebelly-theme
    flatui-theme
    flx
    flx-ido
    flycheck
    flymake-cursor
    fringe-helper
    helm
    markdown-mode
    monokai-theme
    multiple-cursors
    nginx-mode
    org
    paredit
    pkg-info
    puppet-mode
    pymacs
    python-pep8
    queue
    rainbow-mode
    rect-mark
    rust-mode
    s
    smart-tabs-mode
    smart-mode-line
    smartparens
    smex
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
