(require 'maxframe)

(defun set-exec-path-from-shell-PATH ()
  (let ((path-from-shell (replace-regexp-in-string
                          "[ \t\n]*$"
                          ""
                          (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))

;; Need to make sure Emacs successfully inherits PATH variable
(set-exec-path-from-shell-PATH)

(setq mac-allow-anti-aliasing t)

;; Turn to almost-full-screen
(maximize-frame)
