;;; mishok-viewing --- Navigation and viewing Emacs capabilities
;;; Commentary:
;;; Code:
(require 'use-package)
(require 'windmove)

(use-package hydra
  :straight t)

;;* Helpers

(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(use-package transpose-frame
  :straight t
  :config
  (global-set-key
   (kbd "C-S-o")
   (defhydra hydra-frame-management (:color blue :hint nil)
     "
^Flip^                  ^Rotate^           ^Splitter^
^^^^^^^^-----------------------------------------------
_t_: transpose          _r_: rotate 180°   _w_ Up
_f_: flip-vertically    _c_: rotate  90°   _s_ Down
_F_: flip-horizontally  _C_: rotate -90°   _a_ Left
^ ^                     ^ ^                _d_ Right
"
     ("t" transpose-frame)
     ("f" flip-frame)
     ("F" flop-frame)
     ("r" rotate-frame)
     ("c" rotate-frame-clockwise)
     ("C" rotate-frame-anti-clockwise)
     ("w" hydra-move-splitter-up)
     ("s" hydra-move-splitter-down)
     ("a" hydra-move-splitter-left)
     ("d" hydra-move-splitter-right)
     ("." hydra-repeat))))



(provide 'mishok-viewing)
;;; mishok-viewing ends here
