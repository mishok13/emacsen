;;; mishok-viewing --- Navigation and viewing Emacs capabilities
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package hydra
  :ensure t)

(use-package transpose-frame
  :ensure t
  :config
  (global-set-key
   (kbd "C-o")
   (defhydra hydra-frame-management (:color blue :hint nil)
     "
^Flip^                  ^Rotate^           ^Resize^
^^^^^^^^-----------------------------------------------
_t_: transpose          _r_: rotate 180°   _w_ Widen
_f_: flip-vertically    _c_: rotate  90°   _W_ Narrow
_F_: flip-horizontally  _C_: rotate -90°   _h_ Heighten
^ ^                     ^ ^                _H_ Shorten
"
     ("t" transpose-frame)
     ("f" flip-frame)
     ("F" flop-frame)
     ("r" rotate-frame)
     ("c" rotate-frame-clockwise)
     ("C" rotate-frame-anti-clockwise)
     ("w" enlarge-window-horizontally)
     ("W" shrink-window-horizontally)
     ("h" enlarge-window)
     ("H" shrink-window))))



(provide 'mishok-viewing)
;;; mishok-viewing ends here
