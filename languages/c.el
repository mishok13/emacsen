(require 'cc-mode)


(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "k&r")
            (setq c-basic-offset 4)
	    (setq indent-tabs-mode nil)))


(add-hook 'c-mode-common-hook 'fci-mode)
