;;; mishok-display --- All display specific things
;;; Commentary:
;;; Code:

(require 'powerline)
(require 'uniquify)


;; Switch off splash screen at Emacs startup
(setq inhibit-splash-screen t)

(tool-bar-mode 0)
(menu-bar-mode 0)
(tooltip-mode 0)
(setq initial-scratch-message nil)
(scroll-bar-mode 0)
(blink-cursor-mode 0)
(line-move-visual 0)
(global-hl-line-mode t)
(column-number-mode t)

;; FIXME: disable line numbers for certain modes
(global-linum-mode t)

;; Setup fonts
(global-font-lock-mode t)
(set-face-attribute 'default (not 'this-frame-only)
                    :font "Consolas"
                    :height 120)

(load-theme 'zenburn t)

(powerline-default-theme)
;; (add-hook 'after-setting-font-hook 'powerline-reset)

;; Make scrolling with C-v work on last page, instead of notifying
;; "end of buffer" error
(setq scroll-error-top-bottom 'true)

;; Don't let minibufer cursor jump into read-only prompt
(setq minibuffer-prompt-properties
      (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt))

;; 1-2 letters shorter to type!
(fset 'yes-or-no-p 'y-or-n-p)

;; Set up cleaning of unused buffers
(run-at-time t 3600 'clean-buffer-list)

;; Make sure every file name has unique name
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "|")
(setq uniquify-after-kill-buffer-p t)
(setq uniquify-ignore-buffers-re "^\\*")
;; Setup scrolling behaviour

;; make scroll behave more like notepad, he-he
(setq scroll-conservatively 10000)
(setq scroll-step 1)
(setq scroll-preserve-screen-position 't)
(require 'smex)
(smex-initialize)

(provide 'mishok-display)
;;; mishok-display ends here


