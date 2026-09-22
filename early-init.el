(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-echo-area-message (user-login-name))
(setq frame-resize-pixelwise t)
(setq default-frame-alist
      '((fullscreen . fullboth)
        ;; Disable the tool bar here (works in headless/batch too, unlike
        ;; calling `tool-bar-mode' which may be undefined). All these tools
        ;; are in the menu-bar anyway.
        (tool-bar-lines . 0)
        (vertical-scroll-bars . nil)
        (horizontal-scroll-bars . nil)
        ;; Setting background colors prevents aggressive "blinking" when emacs loads
        (background-color . "#000000")
        (foreground-color . "#ffffff")
        (ns-appearance . dark)
        (ns-transparent-titlebar . t)))
;; (setq debug-on-error 1)
(setq package-enable-at-startup nil)
