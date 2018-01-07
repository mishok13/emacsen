;;; mishok-rust --- Rust specific setup
;;; Commentary:
;;; Code:

(require 'use-package)

(use-package racer
  :straight t
  :hook (rust-mode . racer-mode))

(use-package rust-mode
  :straight t
  :init
  (add-hook 'rust-mode-hook 'smartparens-mode)
  (add-hook 'rust-mode-hook 'company-mode)
  (add-hook 'rust-mode-hook 'flycheck-mode))

(use-package racer
  :straight t
  :config
  (add-hook 'rust-mode-hook 'racer-mode)
  (add-hook 'racer-mode-hook 'eldoc-mode)
  ;; Temporary fix for https://github.com/racer-rust/emacs-racer/issues/140
  ;; (setq racer-rust-src-path
  ;;       (let* ((sysroot (string-trim
  ;;                        (shell-command-to-string "rustc --print sysroot")))
  ;;              (lib-path (concat sysroot "/lib/rustlib/src/rust/library"))
  ;;              (src-path (concat sysroot "/lib/rustlib/src/rust/src")))
  ;;         (or (when (file-exists-p lib-path) lib-path)
  ;;             (when (file-exists-p src-path) src-path))))
  )

(use-package company
  :straight t
  :bind ("TAB" . company-indent-or-complete-common)
  :config
  (add-hook 'racer-mode-hook #'company-mode)
  (setq company-tooltip-align-annotations t))


(provide 'mishok-rust)
;;; mishok-rust ends here
