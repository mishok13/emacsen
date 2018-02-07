;;; mishok-typescript --- Clojure setup
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package tide
  :ensure t
  :config
  (require 'web-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(provide 'mishok-typescript)
;;; mishok-typescript ends here
