;;; msk-python --- Summary
;;; Commentary:
;; (require 'flymake)
(require 'python)
(require 'autopair)
(require 'virtualenv)
(require 'compile)

;; Python-mode keybindings
(define-key python-mode-map (kbd "<f2>") 'pep8)
;; (define-key python-mode-map (kbd "<f3>") 'flymake-goto-next-error)
;; (define-key python-mode-map (kbd "<f4>") 'flymake-goto-prev-error)
;; (define-key python-mode-map (kbd "<f5>") 'flymake-display-err-menu-for-current-line)
(define-key python-mode-map (kbd "<f6>") 'py-shell)
(define-key python-mode-map (kbd "<f8>") 'compile)

;; don't run py-shell on startup
(setq py-start-run-py-shell nil)

;; default path to epylint
;; (setq epylint-path "~/.emacs.d/tools/epylint.py")

;; (setq flymake-log-level 0)

;; pylint checking
;; (defun flymake-pylint-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		     'flymake-create-temp-inplace))
;; 	 (local-file (file-relative-name
;; 		      temp-file
;; 		      (file-name-directory buffer-file-name))))
;;     (list epylint-path
;;           (if virtualenv-default-directory
;;               (list (format "-w %s " virtualenv-default-directory) local-file)
;;             (list local-file)))))


;; If I'm ever to change pylint to something else, I should just change init functions
;; (defconst flymake-allowed-python-file-name-masks
;;   '(("\\.py$" flymake-pylint-init)
;;     (".*$" flymake-pylint-init)))

;; (defun flymake-python-init ()
;;   (add-to-list 'flymake-allowed-file-name-masks
;;                '("\\.py$" flymake-pylint-init))
;;   (flymake-mode t))

;; (add-hook 'python-mode-hook
;;           ;; make sure flymake-python-init is only called after all buffer-local
;;           ;; variables have been loaded
;;           ;; this enables use for .dir-locals.el + virtualenv.el for pylint checks
;;           (lambda ()
;;             (add-hook 'hack-local-variables-hook 'flymake-python-init)))


;; This makes tab-traversal correctly recognize function scope
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-variable-buffer-local 'beginning-of-defun-function)
                 'py-beginning-of-def-or-class)
            (setq outline-regexp "def\\|class ")))


(add-hook 'python-mode-hook
          #'(lambda ()
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(add-hook 'python-mode-hook 'fci-mode)
