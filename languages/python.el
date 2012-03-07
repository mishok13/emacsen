(require 'flymake)
(require 'python-mode)
(require 'autopair)
(require 'virtualenv)
(require 'compile)


;; don't run py-shell on startup
(setq py-start-run-py-shell nil)

;; pylint checking
(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "~/.emacs.d/tools/epylint"
          (if virtualenv-default-directory
              (list (format "-w %s" virtualenv-default-directory) local-file)
            (list local-file)))))


;; If I'm ever to change pylint to something else, I should just change init functions
;; (defconst flymake-allowed-python-file-name-masks
;;   '(("\\.py$" flymake-pylint-init)
;;     (".*$" flymake-pylint-init)))

(defun flymake-python-init ()
  (message (format "wtf: %s" 'virtualenv-default-directory))
  (add-to-list 'flymake-allowed-file-name-masks
               '("\\.py$" flymake-pylint-init))
  (add-to-list 'flymake-allowed-file-name-masks
               '(".*$" flymake-pylint-init))
  (flymake-mode t))
(add-hook 'python-mode-hook 'flymake-python-init)


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


(define-key python-mode-map (kbd "<f2>") 'pep8)
(define-key python-mode-map (kbd "<f3>") 'flymake-goto-next-error)
(define-key python-mode-map (kbd "<f4>") 'flymake-goto-prev-error)
(define-key python-mode-map (kbd "<f5>") 'flymake-display-err-menu-for-current-line)
(define-key python-mode-map (kbd "<f6>") 'py-shell)
