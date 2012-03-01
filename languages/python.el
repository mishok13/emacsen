(require 'flymake)
(require 'python-mode)
(require 'autopair)
(require 'virtualenv)

;; pylint checking
(defun flymake-pylint-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name
		      temp-file
		      (file-name-directory buffer-file-name))))
    (list "/usr/local/bin/epylint" (list local-file))))


;; If I'm ever to change pylint to something else, I should just change init functions
(defconst flymake-allowed-python-file-name-masks
  '(("\\.py$" flymake-pylint-init)
    (".*$" flymake-pylint-init)))

(defun flymake-python-load ()
  (setq flymake-allowed-file-name-masks
	(append flymake-allowed-file-name-masks
		flymake-allowed-python-file-name-masks))
  (flymake-mode t))
(add-hook 'python-mode-hook 'flymake-python-load)


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


;; (add-hook 'kill-emacs-hook
;;           (lambda () )
