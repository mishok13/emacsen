;;; mishok-py --- Python setup
;;; Commentary:
;;; Code:
(require 'python)

(add-hook 'python-mode-hook 'smartparens-mode)
(add-hook 'python-mode-hook 'electric-indent-mode)

(add-hook 'python-mode-hook
          '(lambda ()
             (local-set-key (kbd "C-j") 'newline-and-indent)))

(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
 "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
 "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
 "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

(provide 'mishok-py)
;;; mishok-py ends here
