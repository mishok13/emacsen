;;; mishok-org --- Org-mode related functions
;;; Commentary:
;;; Code:
(require 'use-package)

(use-package org
  :ensure t
  :bind (("<f8>" . org-capture)
         ("<f10>" . org-agenda))
  :init
  (defun org-path (tail) (concat org-directory tail))
  (setq org-log-done 'note)
  (org-clock-persistence-insinuate)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-directory (expand-file-name "~/Dropbox/org/"))
  (setq org-default-notes-file (org-path "notes.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline (org-path "todo.org") "Tasks")
           "* TODO %?\n  %i\n  %a")
          ("d" "Dutch" entry (file (org-path "dutch.org"))
           "* On %U:\n%?  %i\n")
          ("j" "Journal" entry (file+datetree (org-path "journal.org"))
           "* %?\nEntered on %U\n  %i\n  %a")))

  (setq org-agenda-files (mapcar 'org-path '("work.org" "nonwork.org")))
  :config
  ;; Set idle time to 10 minutes (10 minutes of idling will lead to
  ;; org-clock asking whether clock-out has to be performed)
  (setq org-clock-idle-time 10)
  (setq org-clock-persist 'history))

(provide 'mishok-org)
;;; mishok-org ends here
