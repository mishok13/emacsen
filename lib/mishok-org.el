;;; mishok-org --- Org-mode related functions
;;; Commentary:
;;; Code:

(require 'org)

(setq org-directory (expand-file-name "~/Dropbox/org/"))
(defun org-path (tail) (concat org-directory tail))
(setq org-default-notes-file (org-path "notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (org-path "todo.org") "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("d" "Dutch" entry (file (org-path "dutch.org"))
         "* On %U:\n%?  %i\n")
        ("j" "Journal" entry (file+datetree (org-path "journal.org"))
         "* %?\nEntered on %U\n  %i\n  %a")))



(setq org-agenda-files (mapcar 'org-path '("work.org")))

(setq org-clock-persist 'history)
(setq org-log-done 'note)
(org-clock-persistence-insinuate)


(provide 'mishok-org)
;;; mishok-org ends here
