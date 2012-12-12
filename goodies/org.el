(require 'org)

(setq org-directory "~/Dropbox/org/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline (concat org-directory "/todo.org") "Tasks")
             "* TODO %?\n  %i\n  %a")
        ("d" "Dutch" entry (file (concat org-directory "/dutch.org"))
             "* On %U:\n%?  %i\n")
        ("j" "Journal" entry (file+datetree (concat org-directory "/journal.org"))
             "* %?\nEntered on %U\n  %i\n  %a")))
