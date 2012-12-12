(defun donuts ()
  "For the love of God"
  (interactive)
  (print "Mmmm, donuts."))


(defun look-of-disapproval ()
  "Just in case we need this"
  (interactive)
  (insert "ಠ_ಠ"))


;; Don't let minibufer cursor jump into read-only prompt
(setq minibuffer-prompt-properties
      (plist-put minibuffer-prompt-properties 'point-entered 'minibuffer-avoid-prompt))

;; Make scrolling with C-v work on last page, instead of notifying
;; "end of buffer" error
(setq scroll-error-top-bottom 'true)
