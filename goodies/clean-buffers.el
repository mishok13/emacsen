;; Set up cleaning of unused buffers

(require 'midnight)

 ;; run midnight every hour
(setq midnight-period (* 60 60))

;; Midnight has a notion of "special" and "normal" buffers,
;; normal meaning all actual code buffers
;; Here we mark all buffers as special, thus
(setq clean-buffer-list-kill-regexps '("^.*$"))

;; Don't ever kill these buffers
(add-to-list 'clean-buffer-list-kill-never-buffer-names
	     '("*Messages*" "*scratch*" ":home"))

;; Every buffer that haven't been active for specified amount
;; of seconds will be considered "inactive" and will be closed
;; duing next clean-buffer-list run
(setq clean-buffer-list-delay-special (* 60 60))

(run-at-time t 3600 'clean-buffer-list)
