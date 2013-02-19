(require 'flymake)

;; remove offending checkers for flymake
(delete '("\\.html?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
(delete '("\\.hs?\\'" flymake-xml-init) flymake-allowed-file-name-masks)
(delete '("\\.tex\\'" flymake-simple-tex-init) flymake-allowed-file-name-masks)
(delete '("\\.xml\\'" flymake-xml-init) flymake-allowed-file-name-masks)
(delete '("\\.php[345]?\\'" flymake-php-init) flymake-allowed-file-name-masks)
(delete '("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup) flymake-allowed-file-name-masks)
