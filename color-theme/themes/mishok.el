(eval-when-compile
  (require 'color-theme))

(defun color-theme-mishok ()
 "Mishok theme, based on dark themes.
Created by Andrii V. Mishkovskyi <mishok13@gmail.com> Dec 2 2008."
 (interactive)
 (color-theme-install
  '(color-theme-mishok
    ((background-color . "gray15")
     (background-mode . dark)
     (border-color . "gray30")
     (cursor-color . "LightYellow2")
     (foreground-color . "gray80")
     (mouse-color . "sienna1"))
    ((buffers-tab-face . buffers-tab)
     (vc-mode-face . highlight))
    (default ((t (:background "gray15" :foreground "gray80"))))
    (blue ((t (:foreground "blue"))))
    (bold ((t (:bold t))))
    (bold-italic ((t (:bold t))))
    (border-glyph ((t (nil))))
    (buffers-tab ((t (:background "grey60" :foreground "black"))))
    (calendar-today-face ((t (:underline t))))
    (custom-button-face ((t (nil))))
    (custom-changed-face ((t (:background "blue" :foreground "white"))))
    (custom-documentation-face ((t (nil))))
    (custom-face-tag-face ((t (:underline t))))
    (custom-group-tag-face ((t (:underline t :foreground "light blue"))))
    (custom-group-tag-face-1 ((t (:underline t :foreground "pink"))))
    (custom-invalid-face ((t (:background "red" :foreground "yellow"))))
    (custom-modified-face ((t (:background "blue" :foreground "white"))))
    (custom-rogue-face ((t (:background "black" :foreground "pink"))))
    (custom-saved-face ((t (:underline t))))
    (custom-set-face ((t (:background "white" :foreground "blue"))))
    (custom-state-face ((t (:foreground "lime green"))))
    (custom-variable-button-face ((t (:underline t :bold t))))
    (custom-variable-tag-face ((t (:underline t :foreground "light blue"))))
    (diary-face ((t (:foreground "IndianRed"))))
    (font-lock-builtin-face ((t (:foreground "LightSteelBlue"))))
    (font-lock-comment-face ((t (:foreground "gray50"))))
    (font-lock-constant-face ((t (:foreground "Aquamarine"))))
    (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
    (font-lock-function-name-face ((t (:foreground "gold2"))))
    (font-lock-keyword-face ((t (:foreground "Salmon3"))))
    (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
    (font-lock-reference-face ((t (:foreground "SlateBlue"))))
    (font-lock-string-face ((t (:foreground "Orange2"))))
    (font-lock-type-face ((t (:foreground "OliveDrab3"))))
    (font-lock-variable-name-face ((t (:foreground "darkseagreen"))))
    (rst-level-1-face ((t (:foreground "brown"))))
    (rst-level-2-face ((t (:foreground "firebrick"))))
    (rst-level-3-face ((t (:foreground "orange"))))
    (rst-level-4-face ((t (:foreground "chocolate"))))
    (rst-level-5-face ((t (:foreground "salmon"))))
    (rst-level-6-face ((t (:foreground "lightsalmon"))))
    (font-latex-bold-face ((t (:foreground "DarkOrange" :bold t))))
    (font-latex-sectioning-0-face ((t (:foreground "brown" :bold t))))
    (font-latex-sectioning-1-face ((t (:foreground "firebrick" :bold t))))
    (font-latex-sectioning-2-face ((t (:foreground "orange" :bold t))))
    (font-latex-sectioning-3-face ((t (:foreground "chocolate" :bold t))))
    (font-latex-sectioning-4-face ((t (:foreground "salmon"))))
    (font-latex-sectioning-5-face ((t (:foreground "lightsalmon"))))
    

    (rst-level-1-face ((t (:foreground "brown" ))))
    (rst-level-2-face ((t (:foreground "firebrick" ))))
    (rst-level-3-face ((t (:foreground "Orange" ))))
    (rst-level-4-face ((t (:foreground "chocolate" ))))
    (rst-level-5-face ((t (:foreground "salmon" ))))
    (rst-level-6-face ((t (:foreground "lightsalmon" ))))
    (font-latex-sectioning-0-face ((t (:foreground "brown" :size "12pt"))))
    (font-latex-sectioning-1-face ((t (:foreground "firebrick" :size "12pt"))))
    (font-latex-sectioning-2-face ((t (:foreground "Orange" :size "12pt"))))
    (font-latex-sectioning-3-face ((t (:foreground "chocolate" :size "12pt"))))
    (font-latex-sectioning-4-face ((t (:foreground "salmon" :size "12pt"))))
    (font-latex-sectioning-5-face ((t (:foreground "lightsalmon" :size "12pt"))))

;; font-latex-bold-face                 abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-doctex-documentation-face abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-doctex-preprocessor-face  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-italic-face               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-math-face                 abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-sectioning-0-face         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-sectioning-1-face         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-sectioning-2-face         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-sectioning-3-face         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-sectioning-4-face         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-sectioning-5-face         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-sedate-face               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-slide-title-face          abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-string-face               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-subscript-face            abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-superscript-face          abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-verbatim-face             abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-latex-warning-face              abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-builtin-face               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-comment-delimiter-face     abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-comment-face               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-constant-face              abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-doc-face                   abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-doc-string-face            abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-function-name-face         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-keyword-face               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-negation-char-face         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-preprocessor-face          abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-reference-face             abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-regexp-grouping-backslash  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-regexp-grouping-construct  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-string-face                abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-type-face                  abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-variable-name-face         abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ
;; font-lock-warning-face               abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ

    (flymake-errline ((t (:underline "RosyBrown4"))))
    (flymake-warnline ((t (:underline "Grey30"))))
    (green ((t (:foreground "green"))))
    (gui-button-face ((t (:background "grey75" :foreground "black"))))
    (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
    (highlight ((t (:background "darkolivegreen"))))
    (highline-face ((t (:background "SeaGreen"))))
    (holiday-face ((t (:background "DimGray"))))
    (info-menu-5 ((t (:underline t))))
    (info-node ((t (:underline t :bold t :foreground "DodgerBlue1"))))
    (info-xref ((t (:underline t :foreground "DodgerBlue1"))))
    (isearch ((t (:background "blue"))))
    (isearch-secondary ((t (:foreground "red3"))))
    (italic ((t (nil))))
    (left-margin ((t (nil))))
    (list-mode-item-selected ((t (:background "gray68" :foreground "white"))))
    (modeline ((t (:background "black" :foreground "grey50" :box (:line-width 1 :style released-button)))))
    (modeline-buffer-id ((t (:background "black" :foreground "grey50"))))
    (modeline-mousable ((t (:background "black" :foreground "grey50"))))
    (modeline-mousable-minor-mode ((t (:background "black" :foreground "grey50"))))
    (which-func ((t (:forground "gray50"))))
    (pointer ((t (nil))))
    (primary-selection ((t (:background "grey15"))))
    (red ((t (:foreground "red"))))
    (region ((t (:background "grey45"))))
    (right-margin ((t (nil))))
    (secondary-selection ((t (:background "darkslateblue"))))
    (show-paren-match-face ((t (:background "Aquamarine" :foreground "SlateBlue"))))
    (show-paren-mismatch-face ((t (:background "Red" :foreground "White"))))
    (text-cursor ((t (:background "yellow" :foreground "black"))))
    (toolbar ((t (nil))))
    (underline ((nil (:underline nil))))
    (vertical-divider ((t (nil))))
    (widget ((t (nil))))
    (widget-button-face ((t (:bold t))))
    (widget-button-pressed-face ((t (:foreground "red"))))
    (widget-documentation-face ((t (:foreground "lime green"))))
    (widget-field-face ((t (:background "dim gray"))))
    (widget-inactive-face ((t (:foreground "light gray"))))
    (widget-single-line-field-face ((t (:background "dim gray"))))
    (woman-bold-face ((t (:bold t))))
    (woman-italic-face ((t (:foreground "beige"))))
    (woman-unknown-face ((t (:foreground "LightSalmon"))))
    (yellow ((t (:foreground "yellow"))))
    (twit-message-face ((t (:slant normal :weight normal :height 120 :width normal :foundry "unknown" :family "Verdana"))))
    (twit-author-face ((t (:height 100 :family "DejaVu Sans Mono"))))
    (twit-info-face ((t (:slant normal :height 80 :family "DejaVu Sans Mono"))))
    (zmacs-region ((t (:background "snow" :foreground "gray50")))))))
