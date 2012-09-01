(eval-when-compile    (require 'color-theme))

(defun color-theme-darkneon ()
  "Color theme for GUI version by hyone"
  (interactive)
  (setq frame-background-mode 'dark)
  (color-theme-install
   '(color-theme-darkneon
     ((foreground-color . "grey78")
      (background-color . "grey9")
      (background-mode  . dark)
      (border-color     . "black")
      (cursor-color     . "dark orange")
      ;; set ime on/off colors
      (add-hook 'mw32-ime-on-hook  (function (lambda () (set-cursor-color "midnight blue"))))
      (add-hook 'mw32-ime-off-hook (function (lambda () (set-cursor-color "dark orange"))))
      (mouse-color . "sienna1"))

     (default     ((t (nil))))
     (bold        ((t (:bold t :weight bold))))
     (bold-italic ((t (:italic t :bold t :slant italic :weight bold))))
     (border      ((t (:background "black"))))
     (highlight   ((t (:background "DarkOrange3" :foreground "white"))))
     (underline   ((t (:underline t))))
     (button      ((t (:underline t))))
     (buffer-menu-buffer ((t (:bold t :weight bold))))

     (mouse ((t (:background "sienna1"))))

     (region ((t (:background "#5A647E"))))

     ;; (completions-common-part
     ;;   ((t (:family "*" :width normal :weight normal
     ;;        :slant normal :underline nil :overline nil
     ;;        :strike-through nil :box nil :inverse-video nil
     ;;        :foreground "white" :background "black" :stipple nil :height 120))))
     (completions-common-part      ((t (:foreground "dark orange" :underline t))))
     (completions-first-difference ((t (:bold t :weight bold))))

     (font-lock-builtin-face              ((t (:foreground "#81d681"))))     ; lightgreen
     (font-lock-comment-delimiter-face    ((t (:foreground "#8b7e66"))))     ; hada iro
     (font-lock-comment-face              ((t (:foreground "#8b7e66"))))
     (font-lock-constant-face             ((t (:foreground "#6ea3ff"))))     ; light bule
     (font-lock-doc-face                  ((t (:foreground "LightSalmon"))))
     (font-lock-doc-string-face           ((t (:foreground "LightSalmon"))))
     (font-lock-function-name-face        ((t (:foreground "#6ea3ff"))))
     (font-lock-keyword-face              ((t (:foreground "dark orange"))))
     (font-lock-negation-char-face        ((t (nil))))
     (font-lock-preprocessor-face         ((t (:foreground "khaki3"))))
     (font-lock-reference-face            ((t (:foreground "#6ea3ff"))))
     (font-lock-regexp-grouping-backslash ((t (:bold t :weight bold))))
     (font-lock-regexp-grouping-construct ((t (:bold t :weight bold))))
     (font-lock-string-face               ((t (:background "#181010" :foreground "PeachPuff3"))))
     ;; (font-lock-string-face               ((t (:background "#210" :foreground "#bbaa99"))))
     (font-lock-type-face                 ((t (:foreground "PaleGreen"))))
     (font-lock-variable-name-face        ((t (:foreground "burlywood3"))))
     (font-lock-warning-face              ((t (:bold t :foreground "Pink" :weight bold))))

     (mode-line                    ((t (:background "#222" :foreground "DarkOrange1" :box nil))))
     (mode-line-inactive           ((t (:background "black"  :foreground "#666" :box nil))))
     (mode-line-buffer-id          ((t (:bold t))))
     ;; (modeline-buffer-id           ((t (:foreground "DarkOrange1"))))
     (modeline-mousable            ((t (:background "white" :foreground "black"))))
     (modeline-mousable-minor-mode ((t (:background "white" :foreground "black"))))
     ;; custom mode line faces
     (mode-line-mode-face          ((t (:foreground "white"))))
     (mode-line-directory-face     ((t (:foreground "grey60"))))
     (mode-line-minor-mode-face    ((t (:foreground "grey60" :height 120))))

     (header-line ((t (:background "grey15"))))

     (my-face-b-1 ((t (:background "gray"))))
     (my-face-b-2 ((t (:background "honeydew"))))
     (my-face-u-1 ((t (:foreground "SteelBlue" :underline t))))

     ;; parentheses matched
     ;  (show-paren-match-face ((t (:background "burlywood4"))))
     (show-paren-match-face    ((t (:background nil       :underline "DarkOrange2"))))
     (show-paren-mismatch-face ((t (:background "#ff0000" :underline nil))))

     ;; info
     (info-xref         ((t (:foreground "DarkOrange2"))))
     (info-xref-visited ((t (:foreground "LightSalmon4"))))
     ;; isearch
     (isearch        ((t (:background "DarkOrange2"     :foreground "black"  :weight bold))))
     (lazy-highlight ((t (:background "LightGoldenrod2" :foreground "grey10"))))

     ;; elscreen
     (elscreen-tab-background-face     ((t (:background "grey20"))))
     (elscreen-tab-control-face        ((t (:background "grey30" :foreground "grey90"))))
     (elscreen-tab-current-screen-face ((t (:background "grey10" :foreground "grey70"))))
     (elscreen-tab-other-screen-face   ((t (:background "grey30" :foreground "grey50"))))

     ;; anything.el
     (anything-candidate-number ((t (:background nil           :foreground "grey"))))
     ;; don't work below and anything-selection-face use 'highlight' face.
     ;; (anything-selection-face   ((t (:background "DarkOrange3" :foreground "white"))))
     (anything-visible-mark     ((t (:background "DarkOrange3" :foreground "white"))))
     (anything-match            ((t (:background nil :foreground "dark orange" :underline t))))

     ;; auto-complete popup menu
     (ac-candidate-face ((t (:background "gray10" :foreground "grey75"))))
     (ac-selection-face ((t (:background "DarkOrange2" :foreground "white"))))

     ;; jaspace
     (jaspace-highlight-jaspace-face   ((t (:foreground "#687888" :background "#090c13"))))
     (jaspace-highlight-tab-face       ((t (:foreground "#687888" :background "#090c13" :underline t))))
     (trailing-whitespace              ((t (:foreground "#687888" :background "#090c13" :underline t))))

     ;; perl-mode
     (cperl-array-face ((t (:bold nil :background nil :foreground "LightGoldenrod"))))
     (cperl-hash-face  ((t (:bold nil :background nil :foreground "LightCoral"))))

     ;; comint (inf-ruby, inf-scheme)
     (comint-highlight-prompt ((t (:foreground "dark orange" :bold nil))))

     ;; flymake
     (flymake-errline ((t (:background "Firebrick4" :bold t))))
     (flymake-warnline ((t (:background "Firebrick4" :bold t))))

     ;; zlc
     (zlc-selected-completion-face ((t (:background "DarkOrange3" :foreground "white" :bold t))))

     ;; yasnippet
     (yas/field-highlight-face ((t (:background "grey15"))))

;;      (next-error ((t (:background "blue"))))
;;      (nobreak-space ((t (:foreground "cyan" :underline t))))
;;      (primary-selection ((t (:background "blue"))))
;;      (query-replace ((t (:background "blue"))))
;;      (scroll-bar ((t (nil))))
;;      (search-buffers-face ((t (:bold t :background "SkyBlue" :foreground "Black" :weight bold))))
;;      (search-buffers-header-face ((t (:bold t :background "gray20" :foreground "azure3" :weight bold))))
;;      (secondary-selection ((t (:background "slateblue"))))
;;      (shadow ((t (:foreground "grey70"))))
;;      (tool-bar ((t (:background "grey75" :foreground "black" :box (:line-width 1 :style released-button)))))
;;      (tooltip ((t (:family "helv" :background "lightyellow" :foreground "black"))))
;;      (trailing-whitespace ((t (:background "red1"))))
;;      (variable-pitch ((t (:family "helv"))))
;;      (vertical-border ((t (nil))))
;;      (widget-button ((t (:bold t :weight bold))))
;;      (widget-button-pressed ((t (:foreground "red"))))
;;      (widget-documentation ((t (:foreground "lime green"))))
;;      (widget-field ((t (:background "dim gray"))))
;;      (widget-inactive ((t (:foreground "light gray"))))
;;      (widget-single-line-field ((t (:background "dim gray"))))
;;      (zmacs-region ((t (:background "blue")))))))
     ))

  ;; parentheses style
  (show-paren-mode 1)
  (setq show-paren-delay 0)
  (setq show-paren-style 'expression)

  ;; diff-mode
  (add-hook 'diff-mode-hook
        (lambda ()
          (set-face-foreground 'diff-file-header-face "light goldenrod")
          (set-face-foreground 'diff-index-face "thistle")
          (set-face-foreground 'diff-hunk-header-face "plum")
          (set-face-foreground 'diff-removed-face "pink")
          (set-face-background 'diff-removed-face "gray26")
          (set-face-foreground 'diff-added-face "light green")
          (set-face-background 'diff-added-face "gray26")
          (set-face-foreground 'diff-changed-face "DeepSkyBlue1")
          ))

  ;; (set-face-background 'show-paren-match-face "grey15")
  ;; (set-face-foreground 'show-paren-match-face "grey75")
)

(add-to-list 'color-themes '(color-theme-darkneon  "Dark Neon" "hyone"))
