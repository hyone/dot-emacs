
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  settings for Japanese
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mule-UCS
;; MUST  configure BEFORE (set-language-environment)
;; (when (and (not (featurep 'carbon-emacs-package)) (not (featurep 'ns)))
;;   (require 'un-define)
;;   (setq bitmap-alterable-charset 'tibetan-1-column)
;;   (require 'jisx0213))

;;; Japanese language environment
(set-language-environment "Japanese")

;; UTF-8 have precedence if enable
(prefer-coding-system 'utf-8)

; prevent japanese info from character collapse
(auto-compression-mode t)

; make euc-jp available in shell-mode on xemacs
(if (featurep 'xemacs)
    (add-hook 'shell-mode-hook (function
       (lambda () (set-buffer-process-coding-system 'euc-japan 'euc-japan))))
)

(if (featurep 'meadow)
    (set-default-coding-systems 'utf-8))

(set-default-coding-systems 'utf-8)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Input Method for Japanese
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; IME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; meadow + IME
(if (featurep 'meadow)
 (mw32-ime-initialize)
 (setq default-input-method "MW32-IME")
 (setq-default mw32-ime-mode-line-state-indicator "[--]")
 (setq mw32-ime-mode-line-state-indicator-list '("[--]" "[Å†]" "[--]"))
 (add-hook 'mw32-ime-on-hook
   (function (lambda () (set-cursor-height 2))))
 (add-hook 'mw32-ime-off-hook
   (function (lambda () (set-cursor-height 4)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Fonts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (featurep 'meadow)
    (let ((make-spec 
           (function 
            (lambda (size charset fontname &optional windows-charset)
              (setq size (- size))
              (if (not windows-charset)
                  (setq windows-charset 
                        (cadr (assq charset mw32-charset-windows-font-info-alist))))
              `(((:char-spec ,charset :height any)
                 strict
                 (w32-logfont ,fontname 0 ,size 400 0 nil nil nil ,windows-charset 1 3 0))
                ((:char-spec ,charset :height any :weight bold)
                 strict
                 (w32-logfont ,fontname 0 ,size 700 0 nil nil nil ,windows-charset 1 3 0)
                 ((spacing . -1)))
                ((:char-spec ,charset :height any :slant italic)
                 strict
                 (w32-logfont ,fontname 0 ,size 400 0   t nil nil ,windows-charset 1 3 0))
                ((:char-spec ,charset :height any :weight bold :slant italic)
                 strict
                 (w32-logfont ,fontname 0 ,size 700 0   t nil nil ,windows-charset 1 3 0)
                 ((spacing . -1)))))))
          (make-spec-list
           (function
            (lambda (size params-list)
              (list (cons 'spec 
                          (apply 'append 
                                 (mapcar (lambda (params)
                                           (apply make-spec (cons size params)))
                                         params-list))))
              )))
          (define-fontset 
            (function
             (lambda (fontname size fontset-list)
               (let ((spec (funcall make-spec-list size fontset-list)))
                 (if (w32-list-fonts fontname)
                     (w32-change-font fontname spec)
                   (w32-add-font fontname spec)
                   )))))
          (consolas-fontset-list
           '(
             (ascii "Consolas")
             (katakana-jisx0201 "MeiryoKe_Console")
             (japanese-jisx0208 "MeiryoKe_Console")
             (korean-ksc5601 "Dotum")
             (chinese-gb2312 "SimHei")
             (chinese-big5-1 "MingLiU")
             (chinese-big5-2 "MingLiU")
             ))
          (meiryoke_console-fontset-list
           '(
             (ascii "MeiryoKe_Console")
             (katakana-jisx0201 "MeiryoKe_Console")
             (japanese-jisx0208 "MeiryoKe_Console")
             (korean-ksc5601 "Dotum")
             (chinese-gb2312 "SimHei")
             (chinese-big5-1 "MingLiU")
             (chinese-big5-2 "MingLiU")
             ))

          )

      ;; Consolas
      (funcall define-fontset "Consolas 10" 10 consolas-fontset-list)
      (funcall define-fontset "Consolas 12" 12 consolas-fontset-list)
      (funcall define-fontset "Consolas 14" 14 consolas-fontset-list)
      (funcall define-fontset "Consolas 16" 16 consolas-fontset-list)
      (funcall define-fontset "Consolas 18" 18 consolas-fontset-list)
      (funcall define-fontset "Consolas 20" 20 consolas-fontset-list)
      (funcall define-fontset "Consolas 22" 22 consolas-fontset-list)
      (funcall define-fontset "Consolas 24" 24 consolas-fontset-list)
      (funcall define-fontset "Consolas 36" 36 consolas-fontset-list)
      (funcall define-fontset "Consolas 48" 48 consolas-fontset-list)

      ;; MeiryoKe_Console
      (funcall define-fontset "MeiryoKe_Console 10" 10 meiryoke_console-fontset-list)
      (funcall define-fontset "MeiryoKe_Console 12" 12 meiryoke_console-fontset-list)
      (funcall define-fontset "MeiryoKe_Console 14" 14 meiryoke_console-fontset-list)
      (funcall define-fontset "MeiryoKe_Console 16" 16 meiryoke_console-fontset-list)
      (funcall define-fontset "MeiryoKe_Console 18" 18 meiryoke_console-fontset-list)
      (funcall define-fontset "MeiryoKe_Console 20" 20 meiryoke_console-fontset-list)
      (funcall define-fontset "MeiryoKe_Console 22" 22 meiryoke_console-fontset-list)
      )
    (add-to-list 'default-frame-alist '(font . "MeiryoKe_Console 16"))
    (set-frame-font "MeiryoKe_Console 16")
)