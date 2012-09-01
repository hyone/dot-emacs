(require 'skk-autoloads nil t)


;    make skk-mode ON
(global-set-key "\C-x\C-j" '(lambda () (interactive) (skk-mode 1)))
(global-set-key (kbd "<s-escape>") '(lambda () (interactive) (skk-mode 1)))
;; ;    make skk-mode OFF
;; (global-set-key "\C-c\C-j" '(lambda () (interactive) (skk-mode -1)))

;; (setq skk-kakutei-key (kbd "C-m"))
(setq skk-kakutei-key (kbd "<s-escape>"))

;; display precedently candidates have correct okuri-gana
(setq skk-henkan-strict-okuri-precedence t)
;; check mistake okuri-gana whene regist KANJI
(setq skk-check-okurigana-on-touroku t)

(setq skk-keep-record t)

;    display candidates for completion
(setq skk-dcomp-activate t)

(setq skk-show-inline t)

;    display message in Japanese
(setq skk-japanenese-message-and-error t)

(setq skk-show-annotation t)

;    decide translation when we type not only [C-j] but also [Enter]
(setq skk-egg-like-newline t)

;;    codepage of user dictionaries (default euc-jp)
; (setq skk-jisyo-code "jis")

;; ;;    turn JIS mode off, when minibuffer
;; (add-hook 'minibuffer-setup-hook
;;   '(lambda ()
;;      (when (boundp 'skk-latin-mode-on)
;;        (skk-latin-mode-on))))

(add-hook 'skk-mode-hook
          (lambda ()
            (require 'evil)
            (evil-define-key 'insert skk-j-mode-map " " 'skk-insert)
            (evil-define-key 'insert skk-jisx0208-latin-mode-map " " 'skk-insert)))


  ;;; Mac Environment
(when (eq system-type 'darwin)
  (global-set-key "\C-x\C-j" 'skk-mode)
  (global-set-key "\C-xj" 'skk-auto-fill-mode)

  (setq skk-user-directory "~/.emacs.d/ddskk")
  (setq skk-server-host "localhost")
  (setq skk-server-portnum 1178)

  (setq skk-jisyo-code 'utf-8-unix)
  )

  ;;; other
(when (not (eq system-type 'darwin))
                                        ;    remove keybind of "l" in hiragana or katakana mode.
  (add-hook 'skk-mode-hook
            '(lambda ()
               (setq skk-rom-kana-base-rule-list
                     (remove '("l" nil skk-latin-mode)
                             skk-rom-kana-base-rule-list))
               (define-key skk-j-mode-map "l" 
                 '(lambda (arg) (interactive "P") (skk-mode-exit)))))

                                        ; ;    set "l" to turn off skk-mode
                                        ; (setq skk-rom-kana-rule-list
                                        ;       (append skk-rom-kana-rule-list
                                        ;       '(("l" nil '(lambda (arg) (skk-mode-exit) nil)))))

                                        ;    path of dictionaries
  (setq skk-jisyo        (expand-file-name "~/etc/skk/user.dic"))
  (setq skk-backup-jisyo (expand-file-name "~/etc/skk/user.dic.BAK"))
  (setq skk-large-jisyo  (expand-file-name "~/etc/skk/public/SKK-JISYO.L"))
  (setq skk-record-file  (expand-file-name "~/etc/skk/skk-record"))
  (setq skk-tut-file     (expand-file-name "~/etc/skk/SKK.tut"))

                                        ;    order of searching dictionaries
  (setq skk-search-prog-list
        `((skk-search-kakutei-jisyo-file skk-kakutei-jisyo 10000 t)
          (skk-search-jisyo-file skk-initial-search-jisyo 10000 t)
          (skk-search-jisyo-file skk-jisyo 0 t)
          (skk-search-small-dic)
          (skk-okuri-search)

          (skk-search-jisyo-file '("~/etc/skk/kaomoji.dic" . euc-jp) 10000 t)

                                        ;    public dictionaries
          (skk-search-jisyo-file skk-large-jisyo 10000)
          (skk-search-jisyo-file '("~/etc/skk/public/SKK-JISYO.fullname"   . euc-jp) 10000 t)
          (skk-search-jisyo-file '("~/etc/skk/public/SKK-JISYO.geo"        . euc-jp) 10000 t)
          (skk-search-jisyo-file '("~/etc/skk/public/SKK-JISYO.jinmei"     . euc-jp) 10000 t)
          (skk-search-jisyo-file '("~/etc/skk/public/SKK-JISYO.propernoun" . euc-jp) 10000 t)
          (skk-search-jisyo-file '("~/etc/skk/public/SKK-JISYO.station"    . euc-jp) 10000 t)
          (skk-search-jisyo-file '("~/etc/skk/public/SKK-JISYO.2ch"        . euc-jp) 10000 t)

                                        ;   (skk-search-server skk-aux-large-jisyo 10000)
          ))

  ;; [uim.el + skk]
                                        ;    display candidates of translation in inline
  (setq uim-candidate-display-inline t)
  )


