
;;-----------------------------------------------------------------------
;; text-mode
;;-----------------------------------------------------------------------

;; tab-width 2
(add-hook 'text-mode-hook
          (lambda ()
            (setq tab-width 2)))


;;-----------------------------------------------------------------------
;; cc-mode
;;-----------------------------------------------------------------------

;;;; オリジナルのローカルキーマップを作成し、\M-\C-a, \M-C-eにC言語用のバインドを行う
;; (defvar my-c-mode-map nil)
;; (setq my-c-mode-map (make-sparse-keymap))
;; (define-key my-c-mode-map "\M-\C-a" 'c-beginning-of-defun)
;; (define-key my-c-mode-map "\M-\C-e" 'c-end-of-defun)


;;;; substatement-openのインデントを0に
;;;; (例)
;;;; for (i = 0; i < 10; i++)
;;;; {
;;;;     printf("test");
;;;; }
;; (add-hook `c-mode-common-hook
;;           `(lambda ()
;;              (c-set-offset `substatement-open 0)))


(defun my-c-mode-common-hook ()
  (setq indent-tabs-mode nil)
  ;; C Style: CC-MODE
  (c-set-style "CC-MODE")
  ;; specify compile window height
  (setq compilation-window-height 8)
  (make-local-variable 'compilation-window-height))
;;  ;; 関数移動のキー割り当て
;;  (use-local-map my-c-mode-map))

(add-hook `c-mode-common-hook `my-c-mode-common-hook)

;; オートステートを有効に
; (setq c-auto-newline t)

;; 一斉消去機能(hungry-mode)を有効に
; (setq c-hungry-delete-key t)


;; jump to a function
(require `imenu)
(defcustom imenu-modes
  `(emacs-lisp-mode c-mode c++-mode makefile-mode)
  "List of major modes for which Imenu mode should be used."
  :group `imenu
  :type `(choice (const :tag "All modes" t)
                 (repeat (symbol :tag "Major mode"))))
(defun my-imenu-ff-hook ()
  "File find hook for Imenu mode."
  (if (member major-mode imenu-modes)
      (imenu-add-to-menubar "imenu")))
(add-hook `find-file-hooks `my-imenu-ff-hook t)

;; format
;; (defun my-c-mode-common-hook ()
;;    (c-set-style "linux") (setq indent-tabs-mode t) ;linux 式がいいとき
;;       /usr/src/linux/Documentation/CodingStyle 参照
;;    (c-set-style "k&r") ;k&r式がいいときはこれを有効にする
;;    (c-set-style "gnu") ;デフォルトの設定
;;  )
;; (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)


;;-----------------------------------------------------------------------
;; html-helper-mode
;;-----------------------------------------------------------------------

(setq html-helper-build-new-buffer t)
;; auto-indent off
;(setq html-helper-never-indent t)
(setq html-helper-basic-offset 4)


;;-----------------------------------------------------------------------
;; python-mode
;;-----------------------------------------------------------------------

;; indentはスペースのみで行なう
(add-hook `python-mode-hook
         (function
           (lambda ()
             (setq indent-tabs-mode nil))))


;;-----------------------------------------------------------------------
;; scheme
;;-----------------------------------------------------------------------

(setq scheme-program-name "gosh")
(require 'cmuscheme)

(defun scheme-other-window ()
  "Run scheme on other window"
  (interactive)
  (switch-to-buffer-other-window
   (get-buffer-create "*scheme*"))
  (run-scheme scheme-program-name))

(define-key global-map
  "\C-cS" 'scheme-other-window)


;;-----------------------------------------------------------------------
;; cperl-mode
;;-----------------------------------------------------------------------

;; 標準をcperl-modeに
;(add-to-list `load-path "/usr/loca/share/emacs/site-lisp")
;(autoload `perl-mode "cperl-mode" "alternate mode for editing Perl programs." t)
(setq cperl-indent-level 4)
;; ファイルの関連付け
(setq auto-mode-alist (append (list (cons "\\.pl$" `cperl-mode)
                    (cons "\\.cgi$" `cperl-mode))
                  auto-mode-alist))


;;-----------------------------------------------------------------------
;; Javascript-mode
;;-----------------------------------------------------------------------

(add-to-list 'auto-mode-alist
               (cons "\\.\\(js\\|as\\|json\\|jsn\\)\\'" 'javascript-mode))
(autoload 'javascript-mode "javascript" nil t)
(setq js-indent-level 4)


;;-----------------------------------------------------------------------
;; AutoHotKey Script
;;-----------------------------------------------------------------------

(setq ahk-syntax-directory "~/application/ahk/Extras/Editors/Syntax")
(add-to-list 'auto-mode-alist '("\\.ahk$" . ahk-mode))
(autoload 'ahk-mode "ahk-mode")


;;-----------------------------------------------------------------------
;; css-mode
;;-----------------------------------------------------------------------

(autoload 'css-mode "css-mode" nil t)
(setq auto-mode-alist (cons '("\\.css$" . css-mode) auto-mode-alist))

;; style
(setq cssm-indent-function #'cssm-c-style-indenter)
(setq cssm-indent-level 4)


;;-----------------------------------------------------------------------
;;; ECB mode
;;-----------------------------------------------------------------------

(custom-set-variables
 '(ecb-options-version "2.21")
 '(ecb-source-path (quote ("/mnt/data/program/c/unix/gtkmm/label1"))))
(custom-set-faces
  ;; custom-set-faces was added by Custom -- don't edit or cut/paste it!
  ;; Your init file should contain only one such instance.
 )

;; (add-hook `ecb-activate-hook
;;   `ecb-rebuild-methods-buffer)

(setq ecb-tip-of-the-day nil) ;; tip of the day を表示しない
(setq ecb-windows-width 0.25)
(setq ecb-layout-name "mylayout2") ;; mylayout
;; C-oで移動できるwindow
(setq ecb-other-window-jump-behavior 'edit-and-compile)
;; (global-set-key [f3] `ecb-goto-window-methods)
;; (global-set-key [f4] `ecb-goto-window-edit-last)

;; F9でECBモードのOnOff
(defun ecb-toggle ()
  (interactive)
  (setq ecb-toggle-flag nil)
  ;; ecb-minor-mode変数が存在しないか、nilならモードをactivate
  (if (boundp `ecb-minor-mode)
      (if ecb-minor-mode
          (setq ecb-toggle-flag t)))
  (if ecb-toggle-flag
      (ecb-deactivate)
    (ecb-activate)))
(global-set-key [f9] `ecb-toggle)