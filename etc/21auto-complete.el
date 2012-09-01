(require 'auto-complete)
(require 'auto-complete-config)

(ac-config-default)

;    turn on by default
(global-auto-complete-mode t)

;    add modes below to ac-modes in order to enable ac-mode in.
(setq ac-modes
  (append ac-modes '(rst-mode
                     coffee-mode
                     clojure-mode
                     clojurescript-mode
                     comint-mode
                     groovy-mode
                     html-mode
                     nxml-mode
                     nxhtml-mode
                     malabar-mode
                     haskell-mode
                     shell-mode
                     sql-mode
                     inferior-emacs-lisp-mode
                     inferior-haskell-mode
                     inferior-lisp-mode
                     inferior-ruby-mode
                     inferior-scheme-mode
                     tuareg-interactive-mode
                     ensime-inf-mode
                     scala-mode-inf)))

;;   dictionary files directory
(add-to-list 'ac-dictionary-directories
             (expand-file-name "ac-dict" hyone:emacs-home))


;;  at least typing 3 chars, don't pop up completion candidates.
(setq ac-auto-start 2)

;;  delay show popup menu
;; (setq ac-auto-show-menu 1.2)
(setq ac-auto-show-menu nil)

;; ;    case sensitive if typings have capitals
;; (setq ac-ignore-case 'smart)
(setq ac-ignore-case nil)

;    default completion sources
(set-default 'ac-sources '(ac-source-dictionary
                           ;; ac-source-yasnippet
                           ac-source-words-in-buffer
                           ac-source-words-in-same-mode-buffers
                           ac-source-filename))

(setq ac-dwim t)

; keybinds
(setq ac-use-menu-map t)

(add-hook 'auto-complete-mode-hook
  (lambda ()
    (mapcar (lambda (m)
      (define-key (eval m) (kbd "C-y") 'ac-expand)
      (define-key (eval m) (kbd "\t") 'ac-expand)
      (define-key (eval m) (kbd "C-o") 'ac-complete)
      ;    avoid return key to decide completion.
      (define-key (eval m) "\r" nil)
      (define-key (eval m) (kbd "<return>") nil)
      ;    to avoid corruption of global ( or other minor modes ) key mappings.
      ;; (define-key (eval m) (kbd "TAB") 'ac-complete)
      (define-key (eval m) (kbd "TAB") nil)
      (define-key (eval m) (kbd "S-<tab>") nil)
      (define-key (eval m) (kbd "C-M-n") nil)
      (define-key (eval m) (kbd "C-M-p") nil)
      (define-key (eval m) (kbd "M-n") nil)
      (define-key (eval m) (kbd "M-p") nil)
      )
      '(ac-menu-map ac-complete-mode-map))
    ))


;; a command to add region string to the file of current major mode ac-dictionary-source file
;; From http://d.hatena.ne.jp/kitokitoki/20100627/p1

(defvar auto-complete-dict-path (car ac-dictionary-directories))
(defun append-region-to-auto-complete-dict ()
  (interactive)
  (when (and transient-mark-mode mark-active)
    (let ((path (expand-file-name (prin1-to-string major-mode) auto-complete-dict-path))
          (str (concat "\n" (buffer-substring-no-properties (region-beginning) (region-end)))))
      (with-temp-buffer
        (insert str)
        (append-to-file (point-min) (point-max) path)))))
