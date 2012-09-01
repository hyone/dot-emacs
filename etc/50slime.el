;; (require 'slime)


;; hack to work with clojure and common-lisp
(defun slime-cl ()
  (interactive)
  (fmakunbound 'slime)
  (load (expand-file-name "~/.quicklisp/slime-helper.el"))
  (setq slime-net-coding-system 'utf-8-unix)
  (set-language-environment "UTF-8")
  (setq inferior-lisp-program "ccl")
  ;; Fix color scheme
  (add-hook 'slime-repl-mode-hook
            (lambda ()
              (set-face-foreground 'slime-repl-inputed-output-face nil)))

  (slime))


(defun slime-repl-backward-kill-line ()
  "kill backward the current line until beginning of the line or prompt"
  (interactive)
  (let ((cur-pos (point))
         (cur-bs  (progn (beginning-of-line) (point))))
    (goto-char cur-pos)
    (cond
      ((= cur-pos cur-bs) (backward-delete-char 1))
      (t (progn
           (slime-repl-bol)
           (delete-region (point) cur-pos))))))


;; (setq slime-protocol-version 'ignore)


(require 'evil)

(add-hook 'slime-repl-mode-hook
          (lambda ()
            (message "run slime-repl-mode-hook.")
            (setq slime-net-coding-system 'utf-8-unix)

            (setq evil-shift-width 2)

            (define-key slime-repl-mode-map (kbd "{") 'paredit-open-curly)
            (define-key slime-repl-mode-map (kbd "}") 'paredit-close-curly)

            (define-key slime-repl-mode-map (kbd "C-c q") 'slime-repl-quit)
            ;;    clear the output inserted since the last input.
            (define-key slime-repl-mode-map (kbd "C-c c") 'slime-repl-clear-output)
            (define-key slime-repl-mode-map (kbd "C-c k") 'slime-repl-clear-buffer)

            ;;    Disable default keybinds of slime-repl-next/previous-matching-input
            ;;    to avoid to corrupt paredit keybinds
            (define-key slime-repl-mode-map (kbd "M-r") nil)
            (define-key slime-repl-mode-map (kbd "M-s") nil)

            ;;    disable to insert automatically correspond character
            (evil-define-key 'insert slime-repl-mode-map "'" 'self-insert-command)
            (evil-key-combo-define 'insert slime-repl-mode-map "'" 'key-combo-execute-orignal)

            (evil-define-key 'insert slime-repl-mode-map (kbd "M-j") 'paredit-newline)
            (evil-define-key 'insert slime-repl-mode-map (kbd "C-p") 'slime-repl-backward-input)
            (evil-define-key 'insert slime-repl-mode-map (kbd "C-n") 'slime-repl-forward-input)
            (evil-define-key 'insert slime-repl-mode-map (kbd "C-M-r") 'slime-repl-previous-matching-input)
            (evil-define-key 'insert slime-repl-mode-map (kbd "C-M-s") 'slime-repl-next-matching-input)

            (evil-define-key 'insert slime-repl-mode-map (kbd "C-a") 'slime-repl-bol)
            ;;     move to the begging of prompt even if multiLine
            (evil-define-key 'insert slime-repl-mode-map (kbd "C-c a") 'beginning-of-defun)
            (evil-define-key 'insert slime-repl-mode-map (kbd "C-u") 'slime-repl-backward-kill-line)
            (evil-define-key 'insert slime-repl-mode-map (kbd "C-c u") 'slime-repl-kill-input)

            (if (functionp 'paredit-newline)
                (evil-define-key 'insert slime-repl-mode-map (kbd "<M-return>") 'paredit-newline)
              (evil-define-key 'insert slime-repl-mode-map (kbd "<M-return>") 'newline-and-indent))

            ;;    evaluate the current input string after closing all open lists.
            (evil-define-key 'insert slime-repl-mode-map (kbd "<S-return>") 'slime-repl-closing-return)
            ))

;; avoid the problem slime mode
(add-hook 'slime-repl-mode-hook
               (lambda ()
                 (clojure-mode-font-lock-setup)
                 (font-lock-mode 0)
                 (font-lock-mode 1)))



;;-----------------------------------------------------------------------
;; ac-slime.el
;;-----------------------------------------------------------------------

(require 'ac-slime)

(setq ac-modes (append ac-modes '(slime-repl-mode)))

;;    make the enter key work fine with auto-complete,
;;    so, hitting the enter key decides the completion.
(define-key ac-menu-map [return] 'ac-complete)
(define-key ac-complete-mode-map [return] 'ac-complete)

(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)


;;-----------------------------------------------------------------------
;; popwin.el
;;-----------------------------------------------------------------------

(when (require 'popwin nil t)
  ;; Apropos
  (push '("*slime-apropos*") popwin:special-display-config)
  ;; Macroexpand
  (push '("*slime-macroexpansion*") popwin:special-display-config)
  ;; Help
  (push '("*slime-description*") popwin:special-display-config)
  ;; Compilation
  (push '("*slime-compilation*" :noselect t) popwin:special-display-config)
  ;; Cross-reference
  (push '("*slime-xref*") popwin:special-display-config)
  ;; Debugger
  (push '(sldb-mode :stick t) popwin:special-display-config)
  ;; ;; REPL
  ;; (push '(slime-repl-mode :stick t) popwin:special-display-config)
  ;; Connections
  (push '(slime-connection-list-mode) popwin:special-display-config))
