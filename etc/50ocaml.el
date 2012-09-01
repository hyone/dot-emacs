
;; ; ocaml-mode
;; (setq auto-mode-alist
;;       (cons '("\\.ml[iylp]?$" . caml-mode) auto-mode-alist))
;; (autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
;; (autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)

; tuareg-mode
(setq auto-mode-alist (cons '("\\.ml[iylp]?$" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'tuareg-run-ocaml "tuareg" "Startup a Caml toplevel" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)


(require 'evil)

(add-hook 'tuareg-mode-hook
          (lambda ()
            (setq evil-shift-width 2)

            (evil-define-key 'insert tuareg-mode-map "'" 'self-insert-command)
            (evil-define-key 'normal tuareg-mode-map (kbd "C-c C-l") 'tuareg-eval-buffer)
            (evil-define-key 'insert tuareg-mode-map (kbd "C-c C-l") 'tuareg-eval-buffer)))

(add-hook 'tuareg-interactive-mode-hook
          (lambda ()
            (setq evil-shift-width 2)

            (define-key tuareg-interactive-mode-map (kbd "C-j") nil)

            (evil-declare-key 'insert tuareg-interactive-mode-map
              "'"             'self-insert-command
              (kbd "C-p")     'comint-previous-input
              (kbd "C-n")     'comint-next-input
              (kbd "RET")     'tuareg-interactive-send-input-end-of-phrase
              (kbd "M-<RET>") 'tuareg-interactive-send-input-or-indent
              (kbd "C-c u")   'comint-kill-input)))
