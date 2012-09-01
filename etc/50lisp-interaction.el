(require 'evil)
(require 'hyone-key-combo)

(evil-define-key 'insert lisp-interaction-mode-map "'" 'self-insert-command)
(evil-define-key 'insert lisp-interaction-mode-map (kbd "C-j") 'eval-print-last-sexp)
(evil-define-key 'insert lisp-interaction-mode-map (kbd "<M-return>") 'eval-print-last-sexp)
;; (evil-define-key 'insert lisp-interaction-mode-map (kbd "M-j") 'eval-print-last-sexp)

(add-hook 'inferior-lisp-mode-hook
  '(lambda ()
     (hyone:set-tab-width 2 t nil)
     (setq evil-shift-width 2)
     (evil-key-combo-define 'insert inferior-lisp-mode-map "'" 'key-combo-execute-orignal)
     
     (evil-declare-key 'insert inferior-lisp-mode-map
       (kbd "C-n") 'comint-next-input
       (kbd "C-p") 'comint-previous-input
       "'"         'self-insert-command
       "]"         'self-insert-command)))
