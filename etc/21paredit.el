(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Clojure code." t)

;; (require 'paredit)


(mapcar (lambda (x)
          (add-hook x (lambda () (paredit-mode +1))))
  '(clojure-mode-hook
    slime-repl-mode-hook
    scheme-mode-hook
    inferior-scheme-mode-hook
    inferior-lisp-mode-hook
    ielm-mode-hook
    emacs-lisp-mode-hook
    lisp-interaction-mode-hook))


(add-hook 'paredit-mode-hook
          (lambda ()
            (when (featurep 'evil)

              ;; To make available motions by per parentheses with evil.el
              (mapcar
                (lambda (state)
                  (evil-define-key state paredit-mode-map (kbd "M-p") 'paredit-forward-up)
                  (evil-define-key state paredit-mode-map (kbd "M-n") 'paredit-backward-down)
                  (evil-define-key state paredit-mode-map (kbd "M-u") 'paredit-backward-up)
                  (evil-define-key state paredit-mode-map (kbd "M-h") 'paredit-forward-down))
                (list 'normal 'insert 'visual 'motion))

              (evil-define-key 'insert paredit-mode-map (kbd "C-h") 'paredit-backward-delete)
              (evil-define-key 'insert paredit-mode-map (kbd "C-w") 'paredit-backward-kill-word)
              (evil-define-key 'insert paredit-mode-map (kbd "C-k") 'paredit-kill)
              (evil-define-key 'insert paredit-mode-map (kbd "M-s") 'paredit-splice-sexp)
              (evil-define-key 'insert paredit-mode-map (kbd "M-r") 'paredit-raise-sexp)
              (evil-define-key 'insert paredit-mode-map (kbd "<S-return>") 'paredit-newline)
              (evil-define-key 'insert paredit-mode-map (kbd "C-c n") 'backward-kill-sexp)
              (evil-define-key 'insert paredit-mode-map (kbd "C-c d") 'kill-sexp)
              (evil-define-key 'insert paredit-mode-map (kbd "C-c j") 'paredit-join-sexps)
              (evil-define-key 'insert paredit-mode-map (kbd "C-c p") 'backward-kill-sexp)
              (evil-define-key 'insert paredit-mode-map (kbd "C-c l") 'paredit-splice-sexp-killing-forward)
              (evil-define-key 'insert paredit-mode-map (kbd "C-c h") 'paredit-splice-sexp-killing-backward)

              ;; remap original behaviors of C-d, C-h
              (evil-define-key 'insert paredit-mode-map (kbd "C-c C-d") 'delete-char)
              (evil-define-key 'insert paredit-mode-map (kbd "C-c C-h") 'backward-delete-char)

              ;; rebind
              (evil-define-key 'insert paredit-mode-map "[" 'paredit-open-square)
              (evil-define-key 'insert paredit-mode-map "]" 'paredit-close-square)
              (evil-define-key 'insert paredit-mode-map "\"" 'paredit-doublequote))))