(setq shell-file-name "zsh")
(setq explicit-shell-file-name "zsh")
(setenv "SHELL" shell-file-name)

(setq shell-command-switch "-c")

(add-hook 'term-mode-hook
  '(lambda ()
     (define-key term-raw-map [return]   'term-send-raw)
     ;; (define-key term-raw-map (kbd "C-j") 'term-line-mode)
   ))
