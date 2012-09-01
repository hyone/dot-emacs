(add-hook 'comint-mode-hook
          (lambda ()
            (define-key comint-mode-map (kbd "C-n") 'comint-next-input)
            (define-key comint-mode-map (kbd "C-p") 'comint-previous-input)
            (define-key comint-mode-map (kbd "C-c q") 'kill-buffer-and-window)))
