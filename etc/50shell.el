(add-hook 'shell-mode-hook
          (lambda ()
            (when (featurep 'evil)
              (evil-define-key 'insert shell-mode-map  (kbd "C-p") 'comint-previous-input)
              (evil-define-key 'insert shell-mode-map  (kbd "C-n") 'comint-next-input))))