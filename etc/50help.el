(add-hook 'help-mode-hook
          (lambda ()
            (evil-declare-key 'motion help-mode-map
              "J" 'scroll-up
              "K" 'scroll-down)))