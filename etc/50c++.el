(add-hook 'c++-mode-hook
          (lambda ()
            (c-set-offset arglist-close 0)))