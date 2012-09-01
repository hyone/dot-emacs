(add-hook 'nxml-mode-hook
          (lambda ()
            (evil-define-key 'insert nxml-mode-map (kbd "M-TAB") 'nxml-complete)

            ;; completion close tag ( i.e. </...> )
            (setq nxml-slash-auto-complete-flag t)))

;; keybinds
;; C-c tab
;;   開始タグの途中で入力すると、タグを補完してくれる。