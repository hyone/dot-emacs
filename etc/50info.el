(require 'anything-startup)
(require 'hyone-elscreen)
(require 'evil)

(add-hook 'Info-mode-hook
          (lambda ()
            (evil-declare-key 'motion Info-mode-map
              ;; defualt bindings
              ;; "d"      go to top level
              ;; "I"      'Info-index
              ;; "m"      'Info-menu
              ;; "u"      'Info-up
              ;; "["      'Info-backward-node
              ;; "]"      'Info-forward-node

              ;; fix elscreen prefix key
              (kbd "C-t") nil

              "b"         'evil-backward-word-begin
              "n"         'evil-search-next
              "N"         'evil-search-previous
              "w"         'evil-forward-word-begin
              "i"         'Info-history-back
              "o"         'Info-history-forward
              "q"         'kill-this-buffer
              "gg"        'evil-goto-first-line
              "G"         'evil-goto-line
              "H"         'hyone:elscreen-cycle-previous
              "L"         'hyone:elscreen-cycle-next
              "J"         'scroll-up
              " "         'scroll-up
              "K"         'scroll-down
              "R"         'anything-execute-extended-command)

            (evil-declare-key 'normal Info-mode-map
              "M-b"       'Info-history-back
              "M-f"       'Info-history-forward
              (kbd "M-:") 'execute-extended-command)))