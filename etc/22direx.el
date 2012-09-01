(require 'direx)

;; (setq direx:leaf-icon "  "
;;       direx:open-icon "▼ "
;;       direx:closed-icon "▶ ")

(when (require 'popwin nil t)
  (push '(direx:direx-mode :position left :width 25 :dedicated t)
        popwin:special-display-config))


;; make work well with evil.el
(eval-after-load 'direx
  '(progn
     (when (featurep 'evil)
       ;; use the standard direx bindings as a base
       (evil-make-overriding-map direx:direx-mode-map 'normal t)
       ;; adjust bindings
       (evil-define-key 'normal direx:direx-mode-map
         "j" 'direx:next-item
         "k" 'direx:previous-item
         ;; use delete-window instead of quit-window
         ;; to avoid the problem that 'quit-window' function cause to change buffer in main window
         "q" 'delete-window))))
