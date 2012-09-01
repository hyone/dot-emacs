(require 'iswitchb)

; (iswitchb-default-keybindings)
(iswitchb-mode 1)

;    display contents of the selected buffer.
(defadvice iswitchb-exhibit
  (after
   iswitchb-exhibit-with-display-buffer
   activate)
  "display the selecting buffer in window"
  (when (and
        (eq iswitchb-method iswitchb-default-method)
         iswitchb-matches)
    (select-window (get-buffer-window (cadr (buffer-list))))
    (iswitchb-visit-buffer (get-buffer (car iswitchb-matches)))
    (select-window (minibuffer-window))))

; customize keybinds
(add-hook 'iswitchb-define-mode-map-hook
  (lambda ()
    (define-key iswitchb-mode-map (kbd "C-n") nil)))
