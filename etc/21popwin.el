(require 'popwin)

(setq display-buffer-function 'popwin:display-buffer)

(setq anything-samewindow nil)

;; default popwin window height
(setq popwin:popup-window-height 25)

(setq popwin:special-display-config
      (append '(("*anything*" :height 30)
                ("*anything file list*" :height 30)
                ("*anything find-file*" :height 30)
                (" *auto-async-byte-compile*" :height 10 :noselect t))
              popwin:special-display-config))