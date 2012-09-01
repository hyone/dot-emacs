;;-----------------------------------------------------------------------
;; mode-compile
;;-----------------------------------------------------------------------

(autoload 'mode-compile "mode-compile"
  "Command to compile current buffer file based on the major mode" t)

(autoload 'mode-compile-kill "mode-compile"
  "Command to kill a compilation launched by `mode-compile'" t)

;    not to ask
(setq mode-compile-always-save-buffer-p t)
(setq mode-compile-never-edit-command-p t)
(setq mode-compile-expert-p t)
(setq mode-compile-reading-time 0)

;    size of complile window
(setq compilation-window-height 25)