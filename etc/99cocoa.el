
(when (eq window-system 'ns)
  ;; font settings
  ;; ----------------------------------------------------------------------------
  ;; font name can be gotten by fc-list command
  (create-fontset-from-ascii-font "Menlo-14:weight=normal:slant=normal" nil "menlokakugo")
  (set-fontset-font "fontset-menlokakugo"
    'unicode
    (font-spec :family "Hiragino Kaku Gothic ProN" :size 16)
    nil
    'append)
  (add-to-list 'default-frame-alist '(font . "fontset-menlokakugo"))

  ;; keybinds
  ;; ----------------------------------------------------------------------------

  ; use command key as Meta
  (setq ns-command-modifier 'meta)
  ; use option key as Super
  (setq ns-alternate-modifier 'super)
  ; use function key as Hyper
  (setq ns-function-modifier 'hyper)

  ;; etcetera
  ;; ----------------------------------------------------------------------------

  ;; open a file when drop 'n drap it
  (global-set-key [ns-drag-file] 'ns-find-file)

  ;; ;; enable to use emacs with AquaSKK
  ;; (mac-input-method-mode t)

  (setq default-frame-alist
      (append '(
                 ;; (border-color . "black")
                 ;; (mouse-color . "white")
                 ;; (cursor-color . "black")
                 ;; (alpha . (95 90))
                 (alpha . 92))
        default-frame-alist)))


(when (eq system-type 'darwin)

  (setq browse-url-generic-program "open")

  ;; color theme
  ;; ----------------------------------------------------------------------------

  (load "darkneon")
  (color-theme-darkneon)

  ;; migemo.el
  ;; ----------------------------------------------------------------------------

  (require 'migemo)
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (load-library "migemo")
  (migemo-init))
