(load (expand-file-name "site-lisp/nxhtml/autostart.el" hyone:emacs-home))
(require 'hyone-key-combo)


(setq popcmp-completion-style 'anything)


(defun setup-nxhtml-mode-key-combo (map)
  (evil-key-combo-define 'insert map (kbd "=") 'key-combo-execute-orignal)
  (evil-key-combo-define 'insert map (kbd "\"") "\"`!!'\""))

(add-hook 'nxhtml-mode-hook
          (lambda ()
            (setup-nxhtml-mode-key-combo nxhtml-mode-map)

            (set-face-background 'mumamo-background-chunk-major nil)
            (set-face-background 'mumamo-background-chunk-submode1 "grey5")
            (set-face-background 'mumamo-background-chunk-submode2 "#161a1e")))
