; to enable "byte-comple-file js2.el"
(unless (boundp 'warning-suppress-types)
  (setq warning-suppress-types nil))

(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(add-hook 'js2-mode-hook
          (lambda ()
            ;; add auto-indent feature to js2 mode enter key
            (evil-define-key 'insert js2-mode-map (kbd "RET")
              (lambda () (interactive)
                (hyone:newline-dwim 'js2-enter-key)
                (indent-for-tab-command)))

            ;; using espresso.el for indentation
            (require 'espresso)
            (setq espresso-indent-level 2
                  espresso-expr-indent-offset 2
                  indent-tabs-mode nil)
            (set (make-local-variable 'indent-line-function) 'espresso-indent-line)
            ))

;; nxhtml override to a entry of auto-mode-alist that map .js files to js-mode.
;; And, nxhtml and js2-mode can't use at the same time in multi major mode
;; (i.e. embbeded javascript in <script></script> ).
;; so, make only *.js files use js2-mode
(add-hook 'js-mode-hook
          (lambda ()
            ;; only *.js file use js2-mode
            (let ((filename (buffer-file-name)))
              (if (and filename (string-match "\\.js$" filename))
                  (js2-mode)))))

;; flymake.el
;;-----------------------------------------------------------------------

(require 'flymake)
(require 'flymake-jslint)

(setq jslint-v8-shell "v8")

(add-hook 'js2-mode-hook
          (lambda () (flymake-mode t)))