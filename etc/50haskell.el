(require 'hyone-key-combo)

;; Add to the path to commands installed by cabal
(when (eq system-type 'darwin)
  (add-to-list 'exec-path (concat (getenv "HOME") "/Library/Haskell/bin")))

(load "haskell-site-file")
(autoload 'ghc-init "ghc" nil t)

(add-to-list 'auto-mode-alist '("\\.hs-boot$" . haskell-mode))

;; (setq haskell-program-name "ghci -XViewPatterns -XGADTs")
(setq haskell-program-name "ghci")

;; Using hlint as flymake check command
(setq ghc-flymake-command t)

(setq haskell-interactive-mode-eval-mode t)


(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'font-lock-mode)
(add-hook 'haskell-mode-hook 'imenu-add-menubar-index)

(defun setup-haskell-mode-key-combo (map)
  (evil-key-combo-define 'insert map (kbd "`")  " ``!!'` ")
  (evil-key-combo-define 'insert map (kbd "'")  '("'" "'`!!''"))
  (evil-key-combo-define 'insert map (kbd "-")  'key-combo-execute-orignal)
  (evil-key-combo-define 'insert map (kbd "->") " -> ")
  (evil-key-combo-define 'insert map (kbd "=<") "=<")
  (evil-key-combo-define 'insert map (kbd "=<<") " =<< ")
  (evil-key-combo-define 'insert map (kbd "<=<") " <=< ")
  (evil-key-combo-define 'insert map (kbd "<*") " <* ")
  (evil-key-combo-define 'insert map (kbd "|") '(" | " " || " " <|> " " ||| "))
  (evil-key-combo-define 'insert map (kbd "&") '("&" " && " " &&& "))
  (evil-key-combo-define 'insert map (kbd "+") '(" + " " ++ " " +++ "))
  (evil-key-combo-define 'insert map (kbd ">>=") " >>= ")
  (evil-key-combo-define 'insert map (kbd "/")  'key-combo-execute-orignal)
  (evil-key-combo-define 'insert map (kbd "/=")  " /= ")
  (evil-key-combo-define 'insert map (kbd ":")  '(" : " " :: "))
  (evil-key-combo-define 'insert map (kbd "*")  '(" * " " <*> " " *** "))
  (evil-key-combo-define 'insert map (kbd "*>")  " *> ")
  (evil-key-combo-define 'insert map (kbd "$")  '(" $ " " <$> "))
  (evil-key-combo-define 'insert map (kbd "!")  "!")
  (evil-key-combo-define 'insert map (kbd "!!")  " !! ")
  )

(add-hook 'haskell-mode-hook
  (lambda ()
    (when (featurep 'evil)
      (hyone:set-tab-width 2 t nil)
      (setq evil-shift-width 2)

      ;; C-c C-i: Displays the info of this expression in another window.
      ;; C-c C-t: Displays the type of this expression in the minibuffer.
      ;;          Type C-c C-t multiple time to enlarge the expression.
      (evil-define-key 'normal haskell-mode-map
        (kbd "C-c h") 'anything-ghc-browse-document
        (kbd "C-c s") 'ghc-flymake-display-errors)

      (evil-declare-key 'insert haskell-mode-map
        "'"             'self-insert-command
        "`"             '(lambda () (interactive) (insert-pairs "`" "`"))
        (kbd "RET")     'newline
        (kbd "M-<RET>") 'newline-and-indent
        (kbd "C-c /") 'ghc-help-key)

      (setup-haskell-mode-key-combo haskell-mode-map)

      (ghc-init)
      (flymake-mode))))

(add-hook 'inferior-haskell-mode-hook
  (lambda ()
    (when (featurep 'evil)
      (hyone:set-tab-width 2 t nil)
      (setq evil-shift-width 2)

      (evil-declare-key 'insert inferior-haskell-mode-map
        "'"         'self-insert-command
        "`"         '(lambda () (interactive) (insert-pairs "`" "`"))
        (kbd "C-n") 'comint-next-input
        (kbd "C-p") 'comint-previous-input)

      (setup-haskell-mode-key-combo inferior-haskell-mode-map)
      ;; (evil-key-combo-define 'insert inferior-haskell-mode-map (kbd ":") '(":" " : " " :: "))
      )))


;;-----------------------------------------------------------------------
;; Auto Complete
;;-----------------------------------------------------------------------

(ac-define-source ghc-mod
  '((depends ghc)
    (candidates . (ghc-select-completion-symbol))
    (symbol . "s")
    (cache)))

(add-hook 'haskell-mode-hook
          (lambda ()
            (add-to-list 'ac-sources 'ac-source-ghc-mod)))


;;-----------------------------------------------------------------------
;; Anything
;;-----------------------------------------------------------------------

(require 'anything)
(require 'anything-config)
(require 'anything-match-plugin)

(defvar anything-c-source-ghc-mod
  '((name . "ghc-browse-document")
    (init . anything-c-source-ghc-mod)
    (candidates-in-buffer)
    (candidate-number-limit . 9999999)
    (action ("Open" . anything-c-source-ghc-mod-action))))

(defun anything-c-source-ghc-mod ()
  (unless (executable-find "ghc-mod")
    (error "ghc-mod を利用できません。ターミナルで which したり、*scratch* で exec-path を確認したりしましょう"))
  (let ((buffer (anything-candidate-buffer 'global)))
    (with-current-buffer buffer
      (call-process "ghc-mod" nil t t "list"))))

(defun anything-c-source-ghc-mod-action (candidate)
  (interactive "P")
  (let* ((pkg (ghc-resolve-package-name candidate)))
    (anything-aif (and pkg candidate)
        (ghc-display-document pkg it nil)
      (message "No document found"))))

(defun anything-ghc-browse-document ()
  (interactive)
  (anything anything-c-source-ghc-mod))


;;-----------------------------------------------------------------------
;; popwin.el
;;-----------------------------------------------------------------------

(when (require 'popwin nil t)
  (push '("*GHC Info*") popwin:special-display-config))
