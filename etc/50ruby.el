
;; set rbenv path to both exec-path and PATH.
(add-to-list 'exec-path "~/.rbenv/shims/")
(setenv "PATH" (mapconcat 'identity exec-path ":"))

(setq auto-mode-alist (append auto-mode-alist
                              '(("\\.rake$" . ruby-mode)
                                ("\\.gemspec$" . ruby-mode)
                                ("\\.ru$" . ruby-mode)
                                ("Rakefile$" . ruby-mode)
                                ("Gemfile$" . ruby-mode)
                                ("Capfile$" . ruby-mode))))


;; ruby-mode.el
;;-----------------------------------------------------------------------

(add-hook 'ruby-mode-hook
  (lambda ()
     (modify-coding-system-alist 'file "\\.rb$" 'utf-8)
     (modify-coding-system-alist 'file "\\.rhtml$" 'utf-8)

     (setq evil-shift-width 2)

     ;; avoid to corrupt with yasnippet trigger key
     (define-key ruby-mode-map "\t" nil)

     ; Fix some keys
     (when (featurep 'evil)
       (evil-define-key 'normal ruby-mode-map (kbd "C-c ]") 'rsense-jump-to-definition)
       (evil-define-key 'insert ruby-mode-map (kbd "RET") 'ruby-reindent-then-newline-and-indent)
       ; C-c C-l    ruby-load-file
       ; C-c C-z    switch-to-ruby
       ; C-c C-r    ruby-send-region
       (evil-define-key 'normal ruby-mode-map (kbd "C-c C-g") 'ruby-send-region-and-go))))


;; inf-ruby.el
;;-----------------------------------------------------------------------

(require 'inf-ruby)

(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set ansi-color-for-cominit-mode-on to t.")

(if (executable-find "pry")
    (progn
      ;; using pry instead of irb
      (setq ruby-program-name "pry --no-pager")
      (setq inferior-ruby-first-prompt-pattern "^\\[[0-9]+\\] pry *[a-zA-Z0-9\\.\\-]* *\\((.*)\\)> *")
      (setq inferior-ruby-prompt-pattern "^\\[[0-9]+\\] pry *[a-zA-Z0-9\\.\\-]* *\\((.*)\\)[>*\"'] *"))
  (progn
    ;; Adjust setting to the customized irb environment
    (setq ruby-program-name "irb -U --noreadline")
    ;; (setq inferior-ruby-first-prompt-pattern ">>")
    ;; (setq inferior-ruby-first-prompt-pattern "irb>")
    ;; (setq inferior-ruby-prompt-pattern       "irb>\\|\\.\\.\\."))
    ))

(add-hook 'inferior-ruby-mode-hook
          (lambda ()
            (setq evil-shift-width 2)

            (define-key inferior-ruby-mode-map (kbd "C-M-l") nil)
            (define-key inferior-ruby-mode-map (kbd "C-n") 'comint-next-input)
            (define-key inferior-ruby-mode-map (kbd "C-p") 'comint-previous-input)
            (define-key inferior-ruby-mode-map (kbd "C-c q") 'kill-buffer-and-window)

            ; enable escape sequence notation in inf-ruby
            (ansi-color-for-comint-mode-on)))

(add-hook 'ruby-mode-hook
          (lambda ()
            (inf-ruby-keys)))

(when (require 'popwin nil t)
  (push '("*ruby*" :stick t) popwin:special-display-config))


;; ruby-electric.el
;;-----------------------------------------------------------------------

(require 'ruby-electric)

(add-hook 'ruby-mode-hook
          (lambda () (ruby-electric-mode)))


;; rspec-mode.el
;;-----------------------------------------------------------------------

(require 'rspec-mode)

;; use 'rspec' command instead of 'rake spec'
(setq rspec-spec-command "rspec")
(setq rspec-use-rake-flag nil)


;; rsense.el
;;-----------------------------------------------------------------------

(setq rsense-home (expand-file-name "~/local/apps/rsense"))
(add-to-list 'load-path (concat rsense-home "/etc"))

(when (require 'rsense nil t)
  (let ((add-ac-sources
          (lambda ()
            ; add rsense sources
            (add-to-list 'ac-sources 'ac-source-rsense-method)
            (add-to-list 'ac-sources 'ac-source-rsense-constant))))
  (add-hook 'ruby-mode-hook add-ac-sources)
  (add-hook 'inferior-ruby-mode-hook add-ac-sources)))


;; yari.el
;;-----------------------------------------------------------------------

(require 'yari)

(add-hook 'ruby-mode-hook
          (lambda ()
            (define-key ruby-mode-map (kbd "C-c h") 'yari-anything)))


;; rvm.el
;;-----------------------------------------------------------------------
;; (when (require 'rvm nil t)
;;   (rvm-use-default))


;; flymake.el
;;-----------------------------------------------------------------------

(defvar flymake-ruby-err-line-patterns '(("^\\(.*\\):\\([0-9]+\\): \\(.*\\)$" 1 2 nil 3)))
(defvar flymake-ruby-allowed-file-name-masks '((".+\\.\\(rb\\|rake\\)$" flymake-ruby-init)
                                               ("Rakefile$" flymake-ruby-init)))

;; Not provided by flymake itself, curiously
(defun flymake-create-temp-in-system-tempdir (filename prefix)
  (make-temp-file (or prefix "flymake-ruby")))

;; Invoke ruby with '-c' to get syntax checking
(defun flymake-ruby-init ()
  (list "ruby" (list "-c" (flymake-init-create-temp-buffer-copy
                           'flymake-create-temp-in-system-tempdir))))

(defun flymake-ruby-load ()
  (interactive)
  (set (make-local-variable 'flymake-allowed-file-name-masks) flymake-ruby-allowed-file-name-masks)
  (set (make-local-variable 'flymake-err-line-patterns) flymake-ruby-err-line-patterns)
  (flymake-mode t))


(add-hook 'ruby-mode-hook 'flymake-ruby-load)