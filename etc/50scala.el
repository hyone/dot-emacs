(require 'hyone-util)
(require 'hyone-key-combo)
(require 'scala-mode-auto)

(add-to-list 'auto-mode-alist '("\\.sbt" . scala-mode))


(setq scala-interpreter "scala -Dfile.encoding=UTF-8")
;; (setq scala-interpreter "scala -Xprint:typer")

(defvar my-scala-current-buffer nil)

(defun scala-run-scala-practice ()
  (interactive)
  (setq scala-interpreter-default scala-interpreter)
  (setq scala-interpreter (concat scala-interpreter " -cp " (expand-file-name "~/program/scala")))
  (scala-run-scala scala-interpreter)
  (setq scala-interpreter scala-interpreter-default))

(defun my-scala-switch-to-interpreter ()
  (interactive)
  (setq my-scala-current-buffer (current-buffer))
  (unless (scala-interpreter-running-p-1)
    (scala-run-scala scala-interpreter))
  (scala-switch-to-interpreter))

(defun my-scala-back-to-editing-buffer ()
  (interactive)
  (if my-scala-current-buffer
      (switch-to-buffer my-scala-current-buffer)))

(when (require 'scala-mode-feature-electric nil t)
  (add-hook 'scala-mode-hook
            (lambda ()
              (scala-electric-mode))))

(defun setup-scala-mode-key-combo (map)
  (evil-key-combo-define 'insert map (kbd ":")  '(": " " :: "))
  (evil-key-combo-define 'insert map (kbd "-")  'key-combo-execute-orignal)
  (evil-key-combo-define 'insert map (kbd "->") " -> ")
  (evil-key-combo-define 'insert map (kbd "<%") " <% ")
  (evil-key-combo-define 'insert map (kbd "*")  " * ")
  ;; (evil-key-combo-define 'insert map (kbd ":=") " := ")
  )

(add-hook 'scala-mode-hook
          '(lambda ()
             (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)
             (local-set-key (kbd "C-c C-z") 'my-scala-switch-to-interpreter)
             ; avoid to map C-tab to scala-undent-line.
             (local-set-key (kbd "<C-tab>") nil)
             (setup-scala-mode-key-combo scala-mode-map)))


(defun scala-inf-mode-setting (map)
  (define-key map (kbd "C-c C-z") 'my-scala-back-to-editing-buffer)
  (define-key map (kbd "<C-tab>") nil)
  (setup-scala-mode-key-combo map)

  (set-buffer-file-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8))

(add-hook 'scala-mode-inf-hook
          (lambda ()
            (scala-inf-mode-setting scala-mode-inf-map)))


(when (require 'popwin nil t)
  (push '("*inferior-scala*" :stick t) popwin:special-display-config))


;;-----------------------------------------------------------------------
;; Fix scala mode indentation
;;-----------------------------------------------------------------------

(defadvice scala-block-indentation (around improve-indentation-after-brace activate)
  (if (eq (char-before) ?\{)
      (setq ad-return-value (+ (current-indentation) scala-mode-indent:step))
    ad-do-it))

(defun scala-newline-and-indent ()
  (interactive)
  (delete-horizontal-space)
  (let ((last-command nil))
    (newline-and-indent))
  (when (scala-in-multi-line-comment-p)
    (insert "* ")))

(add-hook 'scala-mode-hook
          (lambda ()
            (define-key scala-mode-map (kbd "RET") 'scala-newline-and-indent)))


;;-----------------------------------------------------------------------
;; for android SDK
;;-----------------------------------------------------------------------

(if (file-directory-p "~/local/apps/android-sdk-mac_86")
    (setenv "ANDROID_SDK_HOME" (expand-file-name "~/local/apps/android-sdk-mac_86")))


;;-----------------------------------------------------------------------
;;  ensime
;;-----------------------------------------------------------------------

(require 'ensime)

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
;; (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

;; MINI HOWTO: 
;; Open .scala file. M-x ensime (once per project)

(add-hook 'ensime-inf-mode-hook
          (lambda ()
            (scala-inf-mode-setting ensime-inf-mode-map)))

;; Fix ac-sources when turn ensime-mode on
;; referred to http://d.hatena.ne.jp/pokutuna/20110526/1306394768
(defadvice ensime (after my-ensime-mode activate)
  (when ensime-mode
    (setq ac-sources
          (append ac-sources '(ac-source-dictionary
                               ac-source-yasnippet
                               ac-source-words-in-buffer
                               ac-source-words-in-same-mode-buffers
                               ac-source-filename)))))

(add-hook 'ensime-mode-hook
          (lambda ()
            ;; ensime mode
            (let ((prefix-map (lookup-key ensime-mode-map ensime-mode-key-prefix)))
              (if (keymapp prefix-map)
                  (define-key prefix-map (kbd "C-v b") 'ensime-builder-build)))

            ;; search mode
            (define-key ensime-search-mode-map (kbd "C-w") 'hyone:backward-kill-word-like-vim)
            (define-key ensime-search-mode-map (kbd "C-u") 'hyone:backward-kill-line)

            ;; inspector mode
            (when (featurep 'evil)
              ;; To ',' and '.' keys work properly on ensime inspector
              (evil-make-overriding-map ensime-popup-inspector-map 'normal t))))

(when (require 'popwin nil t)
  (push '("*ensime-inferior-scala*" :stick t) popwin:special-display-config)
  (push '("^\*ensime-sbt\*" :regexp t :stick t) popwin:special-display-config)
  (push '("*ENSIME-Compilation-Result*" :height 15) popwin:special-display-config))
