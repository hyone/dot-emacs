
;; set rbenv path to both exec-path and PATH.
(add-to-list 'exec-path "~/.lein/bin")
(setenv "PATH" (mapconcat 'identity exec-path ":"))


;;-----------------------------------------------------------------------
;; clojure-mode.el
;;-----------------------------------------------------------------------

(require 'clojure-mode)
(require 'clojurescript-mode)
(autoload 'clojure-test-mode "clojure-test-mode" "Major mode for editing clojure test")
(autoload 'clojure-test-maybe-enable "clojure-test-mode" "set automatically major-mode for clojure test")
(require 'hyone-util)
(require 'hyone-key-combo)

(add-to-list 'auto-mode-alist '("\\.cljs$" . clojurescript-mode))


(defun hyone:clojure-find-project-file (path)
  (if (or (null path) (equal "" path))
      (progn (message (format "Not found %s" clojure-project-root-file)) nil)
    (let* ((dir          (file-name-directory path))
           (project-file (expand-file-name clojure-project-root-file dir)))
      (cond
       ((not (file-exists-p dir))
        (progn (message (format "%s does not exist." dir)) nil))
       ((file-exists-p project-file)
        project-file)
       (t
        (hyone:clojure-find-project-file
         (file-name-directory (replace-regexp-in-string "/$" "" dir))))))))

(defun hyone:clojure-open-project-file (&optional path)
  "open project.clj file in the project that the path belongs.
if path is omitted, use the current buffer path."
  (interactive)
  (let* ((path         (or path (buffer-file-name)))
         (project-file (hyone:clojure-find-project-file path)))
    (if project-file
        (find-file project-file))))

(defun hyone:clojurescript-repl ()
  "run ClojureScript repl in the project."
  (interactive)
  (let ((project-file (hyone:clojure-find-project-file (buffer-file-name))))
    (when project-file
      (cd (file-name-directory project-file))
      ;; avoid to raise an error like "CLOJURESCRIPT_HOME not configured."
      (remove-hook 'inferior-lisp-mode-hook 'clojurescript-start-cljs-repl t)
      (run-lisp "lein trampoline cljsbuild repl-listen")
      (let ((buffer (get-buffer "*inferior-lisp*")))
        (if buffer (switch-to-buffer buffer))))))

(defun setup-clojure-mode-key-combo (map)
  (evil-key-combo-define 'insert map (kbd ";") '("; " " ;; "))
  (evil-key-combo-define 'insert map (kbd "[") 'key-combo-execute-orignal)
  (evil-key-combo-define 'insert map (kbd "{") 'key-combo-execute-orignal)
  )

; avoid to evaluate expressions when hit return to indend to add new line.
(add-hook 'clojure-mode-hook
  '(lambda ()
     (hyone:set-tab-width 2 t nil)
     (setq evil-shift-width 2)

     (define-key clojure-mode-map (kbd "C-c j") 'clojure-jack-in)
     (define-key clojure-mode-map (kbd "C-c p") 'hyone:clojure-open-project-file)

     (when (featurep 'evil)
       (evil-define-key 'normal clojure-mode-map ",al" 'align-cljlet)
       (evil-define-key 'normal clojure-mode-map ",am" 'align-map)

       (evil-key-combo-define 'insert clojure-mode-map "'" 'key-combo-execute-orignal)
       (evil-define-key 'insert clojure-mode-map "'" 'self-insert-command)
       (evil-define-key 'insert clojure-mode-map "]" 'self-insert-command))

     (setup-clojure-mode-key-combo clojure-mode-map)))


;; adjust indentation
;;---------------------------------

(put-clojure-indent 'reduce 'defun)
(put-clojure-indent 'swap! 1)
(put-clojure-indent 'add-watch 2)
(put-clojure-indent 'take-while 1)
; midje
(put-clojure-indent 'fact 1)
; lazytest
(put-clojure-indent 'describe 1)
(put-clojure-indent 'do-it 1)
(put-clojure-indent 'given 1)
(put-clojure-indent 'it 1)


;;-----------------------------------------------------------------------
;; slime for clojure
;;-----------------------------------------------------------------------

;; Fix paredit behaviors on slime REPL
(defun hyone:slime-repl-paredit-modify-syntax ()
  (modify-syntax-entry ?\{ "(}")
  (modify-syntax-entry ?\} "){")
  (modify-syntax-entry ?\[ "(]")
  (modify-syntax-entry ?\] ")[")
  (modify-syntax-entry ?~ "' ")
  (modify-syntax-entry ?, " ")
  (modify-syntax-entry ?^ "'")
  (modify-syntax-entry ?= "'"))


(add-hook 'slime-connected-hook
          (lambda ()
            (when (equal (slime-lisp-implementation-name) "clojure")
              (hyone:slime-repl-paredit-modify-syntax))))

;; font lock for slime clojure
;;-----------------------------------------------------------------------
;; referred to https://github.com/overtone/live-coding-emacs/blob/master/lib/durendal/durendal.el

;; (slime-lisp-implementation-name) を使って clojure の時にのみ
;; clojure-mode-font-lock-setup を実行したいのが
;; slime-repl-mode-hook 時に (slime-lisp-implementation-name) が nil を返してしまうため、
;; どうもうまくいかない。
;; (add-hook 'slime-repl-mode-hook
;;           (lambda ()
;;             (if (equal (slime-lisp-implementation-name) "clojure")
;;                 (clojure-mode-font-lock-setup))))

(add-hook 'slime-repl-mode-hook 'clojure-mode-font-lock-setup)

(add-hook 'slime-connected-hook
          (lambda ()
            (when (equal (slime-lisp-implementation-name) "clojure")
              (ad-activate #'slime-repl-emit)
              (ad-activate #'slime-repl-insert-prompt))))

;; don't work?
(add-hook 'sldb-mode-hook 'sldb-font-lock)

;; font lock for REPL lisp output
(defadvice slime-repl-emit (after durendal-slime-repl-emit-ad)
  (with-current-buffer (slime-output-buffer)
    (add-text-properties slime-output-start slime-output-end
                         '(font-lock-face slime-repl-output-face
                                          rear-nonsticky (font-lock-face)))))

;; font lock for REPL prompt
(defadvice slime-repl-insert-prompt (after durendal-slime-repl-prompt-ad)
  (with-current-buffer (slime-output-buffer)
    (let ((inhibit-read-only t))
      (add-text-properties slime-repl-prompt-start-mark (point-max)
                           '(font-lock-face slime-repl-prompt-face
                                            rear-nonsticky
                                            (slime-repl-prompt
                                             read-only
                                             font-lock-face
                                             intangible))))))

(defun sldb-font-lock ()
  (font-lock-add-keywords nil
   '(("[0-9]+: \\(clojure\.\\(core\\|lang\\).*\\)" 1 font-lock-comment-face)
     ("[0-9]+: \\(java.*\\)"  1 font-lock-comment-face)
     ("[0-9]+: \\(swank.*\\)" 1 font-lock-comment-face)
     ("\\[\\([A-Z]+\\)\\]"    1 font-lock-function-name-face))))


;;-----------------------------------------------------------------------
;; align-cljlet.el
;;-----------------------------------------------------------------------

(require 'align-cljlet)


;;-----------------------------------------------------------------------
;; midje-mode.el
;;-----------------------------------------------------------------------

(setq midje-mode-key-prefix (kbd "C-c ."))
(require 'midje-mode)
(require 'clojure-jump-to-file)

