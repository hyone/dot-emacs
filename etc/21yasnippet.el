
;    these variables must be set before requiring yasnippet.el
;; (setq yas/trigger-key "C-o")
(setq yas/trigger-key "TAB")
(setq yas/next-field-key '("TAB" "<tab>"))
(setq yas/prev-field-key "S-<tab>")

(setq yas/fallback-behavior 'call-other-command)

;; require yasnippet.el before anything.el
(require 'yasnippet)

;; snippets dir
(setq yas/snippet-dirs
      (list
       ;; put custom snippet dir first becaues it becomes the default snippets save dir.
       (expand-file-name "mysnippets" hyone:emacs-home)
       (expand-file-name "site-lisp/yasnippet/snippets" hyone:emacs-home)))

;; when region is active, replace $0 to contents of region
(setq yas/wrap-around-region t)

;; TAB to trigger another snippet expansion inside the snippet field
(setq yas/triggers-in-field t)

; make only punctuation sequence as snippet key,
; i.e. when typing "world.<TAB>", enable "." as snippet key.
(setq yas/key-syntaxes (list "." "w" "w_" "w_." "^ "))

(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt))

(yas/global-mode 1)


;; snippet mode
;;-----------------------------------------------------------------------

(setq auto-mode-alist (cons '("mysnippets/" . snippet-mode) auto-mode-alist))


;; anyting-yasnippet
;;-----------------------------------------------------------------------

;; From http://d.hatena.ne.jp/sugyan/20120111/1326288445
;; use anything interface as prompt to select from multiple snippets candidates.
(defun hyone:yas/anything-prompt (prompt choices &optional display-fn)
  (let* ((names (loop for choice in choices
                      collect (or (and display-fn (funcall display-fn choice))
                                  coice)))
         (selected (anything-other-buffer
                    `(((name . ,(format "%s" prompt))
                       (candidates . names)
                       (action . (("Insert snippet" . (lambda (arg) arg))))))
                    "*anything yas/prompt*")))
    (if selected
        (let ((n (position selected names :test 'equal)))
          (nth n choices))
      (signal 'quit "user quit!"))))

(custom-set-variables '(yas/prompt-functions '(hyone:yas/anything-prompt)))