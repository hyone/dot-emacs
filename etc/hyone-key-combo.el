(require 'key-combo)
(require 'evil)


(defun evil-key-combo-define (state keymap key commands)
  "key-combo-define with using evil-define-key"
  ;;copy from key-chord-define
  (let ((base-key (list (car (listify-key-sequence key)))))
  (cond
   ;;for sequence '(" = " " == ")
   ((and (not (key-combo-elementp commands))
         (key-combo-elementp (car-safe commands)))
      (let ((seq-keys base-key));;list
      (mapc '(lambda (command)
               (evil-key-combo-define state keymap (vconcat seq-keys) command)
               (setq seq-keys
                     (append seq-keys base-key)))
            commands)))
   (t
    (unless (key-combo-elementp commands)
      (error "%s is not command" commands))
    (evil-define-key state keymap
      (vector 'key-combo (intern (key-description key)))
      (key-combo-get-command commands))
    ))))


;; delete spaces around a word of key-combo if the cursor just in "{}", "[]", "()"
;; e.g.
;; if defined: (key-combo-define evil-insert-state-map (kbd "=~") " =~ ")
;; then, insert "=~" if the case.
(defadvice key-combo-execute-macro (before delete-space-if-in-pairs (x) activate)
  (lexical-let ((b (char-before))
                (a (char-after))
                (pairs '((?[ . ?])
                         (?( . ?))
                         (?{ . ?}))))
    (if (hyone:filter (lambda (c) (and (eq (car c) b)
                                       (eq (cdr c) a))) pairs)
        (ad-set-arg 0 (hyone:trim (ad-get-arg 0))))))


(provide 'hyone-key-combo)
