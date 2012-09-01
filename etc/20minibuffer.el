(require 'hyone-util)


;; use C-d to delete region from current cursor point to separate character in minibuffer
;; (like C-w on insert mode in Vim)
;; http://www.bookshelf.jp/soft/meadow_27.html#SEC345

(defvar minibuf-shrink-type0-chars '((w3m-input-url-history . (?/ ?+ ?:))
                                     (read-expression-history . (?\) ))
                                     (t . (?/ ?+ ?~ ?:)))
  "*minibuffer-history-variable とセパレータと見なす character の alist 。
type0 はセパレータを残すもの。")

(defvar minibuf-shrink-type1-chars '((file-name-history . ())
                                     (w3m-input-url-history . (?# ?? ?& ?.))
                                     (t . (? )))
                                     ;; (t . (?- ?_ ?. ? )))
  "*minibuffer-history-variable とセパレータと見なす character の alist 。
type1 はセパレータを消去するもの。")

(defun minibuf-shrink-get-chars (types)
  (or (cdr (assq minibuffer-history-variable types))
      (cdr (assq t types))))

(defun minibuf-shrink (&optional args)
  "point が buffer の最後なら 1 word 消去する。その他の場合は delete-char を起動する。
単語のセパレータは minibuf-shrink-type[01]-chars 。"
  (interactive "p")
  (if (/= (if (fboundp 'field-end) (field-end) (point-max)) (point))
      (delete-char args)
    (let ((type0 (minibuf-shrink-get-chars minibuf-shrink-type0-chars))
          (type1 (minibuf-shrink-get-chars minibuf-shrink-type1-chars))
          (count (if (<= args 0) 1 args))
          char)
      (while (not (zerop count))
        (when (memq (setq char (char-before)) type0)
          (delete-char -1)
          (while (eq char (char-before))
            (delete-char -1)))
        (setq count (catch 'detect
                      (while (/= (if (fboundp 'field-beginning)
                                     (field-beginning) (point-min))
                                 (point))
                        (setq char (char-before))
                        (cond
                         ((memq char type0)
                          (throw 'detect (1- count)))
                         ((memq char type1)
                          (delete-char -1)
                          (while (eq char (char-before))
                            (delete-char -1))
                          (throw 'detect (1- count)))
                         (t (delete-char -1))))
                      ;; exit
                      0))))))


(defvar minibuf-expand-filename-original nil)
(defvar minibuf-expand-filename-begin nil)

(defun minibuf-expand-filename (&optional args)
  "file-name-history だったら minibuffer の内容を expand-file-name する。
連続して起動すると元に戻す。 C-u 付きだと link を展開する。"
  (interactive "P")
  (when (eq minibuffer-history-variable 'file-name-history)
    (let* ((try-again (eq last-command this-command))
           (beg (cond
                 ;; Emacs21.3.50 + ange-ftp だと 2 回目に変になる
                 ((and try-again minibuf-expand-filename-begin)
                  minibuf-expand-filename-begin)
                 ((fboundp 'field-beginning) (field-beginning))
                 (t (point-min))))
           (end (if (fboundp 'field-end) (field-end) (point-max)))
           (file (buffer-substring-no-properties beg end))
           (remote (when (string-match "^\\(/[^:/]+:\\)/" file)
                     (match-string 1 file)))
           (home (if (string-match "^\\(/[^:/]+:\\)/" file)
                     (expand-file-name (format "%s~" (match-string 1 file)))
                   (expand-file-name "~"))))
      (unless try-again
        (setq minibuf-expand-filename-begin beg))
      (cond
       ((and args try-again minibuf-expand-filename-original)
        (setq file (file-chase-links (expand-file-name file))))
       (args
        (setq minibuf-expand-filename-original file)
        (setq file (file-chase-links (expand-file-name file))))
       ((and try-again minibuf-expand-filename-original)
        (setq file minibuf-expand-filename-original)
        (setq minibuf-expand-filename-original nil))
       (t
        (setq minibuf-expand-filename-original file)
        (if (string-match (concat "^" (regexp-quote home)) file)
            (if remote
                (setq file (concat remote "~" (substring file (match-end 0))))
              (setq file (concat "~" (substring file (match-end 0)))))
          (setq file (expand-file-name file)))))
      (delete-region beg end)
      (insert file))))


;;-----------------------------------------------------------------------
;; minibuffer keybinds
;;-----------------------------------------------------------------------

(mapcar (lambda (map)
          (define-key map (kbd "C-d")   'minibuf-shrink)
          (define-key map (kbd "C-q")   'minibuf-shrink)
          (define-key map (kbd "C-w")   'backward-kill-word)
          (define-key map (kbd "C-u")   'hyone:backward-kill-line)
          ;; do expand-file-name to the current contents on mini-buffer
          (define-key map (kbd "C-M-e") 'minibuf-expand-filename)
          (define-key map (kbd "C-n")   'next-history-element)
          (define-key map (kbd "C-p")   'previous-history-element))
        (delq nil (list (and (boundp 'minibuffer-local-map)
                             minibuffer-local-map)
                        (and (boundp 'minibuffer-local-ns-map)
                             minibuffer-local-ns-map)
                        (and (boundp 'minibuffer-local-completion-map)
                             minibuffer-local-completion-map)
                        (and (boundp 'minibuffer-local-must-match-map)
                             minibuffer-local-must-match-map))))

; delete "\C-n", "\C-p" key bindings when on minibuffer completion
;; (when (require 'mcomplete nil t)
;;   (load "mcomplete-history" nil)
;;   (mapcar (lambda (k)
;; ;;             (message "assoc %s" (assoc k mcomplete-permissive-completion-map-alist))
;;             (delq (assoc k mcomplete-permissive-completion-map-alist)
;;               mcomplete-permissive-completion-map-alist)
;;             (delq (assoc k mcomplete-must-match-completion-map-alist)
;;               mcomplete-must-match-completion-map-alist))
;;     '("\C-n" "\C-p"))

;;   (mapcar (lambda (alist)
;;             (add-to-list alist '("\C-f" . MComplete-next-method))
;;             (add-to-list alist '("\C-v" . MComplete-previous-method))
;;             (add-to-list alist '([return] . MComplete-exit-minibuffer))
;;           )
;;     '(mcomplete-permissive-completion-map-alist
;;        mcomplete-must-match-completion-map-alist)))


;; ;;-----------------------------------------------------------------------
;; ;; mcomplete.el
;; ;;-----------------------------------------------------------------------
;; ;    it makes completion of M-x a kind of like iswitchb

;; ;    mcomplete.el, mcomplete-history.el
;; (require 'mcomplete)
;; (require 'cl)
;; (load "mcomplete-history")
;; (turn-on-mcomplete-mode)


;;-----------------------------------------------------------------------
;; zlc.el
;;-----------------------------------------------------------------------

(require 'zlc nil t)

;;  when hit TAB key first, show completions list as well as select the first completion
;; (setq zlc-select-completion-immediately t)

(define-key minibuffer-local-map (kbd "<tab>") 'minibuffer-complete)
;;  reset selection
(define-key minibuffer-local-map (kbd "M-s")   'zlc-reset)



;;-----------------------------------------------------------------------
;; yaicomplete.el
;;-----------------------------------------------------------------------

;; (when (require 'yaicomplete nil t)
;;   (yaicomplete-mode))

