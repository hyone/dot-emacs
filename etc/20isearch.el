;; -----------------------------------------------------------------------
;; isearch
;; -----------------------------------------------------------------------
;; M-y: paste
;; C-w: search a word on cursor.
;; M-o: isearch-moccur

;; i-search for japanese
;; (define-key isearch-mode-map (kbd "C-k") `isearch-edit-string)

(define-key isearch-mode-map (kbd "C-q") 'isearch-exit)

(define-key isearch-mode-map (kbd "C-n") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "C-p") 'isearch-ring-retreat)
(define-key isearch-mode-map (kbd "C-h") 'isearch-delete-char)
;; ; TODO: map C-u to clear isearch-minibuffer
;; ; (define-key isearch-mode-map (kbd "C-u") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "M-e") 'isearch-complete)
(define-key isearch-mode-map (kbd "C-y") 'isearch-yank-line)