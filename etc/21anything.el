(require 'hyone-util)

;;-----------------------------------------------------------------------
;; anything.el
;;-----------------------------------------------------------------------

(setq anything-command-map-prefix-key "<f3>")

(require 'anything-startup)

(setq
  ;; time to display condidates list (default: 0.5)
  anything-idle-delay 0.5
  ;; time to display redraw from typing (default: 0.1)
  anything-input-idle-delay 0.1
  ;; maximum number of candicates (default: 50)
  anything-candidate-number-limit 50
  ;; speed up response when there is many candidates
  anything-quick-update 1
  ;; make shortcuts of selecting a candicate alphabet
  anything-enable-shortcuts 'alphabet)

;; anything-for-files
(setq anything-c-filelist-file-name (expand-file-name "~/.filelist"))
(setq anything-grep-candidates-fast-directory-regexp "^/tmp")


(setq anything-sources (list anything-c-source-buffers
                             anything-c-source-elscreen
                             anything-c-source-bookmarks
                             anything-c-source-recentf
                             anything-c-source-imenu
                             anything-c-source-files-in-current-dir
                             ; anything-c-source-find-files
                             ; anything-c-source-file-name-history
                             ; anything-c-source-locate
                             ; anything-c-source-google-suggest
                             ; anything-c-source-complex-command-history
                             ))

(setq imenu-auto-rescan t)

(define-key anything-map (kbd "C-n")   'anything-next-line)
(define-key anything-map (kbd "C-p")   'anything-previous-line)
(define-key anything-map (kbd "M-n")   'anything-next-page)
(define-key anything-map (kbd "M-p")   'anything-previous-page)
(define-key anything-map (kbd "C-M-n") 'anything-next-source)
(define-key anything-map (kbd "C-M-p") 'anything-previous-source)
(define-key anything-map (kbd "C-h")   'delete-backward-char)
(define-key anything-map (kbd "C-w")   'backward-kill-word)
(define-key anything-map (kbd "C-u")   'hyone:backward-kill-line)
(define-key anything-map (kbd "C-c h") 'anything-help)
(define-key anything-map (kbd "C-l")   'anything-quit-and-find-file)
(define-key anything-map (kbd "C-x t") 'anything-toggle-resplit-window)
(define-key anything-map (kbd "C-t")   'anything-select-2nd-action)
(define-key anything-map (kbd "C-o")   'anything-select-3rd-action)
(define-key anything-map (kbd "C-q")   'anything-select-4th-action)
(define-key anything-map (kbd "C-z")   'anything-execute-persistent-action)

(setq anything-persistent-action-use-special-display t)

;; turn off skk when open anything buffer
;; (add-hook 'anything-before-initialize-hook 'hyone:skk-turn-off)

;; anything-grep

(define-key anything-c-read-file-map (kbd "C-h") 'delete-backward-char)

(define-key anything-c-grep-map (kbd "C-h") 'delete-backward-char)
(define-key anything-c-grep-map (kbd "C-w") 'backward-kill-word)
(define-key anything-c-grep-map (kbd "M-y") 'anything-yank-text-at-point)


;;-----------------------------------------------------------------------
;; anything-complete.el
;;-----------------------------------------------------------------------

(require 'anything-complete)

; anything interface for document
(setq anything-for-document-sources
  (list
    ; anything-c-source-man-pages
    anything-c-source-info-cl
    anything-c-source-info-pages
    anything-c-source-info-elisp
    anything-c-source-apropos-emacs-commands
    anything-c-source-apropos-emacs-functions
    anything-c-source-apropos-emacs-variables))

(defun anything-for-document ()
  "anything for document"
  (interactive)
  (anything anything-for-document-sources
    (thing-at-point 'symbol) nil nil nil
    "*anything for document*"))

; anything interface for man, perldoc
(setq anything-for-man-sources
  (list
    anything-c-source-man-pages))

(defun anything-for-man ()
  "anything for man pages"
  (interactive)
  (anything anything-for-man-sources
    (thing-at-point 'symbol) nil nil nil
    "*anything for man*"))


;;-----------------------------------------------------------------------
;; anything-find-file (anything-obsolete.el)
;;-----------------------------------------------------------------------

(setq anything-find-file-additional-sources
  '(anything-c-source-bookmarks
     ;; anything-c-source-recentf
     anything-c-source-file-cache
     ;; anything-c-source-locate
     ;; anything-c-source-filelist
     ))

(defun anything-find-file-follow-directory (sel)
  "Follow directory in 'sel'."
  (interactive)
  ;; These variables are bound by `arfn-sources' or `anything-find-file'.
  (declare (special prompt default-filename require-match predicate additional-attrs))
  (setq arfn-followed t)
  (let ((f (expand-file-name sel arfn-dir)))
    (cond ((and (file-directory-p f) (not (string-match "/\\.$" sel)))
            (with-selected-window (minibuffer-window) (delete-minibuffer-contents))
            (setq anything-pattern "")
            ;;(setq arfn-dir f)
            (anything-set-sources
              (arfn-sources
                prompt f default-filename require-match nil predicate additional-attrs))
            (anything-update))
      ((string-match "^\\(.+\\)/\\([^/]+\\)$" sel)
        (with-selected-window (minibuffer-window)
          (delete-minibuffer-contents)
          (insert (match-string 2 sel)))
        (anything-set-sources
          (arfn-sources
            prompt (expand-file-name (match-string 1 sel) arfn-dir)
            nil require-match (match-string 2 sel) predicate additional-attrs))
        (anything-update)))))


(defun anything-find-file-move-parent-directory ()
  (interactive)
  (anything-find-file-follow-directory ".."))


(define-key (anything-read-file-name-map) (kbd "M-i") 'anything-select-action)
(define-key (anything-read-file-name-map) (kbd "M-p") 'anything-find-file-move-parent-directory)

;; remap default behaviour of C-t
(define-key (anything-read-file-name-map) (kbd "C-x t") 'anything-toggle-resplit-window)
(define-key (anything-read-file-name-map) (kbd "C-t") 'anything-select-2nd-action)
(define-key (anything-read-file-name-map) (kbd "C-o") 'anything-select-3rd-action)
(define-key (anything-read-file-name-map) (kbd "C-.") 'anything-select-4th-action)


;;-----------------------------------------------------------------------
;; anything-c-yasnippet.el
;;-----------------------------------------------------------------------

(require 'anything-c-yasnippet)

(setq anything-c-yas-space-match-any-greedy t)


;;-----------------------------------------------------------------------
;; anything-c-javadoc.el
;;-----------------------------------------------------------------------

(require 'anything-c-javadoc)

(setq anything-c-javadoc-dirs
  '("http://download.oracle.com/javase/6/docs/api/"))


;;-----------------------------------------------------------------------
;; anything-font-families
;;-----------------------------------------------------------------------

(require 'cl)  ; loop, delete-duplicates

(defun anything-font-families ()
  "Preconfigured `anything' for font family."
  (interactive)
  (flet ((anything-mp-highlight-match () nil))
    (anything-other-buffer
      '(anything-c-source-font-families)
      "*anything font families*")))

(defun anything-font-families-create-buffer ()
  (with-current-buffer
    (get-buffer-create "*Fonts*")
    (loop for family in (sort (delete-duplicates (font-family-list)) 'string<)
      do (insert
           (propertize (concat family "\n")
             'font-lock-face
             (list :family family :height 2.0 :weight 'bold))))
    (font-lock-mode 1)))

(defvar anything-c-source-font-families
  '((name . "Fonts")
     (init lambda ()
       (unless (anything-candidate-buffer)
         (save-window-excursion
           (anything-font-families-create-buffer))
         (anything-candidate-buffer
           (get-buffer "*Fonts*"))))
     (candidates-in-buffer)
     (get-line . buffer-substring)
     (action
       ("Copy Name" lambda
         (candidate)
         (kill-new candidate))
       ("Insert Name" lambda
         (candidate)
         (with-current-buffer anything-current-buffer
           (insert candidate))))))
