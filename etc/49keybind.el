(require 'hyone-util)

;;-----------------------------------------------------------------------
;; General
;;-----------------------------------------------------------------------
;; C-v, C->, C-<

;; F1, C-? to help
(global-set-key (kbd "C-?") 'help-for-help)
(global-set-key (kbd "<f1>") 'help-for-help)

;; disable default behavior to use yasnippet trigger key
(global-set-key (kbd "C-o") nil)

;   remap default C-u to M-u
(global-set-key (kbd "M-u") 'universal-argument)
;   make M-u M-u is equivalent to M-u 16
(define-key universal-argument-map (kbd "M-u") 'universal-argument-more)


;;-----------------------------------------------------------------------
;; Move cursor
;;-----------------------------------------------------------------------

; history of points have edited
(require 'goto-chg)
(global-set-key (kbd "M-c") 'goto-last-change)
(global-set-key (kbd "M-C") 'goto-last-change-reverse)

; cycle back to previous mark positions
; (global-set-key (kbd "M-n") '(lambda () (interactive) (set-mark-command  1)))
(global-set-key (kbd "M-h") 'pop-to-mark-command)
(global-set-key (kbd "M-H") 'hyone:unpop-to-mark-command)

; select a mark position from mark-ring by anything
(require 'anything-c-source-mark-ring)
(global-set-key (kbd "M-m") '(lambda () (interactive)
                               (anything '(anything-c-source-mark-ring) nil "select mark: ")
                               (recenter)))

; go back and forward between point history
(when (require 'point-undo nil t)
  (global-set-key (kbd "M-j") 'point-undo)
  (global-set-key (kbd "M-J") 'point-redo))


;;-----------------------------------------------------------------------
;; Editing
;;-----------------------------------------------------------------------

;; use <return> instead of RET to be able to distinguish enter key from <C-m>.
;; but, don't work on console
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "M-<RET>") 'newline)

; use delete and C-h to remove a character at point of the cursor
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "C-h") 'delete-backward-char)


;;-----------------------------------------------------------------------
;; C-x
;;-----------------------------------------------------------------------


;;-----------------------------------------------------------------------
;; C-c
;;-----------------------------------------------------------------------

;; comment, uncomment
(define-key mode-specific-map "c" 'hyone:comment-or-uncomment-region-or-line)

;; ;; howm.el
;; (define-key mode-specific-map ",," 'howm-menu)

;; mode-compile.el
(define-key mode-specific-map "e" 'mode-compile)
(define-key mode-specific-map "x" 'mode-compile-kill)
;    jump to a point error happened
(define-key mode-specific-map "n" 'next-error)


;;-----------------------------------------------------------------------
;; Move between buffers and windows
;;-----------------------------------------------------------------------

(global-set-key (kbd "C-M-b") 'hyone:windmove-left-or-other)
(global-set-key (kbd "C-M-n") 'hyone:windmove-down-or-other)
(global-set-key (kbd "C-M-p") 'hyone:windmove-up-or-other)
(global-set-key (kbd "C-M-f") 'hyone:windmove-right-or-other)

;; Remap default behavior of C-q
(global-set-key (kbd "M-q") 'quoted-insert)

(define-prefix-command 'window-map)
(global-set-key (kbd "C-q") 'window-map)
(define-key window-map (kbd "s")   'split-window-vertically)
(define-key window-map (kbd "C-s") 'split-window-vertically)
(define-key window-map (kbd "S")   'split-window-horizontally)

(define-key window-map (kbd "a")   'split-window-horizontally)
(define-key window-map (kbd "C-a") 'split-window-horizontally)

(defun split-window-3pane()
  (interactive)
  (split-window-horizontally)
  (split-window-vertically))
(define-key window-map (kbd "z")   'split-window-3pane)
(define-key window-map (kbd "C-z") 'split-window-3pane)

(define-key window-map (kbd "q")   'other-window)
(define-key window-map (kbd "C-q") 'other-window)
(global-set-key (kbd "C-<tab>")    'other-window)

(define-key window-map (kbd "c")   'split-window-vertically)
(define-key window-map (kbd "C-c") 'split-window-vertically)
(define-key window-map (kbd "C")   'split-window-horizontally)

(define-key window-map (kbd "t")   'hyone:elscreen-create-with-current-buffer)
(define-key window-map (kbd "C-t") 'hyone:elscreen-create-with-current-buffer)

(define-key window-map (kbd "d")   'kill-buffer-and-window)
(define-key window-map (kbd "C-d") 'kill-buffer-and-window)
(define-key window-map (kbd "D")   'hyone:kill-other-buffer-and-window)

(define-key window-map (kbd "e")   'dired-other-window)
(define-key window-map (kbd "C-e") 'dired-other-window)

(define-key window-map (kbd "w")   'delete-window)
(define-key window-map (kbd "C-w") 'delete-window)
(define-key window-map (kbd "W")   'delete-other-windows)
(define-key window-map (kbd "o")   'delete-other-windows)
(define-key window-map (kbd "C-o") 'delete-other-windows)

(define-key window-map (kbd "x")   'hyone:swap-windows)
(define-key window-map (kbd "C-x") 'hyone:swap-windows)

(define-key window-map (kbd "h")   'hyone:windmove-left-or-other)
(define-key window-map (kbd "C-b") 'hyone:windmove-left-or-other)
(define-key window-map (kbd "j")   'hyone:windmove-down-or-other)
(define-key window-map (kbd "C-n") 'hyone:windmove-down-or-other)
(define-key window-map (kbd "k")   'hyone:windmove-up-or-other)
(define-key window-map (kbd "C-p") 'hyone:windmove-up-or-other)
(define-key window-map (kbd "l")   'hyone:windmove-right-or-other)
(define-key window-map (kbd "C-f") 'hyone:windmove-right-or-other)

; map Shift + arrow to move between windows
(when (fboundp 'windmove-default-keybinds)
  (windmove-default-keybinds))


;;-----------------------------------------------------------------------
;; C-t (elscreen.el)
;;-----------------------------------------------------------------------
;; elscreen-clone                C-t C
;; elscreen-swap                 C-t C-s

(require 'hyone-elscreen)

(define-key elscreen-map (kbd "l")    'hyone:elscreen-cycle-next)
(define-key elscreen-map (kbd "C-f")  'hyone:elscreen-cycle-next)
(define-key elscreen-map (kbd "h")    'hyone:elscreen-cycle-previous)
(define-key elscreen-map (kbd "C-b")  'hyone:elscreen-cycle-previous)

(define-key elscreen-map (kbd "t")    'elscreen-create)
(define-key elscreen-map (kbd "C-t")  'elscreen-create)

(mapcar (lambda (key)
          (define-key elscreen-map key
            (lambda ()
              (interactive)
              (elscreen-create)
              (split-window-horizontally))))
  (list (kbd "n") (kbd "C-n")))

(define-key elscreen-map (kbd "T")   'hyone:elscreen-create-with-current-buffer)
(define-key elscreen-map (kbd "w")   'elscreen-kill)
(define-key elscreen-map (kbd "C-w") 'elscreen-kill)
(define-key elscreen-map (kbd "d")   'elscreen-kill-screen-and-buffers)
(define-key elscreen-map (kbd "C-d") 'elscreen-kill-screen-and-buffers)
(define-key elscreen-map (kbd "e")   'elscreen-dired)
(define-key elscreen-map (kbd "C-e") 'elscreen-dired)
(define-key elscreen-map (kbd "K")   'elscreen-kill-screen-and-buffers)
(define-key elscreen-map (kbd "^")   'elscreen-toggle)
(define-key elscreen-map (kbd "C-^") 'elscreen-toggle)


;;-----------------------------------------------------------------------
;; Buffer-menu
;;-----------------------------------------------------------------------

; moccur
(define-key Buffer-menu-mode-map "O" 'Buffer-menu-moccur)


;;-----------------------------------------------------------------------
;; view mode
;;-----------------------------------------------------------------------

(add-hook 'view-mode-hook
  (lambda ()
    (define-key view-mode-map (kbd "C-j") 'next-line)))


;;-----------------------------------------------------------------------
;; moccur
;;-----------------------------------------------------------------------

;; add moccur fook
(defvar moccur-mode-hook nil)
(defvar moccur-grep-mode-hook nil)
(defadvice moccur-mode (after my-ad-moccur-mode activate)
  (run-hooks 'moccur-mode-hook))
(defadvice moccur-grep-mode (after my-ad-moccur-grep-mode activate)
  (run-hooks 'moccur-grep-mode-hook))

(defun my-customize-for-moccur-mode ()
  (local-unset-key (kbd "C-j"))
  (local-unset-key (kbd "C-k"))
  (define-key moccur-mode-map (kbd "M-j") 'moccur-next-file)
  (define-key moccur-mode-map (kbd "M-k") 'moccur-prev-file)
  (define-key moccur-mode-map (kbd "RET") 'moccur-mode-goto-occurrence)
)

(add-hook 'moccur-mode-hook 'my-customize-for-moccur-mode)
(add-hook 'moccur-grep-mode-hook 'my-customize-for-moccur-mode)