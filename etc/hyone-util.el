;;-----------------------------------------------------------------------
;; General
;;-----------------------------------------------------------------------

(require 'cl)

;; functions
;;------------------------------

(defun hyone:filter (condp lst)
  (delq nil
    (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun hyone:trim (s)
  (let ((s1 (replace-regexp-in-string "[ \t]*$" "" s)))
    (replace-regexp-in-string "^[ \t]*" "" s1)))

;; For command
;;------------------------------

(defun hyone:noop ()
  "no operation"
  (interactive))

(defun hyone:region-mark-or-word ()
  "if mark is active, return the active region, which is (region-beginning, region-end),
or return the region from current point to forward-word point."
  (if (use-region-p)
      (list (region-beginning) (region-end))
    (list (point) (progn (forward-word 1) (point)))))


(defun hyone:region-mark-or-line ()
  "if mark is active, return the active region, which is (region-beginning, region-end),
or return the region from (beginning-of-line) to end-of-line point."
  (if mark-active
      (list (region-beginning) (region-end))
    (list
      (progn (move-beginning-of-line nil) (point))
      (progn (move-end-of-line nil) (point)))))


;;-----------------------------------------------------------------------
;; buffers and windows
;;-----------------------------------------------------------------------

(defun hyone:windmove-left-or-other ()
  (interactive)
  (unless (ignore-errors
      (windmove-left))
        (other-window 1)))

(defun hyone:windmove-right-or-other ()
  (interactive)
  (unless (ignore-errors
      (windmove-right))
        (other-window -1)))

(defun hyone:windmove-up-or-other ()
  (interactive)
  (unless (ignore-errors
      (windmove-up))
        (other-window -1)))

(defun hyone:windmove-down-or-other ()
  (interactive)
  (unless (ignore-errors
      (windmove-down))
        (other-window 1)))

(defun hyone:swap-windows(n)
  "Swap two windows. if there is an argument, keep the cursor at the current window, otherwise not."
  (interactive "p")
  (let ((this-window (selected-window))
         (this-buffer (window-buffer))
         (other-window (next-window))
         (other-buffer (window-buffer (next-window))))
    (set-window-buffer other-window this-buffer)
    (set-window-buffer this-window  other-buffer)
    (if (= n 1) (other-window 1))))

(defun hyone:kill-other-buffer ()
  "kill buffer in a other window."
  (interactive)
  (other-window 1)
  (kill-buffer nil)
  (other-window 1))

(defun hyone:kill-other-buffer-and-window ()
  "kill a other window and it's buffer."
  (interactive)
  (other-window 1)
  (kill-buffer-and-window))

(defun hyone:rename-file-and-buffer (new-name)
  "Rename both the current buffer and file to new name."
  (interactive "BNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
      (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
        (message "A buffer name '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))


;;-----------------------------------------------------------------------
;; Editing
;;-----------------------------------------------------------------------

(defun hyone:on-blank-line-p ()
  (save-excursion
    (let ((end (progn (end-of-line) (point))))
      (beginning-of-line)
      (if (re-search-forward "^\\s-*$" end t) t))))

(defun hyone:backward-kill-line (arg)
  "like vim 'C-u'"
  (interactive "p")
  (let ((pos-cur (point))
        (pos-ls  (progn (beginning-of-line) (point))))
    (goto-char pos-cur)
    (cond
     ;; when cursor is at the begging of line, delete newline character.
     ((= pos-cur pos-ls) (delete-backward-char 1))
     (t (kill-line 0)))))

;; From http://dev.ariel-networks.com/Members/matsuyama/tokyo-emacs-02
(defun hyone:kill-line-or-region (beg end)
  "when select region, kill region otherwise line"
  (interactive (list (point) (mark t)))
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (kill-whole-line 1)
    (kill-region beg end)))

(defun hyone:backward-kill-word-like-vim ()
  "delete backward word like vim's C-w behavior"
  (interactive)
  (let* ((pos-cur (point))
          (pos-ls (progn (beginning-of-line) (point)))
          (pos-ts (progn (beginning-of-line-text) (point)))
          (pos-bw (progn (goto-char pos-cur) (backward-word 1) (point))))
    ;;    reset cursor
    (goto-char pos-cur)
    ; (message "%d: %d %d %d" pos-cur pos-ls pos-ts pos-bw)
    (cond
      ;; delete a newline
      ((= pos-cur pos-ls) (delete-backward-char 1))
      ;; delete a word
      ((> pos-bw pos-ts) (backward-kill-word 1))
      ;; delete until beginning of text if there is no word before cursor
      ((< pos-ts pos-cur) (delete-backward-char (- pos-cur pos-ts)))
      ;; delete spaces until beginning of the line
      (t (delete-backward-char (- pos-cur pos-ls))))))

(defun hyone:backward-kill-word-like-vim-or-region (beg end)
  "when select region, kill region otherwise backword word"
  (interactive (list (point) (mark t)))
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (hyone:backward-kill-word-like-vim)
    (kill-region beg end)))

(defun hyone:delete-horizontal-space-region (start end)
  "invoke delete-horizontal-space by each line in region."
  (interactive "r")
  (save-excursion
    (let ((cur (- end 1)))
      (goto-char cur)
      (beginning-of-line)
      (while (>= cur start)
        (delete-horizontal-space)
        (forward-line -1)
        (setq cur (point))))))

(defun hyone:kill-ring-save-rectangle (start end)
  "copy the rectangle."
  (interactive "r")
  (setq killed-rectangle (extract-rectangle start end))
  ; deactivate region
  (setq deactivate-mark t))

(defun hyone:comment-or-uncomment-region-or-line (start end)
  "toggle comment region or line"
  (interactive (hyone:region-mark-or-line))
  (save-excursion
    (comment-or-uncomment-region start end)))

(defun hyone:kill-buffer-file-path ()
  "kill file path of the current buffer"
  (interactive)
  (if buffer-file-name
      (progn
        (kill-new buffer-file-name)
        (message "Killed: %s" buffer-file-name))
    (message "kill-buffer-file-path: buffer has not the file path.")))

(defun* hyone:in-blank-single-line-block-p (&key (has-space t))
  "whether or not be in blank block"
  (let ((cur    (point))
        (start  (progn (beginning-of-line) (point)))
        (end    (progn (end-of-line) (point)))
        (spaces (if has-space "\\s-*" "")))
    (goto-char cur)
    (let ((r (save-excursion
               (re-search-forward (concat spaces "[])}]") end t)
               (match-beginning 0))))
      (if r
          (let ((l (save-excursion
                     (re-search-backward (concat "[[({]" spaces) start t)
                     (match-end 0))))
            (and (= r cur) (= l cur)))))))

(defun* hyone:newline-dwim (&optional (newline-function 'newline)
                                      (indent-function  'indent-according-to-mode))
  "smart insert newline and indent in blank block ( i.e. {} )"
  (interactive)
  (if (hyone:in-blank-single-line-block-p)
      (progn
        (dotimes (i 2) (funcall newline-function))
        (funcall indent-function)
        (previous-line)
        (funcall indent-function))
    (funcall newline-function)))

(defun* hyone:insert-space-dwim (&optional (insert-function 'self-insert-command)
                                           (arg nil))
  "smart insert space in blank block ( i.g. {} )"
  (if (hyone:in-blank-single-line-block-p :has-space nil)
      (progn
        (insert "  ")
        (backward-char))
    (funcall insert-function arg)))

;; align.el
;;--------------------------------

;; add "\\(\\s-*\\)", otherwise don't align-regexp works.
;; see also: http://www.emacswiki.org/emacs/AlignCommands#toc5
(defun hyone:align-repeat (start end regexp &optional spacing)
  "align region by keyword"
  (interactive "r\nsAlign Regexp: ")
  (let ((spacing (or spacing 1)))
    (save-excursion
      (align-regexp
       (region-beginning)
       (region-end)
       (concat "\\(\\s-*\\)" regexp)
       1 spacing t))))

(defun hyone:align-repeat-by-keyword (regexp)
  (if mark-active
      (hyone:align-repeat (region-beginning) (region-end) regexp)))


;;-----------------------------------------------------------------------
;; motions
;;-----------------------------------------------------------------------

(defun hyone:move-to-mark ()
  (interactive)
  (let ((pos (point)))
       (goto-char (mark))
       (push-mark pos)))

; From http://github.com/typester/emacs-config/blob/master/.emacs.d/conf/50_perl.el
(defun hyone:move-to-match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
    ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
    (t (self-insert-command (or arg 1)))))


;;-----------------------------------------------------------------------
;; etcetra
;;-----------------------------------------------------------------------

(defun hyone:reselect-last-region ()
  "re-select last active region"
  (interactive)
  (pop-to-mark-command)
  (exchange-dot-and-mark))

; describe face at the cursor position
(defun hyone:describe-face-at-point ()
  "Return face used at point."
  (interactive)
  (message "%s" (get-char-property (point) 'face)))

(defun hyone:unpop-to-mark-command ()
  "Unpop off mark ring into the buffer's actual mark.
Does not set point.  Does nothing if mark ring is empty."
  (interactive)
  (let ((num-times (if (equal last-command 'pop-to-mark-command) 2
                     (if (equal last-command 'unpop-to-mark-command) 1
                       (error "Previous command was not a (un)pop-to-mark-command")))))
    (dotimes (x num-times)
      (when mark-ring
        (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
        (set-marker (mark-marker) (+ 0 (car (last mark-ring))) (current-buffer))
        (when (null (mark t)) (ding))
        (setq mark-ring (nbutlast mark-ring))
        (goto-char (mark t)))
      (deactivate-mark))))

(defun hyone:eval-expression-or-region (start end)
  (interactive "r")
  (if mark-active
    (progn
      (eval-region start end)
      (deactivate-mark))
    ;; XXX: to use same history, same map with original eval-expression command
    ;; defined in simple.el
    (eval-expression 
      (read-from-minibuffer "Eval: "
        nil read-expression-map t
        'read-expression-history))))

(defun hyone:set-tab-width (num &optional local redraw)
  "set tabwidth and tab-stop-list. if local is t, variable is a buffer local."
  (interactive "nTab Width: ")
  (when local
    (make-local-variable 'tab-width)
    (make-local-variable 'tab-stop-list))
  (setq tab-width num)
  (setq tab-stop-list ())
  (while (<= num 256)
    (setq tab-stop-list `(,@tab-stop-list ,num))
    (setq num (+ num tab-width)))
  (when redraw (redraw-display)) tab-width)

;; TODO: want to ask whether or not create a new directory
;;       if dirname doesn't exist.
(defun hyone:find-file-pyblosxom-entry (dirname)
  "find-file for a pyblosxom entry."
  (interactive "DPyblosxom Entry (directory): ")
  (let* ((filename (format-time-string "%Y-%m-%d-%H-%M.rst"))
         (path     (expand-file-name filename dirname)))
    (find-file path)
    (set-buffer-file-coding-system 'utf-8-unix)))

(defun hyone:count-lines-buffer ()
  "return the line number of the buffer."
  (interactive)
  (save-excursion
    (end-of-buffer)
    (message "%d" (1+ (count-lines 1 (point))))))

(defun hyone:open ()
  "Open current buffer for OS X open command"
  (interactive)
  (shell-command (concat "open '" (buffer-file-name) "'")))


(require 'skk-autoloads nil t)
(defun hyone:skk-turn-off ()
  ;; if skk is on
  (if (featurep 'skk)
      (skk-latin-mode -1)))


(provide 'hyone-util)
