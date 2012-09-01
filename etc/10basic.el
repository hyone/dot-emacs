;;-----------------------------------------------------------------------
;; Basic Settings
;;-----------------------------------------------------------------------

;; Environment Variables
;;-----------------------------------------------------------------------

;; path to search exec command
(setq exec-path (append '(
    "~/bin/local"
    "~/bin"
    "~/local/bin"
    "/usr/local/bin"
) exec-path))

;; set $PATH
(setenv "PATH" (mapconcat 'identity exec-path ":"))


(defun setenv-from-shell (varname)
  (setenv varname (replace-regexp-in-string
                   "[ \t\n]*$"
                   ""
                   (shell-command-to-string (concat "$SHELL --login -c 'echo $" varname "'")))))

(setenv-from-shell "PYTHONPATH")

;; Etcetera
;;-----------------------------------------------------------------------

;; the number of lines of *Message* buffer
(setq message-log-max 10000)

;; keep the cursor position when frame scrolling by C-v and M-v
(setq scroll-preserve-screen-position t)

;; enable syntax hilight
(global-font-lock-mode t)

;; colorize region
(setq transient-mark-mode t)

;; emphasize parentheses
(setq show-paren-delay 0.10)
(show-paren-mode t)

;; set default-tab-width to 4
(setq default-tab-width 4)
(setq tab-stop-list
      `(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

;; don't use TAB as indent
;; ( setq-default: set default value to local variable of each buffer. )
(setq-default indent-tabs-mode nil)

;; ;; preferred the way to indent and offset on lisp
;; (setq lisp-indent-offset 2)

;; enable narrowing
(put 'narrow-to-region 'disabled nil)

;; autosave
;;(setq auto-save-list-file-name nil)
;;(setq auto-save-list-file-prefix nil)

;; type C-k to delete a line feed characteor, as well a line.
(setq kill-whole-line t)

;; type [BackSpace] to remove the region selected
(when transient-mark-mode
  (defadvice backward-delete-char-untabify
    (around delete-region-like-windows activate)
    (if mark-active
      (delete-region (region-beginning) (region-end))
       ad-do-it)))

;; deactivate mark after eval-region
(defadvice eval-region
  (after deactivate-mark activate compile)
  (deactivate-mark))

;; version control
(setq version-control t)

;; when the current buffer was killed,
;; if it hos no contents, we delete it's file iteself, too.
(if (not (memq 'delete-file-if-no-contents after-save-hook))
    (setq after-save-hook
          (cons 'delete-file-if-no-contents after-save-hook)))

(defun delete-file-if-no-contents ()
  (when (and
         (buffer-file-name (current-buffer))
         (= (point-min) (point-max)))
    (when (y-or-n-p "Delete file and kill buffer?")
      (delete-file
       (buffer-file-name (current-buffer)))
      (kill-buffer (current-buffer)))))

;; enable copy & paste to clipboard
(setq x-select-enable-clipboard t)

;; scroll by one line
(setq scroll-step 1)

;; set a sound of bell off
(setq visible-bell nil)

;; diff
(setq diff-switches "-u")

;; completion like m.h to meadow.html
;; (partial-completion-mode t)

;; change confirmation dialog from 'yes/no' to 'y/n
(fset 'yes-or-no-p 'y-or-n-p)

;;  backup
;; ------------------------------------------------------------

;; ;; disable to backup a file editing
;; (setq make-backup-files nil)

(setq backup-directory-alist '(("" . "~/.emacs.d/backup")))

;; number of newest backup keeps to remain
(setq kept-new-versions 5)

;; prevent emacs from asking delete old backup when new backup is created.
(setq delete-old-versions t)


;;  grep
;; ------------------------------------------------------------

(setq grep-command "grep -nH -E ")

; (setq grep-host-defaults-alist nil)
; (setq grep-template "lgrep <C> -n -Ia -Ou8 <R> <F> <N>")
; (setq grep-find-template "find . <X> -type f <F> -print0 | xargs -0 -e lgrep <C> -n -Au <R> <N>")
