;; minimal settings for plain emacs

;;-----------------------------------------------------------------------
;; Settings of load-path
;;-----------------------------------------------------------------------

(defun filter (condp lst)
  (delq nil
    (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun walk-directory (dir action)
  (funcall action dir)
  (mapcar (lambda (x)
            (walk-directory x action))
    (filter 'file-directory-p
      (directory-files dir t "^[^\\.]" t))))

(defvar hyone:emacs-home (expand-file-name "~/etc/emacs/")
  "Root directory for emacs configuration files")

;; set load-path recursively from site-lisp root directory
(let ((root (expand-file-name "site-lisp" hyone:emacs-home)))
  (when (file-directory-p root)
    (walk-directory root
      (lambda (d)
        (setq load-path (cons d load-path))))))

(add-to-list 'load-path (expand-file-name "etc" hyone:emacs-home))

;;-----------------------------------------------------------------------
;; Keybinds
;;-----------------------------------------------------------------------

(require 'hyone-util)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-w") 'hyone:backward-kill-word-like-vim-or-region)
(global-set-key (kbd "C-u") 'hyone:backward-kill-line)

(when (eq window-system 'ns)
  ; use command key as Meta
  (setq ns-command-modifier 'meta))
