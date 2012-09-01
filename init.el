; -*- mode: Lisp -*-

(message "Starting to run emacs...")


(defun filter (condp lst)
  (delq nil
    (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun walk-directory (dir action)
  (funcall action dir)
  (mapcar (lambda (x)
            (walk-directory x action))
    (filter 'file-directory-p
      (directory-files dir t "^[^\\.]" t))))


;;-----------------------------------------------------------------------
;; Settings of load-path
;;-----------------------------------------------------------------------

(defvar hyone:emacs-home (expand-file-name "~/.emacs.d/")
  "Root directory for emacs configuration files")

;; set load-path recursively from site-lisp root directory
(let ((root (expand-file-name "site-lisp" hyone:emacs-home)))
  (when (file-directory-p root)
    (walk-directory root
      (lambda (d)
        (setq load-path (cons d load-path))))))

(add-to-list 'load-path (expand-file-name "etc" hyone:emacs-home))


;;-----------------------------------------------------------------------
;; package.el
;;-----------------------------------------------------------------------

(when (load (expand-file-name "elpa/package.el" hyone:emacs-home))
  (add-to-list 'package-archives '("elpa"      . "http://tromey.com/elpa/"))
  (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
  (package-initialize))


;;-----------------------------------------------------------------------
;; Other Path
;;-----------------------------------------------------------------------

(require 'info)
(add-to-list 'Info-additional-directory-list "~/.emacs.d/info")


;;-----------------------------------------------------------------------
;; Load Global Configurations
;;-----------------------------------------------------------------------

;; load configuration
(require 'debian-run-directories)

(let ((etc-dir (expand-file-name "etc" hyone:emacs-home)))
  (walk-directory etc-dir
    (lambda (x) (debian-run-directories x))))


;;-----------------------------------------------------------------------
;; Load Local Configurations
;;-----------------------------------------------------------------------

(let ((local-conf (expand-file-name "~/etc/local/emacs.local")))
  (if (file-readable-p local-conf)
    (load-file local-conf)))

(put 'downcase-region 'disabled nil)
