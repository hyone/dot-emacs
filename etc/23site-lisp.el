
;;-----------------------------------------------------------------------
;; install-elisp.el
;;-----------------------------------------------------------------------

(require 'install-elisp)
(setq install-elisp-repository-directory
      (expand-file-name "site-lisp" hyone:emacs-home))


;;-----------------------------------------------------------------------
;; auto-install.el
;;-----------------------------------------------------------------------

(when (require 'auto-install nil t)
  (setq auto-install-directory
        (expand-file-name "site-lisp" hyone:emacs-home))
  (auto-install-update-emacswiki-package-name t))


;;-----------------------------------------------------------------------
;; auto-async-byte-compile
;;-----------------------------------------------------------------------

(require 'auto-async-byte-compile)

(setq auto-async-byte-compile-exclude-files-regexp
      (regexp-opt '("/program/emacs-lisp"
                    "/themes"
                    "/emacs.d/etc/"
                    "/etc/emacs/etc/"
                    "/emacs.d/junk/"
                    "/etc/emacs/junk/")))

(add-hook 'emacs-lisp-mode-hook 'enable-auto-async-byte-compile-mode)


;;-----------------------------------------------------------------------
;; dircolors
;;-----------------------------------------------------------------------

(require 'dircolors)


;;-----------------------------------------------------------------------
;; recentf
;;-----------------------------------------------------------------------

(when (require 'recentf nil t)
  (setq recentf-max-saved-items 2000)
  (setq recentf-exclude '(".recentf"))
  ;; Automatically cleanup the recent list
  ;; each time Emacs has been idle that number of seconds.
  ;; (setq recentf-auto-cleanup 1800)
  ;; automatically save the file when emacs is on idle.
  (setq recentf-auto-save-timer
        (run-with-idle-timer 180 t 'recentf-save-list))
  (recentf-mode 1))


;;-----------------------------------------------------------------------
;; grep-edit.el
;;-----------------------------------------------------------------------

(require 'grep-edit)


;;-----------------------------------------------------------------------
;; moccur.el
;;-----------------------------------------------------------------------

;    color-moccur.el
(require 'color-moccur)

;    when type 'q' and then close moccur, I also close all files opened by moccur.
(setq kill-buffer-after-dired-do-moccur t)

(require 'moccur-edit)


;; ;;-----------------------------------------------------------------------
;; ;; abbrev
;; ;;-----------------------------------------------------------------------

;; ;    save abbrevs
;; (setq save-abbrevs t)
;; (setq abbrev-file-name (expand-file-name "abbrev_defs" hyone:emacs-home))

;; (quietly-read-abbrev-file)


;; ;;-----------------------------------------------------------------------
;; ;; dabbrev
;; ;;-----------------------------------------------------------------------

;; (load "dabbrev-ja")

;; (require 'dabbrev-highlight)

;; (setq filename-for-abbrev (list (expand-file-name "~/TAGS")
;;                                 (expand-file-name "dabbrev-completion.txt" hyone:emacs-home)))

;; ;    rename buffers only for completion of dabbrev to " *filename*",
;; ;    so these buffers become unvisible in buffer list such as C-x b
;; ;    from http://www.bookshelf.jp/soft/meadow_34.html#SEC503
;; (progn
;;   (let ((list filename-for-abbrev) filename buf cbuf)
;;     (setq cbuf (current-buffer))
;;     (while list
;;       (setq filename (car list))
;;       (setq list (cdr list))
;;       (if (get-buffer filename)
;;           ()
;;         (progn
;;           (setq buf (find-file-noselect filename))
;;           (set-buffer buf)
;;           (rename-buffer
;;            (concat " *" (file-name-nondirectory filename) "*") t))))
;;     (set-buffer cbuf)))


;;-----------------------------------------------------------------------
;; jaspace.el
;;-----------------------------------------------------------------------

;    make colorize (visible) to tab, zenkaku space, space
;    right after line feed.
(when (require 'jaspace nil t)
  (when (boundp 'jaspace-modes)
    (setq jaspace-modes (append jaspace-modes
                                (list 'php-mode
                                      'yaml-mode
                                      'javascript-mode
                                      'ruby-mode
                                      'python-mode
                                      'rst-mode
                                      'emacs-lisp-mode
                                      'text-mode
                                      'fundamental-mode))))
  (when (boundp 'jaspace-alternate-jaspace-string)
    (setq jaspace-alternate-jaspace-string "□"))
  (when (boundp 'jaspace-highlight-tabs)
    (setq jaspace-highlight-tabs ?^))
  (add-hook 'jaspace-mode-hook
            (lambda()
              (progn
                (when (boundp 'show-trailing-whitespace)
                  (setq show-trailing-whitespace t))
                ))))  (add-hook 'jaspace-mode-off-hook
            (lambda()
              (when (boundp 'show-trailing-whitespace)
                (setq show-trailing-whitespace nil))))


;;-----------------------------------------------------------------------
;; wanderlust
;;-----------------------------------------------------------------------

(autoload `wl "wl" "Wanderlust" t)
(autoload `wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload `wl-draft "wl-draft" "Write draft with Wanderlust." t)

(autoload `wl-user-agent-compose "wl-draft" nil t)
(if (boundp `mail-user-agent)
    (setq mail-user-agent `wl-user-agent))
(if (fboundp `define-mail-user-agent)
    (define-mail-user-agent
     `wl-user-agent
     `wl-user-agent-compose
     `wl-draft-send
     `wl-draft-kill
     `mail-send-hook))

; ;    icon
; (setq wl-icon-directory "~/work/wl/etc")


;;-----------------------------------------------------------------
;; dsvn.el
;;-----------------------------------------------------------------

(autoload 'svn-status "dsvn" "Run `svn status'." t)
(autoload 'svn-update "dsvn" "Run `svn update'." t)


;;-----------------------------------------------------------------------
;; session.el
;;-----------------------------------------------------------------------

(require 'session)

(setq session-initialize '(de-saveplace session keys menus places)
      session-globals-include '((kill-ring 50)
                                (session-file-alist 500 t)
                                (file-name-history 10000)))
(add-hook 'after-init-hook 'session-initialize)

;    default length of file-name-history is only 500, so we change it
(setq session-globals-max-string 100000000)

;    set infinity to size of history (default 30)
(setq history-length t)

;    remember the position of the cursor when closing a buffer,
;    because we want to also remember it for a read only file or a file that doesn't save.
(setq session-undo-check -1)


;    delete duplication fo history
;    http://www.bookshelf.jp/soft/meadow_27.html#SEC343
(require 'cl)
(defun minubuffer-delete-duplicate ()
  (let (list)
    (dolist (elt (symbol-value minibuffer-history-variable))
      (unless (member elt list)
        (push elt list)))
    (set minibuffer-history-variable (nreverse list))))
(add-hook 'minibuffer-setup-hook 'minubuffer-delete-duplicate)


;;-----------------------------------------------------------------------
;; minibuf-isearch.el
;;-----------------------------------------------------------------------
(require 'minibuf-isearch)


;;-----------------------------------------------------------------------
;; ibuffer.el
;;-----------------------------------------------------------------------

;    align text vertically
(setq ibuffer-formats
      '((mark modified read-only " " (name 30 30)
              " " (size 6 -1) " " (mode 16 16) " " filename)
        (mark " " (name 30 -1) " " filename)))


;;-----------------------------------------------------------------------
;; howm
;;-----------------------------------------------------------------------

; (setq howm-menu-lang `ja)

;    save directory
(setq howm-directory (expand-file-name "~/howm/"))

;    display title when show list of recent memo
(setq howm-list-recent-title t)

;    number of recentMemo in menu
(setq howm-menu-recent-num 20)

;    display title when show list of all memo
(setq howm-list-all-title t)


;;-----------------------------------------------------------------------
;; ChangeLog
;;-----------------------------------------------------------------------

(setq add-log-full-name "foo")
(setq add-log-mailing-address "foo@example.com")


;;-----------------------------------------------------------------------
;; navi2ch
;;-----------------------------------------------------------------------
(when (require 'navi2ch nil t)

  ; follow the change of bbsmenu
  (setq navi2ch-list-bbstable-url "http://menu.2ch.net/bbsmenu.html")

  ; monafont
  (setq navi2ch-mona-enable t)
  (setq navi2ch-mona-face-variable 'navi2ch-mona14-face))


;;-----------------------------------------------------------------------
;; woman
;;-----------------------------------------------------------------------

(setq woman-fill-column 105)

(setq woman-cache-filename (expand-file-name "~/.wmncache"))

;    don't open in new frame
(setq woman-use-own-frame nil)

(if (featurep 'meadow)
    (setq woman-manpath '(
                          "c:/cygwin/usr/share/man"
                          "c:/cygwin/usr/man"
;                          "c:/cygwin/usr/local/man"
                          "c:/cygwin/local/man"
                          "c:/cygwin/usr/ssl/man"
                          "c:/cygwin/usr/X11R6/man"
                          "c:/cygwin/usr/share/man/ja"
                          "c:/cygwin/usr/man/ja"
                          )))

;;-----------------------------------------------------------------------
;; sense-region.el
;;-----------------------------------------------------------------------

;; ;; remove 'isearch-forward' from 'sense-region-adviced-functions'
;; ;; because the problem not to work isearch-forward to move cursor
;; ;; but work isearch-forward in region.
;; ;; I don't want this behavior.
;; (setq sense-region-adviced-functions
;;   '((set-mark-command . sense-region-set-mark)
;;     kill-ring-save kill-region yank yank-rectangle
;;     open-line comment-region indent-for-tab-command query-replace query-replace-regexp))

;; ;; C-Space to enable a rectangle region visibility when selecting a region.
;; (require 'sense-region)
;; (sense-region-on)


;;-----------------------------------------------------------------------
;; thing-opt.el
;;-----------------------------------------------------------------------

(require 'thing-opt)
(define-thing-commands)


;;-----------------------------------------------------------------------
;; recentf-ext
;;-----------------------------------------------------------------------

(require 'recentf-ext)


;;-----------------------------------------------------------------------
;; shell-pop.el
;;-----------------------------------------------------------------------

(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-internal-mode-shell "zsh")

(add-hook 'term-mode-hook
  #'(lambda () (setq autopair-dont-activate t)))


;;-----------------------------------------------------------------------
;; magit.el
;;-----------------------------------------------------------------------

(autoload 'magit-status "magit" nil t)


;;-----------------------------------------------------------------------
;; smooth-scrolling.el
;;-----------------------------------------------------------------------

(require 'smooth-scrolling)


;;-----------------------------------------------------------------------
;; open-junk-file
;;-----------------------------------------------------------------------

(require 'open-junk-file)

(setq open-junk-file-format (expand-file-name "junk/%Y/%m/%d-%H%M%S." hyone:emacs-home))

