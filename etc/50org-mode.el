;; (require 'org-install)

;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; (setq org-log-done t)

;; ;; open holding texts when startup
;; (setq org-startup-truncated nil)

;; (setq org-return-follows-link t)

;; (setq org-directory "~/org/")

;; (setq org-default-notes-file (expand-file-name "agenda.org" org-directory))

;; (setq org-agenda-files (mapcar (lambda (f) (expand-file-name f org-directory))
;;                                '("agenda.org" "test1.org")))

;; (org-remember-insinuate)

;; (setq org-remember-templates
;;       '(("Todo" ?t "** TODO %?\n   %i\n   %a\n   %t"         nil "Inbox")
;;         ("Bug"  ?b "** TODO %?   :bug:\n   %i\n   %a\n   %t" nil "Inbox")
;;         ("Idea" ?i "** %?\n   %i\n   %a\n   %t"              nil "New Ideas")))
