
;;-----------------------------------------------------------------------
;; Tramp
;;-----------------------------------------------------------------------

;; Meadow ( Windows )
(when (featurep 'meadow)
  (setq tramp-default-method "plink")
  (setq-default tramp-completion-without-shell-p t)
  (setq-default tramp-shell-prompt-pattern "^[ $]+")
  (setq-default tramp-debug-buffer t)
  (modify-coding-system-alist 'process "plink.exe" 'utf-8-unix))

;; Other ( Unix )
(when (not (featurep 'meadow))
  (require 'tramp)
  (setq tramp-default-method "ssh")
  (setq-default tramp-completion-without-shell-p t)
  (setq-default tramp-debug-buffer t)
  (setq-default tramp-terminal-type "dumb")
  ;; (setq-default tramp-shell-prompt-pattern "^[^$]*\\$ *")
  (setq tramp-rsh-end-of-line "\r")

  ;; (add-to-list 'tramp-remote-coding-commands
  ;;   '(b64 "base64" "base64 -d -i"))

  ;; To avoid the problem cut contents of a file and raise base64 error when writing it
  ;; (setq tramp-chunksize 360)

  ;; (nconc (cadr (assq 'tramp-login-args (assoc "ssh" tramp-methods))) '("-t" "/bin/bash"))
  ;; (setcdr (assq 'tramp-remote-sh (assoc "ssh" tramp-methods)) '("/bin/bash"))

  (setq tramp-verbose 8)
)
;;   (setcdr (assq 'tramp-login-program
;;                 (assoc "sshx" tramp-methods))
;;           (list "/opt/local/bin/ssh"))
 

;; ange-ftp
(setq ange-ftp-generate-anonymous-password "hoge@exmpale.com")
;; (setq ange-ftp-ftp-program-name "c:/cygwin/bin/ftp.exe")
;; (setq ange-ftp-try-passive-mode t)
;; (setq ange-ftp-binary-file-name-regexp ".*")

;; browse-url
(setq browse-url-browser-function `browse-url-generic)
(if (featurep 'meadow)
    (setq browse-url-generic-program "c:/Program Files/Mozilla Firefox/firefox.exe")
  (setq browse-url-generic-program "mozremote.sh"))
