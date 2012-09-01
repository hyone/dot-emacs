(require 'coffee-mode)

(defun hyone:coffee-jump-to-compiled-js ()
  (interactive)
  (let ((path (buffer-file-name)))
    (if (and path (string-match "\\.coffee$" path))
        (find-file (replace-regexp-in-string "\\.coffee$" ".js" (buffer-file-name)))
      (message "Current buffer is not a coffeescript file."))))


(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(add-hook 'coffee-mode-hook
          (lambda ()
            (setq tab-width 2)
            (setq evil-shift-width 2)
            (evil-declare-key 'normal coffee-mode-map
              (kbd "C-c r") 'coffee-compile-file
              (kbd "C-c j") 'hyone:coffee-jump-to-compiled-js)
            ;; (evil-declare-key 'insert coffee-mode-map
            ;;   (kbd "RET") (lambda ()
            ;;                 (interactive)
            ;;                 (hyone:newline-dwim (lambda ()
            ;;                                       (newline)
            ;;                                       (coffee-indent-line)))))
            ))


;; flymake
;;-----------------------------------------------------------------------

(setq flymake-coffeescript-err-line-patterns
  '(("\\(Error: In \\([^,]+\\), .+ on line \\([0-9]+\\).*\\)" 2 3 nil 1)))

(defconst flymake-allowed-coffeescript-file-name-masks
  '(("\\.coffee$" flymake-coffeescript-init)))

(defun flymake-coffeescript-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                     'flymake-create-temp-inplace))
         (local-file (file-relative-name
                      temp-file
                      (file-name-directory buffer-file-name))))
    (list "coffee" (list local-file))))

(defun flymake-coffeescript-load ()
  (interactive)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check)
  (setq flymake-allowed-file-name-masks
        (append flymake-allowed-file-name-masks
                flymake-allowed-coffeescript-file-name-masks))
  (setq flymake-err-line-patterns flymake-coffeescript-err-line-patterns)
  (flymake-mode t))

(add-hook 'coffee-mode-hook 'flymake-coffeescript-load)