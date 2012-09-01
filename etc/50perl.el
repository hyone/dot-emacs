(defalias 'perl-mode 'cperl-mode)

(setq cperl-indent-level 4)
(setq cperl-brace-offset -4)
(setq cperl-close-paren-offset -4)
(setq cperl-indent-parens-as-block t)
; insert automatically the corresponding parenthesis
(setq cperl-electric-parens t)
(setq cperl-tab-always-indent t)
(setq cperl-highlight-variables-indiscriminately t)

(add-to-list 'auto-mode-alist '("\\.psgi$" . perl-mode))
(add-to-list 'auto-mode-alist '("\\.t$"    . perl-mode))


(require 'auto-complete)
(require 'perl-completion)

(add-hook 'cperl-mode-hook
  '(lambda ()
     (when (featurep 'evil)
       ; view perldoc for the name on the cursor.
       (evil-define-key 'normal cperl-mode-map "\\c" 'plcmp-cmd-show-doc-at-point)
       ; view perldoc by anything interface
       (evil-define-key 'normal cperl-mode-map "\\h" 'plcmp-cmd-show-doc))

     (add-to-list 'ac-sources 'ac-source-perl-completion)
     (perl-completion-mode t)))


(defun cperl-indent-region-or-line (start end)
  "indent region or line in cperl mode"
  (interactive (region-mark-or-line))
  (save-excursion
    (cperl-indent-region start end)))


(defun run-perl-method-test ()
  "Run a single test of Test::Class under the cursor"
  (interactive)
  (let ((command compile-command))
    (save-excursion
      (when (or
             (re-search-backward "\\bsub\s+\\([_[:alpha:]]+\\)\s*:\s*Test" nil t)
             (re-search-forward "\\bsub\s+\\([_[:alpha:]]+\\)\s*:\s*Test" nil t))
        (setq command
              (format "TEST_METHOD=%s perl -w %s"
                      (match-string 1) (expand-file-name buffer-file-name)))))
    (when command (compile command))))


;;-----------------------------------------------------------------------
;; with perlbrew
;;-----------------------------------------------------------------------

(defvar perlbrew-current-perl-path nil)

(defun perlbrew-get-current-perl-path ()
  (unless perlbrew-current-perl-path
    (setq perlbrew-current-perl-path
          (replace-regexp-in-string "\n+$" "" (shell-command-to-string "which perl"))))
  perlbrew-current-perl-path)

(defun perlbrew-set-current-exec-path ()
  (add-to-list 'exec-path (file-name-directory (perlbrew-get-current-perl-path)))
  ;; set PATH to be the same as exec-path
  (setenv "PATH" (mapconcat 'identity exec-path ":")))

(add-hook 'cperl-mode-hook
  '(lambda ()
     (perlbrew-set-current-exec-path)))


;;-----------------------------------------------------------------------
;; flymake
;;-----------------------------------------------------------------------

(require 'flymake)
(require 'set-perl5lib)

(defvar flymake-perl-err-line-patterns '(("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)))
(defconst flymake-allowed-perl-file-name-masks '(("\\.pl$"    flymake-perl-init)
                                                  ("\\.pm$"   flymake-perl-init)
                                                  ("\\.t$"    flymake-perl-init)
                                                  ("\\.psgi$" flymake-perl-init)
                                                  ))


(defun flymake-perl-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                      'flymake-create-temp-inplace))
          (local-file (file-relative-name
                        temp-file
                        (file-name-directory buffer-file-name))))
    (list (perlbrew-get-current-perl-path) (list "-wc" local-file))))


(defun flymake-perl-load ()
  (interactive)
  (set-perl5lib)
  (defadvice flymake-post-syntax-check (before flymake-force-check-was-interrupted)
    (setq flymake-check-was-interrupted t))
  (ad-activate 'flymake-post-syntax-check t)
  (setq flymake-allowed-file-name-masks (append flymake-allowed-file-name-masks flymake-allowed-perl-file-name-masks))
  (setq flymake-err-line-patterns flymake-perl-err-line-patterns)
  (flymake-mode t))


(add-hook 'cperl-mode-hook
  '(lambda ()
     (flymake-perl-load)))