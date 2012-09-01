(require 'evil)
(require 'hyone-elscreen)


; moccur in dired mode
(add-hook 'dired-mode-hook
  (lambda ()
    (evil-declare-key 'normal dired-mode-map
      "H" 'hyone:elscreen-cycle-previous ;; override dired-do-hardlink
      "L" 'hyone:elscreen-cycle-next     ;; override dired-do-load
      "O" 'dired-do-moccur
      "r" 'dired-mark-files-regexp ;; override dired-redisplay
      )))

;; replace words of files recursively in directory
;; 1. search by dired-do-moccur
;; 2. moccur-edit (C-x C-q)
;; 3. finish (C-x C-s)
;; see also: Meadow/Emacs memo: バッファの検索 ― occur - http://www.bookshelf.jp/soft/meadow_50.html#SEC769