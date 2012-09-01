(require 'rst)
(require 'hyone-util)


(setq auto-mode-alist
      (append `(("\\.rst$" . rst-mode)
                ("\\.rest$" . rst-mode)) auto-mode-alist))


(defun rst-surround (start end sword &optional eword)
  (save-excursion
    (goto-char end)
    ;; if eword is nil, use sword
    (insert (or eword sword))
    (goto-char start)
    (insert sword)))

(defun rst-emphasize (start end)
  (interactive (hyone:region-mark-or-word))
  (rst-surround start end "**"))

(defun rst-italic (start end)
  (interactive (hyone:region-mark-or-word))
  (rst-surround start end "*"))

(defun rst-literize (start end)
  (interactive (hyone:region-mark-or-word))
  (rst-surround start end "``"))


;; prefix key mapping in local mode map.
(defvar rst-mode-prefix nil)
(define-prefix-command 'rst-mode-prefix)

(add-hook 'rst-mode-hook
          (lambda ()
            (local-set-key (kbd "C-,") 'rst-mode-prefix)

            (define-key 'rst-mode-prefix [(b)] 'rst-emphasize)
            (define-key 'rst-mode-prefix [(i)] 'rst-italic)
            (define-key 'rst-mode-prefix [(l)] 'rst-literize)

            (define-key rst-mode-map (kbd "C-c 1") nil)
            (define-key rst-mode-map (kbd "C-c 2") nil)
            (define-key rst-mode-map (kbd "C-c 3") nil)
            (define-key rst-mode-map (kbd "C-c 4") nil)))
