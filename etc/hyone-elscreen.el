;; have to be defined before requre elscreen
(setq elscreen-prefix-key "\C-t")

(require 'elscreen)

;; don't show screen number on mode-line
(setq elscreen-display-screen-number nil)


(defun hyone:elscreen-create-with-current-buffer ()
  "create the new screen with the current buffer"
  (interactive)
  (if (and (null (one-window-p))
        (< (elscreen-get-number-of-screens) 10))
    (let ((elscreen-split-buffer (current-buffer)))
      (delete-window)
      (elscreen-create)
      (switch-to-buffer elscreen-split-buffer))
    (elscreen-message "There is one window current tab!")))


(defun hyone:elscreen-cycle (count &optional previous)
  "Switch the next/previous screen cyclically by count."
  (cond
   ((elscreen-one-screen-p)
    (elscreen-message
     (format "You cannot escape from screen %d!"
             (elscreen-get-current-screen))))
   (t
    (let* ((screen-list (sort (elscreen-get-screen-list) (if previous '> '<)))
           (rest-screen-list (memq (elscreen-get-current-screen) screen-list))
           (i (mod count (length screen-list)))
           (next-screen
            (nth i (append rest-screen-list screen-list))))
      (elscreen-goto next-screen)))))

(defun hyone:elscreen-cycle-next (count)
  "Switch the next screen cyclically by count."
  (interactive "p")
  (hyone:elscreen-cycle count))

(defun hyone:elscreen-cycle-previous (count)
  "Switch the previous screen cyclically by count."
  (interactive "p")
  (hyone:elscreen-cycle count t))


(provide 'hyone-elscreen)