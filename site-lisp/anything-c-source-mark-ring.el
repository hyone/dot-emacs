(defvar anything-c-source-mark-ring
  '((name . "mark-ring")
    (candidates . anything-c-source-mark-ring-init)
    (init . anything-c-source-mark-ring-init)
    (action . (("Goto line" . (lambda (candidate)
                                (goto-line (string-to-number candidate))))))
    (persistent-action . (lambda (candidate)
                           (switch-to-buffer anything-current-buffer)
                           (goto-line (string-to-number candidate))
                           (set-window-start (get-buffer-window anything-current-buffer) (point))
                           (anything-persistent-highlight-point
                            (line-beginning-position)
                            (line-end-position))))
    ))
(defun anything-c-source-mark-ring-init ()
  (with-current-buffer anything-current-buffer
    (let* ((ring (cons (mark-marker) mark-ring))
           (lines (mapcar (lambda (pos)
                            (save-excursion
                              (goto-char pos)
                              (let ((line  (buffer-substring-no-properties (save-excursion (beginning-of-line) (point))
                                                                           (save-excursion (end-of-line) (point)))))
                                (when (string= "" line) (setq line  "<EMPTY LINE>"))
                                (format "%7d: %s" (current-line) line))))
                          ring)))
      (sort lines 'string<)
      )))

(provide 'anything-c-source-mark-ring)