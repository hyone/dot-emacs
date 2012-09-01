(require 'quickrun)

;; use prove on *.t file
(quickrun-add-command "perl/test"
                      '((:command . "prove")
                        (:exec    . "%c -v %s")
                        (:description . "Run Perl Test script")))

(add-to-list 'quickrun-file-alist '("\\.t$" . "perl/test"))

;; fix output buffer don't display until manually scrolling
(defadvice quickrun/apply-outputter (after hyone:fix-scroll-buffer activate)
  (recenter))


(when (require 'popwin nil t)
  (push '("*quickrun*" :height 20) popwin:special-display-config))


