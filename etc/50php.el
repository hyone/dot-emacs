(autoload 'php-mode "php-mode")

(setq auto-mode-alist (cons '("\\.php\\'" . php-mode) auto-mode-alist))
(setq php-mode-force-pear t)

(add-hook 'php-mode-hook
          (lambda ()
            ;; C-c C-f to open manual in a browser of the word at the cursor
            ;; (setq php-manual-path "/usr/local/share/php/doc/html")

            ;; adjust indentation
            ;; indentation parameter is referred below:
            ;; http://d.hatena.ne.jp/i_s/20091026/1256557730
            (c-set-offset 'arglist-close 0)
            (c-set-offset 'arglist-cont-nonempty 4)
            (c-set-offset 'arglist-intro 4)))
