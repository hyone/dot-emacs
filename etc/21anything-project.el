(require 'anything-project)

(setq ap:project-files-filters
  (list
    (lambda (files)
      (remove-if 'file-directory-p files))))

(ap:add-project :name 'perl
  :look-for '("Makefiel.PL" "Build.PL")
  :exclude-regexp '("/blib"))