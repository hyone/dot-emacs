
(setq undo-limit 100000)
(setq undo-strong-limit 150000)


;;-----------------------------------------------------------------------
;; undohist.el
;;-----------------------------------------------------------------------

(require 'undohist)
(undohist-initialize)


;;-----------------------------------------------------------------------
;; undo-tree.el
;;-----------------------------------------------------------------------

(require 'undo-tree)

(global-undo-tree-mode t)

;; remove C-? from undo-tree-map because it conflicts with help-for-help
(delq (assoc (aref (kbd "C-?") 0) undo-tree-map) undo-tree-map)

(define-key undo-tree-map (kbd "M-/") 'undo-tree-redo)

;; visualizer mode map
(define-key undo-tree-visualizer-map "j" 'undo-tree-visualize-redo)
(define-key undo-tree-visualizer-map "k" 'undo-tree-visualize-undo)
(define-key undo-tree-visualizer-map "l" 'undo-tree-visualize-switch-branch-right)
(define-key undo-tree-visualizer-map "h" 'undo-tree-visualize-switch-branch-left)

;; when typping 'q', quit with delete-window
(defadvice undo-tree-visualizer-quit
  (after delete-window activate)
  (delete-window))