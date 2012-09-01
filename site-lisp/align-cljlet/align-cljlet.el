;;; align-cljlet.el  -- Align clojure let functions

;; Copyrigth (C) 2011  Glen Stampoultzis

;; Author: Glen Stampoultzis <gstamp(at)gmail.com>
;; Version: $Id:$
;; Keywords; clojure, align, let
;; URL: https://github.com/gstamp/align-cljlet
;;

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;; Description:
;;
;; This program exists because I was tired of manually aligning let
;; statements in clojure.  This program is designed to quickly and
;; easily allow let forms to be aligned.  This is my first emacs
;; lisp program and as a result if probably less than optimal.  Feel
;; free to suggest improvements or send in patches.
;;
;; This program was inspired by align-let.el although does not share
;; any of it's code.  I had considered altering align-let.el to
;; work correctly with Clojure however it was easiler to simply
;; start from scratch.
;; 
;;
;;; Known limitations:
;;
;; * This program requires clojure mode to be running in order to
;;   function correctly.
;;
;;; Installation:
;;
;;   To use align-cljlet.el, put it in your load-path and add
;;   the following to your .emacs
;;
;;   (require 'align-cljlet)
;;
;;; Usage:
;;
;; To invoke simply position anywhere inside the let statement and
;; invoke:
;;
;; M-x align-cljlet
;;
;; You may wish to bound this to a specific key.
;;


(defun try-go-up ()
  "move point up to parent expression"
  (condition-case nil
      (up-list -1)
    (error "Form does not match"))
  t)

(defun get-width ()
  "return width of expression starting at point"
  (save-excursion
    (let ((p (current-column)))
      (forward-sexp)
      (- (current-column) p))))

(defun find-start (regexp)
  "find start of expression matching regexp"
  (while (if (looking-at regexp)
             nil
           (try-go-up)))
  t)

(defun goto-next (next-function)
  "calls function `next-function`"
  (interactive)
  (condition-case nil
      (progn
        (funcall next-function)
        t)
    (error nil)))

(defun next-sexp ()
  "moves point forward an expression"
  (forward-sexp)
  (forward-sexp)
  (backward-sexp))

(defun calc-widths-row (test max-cols)
  (if (funcall test)
      (loop for i from 1
            collect (get-width)
            while (and (< i max-cols) (goto-next #'next-sexp)))))

(defun calc-widths (test next-row max-cols) 
  "returns list of list of widths of expressions for rows that pass `test` function"
  (save-excursion
    (loop collect (calc-widths-row test max-cols)
          while (goto-next next-row))))

(defun respace-row (widths test)
  "inserts/removes spaces based on `widths` list"
  (save-excursion
    (if (funcall test)
        (save-excursion
          (loop for w in widths do
                (let ((p (current-column)))
                  (goto-next #'next-sexp)
                  (let* ((current-width (1- (- (current-column) p)))
                         (difference    (- w current-width)))
                    (cond ((> difference 0)
                           (insert (make-string difference ? ))) 
                          ((< difference 0)
                           (delete-backward-char (abs difference)))))))))))

(defun respace-rows (widths test next-row)
  (loop do (respace-row widths test) while (goto-next next-row)))

(defun max-widths (widths)
  "returns list of max widths for each expression column given `widths` list of lists"
  (loop for n from 0 to (1- (length (first widths)))
        collect (apply #'max (mapcar (lambda (coll)
                                       (if (< (length coll) (1+ n))
                                           1
                                         (nth n coll)))
                                     widths))))

(defun align-section (start goto-start test next-row max-cols &optional argh)
  "worker. tries to align based on `start` regexp, `goto-start`
function, `test` function, and `next-row` function"
  (interactive)
  (save-excursion
    (if (find-start start)
        (progn (funcall goto-start)
               (respace-rows (butlast (max-widths (calc-widths test next-row max-cols)))
                             test (or argh next-row))))))

(defun default-goto-start ()
  (down-list 2))

(defun default-test () t)

(defun argh ()
  "the only excuse for this is that there was a wierd bug with
let that I couldn't find and I got tired of looking. for some reason next-sexp wasn't working as next-row arg to respace-rows "
  (forward-sexp)
  (next-sexp))

(defun align-cljlet ()
  (interactive)
  (align-section "\\s(let" #'default-goto-start #'default-test #'next-sexp 2 #'argh))

(defun defroutes-next-row ()
  (backward-up-list)
  (forward-sexp)
  (forward-sexp)
  (backward-sexp)
  (down-list))

(defun defroutes-test ()
  (memq (symbol-at-point) '(GET POST)))

(defun align-defroutes ()
  (interactive)
  (align-section "\\s(defroutes" #'default-goto-start #'defroutes-test
                 #'defroutes-next-row 100))

(provide 'align-cljlet)

(defun align-map ()
  (interactive)
  (align-section "{" (lambda () (forward-char)) #'default-test
                 #'next-sexp 2 #'argh))