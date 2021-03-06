;;; -*- coding: utf-8-unix -*-
;;; org-mem.el - Memorize things.
;;;
;;; Author: Mark Scala <markscala@fastmail.fm>
;;; Version: 0.0.1
;;;
;;; Synopsis
;;; ========
;;;
;;; A very simple memory trainer.

(eval-when-compile (require 'cl))
(require 'org)

(load "~/dev/elisp/my-org-drill/utils.el")

(defun org-mem-learned (pom)
  "True just when we've learned an item."
  (if (org-entry-get pom "SCORE")
      (= (string-to-number (org-entry-get pom "SCORE")) 5)
    (org-entry-put pom "SCORE" "0")))

(defun org-mem-get-drill-items ()
  "Collect the (locations of) items to be drilled."
  (let ((res '()))
    (goto-char (point-min))
    (while (not (org-at-heading-p))
      (forward-line))
    (when (not (org-mem-learned (point)))
      (push (point-marker) res))
    (while (org-get-next-sibling)
      (when (not (org-mem-learned (point)))
        (push (point-marker) res)))
    res))

(defun quit-or-continue ()
  "Query the user to continue or quit."
  (let ((k (read-string "q-or-ENTER: ")))
    (if (string-equal k "q")
        t)))

(defun get-self-evaluation ()
  "Query the user for self-evaluation."
  (let ((val (read-string "0-2: still not learned\n3-4: learned, but not perfectly so\n5: you've nailed it\n--------------------------\n\n RATE YOURSELF: ")))
    (while (not (member val (list "0" "1" "2" "3" "4" "5")))
      (setq val (read-string "Try again dummy! Evaluate 0-5: ")))
    val))

(defun org-mem-reset-outline ()
  "Reset outline to the correct starting position."
  (org-global-cycle 4)
  (widen))

(defun org-mem-show (marker-or-bmk)
  "Show all subheadings of the current drill item."
  (save-excursion
    (org-mem-reset-outline)
    (org-goto-marker-or-bmk marker-or-bmk)
    (org-narrow-to-subtree)
    (org-cycle '(64))))

(defun org-mem-count-headlines ()
  "for now, count(headlines) == count(drillable items)"
  (interactive)
  (let ((res 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (org-at-heading-p))
	(forward-line))
      (if (org-at-heading-p)
	  (setq res (+ 1 res)))
      (while (org-get-next-sibling)
	(setq res (+ 1 res)))
      (message "%d" res))))

(defun org-mem-new (s)
  "Add required properties for a new entry."
  (interactive "sNumber or 'rare' or 'none': ")
  (org-entry-put (point) "SCORE" "0")
  (org-entry-put (point) "RANK" s))

(defun org-mem-drill ()
  "Run a drill session.

Nothing fancy here. If an item is not perfectly well known (rated
5), we review it."
  (interactive)
  (save-excursion
    (let ((items (shuffle (org-mem-get-drill-items))))
      (cond
       ((null items)
        (message "Nothing to review!"))
       (t
        (org-mem-reset-outline)
        (block 'while-loop
          (while items
            (let ((curr (pop items)))
              (org-goto-marker-or-bmk curr)
              (org-narrow-to-subtree)
              (when (quit-or-continue)
                (widen)
                (return-from 'while-loop))
	      (org-mem-show curr)
              (let ((res (get-self-evaluation))
                    (fmt
                     (concat "["
                             (substring (cdr org-time-stamp-formats) 1 -1)
                             "]")))
                (org-entry-put curr "DATE_LAST_REVIEWED" (format-time-string fmt))
                (org-entry-put curr "SCORE" res)
                (when (< (string-to-number res) 3)
                  (setq items (shuffle (push curr items)))))
              (org-mem-reset-outline))))))
      (widen))))
