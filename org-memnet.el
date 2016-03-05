;;; -*- coding: utf-8-unix -*-
;;; org-memnet.el - Memorize things.
;;;
;;; Author: Mark Scala <markscala@gmail.com>
;;; Version: 0.0.1
;;;
;;; Synopsis
;;; ========
;;;
;;; A simple memory trainer, because org-drill is broken and I can't
;;; wait.

(require 'org)

;; nicked these next two from the internet, for shuffling lists.
(defun swap (LIST el1 el2)
  "in LIST swap indices EL1 and EL2 in place"
  (let ((tmp (elt LIST el1)))
    (setf (elt LIST el1) (elt LIST el2))
    (setf (elt LIST el2) tmp)))

(defun shuffle (LIST)
  "Shuffle the elements in LIST.
shuffling is done in place."
  (loop for i in (reverse (number-sequence 1 (1- (length LIST))))
        do (let ((j (random (+ i 1))))
             (swap LIST i j)))
  LIST)

(defun org-memnet-learned (pom)
  "True just when we've learned an item."
  (= (string-to-number (org-entry-get pom "GRASP")) 5))

(defun org-memnet-get-drill-items ()
  "Collect the (locations of) items to be drilled."
  (let ((res '()))
    (goto-char (point-min))
    (while (not (org-at-heading-p))
      (forward-line))
    (when (not (org-memnet-learned (point)))
      (push (point-marker) res))
    (while (org-get-next-sibling)
      (when (not (org-memnet-learned (point)))
        (push (point-marker) res)))
    res))

(defun next-or-end ()
  "Query the user to continue or quit."
  (let ((k (read-string "q-or-any: ")))
    (if (string-equal k "q")
        t)))

(defun get-self-evaluation ()
  "Query the user for self-evaluation."
  (let ((val (read-string "0-2: still not learned\n3-4: learned, but not perfectly so\n5: you've nailed it\n--------------------------\n RATE YOURSELF: ")))
    (while (not (member val (list "0" "1" "2" "3" "4" "5")))
      (setq val (read-string "Try again dummy! Evaluate 0-5: ")))
    val))

(defun org-memnet-reset-outline ()
  "Reset outline to the correct starting position."
  (org-global-cycle 4)
  (widen))

(defun org-memnet-drill ()
  "Run a drill session.

Nothing fancy here. If an item is not perfectly well known (rated
5), we review it."
  (interactive)
  (save-excursion
    (let ((items (shuffle (org-memnet-get-drill-items))))
      (cond
       ((null items)
        (message "Nothing to revue!"))
       (t
        (org-memnet-reset-outline)
        (block 'while-loop
          (while items
            (let ((curr (pop items)))
              (org-goto-marker-or-bmk curr)
              (org-narrow-to-subtree)
              (save-excursion
                (org-goto-first-child)
                (org-cycle))
              (when (next-or-end)
                (widen)
                (return-from 'while-loop))
              (org-show-subtree)
              (let ((res (get-self-evaluation))
                    (fmt
                     (concat "["
                             (substring (cdr org-time-stamp-formats) 1 -1)
                             "]")))
                (org-entry-put curr "DATE_LAST_REVIEWED" (format-time-string fmt))
                (org-entry-put curr "GRASP" res)
                (when (< (string-to-number res) 3)
                  (setq items (shuffle (push curr items)))))
              (org-memnet-reset-outline))))))
      (widen))))
