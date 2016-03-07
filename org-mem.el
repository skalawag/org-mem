;;; -*- coding: utf-8-unix -*-
;;; org-mem.el - Memorize things.
;;;
;;; Author: Mark Scala <markscala@gmail.com>
;;; Version: 0.0.1
;;; License: MIT
;;;
;;; Synopsis
;;; ========
;;;
;;; Org-mem is a simple memory trainer, because org-drill is broken
;;; and I can't wait.
;;;
;;; An item for memorizing is assumed to be a top-level headline with
;;; (at least) two subheadings. The first subheading is understood to
;;; be the prompt and will be shown first, with all subsequent
;;; subheadings folded. After the user checks her memory, she can
;;; press any key to see the answer (in fact, all later subheadings
;;; will be revealed at once). At the same time, the user will be
;;; prompted to evaluate how well she has learned the item. The user
;;; may exit at any time with `C-g', or with `q' when prompted, or
;;; else she can continue until all the items are learned to at least
;;; level 3.

(require 'org)

(defun shuffle (lis)
  "Shuffle the elements in lis."
  (loop for i in (reverse (number-sequence 1 (1- (length lis))))
        do (let ((j (random (+ i 1)))
		 (tmp (elt lis i)))
	     (setf (elt lis i) (elt lis j))
	     (setf (elt lis j) tmp)))
  lis)

(defun org-mem-learned-p (pom)
  "True just when we've learned an item."
  (= (string-to-number (org-entry-get pom "GRASP")) 5))

(defun org-mem-goto-first-heading ()
  (goto-char (point-min))
    (while (not (org-at-heading-p))
      (forward-line)))

(defun org-mem-get-drill-items ()
  "Collect the (locations of) items to be drilled."
  (let ((res '()))
    (goto-char (point-min))
    (org-mem-goto-first-heading)
    (when (not (org-mem-learned-p (point)))
      (push (point-marker) res))
    (while (org-get-next-sibling)
      (when (not (org-mem-learned-p (point)))
        (push (point-marker) res)))
    res))

(defun quit-or-continue ()
  "Query the user to continue or quit."
  (let ((k (read-string "Continue (RET) or Quit (q): ")))
    (if (string-equal k "q")
        t)))

(defun get-self-evaluation ()
  "Query the user for self-evaluation."
  (let ((val (read-string "0-2: still not learned\n3-4: learned, but not perfectly so\n5: you've nailed it\n--------------------------\n\n RATE YOURSELF: ")))
    (while (not (member val (list "0" "1" "2" "3" "4" "5")))
      (setq val (read-string "Try again dummy! Evaluate 0-5: ")))
    val))

(defun org-mem-drill ()
  "Run a drill session."
  (interactive)
  (save-excursion
    (let ((items (shuffle (org-mem-get-drill-items))))
      (cond
       ((null items)
        (message "Nothing to review!"))
       (t
        (block 'while-loop
          (while items
            (let ((curr (pop items)))
	      (widen)
	      (org-global-cycle 4)
              (org-goto-marker-or-bmk curr)
              (org-narrow-to-subtree)
              (save-excursion
                (org-goto-first-child)
                (org-cycle))
              (when (quit-or-continue)
                (widen)
                (return-from 'while-loop))
              (org-show-subtree)
              (let ((res (get-self-evaluation)))
                (org-entry-put curr "GRASP" res)
                (when (< (string-to-number res) 3)
                  (setq items (shuffle (push curr items))))))))))
      (widen))))

;;----------------------------------------------------------------
;;; Statisitics
;;----------------------------------------------------------------
(defun org-mem-total-item-count ()
  (save-excursion
    (let ((count 1))
      (org-mem-goto-first-heading)
      (while (org-get-next-sibling)
	(setq count (+ count 1)))
      count)))

(defun org-mem-learned-count ()
  (save-excursion
    (let ((count 0))
      (org-mem-goto-first-heading)
      (while (org-get-next-sibling)
	(when (org-mem-learned-p (point))
	  (setq count (+ 1 count))))
      count)))
