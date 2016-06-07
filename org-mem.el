;;; -*- coding: utf-8-unix -*-
;;; org-mem.el - Memorize things.
;;;
;;; Author: Mark Scala <markscala@fastmail.fm>
;;; Version: 0.0.1
;;;
;;; Synopsis
;;; ========
;;;
;;; A simple memory trainer, because org-drill is broken and I can't
;;; wait.

(eval-when-compile (require 'cl))
(require 'org)

(load "~/dev/elisp/my-org-drill/utils.el")

(defun org-mem-learned (pom)
  "True just when we've learned an item."
  (= (string-to-number (org-entry-get pom "GRASP")) 5))

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

(defun org-mem-collect-prompts-and-answers ()
  "Collect point-markers for prompts and answers.

Start at top-level headline and search children for prompts and
answers. Return a list of two point-marker lists, the first for
prompts and the second for answers."
  (let ((prompts nil)
	(answers nil))
    (save-excursion
      (org-goto-first-child)
      (if (string= (org-entry-get (point) "TYPE") "prompt")
	  (push (point-marker) prompts))
      (if (string= (org-entry-get (point) "TYPE") "answer")
	  (push (point-marker) answers))
      (while (org-get-next-sibling)
	(if (string= (org-entry-get (point) "TYPE") "prompt")
	    (push (point-marker) prompts))
    	(if (string= (org-entry-get (point) "TYPE") "prompt")
	    (push (point-marker) answers))))
    (list prompts answers)))

(defun org-mem-show (marker-or-bmk)
  "Show all subheadings of the current drill item."
  (save-excursion
    (org-mem-reset-outline)
    (org-goto-marker-or-bmk marker-or-bmk)
    (org-narrow-to-subtree)
    (org-goto-first-child)
    (org-cycle)
    (while (org-get-next-sibling)
      (org-cycle))))

(defun display-item-or-prompt ()
  "Randomly select either the prompt or the answer to display."
  (save-excursion
    (if (= (random 2) 0)
	(progn
	  (org-goto-first-child)
	  (org-cycle))
      (progn
	(org-goto-first-child)
	(org-get-next-sibling)
	(org-cycle)))))

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
	      (display-item-or-prompt)
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
                (org-entry-put curr "GRASP" res)
                (when (< (string-to-number res) 3)
                  (setq items (shuffle (push curr items)))))
              (org-mem-reset-outline))))))
      (widen))))
