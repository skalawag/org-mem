(defun shuffle (LIST)
  "Shuffle the elements in LIST.
shuffling is done in place."
  (loop for i in (reverse (number-sequence 1 (1- (length LIST))))
        do (let ((j (random (+ i 1)))
		 (tmp (elt LIST i)))
	     (setf (elt LIST i) (elt LIST j))
	     (setf (elt LIST j) tmp)))
  LIST)
