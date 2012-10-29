(in-package :common-lisp-user)

;;;; The package ORG.ELDHUSET.K-QUEENS contains code for solving
;;;; the K queens problem as a CSP (constraint satisfaction problem)
;;;; using local search.
(defpackage :org.eldhuset.k-queens
  (:use :common-lisp))
(in-package :org.eldhuset.k-queens)

;; *k* is the number of queens, rows and columns.
(defparameter *k* 8)

;; *board* is a list representing our checkerboard; each element represents
;; a row and its value the index of the column where the queen is located.
(defparameter *board* nil)

(defun generate-random-board (size)
  "Generates a size-by-size board with randomly placed queens."
  (loop for i from 1 to size collect (random size)))

(defun print-board (board)
  "Prints an ASCII representation of the state of the given checkerboard."
  (dolist (queen board)
    (let ((row (make-array (1- (* 2 (length board)))
                           :element-type 'character
                           :initial-element #\ )))
      (dotimes (i (length row))
        (if (evenp i)
          (setf (elt row i) #\0)))
      (setf (elt row (* 2 queen)) #\1)
      (format t "~a~%" row))))

(defun vertical-attacks (board row)
  "Returns the number of vertical attacks for the row-th queen on the board."
  (let ((column (elt board row)))
    (- (count column board) 1)))

(defun diagonal-attacks (board row &optional (other-row 0) (result 0))
  "Returns the number of diagonal attacks for the row-th queen on the board."
  (cond
    ((= other-row (length board))
     result)
    ((= other-row row)
     (diagonal-attacks board row (1+ other-row) result))
    (t
     (let ((column (elt board row))
           (other-column (elt board other-row))
           (offset (abs (- row other-row))))
       (if (or (= other-column (+ column offset))
               (= other-column (- column offset)))
         (diagonal-attacks board row (1+ other-row) (1+ result))
         (diagonal-attacks board row (1+ other-row) result))))))

(declaim (inline total-attacks))
(defun total-attacks (board row)
  "Returns the total number of attacks available to the row-th queen
   on the board."
  (+ (vertical-attacks board row) (diagonal-attacks board row)))

(defun solutionp (board &optional (row 0))
  "Returns true if the board contains no queens in attack position,
   nil otherwise."
  (cond
    ((= row (length board)) t)            ; no attacks found
    ((> (total-attacks board row) 0) nil) ; attack found, return early
    (t (solutionp board (1+ row)))))      ; we're not done yet, recur

(declaim (inline move-queen))
(defun move-queen (board queen column)
  "Moves the queen to the specified column on the board. The move is done
   in-place, mutating the given board."
  (setf (elt board queen) column))

(declaim (inline copy-and-move-queen))
(defun copy-and-move-queen (board queen column)
  "Returns a copy of the board with the given queen's location altered."
  (let ((new-board (copy-list board)))
    (setf (elt new-board queen) column)
    new-board))

(defun min-conflicts (board &optional (max-iterations 1000) (iteration 1))
  "Attempts to solve the K queens problem represented by the given checkerboard.
   Returns the solution if found, else nil. The board is mutated regardless.

   min-conflicts also returns the number of the iteration in which a solution
   was found as a second return value.

   Based on MIN-CONFLICTS, p. 221 Norvig & Russel (2010)."
  (cond
    ((= max-iterations iteration) nil)           ; no solution found, return nil
    ((solutionp board) (values board iteration)) ; solution found, return it
    (t                                           ; keep looking
     (let* ((queen (random (length board)))
            (column (elt board queen))
            (current-attacks (total-attacks board queen)))
       (dotimes (i (length board))
         (if (and (/= i column)
                  (<= (total-attacks (copy-and-move-queen board queen i) queen)
                      current-attacks))
           (move-queen board queen i)))
     (min-conflicts board max-iterations (1+ iteration))))))


(defun main (&optional (k 8))
  (progn
    (setf *k* k)
    (setf *board* (generate-random-board *k*))
    (print-board *board*)
    (format t "~%")
    (min-conflicts *board*)
    (if *board*
      (print-board *board*))))
