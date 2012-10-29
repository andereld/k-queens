(load  "k-queens.cl")
(in-package :org.eldhuset.k-queens)

(defun horizontal-rule (len)
  (make-array len :element-type 'character :initial-element #\-))

(defun main (&optional (k 16))
  (progn
    (defparameter board (generate-random-board k))
    (defparameter iterations 0)

    (format t "Problem~%")
    (format t "~a~%" (horizontal-rule (1- (* 2 (length board)))))
    (print-board board)
    (format t "~%")
    (setf (values board iterations) (min-conflicts board))
    (if board
      (progn
        (format t "Solution (found after ~a iterations)~%" iterations)
        (format t "~a~%" (horizontal-rule (1- (* 2 (length board)))))
        (print-board board)))))

(defmacro average-time (form &optional (iterations 5))
  `(let ((start (get-internal-real-time)))
    ,@(loop for i from 1 to iterations collecting form)
    (float (/ (- (get-internal-real-time) start)
              (* ,iterations internal-time-units-per-second)))))

(defun time-stats ()
  (with-open-file (file "stats.txt"
                        :direction :output
                        :if-exists :supersede)
    (loop for i from 10 to 100 by 10 do
          (format file "~a~t~a~%"
                  i
                  (average-time (min-conflicts (generate-random-board i)))))))
