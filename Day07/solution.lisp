(in-package :adventofcode2025/Day07)

(defparameter +solution-1+ 1524)
(defparameter +solution-2+ 32982105837605)

(defun evaluate (input &key (adder #'logior))
  (let* ((rows (uiop:read-file-lines input))
         (active-beams (make-hash-table))
         (width (length (first rows)))
         (total-splitters 0))

    (setf (gethash (position #\S (first rows)) active-beams) 1)

    (loop for row-string in (cdr rows)
          do (let ((next-beams (make-hash-table)))
               (maphash
                (lambda (col count)
                  (when (< -1 col width)
                    (let ((char (char row-string col)))
                      (cond
                        ((char= char #\^)
                         (incf total-splitters)
                         (let ((left (1- col))
                               (right (1+ col)))
                           (setf (gethash left next-beams)
                                 (funcall adder (gethash left next-beams 0) count))
                           (setf (gethash right next-beams)
                                 (funcall adder (gethash right next-beams 0) count))))

                        ((char= char #\.)
                         (setf (gethash col next-beams)
                               (funcall adder (gethash col next-beams 0) count)))))))
                active-beams)
               (setf active-beams next-beams)))

    (values total-splitters
            (loop for count being the hash-values of active-beams
                  sum count))))

(defun part-1 ()
  (evaluate (input-pathname) :adder #'logior))

(defun part-2 ()
  (nth-value 1 (evaluate (input-pathname) :adder #'+)))
