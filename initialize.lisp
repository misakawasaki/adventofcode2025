(in-package :advent2025)

(defvar *initialized* nil)

(eval-when (:load-toplevel :execute)
  (unless *initialized*
    (setf *initialized* t)
    (loop for n from 1 below 13
          do (let* ((package (find-package (format nil "ADVENTOFCODE2025/DAY~2,'0D" n)))
                    (day (intern "+DAY+" package))
                    (soln1 (intern "+SOLUTION-1+" package))
                    (soln2 (intern "+SOLUTION-2+" package)))
              (eval `(progn
                       (defparameter ,day ,(format nil "Day~2,'0D" n))
                       
                       (defun ,(intern "SOLUTION-DIRECTORY" package) ()
                         (advent-pathname (make-pathname :directory (list :relative ,day))))
                       
                       (defun ,(intern "INPUT-PATHNAME" package) (&optional (,(intern "N" package) 0))
                         (merge-pathnames
                           (make-pathname :name (format nil "input~[~:;-~:*~d~]" ,(intern "N" package))
                                          :type "txt")
                           (,(intern "SOLUTION-DIRECTORY" package))))
                       
                       (defun ,(intern "SAMPLE-INPUT-PATHNAME" package) (&optinal (,(intern "N" package) 0))
                         (merge-pathnames
                           (make-pathname :name (format nil "sample-input~[~:;-~:*~d~]" ,(intern "N" package))
                                          :type "txt")
                           (,(intern "SOLUTION-DIRECTORY" package))))

                       (defun ,(intern "VALIDATE-1" package) ()
                         (cond
                           ((not (fboundp ',(intern "PART-1" package)))
                            (format t "~& Part-1 function not defined.~%"))
                           ((not (boundp ',soln1))
                            (format t "~& Part-1 solution not defined.~%"))
                           (t
                             (format t "~& Part-1~%")
                             (assert (equal (,(intern "PART-1" package)) ,soln1)))))

                       (defun ,(intern "VALIDATE-2" package) ()
                         (cond
                           ((not (fboundp ',(intern "PART-2" package)))
                            (format t "~& Part-2 function not defined.~%"))
                           ((not (boundp ',soln2))
                            (format t "~& Part-2 solution not defined.~%"))
                           (t
                             (format t "~& Part-2~%")
                             (assert (equal (,(intern "PART-2" package)) ,soln2)))))

                       (defun ,(intern "VALIDATE" package) ()
                         (format t "~&~A~%" ,day)
                         (,(intern "VALIDATE-1" package))
                         (,(intern "VALIDATE-2" package)))
                       
                       )))))
)
