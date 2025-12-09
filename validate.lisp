(in-package :advent2025)

(defun validate ()
  (do ((i 1 (+ i 1)))
      ((> i 12) t)
    (let ((package (find-package (format nil "ADVENTOFCODE2025/DAY~2,'0D" i))))
      (when (and (boundp (intern "+SOLUTION-1+" package))
                 (boundp (intern "+SOLUTION-2+" package)))
        (funcall (intern "VALIDATE" pacakge))))))

(eval-when (:load-toplevel :execute)
  (validate))
