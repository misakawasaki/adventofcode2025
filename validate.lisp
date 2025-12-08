(in-package :advent2025)

(defun validate ()
  (do ((i 1 (+ i 1)))
      ((> i 12) t)
    (let ((package (find-package (format nil "ADVENTOFCODE2025/DAY~d" i))))
      (when (and (boundp (intern "+SOLUTION-1+" pacakge))
                 (boundp (intern "+SOLUTION-2+" package)))
        (funcall (intern "VALIDATE" pacakge))))))

(eval-when (:load-topleven :execute)
  (validate))
