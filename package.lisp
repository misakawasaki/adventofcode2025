(defpackage :adventofcode2025
  (:nicknames :advent2025)
  (:use :cl)
  (:export :validate))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((package (n)
           `(defpackage ,(format nil "ADVENTOFCODE2025/DAY~d" n)
              (:use :cl)
              (:export :part-1
                       :part-2
                       :validate))))
    (dotimes (i 12)
      (eval (package (+ i 1))))))
