(defpackage :adventofcode2025
  (:nicknames :advent2025)
  (:use :cl :cl-ppcre :alexandria)
  (:export :validate))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((package (n)
           `(defpackage ,(format nil "ADVENTOFCODE2025/DAY~2,'0D" n)
              (:shadow :validate)
              (:use :cl :advent2025)
              (:export :part-1
                       :part-2
                       :validate))))
    (dotimes (i 12)
      (eval (package (+ i 1))))))
