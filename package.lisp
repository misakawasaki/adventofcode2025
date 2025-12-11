(defpackage :adventofcode2025
  (:nicknames :advent2025)
  (:shadowing-import-from :series :let :let* :multiple-value-bind :funcall :defun)
  (:use :cl :alexandria :series)
  (:export 
    :validate
    :row-list->grid
	  :+north+
	  :+northeast+
	  :+east+
	  :+southeast+
	  :+south+ 
	  :+southwest+ 
	  :+west+ 
	  :+northwest+
	  :2v+
	  :on-grid?
	  :scan-grid
	  :grid-ref))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (flet ((package (n)
           `(defpackage ,(format nil "ADVENTOFCODE2025/DAY~2,'0D" n)
              (:shadow :validate)
              (:use :cl :advent2025 :iterate)
              (:export :part-1
                       :part-2
                       :validate))))
    (dotimes (i 12)
      (eval (package (+ i 1))))))
