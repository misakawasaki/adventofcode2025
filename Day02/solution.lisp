(in-package :adventofcode2025/Day02)

(defparameter +solution-1+ 30608905813)

(defun parse-ranges (input)
  (mapcar (lambda (piece)
            (cl-ppcre:register-groups-bind (start end)
                                           ("^(\\d+)-(\\d+)$" piece)
                                           (cons (parse-integer start)
                                                 (parse-integer end))))
          (cl-ppcre:split "\\s*,\\s*" input)))

(defun invalid-p (id)
  (if (cl-ppcre:scan "^(\\d+)\\1$" (write-to-string id))
      t
      nil))

(defun parse-range (range)
  (let ((start (car range))
        (end   (cdr range)))
    (loop for id from start to end
          when (invalid-p id)
          collect id)))

(defun part-1 ()
  (reduce #'+
          (mapcan
            (lambda (range) (parse-range range))
            (parse-ranges (uiop:read-file-string (input-pathname))))
          :initial-value 0))
