(in-package :adventofcode2025/Day02)

(defparameter +solution-1+ 30608905813)
(defparameter +solution-2+ 31898925685)

(defun parse-ranges (input)
  (mapcar (lambda (piece)
            (cl-ppcre:register-groups-bind (start end)
                                           ("^(\\d+)-(\\d+)$" piece)
                                           (cons (parse-integer start)
                                                 (parse-integer end))))
          (cl-ppcre:split "\\s*,\\s*" input)))

(defun invalid-p (id rule)
  (cl-ppcre:scan rule (write-to-string id)))

(defun sum-invalidid-in-range (range rule)
  (destructuring-bind (start . end) range
    (loop for id from start to end
          when (invalid-p id rule)
          sum id)))

(defun evaluate (pathname rule)
  (reduce #'+
          (mapcar 
            (lambda (range) (sum-invalidid-in-range range rule))
            (parse-ranges (uiop:read-file-string pathname)))))

(defun part-1 ()
  (evaluate (input-pathname) "^(\\d+)\\1$"))

(defun part-2 ()
  (evaluate (input-pathname) "^(\\d+)\\1+$"))
