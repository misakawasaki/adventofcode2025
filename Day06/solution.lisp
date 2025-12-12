(in-package :adventofcode2025/Day06)

(defparameter +solution-1+ 8108520669952)
(defparameter +solution-2+ 11708563470209)

(defun sum-columnwise (operators &rest rows)
  (loop for op in operators
        for i from 0
        sum (apply op (mapcar (lambda (row) (aref row i)) rows))))

(defun get-ruler-indices (ruler-string)
  (loop for char across ruler-string
        for index from 0
        unless (char= char #\Space)
          collect index))

(defun slice-by-indices (target-string indices)
  (loop for (start next-start) on indices
        collect (subseq target-string 
                        start 
                        (and next-start (1- next-start)))))
    
(defun parse-vertical-numbers (lists)
  (let ((groups (apply #'mapcar #'list lists)))
    (loop for group in groups
          collect
          (loop for i from 0 below (length (first group))
                collect
                (let ((vertical-str (map 'string (lambda (s) (char s i)) group)))
                  (parse-integer vertical-str :junk-allowed t))))))

(defun sum-2 (operators &rest rows)
  (let ((vertical-numbers (parse-vertical-numbers rows)))
    (loop for op in operators
          for vn in vertical-numbers
          sum (apply op vn))))

(defun part-1 ()
  (let* ((lines (reverse (uiop:read-file-lines (input-pathname))))
         (args  (mapcar (lambda (line) (cl-ppcre:split "\\s+" (string-trim " " line))) lines)))
    (apply #'sum-columnwise 
	       (mapcar #'intern (car args))
           (mapcar (lambda (arg)
		             (map 'vector #'parse-integer arg))
                   (cdr args)))))

(defun part-2 ()
  (let* ((lines (reverse (uiop:read-file-lines (input-pathname))))
	 (operators (car lines))
	 (rule (get-ruler-indices operators)))
    (apply #'sum-2 
	   (mapcar #'intern (cl-ppcre:split "\\s+" (string-trim " " operators)))
	   (reverse (mapcar (lambda (line) (slice-by-indices line rule)) (cdr lines))))))
