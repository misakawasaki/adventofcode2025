(in-package :adventofcode2025/Day06)

(defparameter +solution-1+ 8108520669952)
(defparameter +solution-2+ 11708563470209)

(defun extract-column-headers (header-line)
  (loop for char across header-line
        for index from 0
        unless (char= char #\Space)
          collect (intern (string char)) into headers
          and collect index into indices
        finally (return (values headers indices))))

(defun extract-column-values (line indices)
  (loop for (start next-start) on indices
        collect (subseq line
                        start
                        (and next-start (1- next-start)))))

(defun parse-column-integers (rows-of-strings)
  (apply #'mapcar
         (lambda (&rest items)
           (mapcar (lambda (item) (parse-integer item :junk-allowed t)) items))
         rows-of-strings))

(defun parse-vertical-integers (column-blocks)
  (let ((transposed-blocks (apply #'mapcar #'list column-blocks)))
    (loop for block in transposed-blocks
          collect
          (loop for i from 0 below (length (first block))
                collect
                (let ((vertical-string (map 'string 
                                            (lambda (s) (char s i)) 
                                            block)))
                  (parse-integer vertical-string :junk-allowed t))))))

(defun sum-columnwise (operators parse-row-fn &rest rows)
  (let ((integers (funcall parse-row-fn rows)))
    (loop for op  in operators
          for num in integers 
          sum (apply op num))))

(defun evaluate (input parse-row-fn)
  (let* ((rows (reverse (uiop:read-file-lines input)))
         (operators-line (car rows))) ;; The bottom line
    
    (multiple-value-bind (ops indices) 
        (extract-column-headers operators-line)
      (let ((parsed-rows 
             (mapcar (lambda (row) 
                       (extract-column-values row indices)) 
                     (cdr rows))))  
        (apply #'sum-columnwise
               ops
               parse-row-fn
               (reverse parsed-rows))))))

(defun part-1 ()
  (evaluate (input-pathname) #'parse-column-integers))

(defun part-2 ()
  (evaluate (input-pathname) #'parse-vertical-integers))
