(in-package :adventofcode2025/Day03)

(defparameter +solution-1+ 17332)
(defparameter +solution-2+ 172516781546707)

(defun find-max-digit (digits initial initial-index
                        &key (start 0)
                             (end (length digits)))
  (iterate
    (with max = initial)
    (with max-index = initial-index)
    (for idx :from start :below end)
    (for ch  :in-string digits :from start)
    (let ((digit (digit-char-p ch)))
      (when (and digit (> digit max))
        (setf max digit
              max-index idx)))
    (finally (return (values max max-index)))))

(defun parse-line (line &optional (digit-count 2))
  (let ((len (length line)))
    (do* ((digits-found 0 (1+ digits-found))
          (sum 0)
          (start 0)
          (end (- len digit-count) (1+ end))) 
         ((= digits-found digit-count) sum)
      (multiple-value-bind (value index)
          (find-max-digit line 
                          (digit-char-p (char line start)) 
                          start 
                          :start (1+ start) 
                          :end (1+ end))
          (setf start (1+ index))
          (setf sum (+ (* sum 10) value))))))

(defun evaluate (pathname &optional (digit-count 2))
  (let ((lines (uiop:read-file-lines pathname)))
    (reduce #'+
            (mapcar (lambda (line) (parse-line line digit-count)) 
                    lines))))

(defun part-1 ()
  (evaluate (input-pathname)))

(defun part-2 ()
  (evaluate (input-pathname) 12))
