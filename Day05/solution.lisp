(in-package :adventofcode2025/Day05)

(defparameter +solution-1+ 520)
(defparameter +solution-2+ 347338785050515)

(defun parse-input (input)
  (destructuring-bind (block1 block2)
    (cl-ppcre:split "\\r?\\n\\r?\\n" input :limit 2)
    
    (values
      (let (ranges)
        (cl-ppcre:do-register-groups ((#'parse-integer start) (#'parse-integer end))
          ("(\\d+)-(\\d+)" block1)
          (push (cons start end) ranges))
        (nreverse ranges))
      
      (let (numbers)
        (cl-ppcre:do-register-groups ((#'parse-integer n))
          ("(\\d+)" block2)
          (push n numbers))
        (nreverse numbers)))))

(defun in-range-p (n ranges)
  (some (lambda (range)
          (destructuring-bind (start . end) range
            (<= start n end)))
        ranges))

(defun merge-range (ranges)
  (let* ((sorted (sort ranges #'< :key #'car))
         (cur-start (caar sorted))
         (cur-end   (cdar sorted))
         (out       nil))
    (dolist (range (cdr sorted))
      (destructuring-bind (s . e) range
        (cond
          ((<= s (1+ cur-end))
           (setf cur-end (max cur-end e)))
          (t
           (push (cons cur-start cur-end) out)
           (setf cur-start s
                 cur-end   e)))))
      (push (cons cur-start cur-end) out)
      (nreverse out)))

(defun range-length (range)
  (destructuring-bind (start . end) range
    (1+ (- end start))))

(defun part-1 ()
  (multiple-value-bind (ranges numbers)
    (parse-input (uiop:read-file-string (input-pathname)))
    (count-if (lambda (n) (in-range-p n ranges))
              numbers)))

(defun part-2 ()
  (multiple-value-bind (ranges numbers)
    (parse-input (uiop:read-file-string (input-pathname)))
    (reduce #'+ (mapcar #'range-length (merge-range ranges)))))
