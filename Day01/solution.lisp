(in-package :adventofcode2025/Day01)

(defparameter +dial-start-point+ 50)
(defparameter +solution-1+ 1145)
(defparameter +solution-2+ 6561)

(defun wrap (start delta &optional (limit 100) (ignore-cross t))
  (let* ((end (+ start delta))
         (wrapped (mod end limit)))
    (values (if ignore-cross
                (if (zerop wrapped) 1 0)
                (if (>= delta 0)
                    (- (floor end limit)
                       (floor start limit))
                    (- (ceiling start limit)
                       (ceiling end limit))))
            wrapped)))


(defun parse-instruction (line)
  (cl-ppcre:register-groups-bind 
    (dir 
     (#'parse-integer dist))
    ("^([LR])(\\d+)$" line)
    
    (list :dir dir :dist dist)))

(defun evaluate-instruction (point instruction &optional (limit 100) (ignore-cross t))
  (let* ((delta (alexandria:eswitch ((getf instruction :dir) :test #'string=)
                  ("L" (- (getf instruction :dist)))
                  ("R" (getf instruction :dist)))))
    (wrap point delta limit ignore-cross)))

(defun part-1 ()
  (evaluate (input-pathname)))

(defun part-2 ()
  (evaluate (input-pathname) nil))

(defun evaluate (pathname &optional (ignore-cross t))
  (let* ((lines (uiop:read-file-lines pathname))
         (instructions (mapcar #'parse-instruction lines))
         (start +dial-start-point+)
         (count 0))
    (dolist (instr instructions count)
      (multiple-value-bind (hit rem)
        (evaluate-instruction start instr 100 ignore-cross)
        (setf start rem)
        (incf count hit)))))
