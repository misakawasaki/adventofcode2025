(in-package :adventofcode2025/Day01)

(defparameter +solution-1+ 1145)

(defun wrap (n &optional (limit 100))
  (mod n limit))

(defun parse-instruction (line)
  (cl-ppcre:register-groups-bind 
    (dir 
     (#'parse-integer dist))
    ("^([LR])(\\d+)$" line)
    
    (list :dir dir :dist dist)))

(defun evaluate-instruction (point instruction)
  (wrap
    (alexandria:eswitch ((getf instruction :dir) :test #'string=)
      ("L" (- point (getf instruction :dist)))
      ("R" (+ point (getf instruction :dist)))))) 

(defun part-1 ()
  (let* ((lines (uiop:read-file-lines (input-pathname)))
	 (instructions (mapcar #'parse-instruction lines))
	 (start 50)
	 (count 0))
    (dolist (instr instructions count)
      (setf start (evaluate-instruction start instr))
      (when (zerop start)
	(incf count)))))
