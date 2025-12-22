(in-package :adventofcode2025/Day10)

(defparameter +solution-1+ 481)

(defun parse-indicator-lights (indicator-lights)
  (let ((lights (subseq indicator-lights 1 (1- (length indicator-lights))))
        (result 0))
    (loop for char across lights
          for i from 0
          when (char= char #\#)
            do (setf (ldb (byte 1 i) result) 1))
    result))

(defun parse-buttons (buttons)
  (loop for button in buttons
        collect (let ((btn (subseq button 1 (1- (length button))))
                      (result 0))
                  (dolist (num-str (cl-ppcre:split "," btn))
                    (let ((idx (parse-integer num-str)))
                      (setf (ldb (byte 1 idx) result) 1)))
                  result)))

(defun parse-line (line)
  (let ((parts (cl-ppcre:split "\\s+" line)))
    (values
     (parse-indicator-lights (car parts))
     (parse-buttons (butlast (cdr parts))))))

(defun evaluate-line (line)
  (let ((queue (list (list 0 0)))
        (visited (make-hash-table)))
    (multiple-value-bind (indicator-lights buttons)
        (parse-line line)
      (setf (gethash 0 visited) t)
      (when (= 0 indicator-lights) (return-from evaluate-line 0))
      (loop while queue do
         (let* ((entry (pop queue))
                (current (first entry))
                (steps (second entry)))
           (dolist (btn buttons)
             (let ((next-state (logxor current btn)))
               (when (= next-state indicator-lights)
                     (return-from evaluate-line (1+ steps)))
               (unless (gethash next-state visited)
                 (setf (gethash next-state visited) t)
                 (setf queue (append queue (list (list next-state (1+ steps)))))))))))))

(defun evaluate (input)
  (apply #'+
         (mapcar #'evaluate-line
                 (cl-ppcre:split "\\r?\\n" (uiop:read-file-string input)))))

(defun part-1 ()
  (evaluate (input-pathname)))
