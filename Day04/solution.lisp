(in-package :adventofcode2025/Day04)

(defparameter +solution-1+ 1493)
(defparameter +solution-2+ 9194)

(defparameter +all-directions+
  (list +north+ +northeast+ +east+ +southeast+
        +south+ +southwest+ +west+ +northwest+))

(defun read-char-grid (pathname)
  (let ((lines-stream (series:scan-file pathname #'read-line)))
    (row-list->grid
     (series:collect 'list
       (series:map-fn 'list (lambda (s) (coerce s 'list))
         (series:scan-file pathname #'read-line))))))

(defun count-neighboring-ats (grid c)
  (count-if (lambda (dir)
              (let ((n (2v+ c dir)))
                (and (on-grid? grid n)
                     (char= (grid-ref grid n) #\@))))
            +all-directions+))

(defun evaluate (pathname &optional (recur nil))
  (let* ((grid (read-char-grid pathname))
         (active-coords (loop for c in (series:collect 'list (scan-grid-coords grid))
			      when (char= (grid-ref grid c) #\@)
			      collect c))
         (count 0)) 
    (loop
      (multiple-value-bind (flippable survivors)
	(loop for c in active-coords
	      if (< (count-neighboring-ats grid c) 4)
	         collect c into f
	      else
	         collect c into s
	      finally (return (values f s)))
	(let ((flip-count (length flippable)))
	  (incf count flip-count)
	  (unless recur
	    (return count))
	  (when (zerop flip-count)
	    (return count))
	  (dolist (c flippable)
	    (setf (grid-ref grid c) #\x))
	  (setf active-coords survivors))))))
      
(defun part-1 ()
  (evaluate (input-pathname)))

(defun part-2 ()
  (evaluate (input-pathname) t))
