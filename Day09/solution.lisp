(in-package :adventofcode2025/Day09)

(defparameter +solution-1+ 4758598740)

(defun parse-points (input)
  (mapcar (lambda (piece)
            (cl-ppcre:register-groups-bind (px py)
              ("^(\\d+),(\\d+)$" piece)
              (list (parse-integer px)
                    (parse-integer py))))
          (cl-ppcre:split "\\r?\\n" input)))

(defun area-rectangle (p1 p2)
  (let ((height (abs (- (first p1) (first p2))))
        (width  (abs (- (second p1) (second p2)))))
    (* (1+ height) (1+ width))))

(defun strictly-inside-p (pt rect-x1 rect-x2 rect-y1 rect-y2)
  "Returns T if pt is strictly inside the bounds (not on edges)."
  (destructuring-bind (px py) pt
    (and (> px (min rect-x1 rect-x2))
         (< px (max rect-x1 rect-x2))
         (> py (min rect-y1 rect-y2))
         (< py (max rect-y1 rect-y2)))))

(defun find-biggest-rectangle (points)
  (let ((rectangles '()))
    (loop for sublist on points
          for p1 = (car sublist)
          do (loop for p2 in (cdr sublist)
                   for area = (area-rectangle p1 p2)
                   do (push area rectangles)))
    (first (sort rectangles #'>))))

(defun part-1 ()
  (let ((content (uiop:read-file-string (input-pathname))))
    (find-biggest-rectangle (parse-points content))))
