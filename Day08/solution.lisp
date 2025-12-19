(in-package :adventofcode2025/Day08)

(defparameter +solution-1+ 164475)

(defun parse-points (input)
  (mapcar (lambda (piece)
            (cl-ppcre:register-groups-bind (px py pz)
              ("^(\\d+),(\\d+),(\\d+)$" piece)
              (list (parse-integer px)
                    (parse-integer py)
                    (parse-integer pz))))
          (cl-ppcre:split "\\r?\\n" input)))

(defun distance-sq-3d (p1 p2)
  (let ((dx (- (first p1) (first p2)))
        (dy (- (second p1) (second p2)))
        (dz (- (third p1) (third p2))))
    (+ (* dx dx)
       (* dy dy)
       (* dz dz))))

(defun sort-pairs-by-distance (points)
  (let ((pairs '()))
    (loop for sublist on points
          for p1 = (car sublist)
          do (loop for p2 in (cdr sublist)
                   for dist-sq = (distance-sq-3d p1 p2)
                   do (push (list p1 p2 dist-sq) pairs)))
    (mapcar (lambda (pair) (list (first pair) (second pair)))
            (sort pairs #'< :key #'third))))

(defun make-union-find (points)
  (let ((parents (make-hash-table :test #'equal)))
    (dolist (p points)
      (setf (gethash p parents) p))
    parents))

(defun find-set (parents point)
  (let ((parent (gethash point parents)))
    (if (equal point parent)
        point
        (setf (gethash point parents)
              (find-set parents parent)))))

(defun union-set (parents p1 p2)
  (let ((root1 (find-set parents p1))
        (root2 (find-set parents p2)))
    (unless (equal root1 root2)
      (setf (gethash root1 parents) root2)
      t)))

(defun connect-points-upto (points limit)
  (let* ((sorted-edges (sort-pairs-by-distance points))
         (uf (make-union-find points))
         (count 0))
    (loop for (p1 p2) in sorted-edges
          until (= count limit)
          do (let ((root1 (find-set uf p1))
                   (root2 (find-set uf p2)))

               (incf count)    
               (unless (equal root1 root2)
                 (union-set uf p1 p2))))
    uf))

(defun get-clusters (points uf-parents)
  (let ((clusters (make-hash-table :test #'equal)))
    (dolist (p points)
      (let ((root (find-set uf-parents p)))
        (push p (gethash root clusters))))
    (sort (loop for group being the hash-values of clusters
                collect group)
          #'>           
          :key #'length)))

(defun part-1 ()
  (let* ((points  (parse-points (uiop:read-file-string (input-pathname))))
         (parents (connect-points-upto points 1000)))
    (apply #'* (mapcar (lambda (item) (length item))
                       (subseq (get-clusters points parents) 0 3)))))
