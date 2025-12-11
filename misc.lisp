(in-package :advent2025)

(defun advent-pathname (pathname)
  (merge-pathnames pathname
                   (asdf/system:system-source-directory :adventofcode2025)))

;;; A grid index is an integer that can range from
;;; -array-dimension-limit to array-dimension-limit - 1

(deftype grid-index ()
  `(integer ,(- array-dimension-limit) ,(1- array-dimension-limit)))

;;; Lightweight Coord abstraction
;;; A coord is a point in a 2D grid, represented as a cons of (column . row)

(deftype coord ()
  '(cons (grid-index) (grid-index)))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun coord (column row)
  "Create a coord with given column and row."
  (check-type column grid-index)
  (check-type row grid-index)
  (cons column row))
)

(define-compiler-macro coord (column row)
  `(CONS ,column ,row))

(defun column (coord)
  "Return the column component of the coord."
  (check-type coord coord)
  (car coord))

(define-compiler-macro column (coord)
  `(CAR ,coord))

(defun row (coord)
  "Return the row component of the coord."
  (check-type coord coord)
  (cdr coord))

(define-compiler-macro row (coord)
  `(CDR ,coord))

(defun scan-coords (row-count col-count)
  (declare (optimizable-series-function))
  (multiple-value-bind (rows cols)
      (map-fn '(values grid-index grid-index)
              #'floor
              (scan-range :below (* row-count col-count))
              (series col-count))
    (declare (type (series grid-index) rows cols))
    (map-fn 'coord #'coord cols rows)))

(defun scan-coords-n (start count)
  (declare (optimizable-series-function))
  (map-fn 'coord (lambda (i) (coord (column start)
                                    (+ (row start) i)))
          (scan-range :below count)))

(defun scan-coords-ne (start count)
  (declare (optimizable-series-function))
  (map-fn 'coord (lambda (i) (coord (+ (column start) i)
                                    (+ (row start) i)))
          (scan-range :below count)))

(defun scan-coords-e (start count)
  (declare (optimizable-series-function))
  (map-fn 'coord (lambda (i) (coord (+ (column start) i)
                                    (row start)))
          (scan-range :below count)))

(defun scan-coords-se (start count)
  (declare (optimizable-series-function))
  (map-fn 'coord (lambda (i) (coord (+ (column start) i)
                                    (- (row start) i)))
          (scan-range :below count)))

(defun scan-coords-s (start count)
  (declare (optimizable-series-function))
  (map-fn 'coord (lambda (i) (coord (column start)
                                    (- (row start) i)))
          (scan-range :below count)))

(defun scan-coords-sw (start count)
  (declare (optimizable-series-function))
  (map-fn 'coord (lambda (i) (coord (- (column start) i)
                                    (- (row start) i)))
          (scan-range :below count)))

(defun scan-coords-w (start count)
  (declare (optimizable-series-function))
  (map-fn 'coord (lambda (i) (coord (- (column start) i)
                                    (row start)))
          (scan-range :below count)))

(defun scan-coords-nw (start count)
  (declare (optimizable-series-function))
  (map-fn 'coord (lambda (i) (coord (- (column start) i)
                                    (+ (row start) i)))
          (scan-range :below count)))

;; A unit is an integer that can be -1, 0, or 1
(deftype unit ()
  `(integer -1 1))

;; An orientation is a 2-D unit vector
(deftype orientation ()
  '(cons (unit) (unit)))

(defun unit-vector (column row)
  "Create a unit vector with given column and row components."
  (check-type column unit)
  (check-type row unit)
  (cons column row))

;; Unit vectors in each cardinal direction.
(defparameter +north+     (unit-vector 0 1)   "Unit vector in the north direction.")
(defparameter +northeast+ (unit-vector 1 1)   "Unit vector in the northeast direction.")
(defparameter +east+      (unit-vector 1 0)   "Unit vector in the east direction.")
(defparameter +southeast+ (unit-vector 1 -1)  "Unit vector in the southeast direction.")
(defparameter +south+     (unit-vector 0 -1)  "Unit vector in the south direction.")
(defparameter +southwest+ (unit-vector -1 -1) "Unit vector in the southwest direction.")
(defparameter +west+      (unit-vector -1 0)  "Unit vector in the west direction.")
(defparameter +northwest+ (unit-vector -1 1)  "Unit vector in the northwest direction.")

;;; 2D vector arithmetic

(defun 2v- (left right)
  "Subtract right vector from left vector."
  (check-type left coord)
  (check-type right coord)
  (coord (- (column left) (column right))
         (- (row left) (row right))))

(define-compiler-macro 2v- (left right)
  (let ((left-var (gensym "LEFT-"))
        (right-var (gensym "RIGHT-")))
    `(LET ((,left-var ,left)
           (,right-var ,right))
       (COORD (- (COLUMN ,left-var) (COLUMN ,right-var))
              (- (ROW ,left-var) (ROW ,right-var))))))

(defun 2v+ (left right)
  "Add two 2D vectors."
  (check-type left coord)
  (check-type right coord)
  (coord (+ (column left) (column right))
         (+ (row left) (row right))))

(define-compiler-macro 2v+ (left right)
  (let ((left-var (gensym "LEFT-"))
        (right-var (gensym "RIGHT-")))
    `(LET ((,left-var ,left)
           (,right-var ,right))
       (COORD (+ (COLUMN ,left-var) (COLUMN ,right-var))
              (+ (ROW ,left-var) (ROW ,right-var))))))

(defun 2v* (vector scalar)
  "Scale a 2D vector by a scalar."
  (check-type vector coord)
  (check-type scalar integer)
  (coord (* (column vector) scalar)
         (* (row vector) scalar)))

(define-compiler-macro 2v* (vector scalar)
  (let ((left-var (gensym "VECTOR-"))
        (right-var (gensym "SCALAR-")))
    `(LET ((,vector-var ,vector)
           (,scalar-var ,scalar))
       (COORD (* (COLUMN ,vector-var) ,scalar-var)
              (* (ROW ,vector-var) ,scalar-var)))))

(defun 2v-mod (vector modulus)
  "Compute the 2d-vector modulo the modulus."
  (check-type vector coord)
  (check-type modulus coord)
  (coord (mod (column vector) (column modulus))
         (mod (row vector) (row modulus))))

(define-compiler-macro 2v-mod (vector modulus)
  (let ((left-var (gensym "VECTOR-"))
        (right-var (gensym "MODULUS-")))
    `(LET ((,left-var ,vector)
           (,right-var ,modulus))
       (COORD (MOD (COLUMN ,left-var) (COLUMN ,right-var))
              (MOD (ROW ,left-var) (ROW ,right-var))))))

(defun coord-northwest (coord)
  "Return the coord to the northwest of the given coord."
  (check-type coord coord)
  (2v+ coord +northwest+))

(define-compiler-macro coord-northwest (coord)
  (let ((coord-var (gensym "COORD-")))
    `(LET ((,coord-var ,coord))
       (COORD (1- (COLUMN ,coord-var)) (1+ (ROW ,coord-var))))))

(defun coord-north (coord)
  "Return the coord to the north of the given coord."
  (check-type coord coord)
  (2v+ coord +north+))

(define-compiler-macro coord-north (coord)
  (let ((coord-var (gensym "COORD-")))
    `(LET ((,coord-var ,coord))
       (COORD (COLUMN ,coord-var) (1+ (ROW ,coord-var))))))

(defun coord-northeast (coord)
  "Return the coord to the northeast of the given coord."
  (check-type coord coord)
  (2v+ coord +northeast+))

(define-compiler-macro coord-northeast (coord)
  (let ((coord-var (gensym "COORD-")))
    `(LET ((,coord-var ,coord))
       (COORD (1+ (COLUMN ,coord-var)) (1+ (ROW ,coord-var))))))

(defun coord-east (coord)
  "Return the coord to the east of the given coord."
  (check-type coord coord)
  (2v+ coord +east+))

(define-compiler-macro coord-east (coord)
  (let ((coord-var (gensym "COORD-")))
    `(LET ((,coord-var ,coord))
       (COORD (1+ (COLUMN ,coord-var)) (ROW ,coord-var)))))

(defun coord-southeast (coord)
  "Return the coord to the southeast of the given coord."
  (check-type coord coord)
  (2v+ coord +southeast+))

(define-compiler-macro coord-southeast (coord)
  (let ((coord-var (gensym "COORD-")))
    `(LET ((,coord-var ,coord))
       (COORD (1+ (COLUMN ,coord-var)) (1- (ROW ,coord-var))))))

(defun coord-south (coord)
  "Return the coord to the south of the given coord."
  (check-type coord coord)
  (2v+ coord +south+))

(define-compiler-macro coord-south (coord)
  (let ((coord-var (gensym "COORD-")))
    `(LET ((,coord-var ,coord))
       (COORD (COLUMN ,coord-var) (1- (ROW ,coord-var))))))

(defun coord-southwest (coord)
  "Return the coord to the southwest of the given coord."
  (check-type coord coord)
  (2v+ coord +southwest+))

(define-compiler-macro coord-southwest (coord)
  (let ((coord-var (gensym "COORD-")))
    `(LET ((,coord-var ,coord))
       (COORD (1- (COLUMN ,coord-var)) (1- (ROW ,coord-var))))))

(defun coord-west (coord)
  "Return the coord to the west of the given coord."
  (check-type coord coord)
  (2v+ coord +west+))

(define-compiler-macro coord-west (coord)
  (let ((coord-var (gensym "COORD-")))
    `(LET ((,coord-var ,coord))
       (COORD (1- (COLUMN ,coord-var)) (ROW ,coord-var)))))

;; An ocoord is a coord with an orientation
(deftype ocoord ()
  '(cons coord orientation))

(defun ocoord (coord orientation)
  "Create an oriented coord with given coord and orientation."
  (check-type coord coord)
  (check-type orientation orientation)
  (cons coord orientation))

(define-compiler-macro ocoord (coord orientation)
  `(CONS ,coord ,orientation))

(defun ocoord-coord (ocoord)
  "Return the coord part of the oriented coord."
  (check-type ocoord ocoord)
  (car ocoord))

(define-compiler-macro ocoord-coord (ocoord)
  `(CAR ,ocoord))

(defun ocoord-orientation (ocoord)
  "Return the orientation part of the oriented coord."
  (check-type ocoord ocoord)
  (cdr ocoord))

(define-compiler-macro ocoord-orientation (ocoord)
  `(CDR ,ocoord))

(defun ocoord-advance (ocoord)
  "Return a new oriented coord advanced one step in its orientation."
  (check-type ocoord ocoord)
  (ocoord (2v+ (ocoord-coord ocoord) (ocoord-orientation ocoord))
          (ocoord-orientation ocoord)))

(define-compiler-macro ocoord-advance (ocoord)
  (let ((ocoord-var (gensym "OCOORD-")))
    `(LET ((,ocoord-var ,ocoord))
       (OCOORD (2V+ (OCOORD-COORD ,ocoord-var) (OCOORD-ORIENTATION ,ocoord-var))
               (OCOORD-ORIENTATION ,ocoord-var)))))

(defun ocoord-cw (ocoord)
  "Return a new oriented coord rotated 90 degrees clockwise."
  (check-type ocoord ocoord)
  (ocoord (ocoord-coord ocoord)
          (cond ((equal (ocoord-orientation ocoord) +north+) +east+)
                ((equal (ocoord-orientation ocoord) +east+) +south+)
                ((equal (ocoord-orientation ocoord) +south+) +west+)
                ((equal (ocoord-orientation ocoord) +west+) +north+))))

(defun ocoord-ccw (ocoord)
  "Return a new oriented coord rotated 90 degrees counter-clockwise."
  (check-type ocoord ocoord)
  (ocoord (ocoord-coord ocoord)
          (cond ((equal (ocoord-orientation ocoord) +north+) +west+)
                ((equal (ocoord-orientation ocoord) +east+) +north+)
                ((equal (ocoord-orientation ocoord) +south+) +east+)
                ((equal (ocoord-orientation ocoord) +west+) +south+))))

;;; Lightweight Grid abstraction
;;; A grid is a 2D array of atoms indexed by a coord.
;;; Grids are stored with their rows in reverse order so that first row in the array is
;;; the topmost row of the grid.

(deftype grid () `(array atom 2))

(defun make-grid (height width &rest keys)
  "Create a grid of given height and width, optionally initialized with keys."
  (apply #'make-array (list height width) keys))

(defun row-list->grid (row-list)
  "Create a grid from a list of rows, where each row is a list of elements."
  (make-grid (length row-list) (length (first row-list)) :initial-contents row-list))

(defun grid-height (grid)
  "Return the height (number of rows) of the grid."
  (check-type grid grid)
  (array-dimension grid 0))

(define-compiler-macro grid-height (grid)
  `(ARRAY-DIMENSION ,grid 0))

(defun grid-width (grid)
  "Return the width (number of columns) of the grid."
  (check-type grid grid)
  (array-dimension grid 1))

(define-compiler-macro grid-width (grid)
  `(ARRAY-DIMENSION ,grid 1))

(defun on-grid? (grid coord)
  "Check if the given coord is within the bounds of the grid."
  (and (>= (column coord) 0)
       (< (column coord) (grid-width grid))
       (>= (row coord) 0)
       (< (row coord) (grid-height grid))))

(defun grid-ref (grid coord)
  "Access the element at the given coord in the grid."
  (check-type grid grid)
  (check-type coord coord)
  (aref grid (- (grid-height grid) (row coord) 1) (column coord)))

(define-compiler-macro grid-ref (grid coord)
  (let ((grid-var (gensym "GRID-"))
        (coord-var (gensym "COORD-")))
    `(LET ((,grid-var ,grid)
           (,coord-var ,coord))
       (AREF ,grid-var (- (GRID-HEIGHT ,grid-var) (ROW ,coord-var) 1) (COLUMN ,coord-var)))))

(defsetf grid-ref (grid coord) (value)
  (let ((grid-var (gensym "GRID-"))
        (coord-var (gensym "COORD-")))
    `(LET ((,grid-var ,grid)
           (,coord-var ,coord))
       (SETF (AREF ,grid-var (- (grid-height ,grid-var) (ROW ,coord-var) 1) (COLUMN ,coord-var)) ,value))))

(defun scan-grid-coords (grid)
  (declare (optimizable-series-function))
  (scan-coords (grid-height grid) (grid-width grid)))

(defun scan-grid (grid)
  (declare (optimizable-series-function 2))
  (map-fn '(values coord atom)
          (lambda (grid coord)
            (values coord (grid-ref grid coord)))
          (series grid)
          (scan-coords (grid-height grid) (grid-width grid))))

(defun invert-grid (grid &optional initial-value)
  (if initial-value
      (multiple-value-bind (coords vals) (scan-grid grid)
        (collect-hash-push-except vals coords (list initial-value)))
      (multiple-value-bind (coords vals) (scan-grid grid)
        (collect-hash-push vals coords))))

(defun scan-grid-s (grid column)
  (declare (optimizable-series-function))
  (map-fn 'atom (lambda (coord) (grid-ref grid coord))
          (scan-coords-s (coord column (1- (grid-height grid)))
                         (grid-height grid))))

(defun scan-grid-n (grid column)
  (declare (optimizable-series-function))
  (map-fn 'atom (lambda (coord) (grid-ref grid coord))
          (scan-coords-n (coord column 0)
                         (grid-height grid))))

(defun scan-grid-e (grid row)
  (declare (optimizable-series-function))
  (map-fn 'atom (lambda (coord) (grid-ref grid coord))
          (scan-coords-e (coord 0 row) (grid-width grid))))

(defun scan-grid-w (grid row)
  (declare (optimizable-series-function))
  (map-fn 'atom (lambda (coord) (grid-ref grid coord))
          (scan-coords-w (coord (1- (grid-width grid)) row) (grid-width grid))))

(defun scan-grid-ne (grid n)
  (declare (optimizable-series-function))
  (map-fn 'atom (lambda (coord) (grid-ref grid coord))
          (scan-coords-ne (coord (max 0 (- n)) (max 0 n))
                          (min (- (grid-width grid) (max 0 (- n)))
                               (- (grid-height grid) (max 0 n))))))

(defun scan-grid-nw (grid n)
  (map-fn 'atom (lambda (coord) (grid-ref grid coord))
          (scan-coords-nw (coord (min (- (grid-width grid) 1)
                                      (+ (grid-width grid) n -1))
                                 (max 0 n))
                          (min (- (grid-width grid) (max 0 (- n)))
                               (- (grid-height grid) (max 0 n))))))

(defun scan-grid-se (grid n)
  (declare (optimizable-series-function))
  (map-fn 'atom (lambda (coord) (grid-ref grid coord))
          (scan-coords-se (coord (max 0 n)
                                 (- (grid-height grid)
                                    (max 0 (- n))
                                    1))
                          (min (- (grid-width grid) (max 0 n))
                               (- (grid-height grid) (max 0 (- n)))))))

(defun scan-grid-sw (grid n)
  (declare (optimizable-series-function))
  (map-fn 'atom (lambda (coord) (grid-ref grid coord))
          (scan-coords-sw (coord (+ (max 0 (- n))
                                    (min (- (grid-width grid) (max 0 (- n)))
                                         (- (grid-height grid) (max 0 n)))
                                    -1)
                                 (+ (max 0 n)
                                    (min (- (grid-width grid) (max 0 (- n)))
                                         (- (grid-height grid) (max 0 n)))
                                    -1))
                          (min (- (grid-width grid) (max 0 (- n)))
                               (- (grid-height grid) (max 0 n))))))
