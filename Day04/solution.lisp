(in-package :adventofcode2025/Day04)

(defun read-char-grid (pathname)
  (let* ((lines-stream (series:scan-file pathname #'read-line))
         (rows-list (series:collect 'list
                                    (series:map-fn 'list (lambda (s) (coerce s 'list))
                                                   lines-stream))))
    (row-list->grid rows-list)))

(defun count-neighboring-ats (grid center-coord)
  (let ((directions (list +north+ +northeast+ +east+ +southeast+
                          +south+ +southwest+ +west+ +northwest+))
        (count 0))
    (dolist (dir directions)
      (let ((neighbor-coord (2v+ center-coord dir)))
        (when (and (on-grid? grid neighbor-coord)
                   (char= #\@ (grid-ref grid neighbor-coord)))
          (incf count))))
    count))

(defun part-1 ()
  (let ((grid (read-char-grid (input-pathname))))
    (multiple-value-bind (coords vals) 
      (scan-grid grid)
      (series:collect-length
        (series:choose-if #'identity
          (series:map-fn 'boolean
            (lambda (coord val)
              (and (char= val #\@)
                   (< (count-neighboring-ats grid coord) 4)))
            coords
            vals))))))

