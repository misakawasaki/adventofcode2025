(in-package :advent2025)

(defun advent-pathname (pathname)
  (merge-pathnames pathname
                   (asdf/system:system-source-directory :adventofcode2025)))
