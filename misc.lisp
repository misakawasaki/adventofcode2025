(in :advent2025)

(defun advent-pathname (pathname)
  (merge-pathnames pathname
                   (asdf/system:system-source-directory :advent2025)))
