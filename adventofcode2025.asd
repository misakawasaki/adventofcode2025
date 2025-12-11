(asdf:defsystem #:adventofcode2025
  :depends-on (:alexandria :cl-ppcre :iterate :series)
  :components #.(append
                 '((:file "package")
                   (:file "misc"       :depends-on ("package"))
                   (:file "initialize" :depends-on ("package")))

                 (loop for i from 1 to 12
                       for day-string = (format nil "Day~2,'0d/solution" i)
                       collect `(:file ,day-string :depends-on ("misc" "package")))

                 `((:file "validate"
                    :depends-on ("package"
                                 ,@(loop for i from 1 to 12
                                         collect (format nil "Day~2,'0d/solution" i)))))))
