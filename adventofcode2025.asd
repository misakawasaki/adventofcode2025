(asdf:defsystem #:adventofcode2025
  :depends-on (:alexandria :cl-ppcre)
  :components ((:file "Day01/solution"  :depends-on ("misc" "package"))
               (:file "Day02/solution"  :depends-on ("misc" "package"))
               (:file "Day03/solution"  :depends-on ("misc" "package"))
               (:file "Day04/solution"  :depends-on ("misc" "package"))
               (:file "Day05/solution"  :depends-on ("misc" "package"))
               (:file "Day06/solution"  :depends-on ("misc" "package"))
               (:file "Day07/solution"  :depends-on ("misc" "package"))
               (:file "Day08/solution"  :depends-on ("misc" "package"))
               (:file "Day09/solution"  :depends-on ("misc" "package"))
               (:file "Day10/solution"  :depends-on ("misc" "package"))
               (:file "Day11/solution"  :depends-on ("misc" "package"))
               (:file "Day12/solution"  :depends-on ("misc" "package"))
               (:file "initialize"      :depends-on ("package"))
               (:file "misc"            :depends-on ("package"))
               (:file "package")
               (:file "validate"        :depends-on ("package"
                                                     "Day01/solution"
                                                     "Day02/solution"
                                                     "Day03/solution"
                                                     "Day04/solution"
                                                     "Day05/solution"
                                                     "Day06/solution"
                                                     "Day07/solution"
                                                     "Day08/solution"
                                                     "Day09/solution"
                                                     "Day10/solution"
                                                     "Day11/solution"
                                                     "Day12/solution"
                                                      ))
                              ))
