(in-package :cl)

(defpackage :lisputils-asdf
    (:use :cl :asdf))

(in-package :lisputils-asdf)

(defsystem :lisputils
  :depends-on (:cl-fad
               :cl-ppcre
               :alexandria)
  :serial t
  :components ((:file "package")
               (:file "lisputils")))

(defsystem :lisputils-tests
  :depends-on (:lisputils :hu.dwim.stefil)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "lisputils-tests")))))
