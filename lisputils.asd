(in-package :cl)

(defpackage :lisputils-asdf
  (:use :cl :asdf))

(in-package :lisputils-asdf)

(defsystem :lisputils
  :version "1.1.0"
  :serial t
  :depends-on (:cl-fad
               :cl-ppcre
               :alexandria)
  :components ((:file "package")
               (:file "lisputils")))

(defsystem :lisputils-tests
  :serial t
  :depends-on (:lisputils :hu.dwim.stefil)
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "lisputils-tests")))))
