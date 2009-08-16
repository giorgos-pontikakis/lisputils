(in-package :cl)

(asdf:defsystem :lisputils
  :serial t
  :depends-on (:cl-fad
	       :cl-ppcre
	       :iterate
	       :alexandria)
  :components ((:file "package")
	       (:file "lisputils")))

