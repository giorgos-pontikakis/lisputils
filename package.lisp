(in-package :cl-user)

(defpackage :lisputils 
  (:use :common-lisp
	:cl-fad
	:cl-ppcre
	:iterate
	:alexandria)
  (:shadow alexandria::copy-stream alexandria::copy-file)
  (:export
   :with-package-check
   :package-key
   :make-pathname*
   :make-symbol*
   :string-upcase-gr
   :parse-float
   :parse-rational
   :iso-time
   :zip
   :plist-union
   :parallel))

