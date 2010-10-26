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
   :white-trim
   :white-char-p
   :parse-float
   :parse-rational
   :iso-time
   :zip
   :parallel
   :find-duplicates
   :make-plist
   :plist-union
   :plist-collect
   :plist-collect-if
   :plist-keys
   :plist-vals
   :plist-map
   :plist-map-vals
   :plist-do))
