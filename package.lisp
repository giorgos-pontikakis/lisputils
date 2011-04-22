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
   :parent-directory
   :make-symbol*
   ;; strings
   :string-upcase-gr
   :white-trim
   :white-char-p
   ;; number parsing
   :parse-float
   :parse-rational
   ;; time
   :iso-time
   ;; lists
   :zip
   :parallel
   :find-duplicates
   :make-plist
   :ninsert-list
   ;; property lists
   :plist-union
   :plist-collect
   :plist-collect-if
   :plist-keys
   :plist-vals
   :plist-map
   :plist-mapc
   :plist-map-vals))
