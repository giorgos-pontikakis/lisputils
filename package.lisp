(in-package :cl-user)

(defpackage :lisputils
  (:use :common-lisp :cl-fad :cl-ppcre :alexandria)
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
   ;; lists
   :zip
   :parallel
   :find-duplicates
   :make-plist
   :ninsert-list
   :insert-list
   ;; property lists
   :plist-union
   :plist-collect
   :plist-collect-if
   :plist-keys
   :plist-vals
   :plist-map
   :plist-mapc
   :plist-map-vals
   ;; with-hashe-identity
   :*with-hashed-identity-body-forms*
   :with-hashed-identity
   ;; trees
   :dfs
   :dft))
