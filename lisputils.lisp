(in-package :lisputils)


;; ----------------------------------------------------------------------
;; Packages   
;; ----------------------------------------------------------------------
(defmacro with-package-check (package &body body)
  (with-gensyms (pkg)
    `(let ((,pkg (find-package ,package)))
       (if ,pkg 
	   (progn
	     ,@body)
	   (error "Package ~A does not exist." ,package)))))

(defun package-key (package)
  (if (keywordp package)
      package
      (make-keyword (package-name package))))


;; ----------------------------------------------------------------------
;; Pathnames
;; ----------------------------------------------------------------------
(defun make-pathname* (&key file (dir *default-pathname-defaults*))
  (if file
      (iter (for i in (ensure-list dir))
	    (reducing (pathname-as-directory i)
		      by #'(lambda (parent child)
			     (merge-pathnames child parent))
		      initial-value (pathname-as-file file)))
      (iter (for i in (ensure-list dir))
	    (reducing (pathname-as-directory i)
		      by #'(lambda (parent child)
			     (merge-pathnames child parent))))))


;; ----------------------------------------------------------------------
;; Symbols
;; ----------------------------------------------------------------------
(defun make-symbol* (string &optional (package *package*))
  (intern (string-upcase string) package))

;; ----------------------------------------------------------------------
;; Strings
;; ----------------------------------------------------------------------
(defun string-upcase-gr (string)
  (let ((result-string (string-upcase string)))
    (mapc #'(lambda (pair)
	      (nsubstitute (cdr pair)
			   (car pair)
			   result-string))
	  '((#\Ά . #\A)
	    (#\Έ . #\Ε)
	    (#\Ή . #\Η)
	    (#\Ί . #\Ι)
	    (#\ΐ . #\Ϊ)
	    (#\Ό . #\Ο)
	    (#\ς . #\Σ)
	    (#\Ύ . #\Υ)
	    (#\Ώ . #\Ω)
	    (#\ΰ . #\Ϋ)))
    result-string))


;;; -------------------------------------------------------------
;;; Number parsing
;;; -------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-regex-scanner ((scanner regex &rest args) &body body)
    `(let ((,scanner (load-time-value (cl-ppcre:create-scanner ,regex ,@args))))
       ,@body)))

(defun parse-float (str)
  (with-regex-scanner (s "^ *[+-]?[0-9]+(\\.[0-9]*)?([eEdDsSlL][+-]?[0-9]+)? *$")
    (if (cl-ppcre:scan s str)
	(read-from-string str)
	nil)))

(defun parse-rational (str)
  (with-regex-scanner (s "^ *[+-]?(([0-9]+)|([0-9]+/[0-9]+))+ *$")
    (if (cl-ppcre:scan s str)
	(read-from-string str)
	nil)))


;;; ----------------------------------------------------------------------
;;; Time
;;; ----------------------------------------------------------------------
(defun iso-time (&optional (time (get-universal-time)))
  "Returns the universal time TIME as a string in full ISO format."
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))


;;; ----------------------------------------------------------------------
;;; Lists
;;; ----------------------------------------------------------------------
(defun zip (list1 list2 &key (key1 #'identity) (key2 #'identity))
  "Combine two lists in one, picking items in alternating order."
  (iter (for item1 in list1)
        (for item2 in list2)
        (collect (funcall key1 item1))
        (collect (funcall key2 item2))))

(defun plist-union (plist1 plist2 &key (test #'null))
  "Copy plist1 in a new plist, but for the values satisfying the test
function, search the corresponding key in plist2. If found there, use
val2, not val1. Finally copy to plist1 the keys/values of plist2 that
are not found in plist1"
  (let ((part1 (iter (for key1 in plist1 by #'cddr)
                     (for val1 in (rest plist1) by #'cddr)
                     (collect key1)
                     (collect (if (funcall test val1)
                                  (let* ((default (gensym)) ;; this default value cannot be matched 
                                         (val2 (getf plist2 key1 default))) 
                                    (if (eql val2 default) val1 val2)) 
                                  val1))))
        (part2 (iter (for key2 in plist2 by #'cddr)
                     (for val2 in (rest plist2) by #'cddr)
                     (let* ((default (gensym))
                            (val1 (getf plist1 key2 default))) 
                       (when (eql val1 default)
                         (collect key2)
                         (collect val2))))))
    (append part1 part2)))
