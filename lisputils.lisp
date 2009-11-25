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


