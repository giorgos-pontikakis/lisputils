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
(defun parent-directory (pathname)
  (merge-pathnames (make-pathname :directory '(:relative :up) :defaults pathname)
                   pathname))



;; ----------------------------------------------------------------------
;; Symbols
;; ----------------------------------------------------------------------
(defun make-symbol* (string &optional (package *package*))
  (intern (string-upcase string) package))



;; ----------------------------------------------------------------------
;; Strings
;; ----------------------------------------------------------------------

(defun string-upcase-gr (string)
  "Upcase a string and converted accented characters to
non-accented. Also take care of final sigma."
  (let ((result-string (string-upcase string)))
    (mapc #'(lambda (pair)
              (nsubstitute (cdr pair)
                           (car pair)
                           result-string))
          '((#\Ά . #\Α)
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

(defun white-trim (str)
  "String-trims whitespace characters"
  (string-trim '(#\newline #\space #\tab #\return #\linefeed #\page) str))

(defun white-char-p (c)
  "Predicate for whitespace characters"
  (find c '(#\newline #\space #\tab #\return #\linefeed #\page)))



;;; -------------------------------------------------------------
;;; Number parsing
;;; -------------------------------------------------------------
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-regex-scanner ((scanner regex &rest args) &body body)
    `(let ((,scanner (load-time-value (cl-ppcre:create-scanner ,regex ,@args))))
       ,@body)))

(defun parse-float (str &key junk-allowed)
  (with-regex-scanner (s "^ *[+-]?[0-9]+((\\.|\\,)[0-9]*)?([eEdDsSlL][+-]?[0-9]+)? *$")
    (if (cl-ppcre:scan s str)
        (read-from-string (substitute #\. #\, str))
        (if junk-allowed
            nil
            (error 'parse-error)))))

(defun parse-rational (str &optional junk-allowed)
  (with-regex-scanner (s "^ *[+-]?(([0-9]+)|([0-9]+/[0-9]+))+ *$")
    (if (cl-ppcre:scan s str)
        (read-from-string str)
        (if junk-allowed
            nil
            (error 'parse-error)))))



;;; ----------------------------------------------------------------------
;;; Lists
;;; ----------------------------------------------------------------------
(defun zip (list1 list2 &key (key1 #'identity) (key2 #'identity))
  "Combine two lists in one, picking items in alternating order."
  (loop for item1 in list1
        for item2 in list2
        collect (funcall key1 item1)
        collect (funcall key2 item2)))

(defun parallel (list1 list2 item1 &key (test #'eql))
  "Find the item in list2 that has the same position as item1 in list1."
  (if (null list2)
      nil
      (let ((pos (position item1 list1 :test test)))
        (if pos
            (nth pos list2)
            nil))))

(defun find-duplicates (list &key (test #'eql) (key #'identity))
  "Returns two values: the primary value is a list of duplicate elements of a list. The
secondary is the original list with the duplicates removed."
  (loop for item in list
        if (member (funcall key item) uniques :test test :key key)
        collect item into duplicates
        else
        collect item into uniques
        finally (return (values duplicates uniques))))

(defun ninsert-list (n thing list)
  ;; by Kent Pitman (named insert-before-element-n-destructively, comp-lang-lisp: 28 Oct 1992)
  (if (= n 0)
      (cons thing list) ;There's no way to be destructive in this case, so just cons.
      (let ((tail (nthcdr (1- n) list)))
        (when (null tail)
          (error "There is no position ~D in ~S." n list))
        (push thing (cdr tail))
        list)))

(defun insert-list (n thing list)
  (if (= n 0)
      (cons thing list)
      (let ((tail (nthcdr (1- n) list)))
        (if (null tail)
            (error "There is no position ~D in ~S." n list)
            (append (subseq list 0 n)
                    (cons thing (cdr tail)))))))



;;; ----------------------------------------------------------------------
;;; Property lists
;;; ----------------------------------------------------------------------

(defun make-plist (keys data)
  (mapcan #'list keys data))

(defun plist-union (plist1 plist2 &key (test #'null))
  "Take the union of the two property lists. To choose between values
of keys which are common to both plists, apply the test function to the
value of the first plist. If true, discard it and use the value from
the second plist instead."
  (let ((part1 (loop for default = (gensym) ;; this default value cannot be in the plist already
                     for key1 in plist1 by #'cddr
                     for val1 in (rest plist1) by #'cddr
                     for val2 = (getf plist2 key1 default)
                     collect key1
                     if (and (funcall test val1) (not (eql val2 default)))
                     collect val2
                     else
                     collect val1))
        (part2 (loop for default = (gensym)
                     for key2 in plist2 by #'cddr
                     for val2 in (rest plist2) by #'cddr
                     for val1 = (getf plist1 key2 default)
                     when (eql val1 default)
                     collect key2
                     and
                     collect val2)))
    (append part1 part2)))

(defun plist-collect (bag plist &key on-values-p (test #'eql))
  (iter (for key in plist by #'cddr)
        (for val in (rest plist) by #'cddr)
        (nconcing
         (if (member (if on-values-p val key) bag :test test)
             (list key val)
             nil))))

(defun plist-collect-if (pred plist &key on-values-p)
  (iter (for key in plist by #'cddr)
        (for val in (rest plist) by #'cddr)
        (nconcing
         (if (funcall pred (if on-values-p val key))
             (list key val)
             nil))))

(defun plist-keys (plist)
  "Get the keys of the plist."
  (iter (for key in plist by #'cddr)
        (collect key)))

(defun plist-vals (plist)
  "Get the values of the plist."
  (iter (for key in (rest plist) by #'cddr)
        (collect key)))

(defun plist-map-vals (fn plist)
  "Map the plist to a new one, with values coming from applying fn
applied to every original plist value."
  (iter (for key in plist by #'cddr)
        (for val in (rest plist) by #'cddr)
        (nconcing (list key (funcall fn val)))))

(defun plist-map (fn plist)
  "Map the plist to a new one, with values coming from applying fn to
 every original plist key and value."
  (iter (for key in plist by #'cddr)
        (for val in (rest plist) by #'cddr)
        (nconcing (list key (funcall fn key val)))))

(defun plist-mapc (fn plist)
  "Map the plist to a new one, with values coming from applying fn to
 every original plist key and value."
  (iter (for key in plist by #'cddr)
        (for val in (rest plist) by #'cddr)
        (funcall fn key val)))


;;;
;;; WITH-HASHED-IDENTITY
;;; by Erik Naggum, comp.lang.lisp, 9/4/2004

;;; Corrected a small error: in the original there was an missing closing
;;; parenthesis after the error form, and an extra closing parenthesis at
;;; the end of the macro.

#|Nothing beats reading the specification to find out how such things work.
A copy of the standard or something very much like it almost certainly
comes with your Common Lisp implementation or it provides pointers to
resources on the Net.  I would suggest you look for the documentation
available with your Common Lisp system.

However, I trust that you will look for the documentatio, so this sketchy
answer will be valuable in context of what you find in the documentation.
You are quite right that case, ccase and ecase all use eql for the test
and you cannot change this.  This is because the cases are specified as
literals in the source of your program, quite unlike the elements of a
sequence searched by member and the like at run-time.  It is expected
that the compiler make good use of the fact that these are literals.  You
find the same "restriction" in all other language that have a case-like
branching form.  However, it would be in the Common Lisp spirit to make a
more general form available without cost to the programmer, although it
would be a little more expensive to implement.  The key to implement a
more general form is that we should keep the very valuable optimization
quality of testing for identity.  There are several ways to accomplish
this, but I prefer the following:|#

(defun extract-case-keys (case-form)
  (loop for clause in (cddr case-form)
        until (and (eq (car case-form) 'case) (member (car clause) '(t otherwise)))
        if (listp (car clause))
        append (car clause)
        else
        collect (car clause)))

(defparameter *with-hashed-identity-body-forms*
  '((case . extract-case-keys)
    (ccase . extract-case-keys)
    (ecase . extract-case-keys))
  "Alist of the valid operators in body forms of a with-hashed-identity form
with their key-extraction function.")

(defun with-hashed-identity-error (body)
  (error "Body form of with-hashed-identity is ~A, but must be one of:~{ ~A~}."
         (caar body) (mapcar #'car *with-hashed-identity-body-forms*)))

(defun with-hashed-identity-hashtable (hash-table body)
  (dolist (key (funcall (or (cdr (assoc (car body) *with-hashed-identity-body-forms*))
                            'with-hashed-identity-error)
                        body))
    (setf (gethash key hash-table) key))
  hash-table)

(defmacro with-hashed-identity (hash-options &body body)
  "A wrapper around case forms to enable case tests via a hashtable."
  (unless (and (listp (car body))
               (null (cdr body)))                  ;TODO: Allow multiple body forms.
    (error "Body of with-hashed-identity must be a single form."))
  (let ((hash-table (make-symbol "hashtable")))
    `(let ((,hash-table (load-time-value
                         (with-hashed-identity-hashtable (make-hash-table ,@hash-options)
                           ',(car body)))))
       (,(caar body) (gethash ,(cadar body) ,hash-table) ,@(cddar body)))))

;; This allows the following forms to succeed:

;; (with-hashed-identity (:test #'equal)
;;   (case "foo"
;;     ("foo" 'yeah)
;;     (t 'bummer)))

;; (with-hashed-identity (:test #'equalp)
;;   (case "foo"
;;     ("FOO" 'yeah)
;;     (t 'bummer)))

;; (with-hashed-identity (:test #'equalp)
;;   (case (vector #\f #\o #\o)
;;     (#(#\F #\O #\O) 'yeah)
;;     (t 'bummer)))
