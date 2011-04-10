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

(defun parse-float (str &optional no-error-on-failure)
  (with-regex-scanner (s "^ *[+-]?[0-9]+(\\.[0-9]*)?([eEdDsSlL][+-]?[0-9]+)? *$")
    (if (cl-ppcre:scan s str)
        (read-from-string str)
        (if no-error-on-failure
            nil
            (error 'parse-error)))))

(defun parse-rational (str &optional no-error-on-failure)
  (with-regex-scanner (s "^ *[+-]?(([0-9]+)|([0-9]+/[0-9]+))+ *$")
    (if (cl-ppcre:scan s str)
        (read-from-string str)
        (if no-error-on-failure
            nil
            (error 'parse-error)))))



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

(defun parallel (list1 list2 item1 &key (test #'eql))
  "Find the item in list2 that has the same position as item1 in list1."
  (if (null list2)
      nil
      (let ((pos (position item1 list1 :test test)))
        (if pos
            (nth pos list2)
            nil))))

(defun find-duplicates (list &key (test #'eql) (key #'identity))
  "Returns a list of duplicate elements of a list"
  (let ((uniques nil)
        (duplicates nil))
    (iter (for item in list)
          (if (member (funcall key item) uniques :test test :key key)
              (push item duplicates)
              (push item uniques)))
    duplicates))

(defun ninsert-list (n thing list)
  ;; by Kent Pitman (named insert-before-element-n-destructively, comp-lang-lisp: 28 Oct 1992)
  (if (= n 0)
      (cons thing list) ;There's no way to be destructive in this case, so just cons.
      (let ((tail (nthcdr (1- n) list)))
        (when (null tail)
          (error "There is no position ~D in ~S." n list))
        (push thing (cdr tail))
        list)))



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
