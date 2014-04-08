;;; -*- mode:common-lisp coding:utf-8 -*-

(defpackage test-persistent-database
  (:nicknames tpdb)
  (:use common-lisp persistent-database stefil #+sbcl sb-mop #+lispworks clos #+ccl ccl #+allegro mop)
  (:import-from persistent-database
                index-number-effective-slot-definition
                index-any-effective-slot-definition
                index-any-unique-effective-slot-definition
                slot-definition-index persistent-instances class-instances
                initialize-persistent-classes)
  (:shadow deftest))

(in-package test-persistent-database)

(defparameter *stefil-tpdb-suite* (defsuite stefil-tpdb-suite))

(defmacro deftest (name args &body body)
  `(let ((stefil::*suite* *stefil-tpdb-suite*))
     (stefil:deftest ,name ,args
       ,@body)))

(defclass foo ()
  ((i :index :number)
   (f :index :number)
   (u :index :any-unique)
   (s :index :any))
  (:metaclass persistent-class))

(defclass quz (foo bar)
  ((i :index :number)
   (f :index :number)
   (u :index :any-unique)
   (s :index :any))
  (:metaclass persistent-class))

(defclass bar ()
  ((i :index :number)
   (f :index :number)
   (u :index :any-unique)
   (s :index :any))
  (:metaclass persistent-class))

(defclass quux (quz)
  ()
  (:metaclass persistent-class))

(defvar *irandom*
    '(42 9 59 47 69 69 63 3 6 31 53 65 40 81 71 96 53 32 10 61 77 42 9 26 15 28
      44 52 45 87 51 94 63 95 24 67 28 67 69 6 25 22 66 84 34 78 67 0 60 38 91
      0 46 42 46 77 32 78 47 3 17 72 47 15 34 60 19 73 24 91 26 76 18 28 9 57
      68 54 42 64 64 67 63 94 20 70 23 11 60 45 45 66 77 35 66 41 84 83 25 61))

(defvar *frandom*
    '(58.0 54.0 86.0 26.0 31.0 11.0 93.0 64.0 47.0 63.0 54.0 64.0 54.0 72.0 52.0
      99.0 21.0 10.0 10.0 6.0 40.0 44.0 36.0 76.0 62.0 77.0 93.0 97.0 19.0 13.0
      69.0 9.0 52.0 53.0 86.0 48.0 39.0 67.0 74.0 52.0 34.0 14.0 58.0 26.0 4.0
      75.0 24.0 44.0 68.0 35.0 73.0 39.0 68.0 70.0 44.0 1.0 33.0 42.0 27.0 19.0
      82.0 42.0 88.0 39.0 76.0 39.0 80.0 75.0 37.0 21.0 79.0 94.0 32.0 67.0 43.0
      83.0 76.0 16.0 86.0 98.0 51.0 88.0 58.0 15.0 19.0 40.0 74.0 82.0 78.0 31.0
      53.0 8.0 11.0 13.0 67.0 49.0 18.0 49.0 14.0 5.0))

(eval-when (:load-toplevel :execute)
  (finalize-inheritance (find-class 'persistent-database:persistent-standard-object)))

(deftest redefine-class-foo ()
  (finishes (defclass foo ()
              ((i :index :number)
               (f :index :number)
               (u :index :any-unique)
               (s :index :any))
              (:metaclass persistent-class))))

(deftest redefine-class-bar ()
  (finishes (defclass bar ()
              ((i :index :number)
               (f :index :number)
               (u :index :any-unique)
               (s :index :any))
              (:metaclass persistent-class))))

(defixture with-redefine-class-foo
  (:setup (redefine-class-foo))
  (:teardown))

(defixture with-redefine-class-bar
  (:setup (redefine-class-bar))
  (:teardown))

(deftest make-instance-foo ()
  (finishes (when (null (class-instances (find-class 'foo))) ; only once
              (loop for irandom in *irandom*
                  for frandom in *frandom*
                  collect
                    (let ((obj (make-instance 'foo)))
                      (setf (slot-value obj 'i) irandom)
                      (setf (slot-value obj 'f) frandom)
                      (setf (slot-value obj 'u)
                        (princ-to-string irandom))
                      (setf (slot-value obj 's)
                        (princ-to-string irandom))
                      obj)))))

(deftest make-instance-bar ()
  (finishes (when (null (class-instances (find-class 'bar))) ; only once
              (loop for irandom in *irandom*
                  for frandom in *frandom*
                  collect
                    (let ((obj (make-instance 'bar)))
                      (setf (slot-value obj 'i) irandom)
                      (setf (slot-value obj 'f) frandom)
                      (setf (slot-value obj 'u)
                        (princ-to-string irandom))
                      (setf (slot-value obj 's)
                        (princ-to-string irandom))
                      obj)))))

(deftest make-instance-quz ()
  (finishes (when (null (class-instances (find-class 'quz))) ; only once
              (loop for irandom in *irandom*
                  for frandom in *frandom*
                  collect
                    (let ((obj (make-instance 'quz)))
                      (setf (slot-value obj 'i) irandom)
                      (setf (slot-value obj 'f) frandom)
                      (setf (slot-value obj 'u)
                        (princ-to-string irandom))
                      (setf (slot-value obj 's)
                        (princ-to-string irandom))
                      obj)))))

(deftest make-instance-quux ()
  (finishes (when (null (class-instances (find-class 'quux))) ; only once
              (loop for irandom in *irandom*
                  for frandom in *frandom*
                  collect
                    (let ((obj (make-instance 'quux)))
                      (setf (slot-value obj 'i) irandom)
                      (setf (slot-value obj 'f) frandom)
                      (setf (slot-value obj 'u)
                        (princ-to-string irandom))
                      (setf (slot-value obj 's)
                        (princ-to-string irandom))
                      obj)))))

(defixture with-make-instance-foo
  (:setup (make-instance-foo))
  (:teardown))

(defixture with-make-instance-bar
  (:setup (make-instance-bar))
  (:teardown))

(defixture with-make-instance-quz
  (:setup (make-instance-quz))
  (:teardown))

(defixture with-make-instance-quux
  (:setup (make-instance-quux))
  (:teardown))

(deftest test-class-slots-foo ()
  (with-fixture with-make-instance-foo
    (let* ((foo (first (last (class-instances (find-class 'foo)))))
           (class (class-of foo))
           (class-slots (class-slots class))
           (esd-i (find-if (lambda (esd) (eq 'i (slot-definition-name esd)))
                           class-slots))
           (esd-f (find-if (lambda (esd) (eq 'f (slot-definition-name esd)))
                           class-slots)))
      (is (typep esd-i 'index-number-effective-slot-definition))
      (is (typep esd-f 'index-number-effective-slot-definition))
      (let ((sorted-list-i (slot-definition-index esd-i))
            (sorted-list-f (slot-definition-index esd-f)))
        (is (= (length sorted-list-i) 100))
        (is (= (length sorted-list-f) 100))
        (values class foo esd-i esd-f)))))

(deftest test-class-slots-bar (&optional obj)
  (with-fixture with-make-instance-bar
    (let* ((obj (if obj obj (first (last (class-instances (find-class 'bar))))))
           (class (class-of obj))
           (class-slots (class-slots class))
           (esd-i (find-if (lambda (esd) (eq 'i (slot-definition-name esd)))
                           class-slots))
           (esd-f (find-if (lambda (esd) (eq 'f (slot-definition-name esd)))
                           class-slots)))
      (is (typep esd-i 'index-number-effective-slot-definition))
      (is (typep esd-f 'index-number-effective-slot-definition))
      (let ((sorted-list-i (slot-definition-index esd-i))
            (sorted-list-f (slot-definition-index esd-f)))
        (is (= (length sorted-list-i) 100))
        (is (= (length sorted-list-f) 100))
        (values class obj esd-i esd-f)))))

(deftest test-object= (&optional obj)
  (multiple-value-bind (class obj esd-i esd-f)
      (test-class-slots-bar obj)
    (let ((objects (object= class obj esd-f)))
      (is (= (length objects) 3))
      (is (= (first (first objects)) 58.0))
      (is (typep (second (first objects)) 'bar)))
    (let ((objects (object= class obj esd-i)))
      (is (= (length objects) 4))
      (is (= (first (first objects)) 42))
      (is (typep (second (first objects)) 'bar)))))

(deftest test-object> (&optional obj)
  (multiple-value-bind (class obj esd-i esd-f)
      (test-class-slots-bar obj)
    (let ((objects (object> class obj esd-f)))
      (is (= (length objects) 38))
      (is (= (first (first objects)) 62.0))
      (is (= (first (first (last objects))) 99.0))
      (is (typep (second (first objects)) 'bar)))
    (let ((objects (object> class obj esd-i)))
      (is (= (length objects) 59))
      (is (= (first (first objects)) 44))
      (is (= (first (first (last objects))) 96))
      (is (typep (second (first objects)) 'bar)))))

(deftest test-object< ()
  (multiple-value-bind (class bar esd-i esd-f)
      (test-class-slots-bar)
    (finishes (object< class bar esd-f))
    (finishes (object< class bar esd-i))))

(deftest test-object<= ()
  (multiple-value-bind (class bar esd-i esd-f)
      (test-class-slots-bar)
    (finishes (object<= class bar esd-i))
    (finishes (object<= class bar esd-f))))

(deftest test-object>= ()
  (multiple-value-bind (class bar esd-i esd-f)
      (test-class-slots-bar)
    (finishes (object>= class bar esd-i))
    (finishes (object>= class bar esd-f))))

(deftest test-retrieve-from-index ()
  (with-fixture with-make-instance-foo
    (let ((obj (retrieve-from-index 'foo 'i 25 :all nil)))
      (is (typep obj 'foo))
      (is (= (slot-value obj 'i) 25)))
    (let ((objs (retrieve-from-index 'foo 'i 25 :all t)))
      (is (= (length objs) 2))
      (is (typep (second objs) 'foo))
      (is (= (slot-value (first objs) 'i) 25)))
    (let ((objs (retrieve-from-index 'foo 'i 125 :all t)))
      (is (null objs)))
    (let ((obj (retrieve-from-index 'foo 'f 58.0 :all nil)))
      (is (typep obj 'foo))
      (is (= (slot-value obj 'f) 58.0)))
    (let ((objs (retrieve-from-index 'foo 'f 58.0 :all t)))
      (is (= (length objs) 3))
      (is (typep (second objs) 'foo))
      (is (= (slot-value (first objs) 'f) 58.0)))
    (let ((obj (retrieve-from-index 'foo 'f 158.0 :all nil)))
      (is (null obj)))
    (let ((objs (retrieve-from-index 'foo 'u "0" :all t)))
      (is (= (length objs) 1))
      (is (typep (first objs) 'foo))
      (is (string= (slot-value (first objs) 'u) "0")))
    (let ((obj (retrieve-from-index 'foo 'u "0" :all nil)))
      (is (typep obj 'foo))
      (is (string= (slot-value obj 'u) "0")))
    (let ((objs (retrieve-from-index 'foo 'u "100" :all t)))
      (is (null objs)))
    (let ((obj (retrieve-from-index 'foo 'u "83" :all nil)))
      (is (typep obj 'foo))
      (is (string= (slot-value obj 'u) "83")))
    (let ((objs (retrieve-from-index 'foo 's "42" :all t)))
      (is (= (length objs) 4))
      (is (typep (first objs) 'foo))
      (is (string= (slot-value (first  objs) 's) "42"))
      (is (string= (slot-value (second objs) 's) "42"))
      (is (string= (slot-value (third  objs) 's) "42")))
    (let ((obj (retrieve-from-index 'foo 's "42" :all nil)))
      (is (typep obj 'foo))
      (is (string= (slot-value obj 's) "42")))
    (let ((objs (retrieve-from-index 'foo 's "83" :all t)))
      (is (= (length objs) 1))
      (is (typep (first objs) 'foo))
      (is (string= (slot-value (first objs) 's) "83")))
    (let ((objs (retrieve-from-index 'foo 's "183" :all t)))
      (is (null objs)))
    (let ((objs (retrieve-from-index 'foo 's "128" :all nil)))
      (is (null objs)))))

#+ignore
(deftest test-retrieve-from-index* ()
  (with-fixture with-make-instance-foo
    (let ((obj (retrieve-from-index* 'foo 'i 25 :all nil)))
      (is (typep obj 'foo))
      (is (= (slot-value obj 'i) 25)))
    (let ((objs (retrieve-from-index* 'foo 'i 25 :all t)))
      (is (= (length objs) 2))
      (is (typep (second objs) 'foo))
      (is (= (slot-value (first objs) 'i) 25)))
    (let ((objs (retrieve-from-index* 'foo 'i 125 :all t)))
      (is (null objs)))
    (let ((obj (retrieve-from-index* 'foo 'f 58.0 :all nil)))
      (is (typep obj 'foo))
      (is (= (slot-value obj 'f) 58.0)))
    (let ((objs (retrieve-from-index* 'foo 'f 58.0 :all t)))
      (is (= (length objs) 3))
      (is (typep (second objs) 'foo))
      (is (= (slot-value (first objs) 'f) 58.0)))
    (let ((obj (retrieve-from-index* 'foo 'f 158.0 :all nil)))
      (is (null obj)))
    (let ((objs (retrieve-from-index* 'foo 'u "0" :all t)))
      (is (= (length objs) 1))
      (is (typep (first objs) 'foo))
      (is (string= (slot-value (first objs) 'u) "0")))
    (let ((obj (retrieve-from-index* 'foo 'u "0" :all nil)))
      (is (typep obj 'foo))
      (is (string= (slot-value obj 'u) "0")))
    (let ((objs (retrieve-from-index* 'foo 'u "100" :all t)))
      (is (null objs)))
    (let ((obj (retrieve-from-index* 'foo 'u "83" :all nil)))
      (is (typep obj 'foo))
      (is (string= (slot-value obj 'u) "83")))
    (let ((objs (retrieve-from-index* 'foo 's "42" :all t)))
      (is (= (length objs) 4))
      (is (typep (first objs) 'foo))
      (is (string= (slot-value (first  objs) 's) "42"))
      (is (string= (slot-value (second objs) 's) "42"))
      (is (string= (slot-value (third  objs) 's) "42")))
    (let ((obj (retrieve-from-index* 'foo 's "42" :all nil)))
      (is (typep obj 'foo))
      (is (string= (slot-value obj 's) "42")))
    (let ((objs (retrieve-from-index* 'foo 's "83" :all t)))
      (is (= (length objs) 1))
      (is (typep (first objs) 'foo))
      (is (string= (slot-value (first objs) 's) "83")))
    (let ((objs (retrieve-from-index* 'foo 's "183" :all t)))
      (is (null objs)))
    (let ((objs (retrieve-from-index* 'foo 's "128" :all nil)))
      (is (null objs)))))

(deftest test-db-object-oid ()
  (with-fixture with-make-instance-foo
    (doclass (obj 'foo)
      (is (eq obj (oid-to-object 'foo (db-object-oid obj)))))
    (doclass (obj 'foo)
      (is (null (oid-to-object 'bar (db-object-oid obj))))))
  (with-fixture with-make-instance-bar
    (doclass (obj 'bar)
      (is (eq obj (oid-to-object 'bar (db-object-oid obj)))))
    (doclass (obj 'bar)
      (is (null (oid-to-object 'foo (db-object-oid obj))))))
  ;; oid-to-objectはサブクラスは検索対象外
  (with-fixture with-make-instance-quz
    (doclass (obj 'quz)
      (is (eq obj (oid-to-object 'quz (db-object-oid obj)))))
    (doclass (obj 'quz)
      (is (null (oid-to-object 'foo (db-object-oid obj)))))
    (doclass (obj 'quz)
      (is (null (oid-to-object 'bar (db-object-oid obj)))))))

(deftest test-retrieve-from-index* ()
  (with-fixture with-make-instance-foo
    (with-fixture with-make-instance-bar
      (with-fixture with-make-instance-quz
        (with-fixture with-make-instance-quux
          (let ((obj (retrieve-from-index* 'foo 'i 25 :all nil)))
            (is (or (typep obj 'foo) (typep obj 'quz)))
            (is (= (slot-value obj 'i) 25)))
          (let ((objs (retrieve-from-index* 'foo 'i 25 :all t)))
            (is (= (length objs) 6))
            (is (loop for obj in objs
                    always
                      (and (or (typep obj 'foo) (typep obj 'quz))
                           (= (slot-value obj 'i) 25)))))
          (let ((obj (retrieve-from-index* 'bar 'i 25 :all nil)))
            (is (or (typep obj 'bar) (typep obj 'quz)))
            (is (= (slot-value obj 'i) 25)))
          (let ((objs (retrieve-from-index* 'bar 'i 25 :all t)))
            (is (= (length objs) 6))
            (is (loop for obj in objs
                    always
                      (and (or (typep obj 'bar) (typep obj 'quz))
                           (slot-value obj 'i) 25)))))))))
      
(defixture make-reverse-instance-foo
  (:setup (loop for irandom in *irandom*
              for frandom in *frandom*
              for obj in (class-instances (find-class 'foo))
              do (setf (slot-value obj 'i) irandom)
                 (setf (slot-value obj 'f) frandom)
                 (setf (slot-value obj 'u)
                   (princ-to-string irandom))
                 (setf (slot-value obj 's)
                   (princ-to-string irandom))))
  (:teardown (loop for irandom in *irandom*
                   for frandom in *frandom*
                   for obj in (reverse (class-instances (find-class 'foo)))
                   do (setf (slot-value obj 'i) irandom)
                      (setf (slot-value obj 'f) frandom)
                      (setf (slot-value obj 'u)
                            (princ-to-string irandom))
                      (setf (slot-value obj 's)
                            (princ-to-string irandom)))))

(defixture make-reverse-instance-bar
  (:setup (loop for irandom in *irandom*
              for frandom in *frandom*
              for obj in (class-instances (find-class 'bar))
              do (setf (slot-value obj 'i) irandom)
                 (setf (slot-value obj 'f) frandom)
                 (setf (slot-value obj 'u)
                   (princ-to-string irandom))
                 (setf (slot-value obj 's)
                   (princ-to-string irandom))))
  (:teardown (loop for irandom in *irandom*
                 for frandom in *frandom*
                 for obj in (reverse (class-instances (find-class 'bar)))
                 do (setf (slot-value obj 'i) irandom)
                    (setf (slot-value obj 'f) frandom)
                    (setf (slot-value obj 'u)
                      (princ-to-string irandom))
                    (setf (slot-value obj 's)
                      (princ-to-string irandom)))))

(deftest test-setf-slot-value ()
  (with-fixture with-make-instance-foo
    (with-fixture make-reverse-instance-foo
      (test-retrieve-from-index)))
  (with-fixture with-make-instance-bar
    (with-fixture make-reverse-instance-bar
      (with-slots (class-instances)
          (find-class 'bar)
        (test-object= (first class-instances))
        (test-object> (first class-instances))))))

;;; the following examples are from acache tutorial

(defclass node ()
  ((name :initarg :name :reader name :index :any-unique)
   (children :initarg :children :reader children)
   (max-depth :initform nil :accessor max-depth :index :any))
  (:metaclass persistent-class))

(defmethod print-object ((node node) stream)
  (format stream "#<node ~s max-depth: ~s>" 
          (name node) (max-depth node)))

(defixture with-create-testit
  (:setup (open-file-database "testit" :if-does-not-exist :create :if-exists :supersede))
  (:teardown (close-database)))

(deftest make-node-instance ()
  (with-fixture with-create-testit
    (make-instance 'node :name "foo")
    (is (not (null (retrieve-from-index 'node 'name "foo"))))
    (rollback)
    (is (null (retrieve-from-index 'node 'name "foo")))
    (make-instance 'node :name "foo")
    (is (not (null (retrieve-from-index 'node 'name "foo"))))
    (commit)
    (make-instance 'node :name "bar")
    (is (not (null (retrieve-from-index 'node 'name "foo"))))
    (is (not (null (retrieve-from-index 'node 'name "bar"))))
    (rollback)
    (is (not (null (retrieve-from-index 'node 'name "foo"))))
    (is (null (retrieve-from-index 'node 'name "bar")))))

(defparameter *my-random-state* nil)

(defun my-random (max)
  (mod (or (pop *my-random-state*) 
           (progn (setq *my-random-state* '(4 3 2 1 0 0 1 2 3 4))
                  11))
       max))

(defun buildtree (name max-children)
  (let (children) 
    (when (> max-children 0)
      (dotimes (i (1+ (my-random max-children)))
        (push (buildtree (format nil "~a-~a" name i)
                         (- max-children (my-random 3)))
              children)))
    (make-instance 'node :name name 
                   :children (nreverse children))))

(defixture init-buildtree
  (:setup (setq *my-random-state* nil))
  (:teardown))

(deftest run-buildtree ()
  (with-fixture init-buildtree
    (finishes (when (<= (length (class-instances (find-class 'node))) 1)
                (buildtree "root" 5)))
    (finishes (doclass (obj 'node) (print obj)))))

(defun compute-max-depth (node)
  (let ((max 0))
    (dolist (child (children node))
      (setq max (max max (compute-max-depth child))))
    (setf (max-depth node) (1+ max))))

(defixture with-run-buildtree
  (:setup (run-buildtree))
  (:teardown))

(deftest run-compute-max-depth ()
  (with-fixture with-run-buildtree
    (is (= 9 (compute-max-depth (retrieve-from-index 'node 'name "root"))))))

(defixture with-compute-max-depth
  (:setup (run-compute-max-depth))
  (:teardown))


(deftest verify-max-depth ()
  (with-fixture with-compute-max-depth
    (doclass (obj 'node) (is (numberp (max-depth obj))))))

(defun print-longest-paths (&optional stream)
  ;; print the nodes in the longest path from root to leaf
  ;; there may be more than one solution, we print all
  ;; of them
  (let* ((root-node
          (retrieve-from-index 'node 'name "root")))
    (search-longest-paths nil root-node stream)))

(defun search-longest-paths (sofar node &optional (stream t))
  (let ((depth (max-depth node)))
    (if (children node)
        (dolist (child (children node))
          (when (eql (max-depth child) (1- depth))
            (search-longest-paths (cons node sofar) child stream)))
      (let ((ind 0))                    ; hit a leaf, print solution
        (dolist (node (reverse (cons node sofar)))
          (dotimes (i ind) (write-char #\space))
          (format stream "~s~%" node)
          (incf ind))))))

(defvar *longest-paths*
    "#<node \"root\" max-depth: 9>
#<node \"root-0\" max-depth: 8>
#<node \"root-0-2\" max-depth: 7>
#<node \"root-0-2-0\" max-depth: 6>
#<node \"root-0-2-0-0\" max-depth: 5>
#<node \"root-0-2-0-0-1\" max-depth: 4>
#<node \"root-0-2-0-0-1-0\" max-depth: 3>
#<node \"root-0-2-0-0-1-0-0\" max-depth: 2>
#<node \"root-0-2-0-0-1-0-0-0\" max-depth: 1>
#<node \"root\" max-depth: 9>
#<node \"root-0\" max-depth: 8>
#<node \"root-0-2\" max-depth: 7>
#<node \"root-0-2-0\" max-depth: 6>
#<node \"root-0-2-0-2\" max-depth: 5>
#<node \"root-0-2-0-2-1\" max-depth: 4>
#<node \"root-0-2-0-2-1-0\" max-depth: 3>
#<node \"root-0-2-0-2-1-0-0\" max-depth: 2>
#<node \"root-0-2-0-2-1-0-0-0\" max-depth: 1>
")

(deftest test-longest-paths ()
  (with-fixture with-create-testit
    (with-fixture with-compute-max-depth
      (let ((s (make-string-output-stream)))
        (print-longest-paths s)
        (is (string= (get-output-stream-string s) *longest-paths*))
        (commit)))))

(defixture with-open-testit
  (:setup (open-file-database "testit" :if-does-not-exist :error))
  (:teardown (close-database)))

(deftest test-read-longest-paths ()
  (test-longest-paths)
  (with-fixture with-open-testit
      (let ((s (make-string-output-stream)))
        (print-longest-paths s)
        (is (string= (get-output-stream-string s) *longest-paths*)))))

;;; acache tutorial continue

(defclass game ()
  ((number :index :any-unique :initarg :number :reader game-number)
                                        ; :victory, :draw
   (result :initarg :result :reader game-result)
                                        ; if :draw then winner slot is filled with player
                                        ; with the lower player-number
   (winner :initarg :winner :reader game-winner)
   (loser :initarg :loser :reader game-loser)
   (encoded :initarg :encoded :index :any :reader game-encoded)
   (encoded-rev :initarg :encoded-rev :index :any :reader game-encoded-rev))
  (:metaclass persistent-class))

(defclass player ()
  ((number :index :any-unique :initarg :number :reader player-number)
   (name :initarg name :reader player-name))
  (:metaclass persistent-class))

(defparameter *number-of-players* 50)
(defparameter *number-of-try* 10000)    ; too slow with 10000
(defparameter *db-file-name* "chess.db")
(defun build-test-case ()
  (create-file-database *db-file-name*)
                                        ; create a set of players
                                        ; we should name them but that's not important
                                        ; for the demo
  (dotimes (i *number-of-players*)
    (make-instance 'player :number i))
                                        ; make up a set of games
  (dotimes (i *number-of-try*)
    (multiple-value-bind (result winner loser)
        (choose-random-result)
                                        ; in the case of a draw we put lower numbered player
                                        ; in the winner slot
      (when (and (eq result :draw)
                 (> winner loser))
        (rotatef winner loser))
      (make-instance 'game
        :number i
        :result result
        :winner (retrieve-from-index 'player 'number winner)
        :loser (retrieve-from-index 'player 'number loser)
        :encoded (encode-game result winner loser)
        :encoded-rev (encode-game result loser winner))))
  (commit)
  (close-database))

(defun choose-random-result ()
  (let* ((result (case (random 2)
                   (0 :victory)
                   (1 :draw)))
         (winner (random *number-of-players*))
         (loser (loop
                  (let ((player (random *number-of-players*)))
                    (unless (eql player winner)
                      (return player))))))
    (values result winner loser)))

(defun encode-game (result playera-number playerb-number)
  (+ (ash (ccase result
            (:victory 0)
            (:draw 1))
          40)
     (ash playera-number 20)
     playerb-number))

(defun query-a ()
  (open-file-database *db-file-name*)
  (dotimes (i 10)
    (multiple-value-bind (result winner loser)
        (choose-random-result)
      (if (eq result :draw)
          (progn
            (format t "In which games did player ~s and ~s draw?~%" winner loser)
            (when (> winner loser)      ; put in canonical order
              (rotatef winner loser)))
        (format t "In which games did player ~s defeat player ~s~%" winner loser))
      (let ((enc (encode-game result winner loser)))
        (let ((games (retrieve-from-index 'game 'encoded enc :all t)))
          (if (null games)
              (format t " no games~%")
            (dolist (game games)
              (format t " game ~s~%" (game-number game))))))
      (terpri)))
  (close-database))

(defun query-b ()
  (open-file-database *db-file-name*)
  (dotimes (i 10)
    (multiple-value-bind (result winner loser)
        (choose-random-result)
      (if (eq result :draw)
          (progn
            (format t "In how many games did player ~s and ~s draw?~%" winner loser)
            (when (> winner loser)
              (rotatef winner loser)))
        (format t "In how many games did player ~s defeat player ~s~%" winner loser))
      (let ((enc (encode-game result winner loser)))
        (let ((count (length (retrieve-from-index 'game 'encoded enc :all t :oid t))))
          (format t " ~s game~p~%" count count)))
      (terpri)))
  (close-database))

(defixture with-build-test-case
  (:setup (build-test-case))
  (:teardown))

(deftest test-query-a ()
  (with-fixture with-build-test-case
    (finishes (query-a))))

(deftest test-query-b ()
  (with-fixture with-build-test-case
    (finishes (query-b))))

;;; delete instance test

(deftest test-delete-instance-1 ()
  (with-fixture with-create-testit
    (with-fixture with-make-instance-foo
      (let ((nlist '(0 20 40 60 80))
            (count 0))
        (doclass (obj 'foo)
          (incf count))
        (commit)
        (loop for n in nlist
            do (let ((obj (oid-to-object 'foo n)))
                 (assert obj)
                 (delete-instance obj)))
        (commit)
        (let ((c 0))
          (doclass (obj 'foo)
            (incf c))
          (is (= c (- count (length nlist)))))
        (close-database)
        (open-file-database "testit" :if-does-not-exist :error)
        (let ((c 0))
          (doclass (obj 'foo)
            (incf c))
          (is (= c (- count (length nlist)))))
        ;; re-used oid check
        (is (loop for n in (reverse nlist)
                always (let ((new (make-instance 'foo)))
                         (= (db-object-oid new) n))))
        (commit)
        (let ((c 0))
          (doclass (obj 'foo)
            (incf c))
          (is (= c count)))
        (close-database)
        (open-file-database "testit" :if-does-not-exist :error)
        (let ((c 0))
          (doclass (obj 'foo)
            (incf c))
          (is (= c count)))))))

(deftest test-delete-instance-2 ()
  (with-fixture with-create-testit
    (with-fixture with-make-instance-foo
      (let ((nlist '(0 20 40 60 80))
            (count 0))
        (doclass (obj 'foo)
          (incf count))
        (commit)
        (loop for n in nlist
            do (let ((obj (oid-to-object 'foo n)))
                 (assert obj)
                 (delete-instance obj)))
        (commit)
        (let ((c 0))
          (doclass (obj 'foo)
            (incf c))
          (is (= c (- count (length nlist)))))
        (close-database)
        (open-file-database "testit" :if-does-not-exist :error)
        (let ((c 0))
          (doclass (obj 'foo)
            (incf c))
          (is (= c (- count (length nlist)))))
        ;; re-used oid check
        (is (loop for n in (reverse nlist)
                always (let ((new (make-instance 'bar)))
                         (= (db-object-oid new) n))))
        (commit)
        (let ((c 0))
          (doclass (obj 'foo)
            (incf c))
          (is (= c (- count (length nlist)))))
        (let ((c 0))
          (doclass (obj 'bar)
            (incf c))
          (is (= c (length nlist))))
        (close-database)
        (open-file-database "testit" :if-does-not-exist :error)
        (let ((c 0))
          (doclass (obj 'foo)
            (incf c))
          (is (= c (- count (length nlist)))))
        (let ((c 0))
          (doclass (obj 'bar)
            (incf c))
          (is (= c (length nlist))))
        ;; delete and make again
        (loop for n in nlist
            do (let ((obj (oid-to-object 'bar n)))
                 (assert obj)
                 (delete-instance obj)))
        (is (loop for n in (reverse nlist)
                always (let ((new (make-instance 'foo)))
                         (= (db-object-oid new) n))))
        ;; length of class-instances must be constant
        (is (= count
               (length (class-instances (find-class 'foo)))))
        (is (= (length nlist)
               (length (class-instances (find-class 'bar)))))
        ;; One more time, delete and make
        (loop for n in nlist
            do (let ((obj (oid-to-object 'foo n)))
                 (assert obj)
                 (delete-instance obj)))
        (is (loop for n in (reverse nlist)
                always (let ((new (make-instance 'bar)))
                         (= (db-object-oid new) n))))
        ;; length of class-instances still must be constant
        (is (= count
               (length (class-instances (find-class 'foo)))))
        (is (= (length nlist)
               (length (class-instances (find-class 'bar)))))))))
