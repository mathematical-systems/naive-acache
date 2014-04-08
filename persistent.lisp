;;; -*- mode:lisp coding:utf-8 -*-

(defpackage persistent-database
  (:nicknames pdb)
  (:use common-lisp #-allegro allegro #+allegro excl #+allegro sys #+allegro mp
        #+sbcl sb-mop #+lispworks clos #+ccl ccl #+allegro mop)
  #-allegro
  (:shadowing-import-from allegro
                          directory
                          probe-file
                          pathname-name
                          pathname-directory
                          pathname-type)
  (:export persistent-class persistent-standard-object
           class-instances persistent-instances persistent-max-oid
           object= object>= object<= object> object<
           retrieve-from-index retrieve-from-index* doclass
           db-object-oid oid-to-object
           delete-instance deleted-instance-p
           *pdb* open-file-database create-file-database
           commit rollback close-database copy-file-database))
  
(in-package persistent-database)

;;; persistent standard object class

(defclass persistent-standard-object ()
  ((db-object-oid :accessor db-object-oid
                  :initarg :db-object-oid
                  :initform 0)
   (deleted-instance-p :accessor deleted-instance-p
                       :initarg :deleted-instance-p
                       :initform nil)
   (slots-on-memory-p :accessor slots-on-memory-p
                      :initarg :slots-on-memory-p
                      :initform t)))

(defmethod initialize-instance :after ((instance persistent-standard-object) &rest args)
  (declare (ignore args))
  (let ((instance-class (class-of instance)))
    (with-slots (class-instances deleted-instance-oids persistent-instances persistent-max-oid on-memory-obj-list db-process-lock)
        instance-class
      (with-process-lock (db-process-lock)
        (let ((reused-oid (pop deleted-instance-oids)))
          (if (not (null reused-oid))
              (progn                    ; re-use class-instances
                (let* ((deleted-instance (aref persistent-instances reused-oid))
                       (deleted-class (class-of deleted-instance)))
                  (loop for class-list on (class-instances deleted-class)
                      when (eql deleted-instance (first class-list)) return
                        (setf (first class-list) 
                          (if (eql deleted-class instance-class)
                              instance
                            nil))
                      finally
                        (error "No instance (~A) in class (~A)" deleted-instance deleted-class))
                  (unless (eql deleted-class instance-class)
                    (loop for class-list on class-instances
                        when (null (first class-list)) return
                          (setf (first class-list) instance)
                        finally
                          (push instance class-instances))))
                (setf (db-object-oid instance) reused-oid
                      (aref persistent-instances reused-oid) instance))
            (progn
              (setf (db-object-oid instance)
                (incf persistent-max-oid))
              (vector-push-extend instance persistent-instances)
              (push instance class-instances))))
        (when (and (slots-on-memory-p instance)
                   ;; rotate when slots-on-memory-p is true
                   ;; unless when db read
                   (not *read-database-p*))
          (rotate-instances-on-memory instance))))))

;;; persistent indexed class

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *object-cache-size* 20000)
  (defun make-on-memory-obj-list ()
    "Make a cyclic list."
    (let ((list (make-list *object-cache-size* :initial-element nil)))
      (setf (rest (last list)) list))))

(defclass persistent-class (standard-class)
  ((persistent-instances :accessor persistent-instances
                         :initform (make-array 0 :fill-pointer t :adjustable t)
                         :allocation :class)
   (deleted-instance-oids :accessor deleted-instance-oids :initform '() :allocation :class)
   (persistent-max-oid :accessor persistent-max-oid :initform -1 :allocation :class)
   (on-memory-obj-list :accessor on-memory-obj-list
                       :initform (make-on-memory-obj-list)
                       :allocation :class
                       :documentation "A cyclic list to have oid of instances which have slot values on memory")
   (class-instances :accessor class-instances :initform '())
   (db-process-lock :accessor db-process-lock :initform (make-process-lock :name "db-process-lock") :allocation :class)))

#-allegro
(defmethod validate-superclass ((class persistent-class)
                                (superclass standard-class))
  t)

(define-symbol-macro class-persistent-standard-object
    (load-time-value (find-class 'persistent-standard-object)))

(defmethod shared-initialize :around ((class persistent-class) slot-names
                                      &rest rest
                                      &key (direct-slots nil direct-slots-p)
                                           (direct-superclasses nil direct-superclasses-p))
  "Append persistent-standard-object to direct-superclasses of the class."
  (when direct-superclasses-p
    (let ((persistent-standard-object class-persistent-standard-object))
      ;; Ensure persistent-standard-object is in the superclass list.
      (unless (member persistent-standard-object direct-superclasses)
        (setf direct-superclasses
          (append direct-superclasses
                  (list persistent-standard-object))))))
  (unless direct-slots-p
    (return-from shared-initialize (call-next-method)))
  (remf rest :direct-slots)
  (apply #'call-next-method class slot-names
         (append (when direct-slots-p (list :direct-slots direct-slots))
                 (if direct-superclasses-p
                     (list* :direct-superclasses direct-superclasses rest)
                   rest))))

(defclass indexed-direct-slot-definition (standard-direct-slot-definition)
  ((index :initarg :index :accessor slot-definition-index))
  (:documentation
   "対応する indexed slot の direct-slot-definition"))

(defmethod direct-slot-definition-class ((class persistent-class) &rest initargs &key index &allow-other-keys)
  "永続クラスの direct-slot-definition-class"
  (declare (ignore initargs))
  ;; &key index には index 指定された :any, :any-unique or :number
  ;; が入る。それ以外の index 指定は無視される。
  (ccase index
    ((:any :any-unique :number)
     'indexed-direct-slot-definition)
    ((nil)
     (call-next-method))))

(defclass indexed-effective-slot-definition (standard-effective-slot-definition)
  ()
  (:documentation
   "対応する indexed slot の effective-slot-definition."))

(defclass index-any-effective-slot-definition (indexed-effective-slot-definition)
  ((index :initarg :index :accessor slot-definition-index :initform (make-hash-table :test #'equal)))
  (:documentation
   "対応する indexed slot の effective-slot-definition.
    index は検索のためのハッシュテーブルを持つ"))

(defclass index-any-unique-effective-slot-definition (indexed-effective-slot-definition)
  ((index :initarg :index :accessor slot-definition-index :initform (make-hash-table :test #'equal)))
  (:documentation
   "対応する indexed slot の effective-slot-definition.
    index は検索のためのハッシュテーブルを持つ"))

(defclass index-number-effective-slot-definition (indexed-effective-slot-definition)
  ((index :initarg :index :accessor slot-definition-index :initform '()))
  (:documentation
   "対応する indexed slot の effective-slot-definition.
    index は二分検索のためソートされた列を持つ"))

;; COMPUTE-EFFECTIVE-SLOT-DEFINITION would bind the variable.
(defvar *index*)

(defmethod effective-slot-definition-class ((class persistent-class) &rest initargs)
  (declare (ignore initargs))
  (ccase *index*
    (:any 'index-any-effective-slot-definition)
    (:any-unique 'index-any-unique-effective-slot-definition)
    (:number 'index-number-effective-slot-definition)
    ((nil) (call-next-method))))

(defmethod compute-effective-slot-definition ((class persistent-class) slot-name dsds)
  "永続クラスの effective-slot-definition の算出"
  (let* ((dsd (find-if (lambda (dsd)
                         (and (typep dsd 'indexed-direct-slot-definition)
                              (eq slot-name (slot-definition-name dsd))))
                       dsds))
         (*index* (and dsd (slot-definition-index dsd)))) ; EFFECTIVE-SLOT-DEFINITION-CLASS would use the variable
    ;; 既に finelize されてゐるときは、前の e-s-d を使ひ廻す。
    ;; さもないと新に e-s-d が alloc されて、以前の index が消えてしまふ
    (if (class-finalized-p class)
        (let ((esd (find-if (lambda (esd) ; esd already exists?
                              (and (typep esd 'indexed-effective-slot-definition)
                                   (eq slot-name (slot-definition-name esd))))
                            (class-slots class))))
          (if (and esd dsd)             ; yes, esd already exists
              esd
            (call-next-method)))
      (call-next-method))))

;;; setf slot-value の定義
#+lispworks                             ; re-definition of initialize-instance
(defmethod initialize-instance ((instance persistent-standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

#+lispworks
(defmethod shared-initialize ((instance persistent-standard-object)
                              slot-names &rest all-keys)
  (loop for slot in (class-slots (class-of instance))
      for slot-name = (slot-definition-name slot)
      do (multiple-value-bind (init-key init-value foundp)
             (get-properties all-keys
                             (slot-definition-initargs slot))
           (declare (ignore init-key))
           (if foundp
               (setf (slot-value instance slot-name) init-value)
             (when (and (not (slot-boundp instance slot-name))
                        (not (null (slot-definition-initfunction slot)))
                        (or (eq slot-names t)
                            (member slot-name slot-names)))
               (setf (slot-value instance slot-name)
                 (funcall (slot-definition-initfunction slot)))))))
  instance)

#+lispworks                             ; patch for odd behavior of lispworks
(defmethod (setf slot-value-using-class) (value
                                          (class persistent-class)
                                          object
                                          slot-name)
  (let ((esd (find-if (lambda (esd)
                        (and (or (typep esd 'indexed-effective-slot-definition)
                                 (typep esd 'standard-effective-slot-definition))
                             (eq slot-name (slot-definition-name esd))))
                      (class-slots class))))
    (unless (null esd)
      (setf (slot-value-using-class class object esd) value))
    (call-next-method)))

#+lispworks                             ; patch for odd behavior of lispworks
(defmethod slot-value-using-class ((class persistent-class)
                                   object
                                   slot-name)
  (let ((esd (find-if (lambda (esd)
                        (and (typep esd 'standard-effective-slot-definition)
                             (eq slot-name (slot-definition-name esd))))
                      (class-slots class))))
    (unless (null esd)
      (slot-value-using-class class object esd))
    (call-next-method)))

#+lispworks                             ; patch for odd behavior of lispworks
(defmethod slot-boundp-using-class ((class persistent-class)
                                    object
                                    slot-name)
  (let ((esd (find-if (lambda (esd)
                        (and (typep esd 'standard-effective-slot-definition)
                             (eq slot-name (slot-definition-name esd))))
                      (class-slots class))))
    (unless (null esd)
      (slot-boundp-using-class class object esd))
    (call-next-method)))

(defmethod (setf slot-value-using-class) (value
                                          (class persistent-class)
                                          object
                                          (esd index-number-effective-slot-definition))
  "index-number-effective-slot-definition に、値を setf するメソッド
   value の出し入れのたびに index を管理する
   index は、小さいほうから sort されており、(小 ... 自分 ... 大) の構造
   先づ index から自分を取り除いた後、新に插入する"
  (with-process-lock ((db-process-lock class))
    (let ((slot-name (slot-definition-name esd)))
      (slot-makunbound object slot-name)
      ;; (assert (not (slot-boundp object slot-name)))
      (setf (slot-definition-index esd)
        (binary-object-insert (list value object) (slot-definition-index esd)))))
  #-lispworks (call-next-method))

(defmethod (setf slot-value-using-class) (value
                                          (class persistent-class)
                                          object
                                          (esd index-any-effective-slot-definition))
  "index-any-effective-slot-definition に、値を setf するメソッド
   value の出し入れのたびに index を管理する
   index から自分を取り除いた後、新に插入する"
  (with-process-lock ((db-process-lock class))
    (let ((slot-name (slot-definition-name esd))
          (hashtable (slot-definition-index esd)))
      (slot-makunbound object slot-name)
      ;; (assert (not (slot-boundp object slot-name)))
      (let ((new-list (gethash value hashtable)))
        (setf (gethash value hashtable)
          (cons (list value object) new-list)))))
  #-lispworks (call-next-method))

(defmethod (setf slot-value-using-class) (value
                                          (class persistent-class)
                                          object
                                          (esd index-any-unique-effective-slot-definition))
  "index-any-unique-effective-slot-definition に、値を setf するメソッド
   value の出し入れのたびに index を管理する
   index から自分を取り除いた後、新に插入する"
  (with-process-lock ((db-process-lock class))
    (let ((slot-name (slot-definition-name esd))
          (hashtable (slot-definition-index esd)))
      (slot-makunbound object slot-name)
      ;; (assert (not (slot-boundp object slot-name)))
      (unless (null (remhash value hashtable))
        (warn "Value ~A not unique." value))
      (setf (gethash value hashtable) object)))
  #-lispworks (call-next-method))

;;; lazy slots

(defun slot-value-lazy (object esd)
  "一旦ファイルに保存されてゐる slot-values を讀出した後 setf を行なふ"
  (unless (or (typep esd 'indexed-effective-slot-definition) 
              (member (slot-definition-name esd)
                      (class-slots class-persistent-standard-object)
                      :key #'slot-definition-name))
    (unless (slots-on-memory-p object)
      (read-slot-values-from-file object))))

#+lispworks
(defmethod slot-value-using-class ((class persistent-class)
                                   object
                                   (esd standard-effective-slot-definition)))

(defmethod slot-value-using-class :before
           ((class persistent-class)
            object
            (esd standard-effective-slot-definition))
  (with-process-lock ((db-process-lock class))
    (slot-value-lazy object esd)))

#+lispworks
(defmethod (setf slot-value-using-class) (value
                                          (class persistent-class)
                                          object
                                          (esd standard-effective-slot-definition)))

(defmethod (setf slot-value-using-class) :before
           (value
            (class persistent-class)
            object
            (esd standard-effective-slot-definition))
  (with-process-lock ((db-process-lock class))
    (slot-value-lazy object esd)))

#+lispworks
(defmethod slot-boundp-using-class ((class persistent-class)
                                    object
                                    (esd standard-effective-slot-definition)))

(defmethod slot-boundp-using-class :before
           ((class persistent-class)
            object
            (esd standard-effective-slot-definition))
  (with-process-lock ((db-process-lock class))
    (slot-value-lazy object esd)))

#+lispworks
(defmethod slot-unbound :around ((class persistent-class)
                                 object
                                 slot-name)
  (if (slot-boundp object slot-name)
      (slot-value object slot-name)
    (call-next-method)))

;;;

(defun rotate-instances-on-memory (object)
  (with-slots (on-memory-obj-list)
      (class-of object)
    (let ((old-obj (first on-memory-obj-list)))
      (unless (null old-obj)
        (assert (slots-on-memory-p old-obj))
        ;; oldest generation instance slots are made to be unbound
        ;; and slot values are written in file.
        (slots-makunbound old-obj)))
    (setf (first on-memory-obj-list)
      object)
    (setf on-memory-obj-list
      (rest on-memory-obj-list))))

(defun class-user-slots (class)
  (loop with base-slot-names =
        (mapcar #'slot-definition-name
                (class-slots class-persistent-standard-object))
      for slot in (class-slots class)
      for name = (slot-definition-name slot)
      unless (or (typep slot 'indexed-effective-slot-definition)
                 (member name base-slot-names)) collect
        slot))

(defun class-indexed-slots (class)
  (loop for slot in (class-slots class)
      when (typep slot 'indexed-effective-slot-definition) collect
        slot))

(defun slots-makunbound (object)
  "Make unbound all slots and save them into file.
   except indexed slots."
  (write-slot-values-into-file object)
  (loop for slot in (class-user-slots (class-of object))
      for name = (slot-definition-name slot)
      do (slot-makunbound object name)))

(defvar *read-database-p* nil)

(defmacro slots-saved-file-name (object)
  `(merge-pathnames (princ-to-string (db-object-oid ,object))
                    *pdb*))

(defun read-slot-values-from-file (object)
  "Read unbound slots value from file."
  (assert (not (slots-on-memory-p object)))
  (unless (deleted-instance-p object)
    (setf (slots-on-memory-p object) t)
    (rotate-instances-on-memory object)
    (with-open-file (stream (slots-saved-file-name object)
                     :external-format (stream-external-format *pdb*))
      (let ((*read-database-p* t)
            #+sbcl (sb-ext:*evaluator-mode* :interpret))
        (read stream)))))

(defun write-slot-values-into-file (object)
  "Write slots values into file."
  (assert (slots-on-memory-p object))
  (unless (deleted-instance-p object)
    (with-open-file (stream (slots-saved-file-name object)
                     :direction :output :if-exists :supersede
                     :external-format (stream-external-format *pdb*))
      (let ((*print-level* nil)
            (*print-length* nil))
        (format stream "#.~S~%" (make-load-form-2 object))))
    (setf (slots-on-memory-p object) nil)))

#+(or allegro lispworks)                ; patch for odd behavior of allegro
(defmethod slot-makunbound-using-class ((class persistent-class)
                                        object
                                        slot-name)
  (let ((esd (find-if (lambda (esd)
                        (and (typep esd 'indexed-effective-slot-definition)
                             (eq slot-name (slot-definition-name esd))))
                      (class-slots class))))
    (unless (null esd)
      (slot-makunbound-using-class class object esd))
    (call-next-method)))

(defmethod slot-makunbound-using-class ((class persistent-class)
                                        object
                                        (esd index-number-effective-slot-definition))
  (let ((slot-name (slot-definition-name esd)))
    (when (slot-boundp object slot-name)
      (setf (slot-definition-index esd)
        (binary-object-delete (list (slot-value object slot-name) object)
                              (slot-definition-index esd)))))
  #-(or allegro lispworks) (call-next-method))

(defmethod slot-makunbound-using-class ((class persistent-class)
                                        object
                                        (esd index-any-effective-slot-definition))
  (let ((slot-name (slot-definition-name esd))
        (hashtable (slot-definition-index esd)))
    (when (slot-boundp object slot-name)
      (let* ((old-value (slot-value object slot-name))
             (old-list (gethash old-value hashtable)))
        (assert (not (null old-list)))
        (setf (gethash old-value hashtable)
          (delete object old-list :key #'second :test #'equal)))))
  #-(or allegro lispworks) (call-next-method))

(defmethod slot-makunbound-using-class ((class persistent-class)
                                        object
                                        (esd index-any-unique-effective-slot-definition))
  (let ((slot-name (slot-definition-name esd))
        (hashtable (slot-definition-index esd)))
    (when (slot-boundp object slot-name)
      (let ((old-value (slot-value object slot-name)))
        (when (eq object (gethash old-value hashtable))
          (remhash old-value hashtable)))))
  #-(or allegro lispworks) (call-next-method))

;;; collectors

(defun collect-object= (n slot-value sorted-list)
  (symbol-macrolet ((value (first obj)))
    (nconc (loop for i from n below (length sorted-list)
               for obj = (nth i sorted-list)
               while (= slot-value value)
               collect obj)
           (loop for j from (1- n) downto 0
               for obj = (nth j sorted-list)
               while (= slot-value value)
               collect obj))))

(defmethod object= ((class persistent-class)
                    object
                    (esd index-number-effective-slot-definition))
  "自分と同じ値を持つ object list を返す"
  (let ((slot-name (slot-definition-name esd)))
    (when (slot-boundp object slot-name)
      (let* ((slot-value (slot-value object slot-name))
             (sorted-list (slot-definition-index esd))
             (n (binary-object-search (list slot-value object) sorted-list)))
        (collect-object= n slot-value sorted-list)))))

(defmethod object-index ((class persistent-class)
                         object
                         (esd index-number-effective-slot-definition))
  "slot-definition-index の中の自分の index を返す"
  (let ((slot-name (slot-definition-name esd)))
    (when (slot-boundp object slot-name)
      (let* ((slot-value (slot-value object slot-name))
             (sorted-list (slot-definition-index esd)))
        (values (binary-object-search (list slot-value object) sorted-list)
                slot-value
                sorted-list)))))

(defmethod object> ((class persistent-class)
                    object
                    (esd index-number-effective-slot-definition))
  "自分より大きな値を持つ object list を返す"
  (multiple-value-bind (n slot-value sorted-list)
      (object-index class object esd)
    (symbol-macrolet ((value (first (nth i sorted-list))))
      (loop for i from (1+ n) below (length sorted-list)
          while (= slot-value value)
          finally (return (nthcdr i sorted-list))))))

(defmethod object>= ((class persistent-class)
                    object
                    (esd index-number-effective-slot-definition))
  "自分と同じか、より大きな値を持つ object list を返す"
  (multiple-value-bind (n slot-value sorted-list)
      (object-index class object esd)
    (symbol-macrolet ((value (first (nth i sorted-list))))
      (loop for i from n downto 0
          while (= slot-value value)
          finally (return (nthcdr (1+ i) sorted-list))))))

(defmethod object<= ((class persistent-class)
                     object
                     (esd index-number-effective-slot-definition))
  "自分と等しいかより小さな値を持つ object list を返す"
  (set-difference (slot-definition-index esd)
                  (object> class object esd)))

(defmethod object< ((class persistent-class)
                     object
                     (esd index-number-effective-slot-definition))
  "自分より小さな値を持つ object list を返す"
  (set-difference (slot-definition-index esd)
                  (object>= class object esd)))

;;; binary serarch & insert

(defun binary-search (eqlval list)
  (let ((low 0)
        (high (1- (length list))))
    (loop until (< high low)
        do (let ((middle (floor (/ (+ low high) 2))))
             (cond ((> (first (nth middle list)) eqlval)
                    (setf high (1- middle)))
                   ((< (first (nth middle list)) eqlval)
                    (setf low (1+ middle)))
                   (t (return middle)))))))

(defun binary-object-search (object list)
  (let* ((eqlval (first object))
         (eqlobj (second object))
         (middle (binary-search eqlval list)))
    (loop for i from middle below (length list)
        when (eq eqlobj (second (nth i list)))
        return i
        when (/= (first (nth i list)) eqlval)
        return
          (loop for j from middle downto 0
              when (eq eqlobj (second (nth j list)))
              return j
              when (/= (first (nth j list)) eqlval)
              return nil))))

(defun binary-object-delete (object list)
  (let ((n (binary-object-search object list)))
    (if (null n)
        list
      (if (zerop n)
          (cdr list)
        (prog1
            list
          (rplacd (nthcdr (1- n) list)
                  (cdr (nthcdr n list))))))))

(defun binary-object-insert (object list)
  (flet ((insert-object (object n list)
           (if (< n 0)
               (cons object list)
             (let ((cell (list object))
                   (nthcdr (nthcdr n list)))
               (setf (cdr cell) (cdr nthcdr))
               (setf (cdr nthcdr) cell)
               list))))
    (let ((value (first object)))
      (loop with low = 0
          with high = (1- (length list))
          until (< high low)
          do (let* ((middle (floor (/ (+ low high) 2)))
                    (middle-list (first (nth middle list))))
               (cond ((> middle-list value)
                      (setf high (1- middle)))
                     ((< middle-list value)
                      (setf low (1+ middle)))
                     (t (return (insert-object object middle list)))))
          finally (return (insert-object object high list))))))

;;; retrieve-from-index (acache emulation)

(defmethod retrieve-esd-from-index ((esd index-number-effective-slot-definition)
                                    value &key (all nil))
  (let* ((sorted-list (slot-definition-index esd))
         (n (binary-search value sorted-list)))
    (when (not (null n))
      (if (null all)
          (second (nth n sorted-list))
        (mapcar #'second
                (collect-object= n value sorted-list))))))

(defmethod retrieve-esd-from-index ((esd index-any-unique-effective-slot-definition)
                                    value &key (all nil))
  (let* ((hashtable (slot-definition-index esd))
         (gethash (gethash value hashtable)))
    (if (null all)
        gethash
      (if (not (null gethash))
          (list gethash)
        nil))))

(defmethod retrieve-esd-from-index ((esd index-any-effective-slot-definition)
                                    value &key (all nil))
  (let* ((hashtable (slot-definition-index esd))
         (gethash (gethash value hashtable)))
    (if (null all)
        (second (first (remove-if #'deleted-instance-p gethash :key #'second)))
      (mapcar #'second gethash))))

(defmacro without-deleted-instance ((&optional oidp) body)
  (let ((oid (gensym))
        (obj (gensym)))
    `(let ((,obj ,body)
           (,oid ,oidp))
       (setq ,obj
         (if (listp ,obj)
             (remove-if #'deleted-instance-p ,obj)
           (if (deleted-instance-p ,obj)
               nil
             ,obj)))
       (if (null ,oid)
           ,obj
         (if (listp ,obj)
             (mapcar #'db-object-oid ,obj)
           (db-object-oid ,obj))))))

(defmethod retrieve-from-index ((class persistent-class) slot-name value &key (all nil) (oid nil))
  (let ((esd (find-if (lambda (esd)
                        (eq slot-name (slot-definition-name esd)))
                      (class-slots class))))
    (without-deleted-instance (oid)
      (retrieve-esd-from-index esd value :all all))))

(defmethod retrieve-from-index ((class symbol) slot-name value &key (all nil) (oid nil))
  (retrieve-from-index (find-class class) slot-name value :all all :oid oid))

(defmethod retrieve-from-index* ((class persistent-class) slot-name value &key (all nil) (oid nil))
  (let ((result (retrieve-from-index class slot-name value :all all :oid oid)))
    (when (and (null all)
               (not (null result)))
      (return-from retrieve-from-index* result))
    (if (null all)
        (loop for subclass in (class-direct-subclasses class)
            for obj =
              (retrieve-from-index* subclass slot-name value :all nil :oid oid)
            when obj return obj)
      (append result
              (loop for subclass in (class-direct-subclasses class)
                  append (retrieve-from-index* subclass slot-name value :all t :oid oid))))))

(defmethod retrieve-from-index* ((class symbol) slot-name value &key (all nil) (oid nil))
  (retrieve-from-index* (find-class class) slot-name value :all all :oid oid))

(defmethod oid-to-object ((class persistent-class) oid)
  (let* ((metaclass class)
         (instances (persistent-instances metaclass)))
    (if (< oid 0)
        (error "Oid (~A) must be >= 0" oid)
      (let ((instance (aref instances oid)))
        (if (eql (class-of instance) class)
            instance
          nil)))))

(defmethod oid-to-object ((class symbol) oid)
  (oid-to-object (find-class class) oid))

(defmethod oid-to-object* ((class persistent-class) oid)
  (let ((object (oid-to-object class oid)))
    (if (not (null object))
        object
      (loop for subclass in (class-direct-subclasses class)
          for object = (oid-to-object subclass oid)
          unless (null object) return object))))

(defmethod oid-to-object* ((class symbol) oid)
  (oid-to-object* (find-class class) oid))

#+ignore
(defmethod update-instance-for-redefined-class ((instance persistent-standard-object)
                                                added-slots
                                                discarded-slots
                                                property-list
                                                &rest initargs
                                                &key &allow-other-keys)
  (print added-slots)
  (print discarded-slots))

;;; doclass

(defmacro doclass ((var class-expr) &body body)
  (let ((class (gensym))
        (class-object (gensym))
        (instances (gensym)))
    `(let* ((,class ,class-expr)
            (,class-object (if (symbolp ,class) (find-class ,class) ,class))
            (,instances (class-instances ,class-object)))
       (dolist (,var ,instances)
         (unless (or (null ,var)
                     (deleted-instance-p ,var))
           ,@body)))))


;;; serialize

(defvar *pdb* nil
  "persistent database stream")


(defun open-file-database (dbdirname &key (if-exists :append if-exists-p)
                                          (if-does-not-exist nil if-does-not-exist-p)
                                          (use :db)
                                          (external-format #+allegro :932
                                                           #+sbcl :cp932
                                                           #+lispworks :sjis)
                                          object-cache-size)
  (assert (or (null *pdb*) (not (open-stream-p *pdb*))))
  (if (probe-directory dbdirname)
      (ccase if-exists
        (:supersede
         (delete-directory-and-files dbdirname)
         (make-directory dbdirname))
        (:append)
        ((nil)))
    (ccase if-does-not-exist
      (:create (make-directory dbdirname))
      (:error)
      ((nil))))
  (unless (probe-directory dbdirname)
    (error "DB pathname ~A does not exit." dbdirname))
  (let ((dbfilename (merge-pathnames "pdb" (pathname-as-directory dbdirname))))
    (setq *pdb* (open dbfilename :direction :io
                      :if-exists if-exists
                      :if-does-not-exist if-does-not-exist
                      :external-format external-format))
    (assert (not (null *pdb*))))
  (when object-cache-size
    (setq *object-cache-size* object-cache-size))
  (ccase use
    (:db (read-from-database))
    (:memory)))

(defun create-file-database (dbdirname &key (external-format #+allegro :932
                                                             #+sbcl :cp932
                                                             #+lispworks :sjis)
                                            object-cache-size)
  (open-file-database dbdirname
                      :if-exists :supersede
                      :if-does-not-exist :create
                      :use :db
                      :external-format external-format
                      :object-cache-size object-cache-size))

(defun initialize-effective-slots (class)
  ;; compute-effective-slot-definition にうまく纏めたかつたのだが、
  ;; うまく行かなかつたので…
  (loop for slot in (class-slots class)
      do (cond ((typep slot 'index-any-unique-effective-slot-definition)
                (setf (slot-definition-index slot)
                  (make-hash-table :test #'equal)))
               ((typep slot 'index-any-effective-slot-definition)
                (setf (slot-definition-index slot)
                  (make-hash-table :test #'equal)))
               ((typep slot 'index-number-effective-slot-definition)
                (setf (slot-definition-index slot) '())))))

(defun initialize-persistent-classes ()
  (let* ((classes (class-direct-subclasses class-persistent-standard-object))
         (first-class (first classes)))
    ;; initialize persistent classes
    (loop for class in classes
        do (finalize-inheritance class)
           (setf (class-instances class) '())
           (initialize-effective-slots class)) ; reinitialize effective slot definition
    (setf (persistent-instances first-class) (make-array 0 :fill-pointer t :adjustable t)
          (deleted-instance-oids first-class) '()
          (on-memory-obj-list first-class) (make-on-memory-obj-list)
          (persistent-max-oid first-class) -1)))

(defun read-from-database (&optional (stream *pdb*))
  ;; initialize and clear all slots of persistent classes
  (initialize-persistent-classes)
  ;; file-position
  (file-position stream 0)
  ;; read from database
  (prog1
      (let ((*read-database-p* t)
            (end-value '#:nil)          ; unique end value
            #+sbcl (sb-ext:*evaluator-mode* :interpret)) 
        (loop for exp = (read stream nil end-value nil)
            until (eq exp end-value)))
    (loop with class = (first (class-direct-subclasses class-persistent-standard-object))
        for instance across (persistent-instances class)
        when (deleted-instance-p instance)
        do (push (db-object-oid instance)
                 (deleted-instance-oids class)))))

(defun rollback (&optional (stream *pdb*))
  (read-from-database stream))

(defun commit (&optional (stream-symbol '*pdb*))
  "`commit' always save all of the persistent class instances into the database stream."
  (let* ((class (first (class-direct-subclasses class-persistent-standard-object)))
         (instances (persistent-instances class)))
    (assert (symbolp stream-symbol))
    ;; close and open to flush db file
    (let* ((stream (symbol-value stream-symbol))
           (external-format (stream-external-format stream)))
      (close stream)
      (setq stream
        (open stream :direction :io :if-exists :supersede
              :external-format external-format))
      (set stream-symbol stream)
      (loop with *print-level* = nil
          and *print-length* = nil
          for n from 0 to (persistent-max-oid class)
          for instance = (aref instances n)
          do (format stream "#.~S~%" (make-load-form-1 instance))
             (when (slots-on-memory-p instance)
               (slots-makunbound instance)))
      (setf (on-memory-obj-list class) (make-on-memory-obj-list))
      (force-output stream))))

(defmethod delete-instance ((instance persistent-standard-object))
  (let ((class (class-of instance)))
    (with-process-lock ((db-process-lock class))
      (setf (deleted-instance-p instance) t)
      (loop for slot in (class-indexed-slots class)
          do (slot-makunbound instance (slot-definition-name slot)))
      (pushnew (db-object-oid instance)
               (deleted-instance-oids class))
      instance)))

(defmethod make-load-form-1 ((instance persistent-standard-object) &optional environment)
  (declare (ignore environment))
  (let ((object (intern "object"))
        (class (class-of instance)))
    `(let ((,object (make-instance ',(class-name class))))
       ,@(if (deleted-instance-p instance)
             (list `(setf (slot-value ,object 'deleted-instance-p) 't))
           (append (list `(setf (slot-value ,object 'slots-on-memory-p) 'nil))
                   (loop for slot in (class-indexed-slots class)
                       for name = (slot-definition-name slot)
                       when (slot-boundp instance name) collect
                         `(setf (slot-value ,object ',name)
                            ,(make-slot-value-load-form instance name))))))))

(defmethod make-load-form-2 ((instance persistent-standard-object) &optional environment)
  (declare (ignore environment))
  (let ((object (intern "object"))
        (class (class-of instance)))
    `(let ((,object (oid-to-object ',(class-name (class-of instance))
                                   ',(db-object-oid instance))))
       ,@(unless (deleted-instance-p instance)
           (loop for slot in (class-user-slots class)
               for name = (slot-definition-name slot)
               when (slot-boundp instance name) collect
                 `(setf (slot-value ,object ',name)
                    ,(make-slot-value-load-form instance name)))))))

(defmethod make-slot-value-load-form ((instance persistent-standard-object) name)
  (labels ((make-each-slot-value-load-form (slot-value)
             (cond ((consp slot-value)
                    `(list* ,@(loop for item on slot-value
                                  if (consp (cdr item)) ; (a b) case
                                  collect (make-each-slot-value-load-form (first item))
                                  else  ; (a . b) case
                                  collect (make-each-slot-value-load-form (car item))
                                  and
                                  collect (make-each-slot-value-load-form (cdr item)))))
                   ((simple-vector-p slot-value)
                    `(vector ,@(loop for sv across slot-value
                                   collect (make-each-slot-value-load-form sv))))
                   ((typep slot-value 'persistent-standard-object)
                    `(oid-to-object ',(class-name (class-of slot-value)) ,(db-object-oid slot-value)))
                   ((typep slot-value 'standard-object)
                    (error "The `persistent-class' not yet support to have a standard-object instance in its slot."))
                   (t `(quote ,slot-value)))))
    (make-each-slot-value-load-form (slot-value instance name))))

(defun close-database (&optional (stream *pdb*))
  (initialize-persistent-classes)
  (close stream))

(defun copy-file-database (source-name target-name &key (if-target-exists :error))
  (unless (probe-directory source-name)
    (error "Source database (~A) does not exist." source-name))
  (when (probe-directory target-name)
    (ccase if-target-exists
      (:error
       (error "Target database (~A) already exists." target-name))
      (:supersede
       (delete-directory-and-files target-name)))) 
  (make-directory target-name)
  (loop for file in (directory (truename source-name))
      do (copy-file file
                    (merge-pathnames (truename target-name)
                                     file))))

#||
;;; See persistent-test.lisp
||#
