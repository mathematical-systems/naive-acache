;;; -*- mode:lisp; coding:utf-8; package:allegro -*-

(defpackage allegro
  (:use common-lisp)
  #+sbcl (:shadowing-import-from sb-ext delete-directory)
  #+ccl (:shadowing-import-from ccl delete-directory copy-file)
  #+lispworks (:shadowing-import-from system delete-directory copy-file make-directory file-directory-p)
  (:shadow pathname-directory 
           #+sbcl probe-file
           #+(or sbcl ccl) pathname-name #+(or sbcl ccl) pathname-type
           #+(or sbcl ccl) directory)
  (:export directory probe-file
           pathname-name pathname-directory pathname-type
           probe-directory make-directory
           delete-directory-and-files
           pathname-as-directory file-directory-p copy-file
           list-to-delimited-string delimited-string-to-list
           object-class make-temp-file-name
           re-let split-re replace-re
           make-process-lock with-process-lock))

(in-package allegro)

#+(or sbcl ccl)
(defun pathname-name (pathname)
  "A patch for odd sbcl behavior."
  (let ((name (cl:pathname-name pathname)))
    (if (or (equal ""  name)
            (equal "." name))
        nil
      name)))

#+(or sbcl ccl)
(defun pathname-directory (pathname)
  "A patch for odd sbcl behavior."
  (let* ((namestring (namestring pathname))
         (up-position (search ".." namestring :from-end t)))
    (if (and (not (null up-position))
             (= up-position (- (length namestring) 2))) ; ".." => (:UP)
        (let ((directory (remove "."    ; (:RELATIVE ".") => (:RELATIVE)
                                 (cl:pathname-directory (subseq namestring 0 up-position))
                                 :test #'equal)))
          (if (or (eq :absolute (first directory))
                  (eq :relative (first directory)))
              (append directory (list :up))
            (append '(:relative) directory (list :up))))
      (let ((dot-position (search "." namestring :from-end t)))
        (if (and (not (null dot-position))
                 (= dot-position (- (length namestring) 1))) ; "." => (:RELATIVE)
            (let ((directory (remove "." ; (:RELATIVE ".") => (:RELATIVE)
                                     (cl:pathname-directory (subseq namestring 0 dot-position))
                                     :test #'equal)))
              (if (or (eq :absolute (first directory))
                      (eq :relative (first directory)))
                  directory
                (append '(:relative) directory)))
          (remove "." (cl:pathname-directory pathname) :test #'equal))))))

#+lispworks
(defun pathname-directory (pathname)
  (if (equal "." pathname)
      '(:RELATIVE)
    (cl:pathname-directory pathname)))

#+(or sbcl ccl)
(defun pathname-type (pathname)
  "A patch for odd sbcl behavior."
  (let ((type (cl:pathname-type pathname)))
    (if (equal type "")
        nil
      type)))

#+sbcl
(defun probe-file (filename)
  "A patch for odd sbcl behavior."
  (let ((name (pathname-name filename)))
    (if (null name)
        (probe-directory filename)
      (cl:probe-file filename))))

#+(or sbcl ccl)
(defun directory (pathname)
  "Allegro Compatible directory function."
  (cl:directory (make-pathname :host (pathname-host pathname)
                               :device (pathname-device pathname)
                               :directory (pathname-directory pathname)
                               :name (or (pathname-name pathname) :wild)
                               :version (pathname-version pathname)
                               :type (or (pathname-type pathname) :wild))))

(defun probe-directory (filename)
  "This function tests whether pathspec exists and is a directory, 
   returning the result of applying truename to pathspec if it is, 
   and returning nil if it does not exist or is not a directory."
  (and (cl:probe-file filename)
       (cl:probe-file (make-pathname :directory (pathname-directory (pathname-as-directory (namestring filename)))
                                     #+sbcl :name #+sbcl "."))))

#+(or sbcl ccl)
(defun make-directory (dirname &key verbose #+unix (mode #o775))
  (let ((parent (butlast (pathname-directory (pathname-as-directory dirname)))))
    (if (probe-directory (make-pathname :directory parent))
        (multiple-value-bind (path flag)
            (ensure-directories-exist (pathname-as-directory dirname)
                                      :verbose verbose #+unix :mode #+unix mode)
          (if (not flag)
              (error 'file-error :pathname dirname)
            path))
      (error 'file-error :pathname dirname))))

(defun delete-directory-and-files (directory &key (if-does-not-exist :error)
                                                  (quiet t) force)
  (declare (ignore if-does-not-exist quiet force))
  (loop for path in (directory (pathname-as-directory directory))
      do (if (file-directory-p path)
             (delete-directory-and-files path)
           (delete-file path)))
  (delete-directory directory))

(defun pathname-as-directory (pathname)
  (let* ((pathname (pathname pathname))
         (pathname-name (pathname-name pathname))
         (pathname-type (pathname-type pathname))
         (pathname-directory (or (pathname-directory pathname)
                                 (list :relative))))
    (if (null pathname-name)
        (make-pathname :directory pathname-directory)
      (make-pathname :directory
                     (append pathname-directory (list (format nil
                                                              "~A~@[.~A~]"
                                                              pathname-name
                                                              pathname-type)))))))

#+(or sbcl ccl)
(defun file-directory-p (pathname)
  "Returns t if pathname names a directory and nil if it names a file."
  (if (probe-directory pathname)
      t
    nil))

#+sbcl
(defun copy-file (source destination &key force)
  (let* ((buffer-size 8192)
         (buffer (make-array buffer-size :element-type '(unsigned-byte 8))))
    (with-open-file (in source
                     :direction :input
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist :error)
      (with-open-file (out destination
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists (if force :supersede :error))
        (loop for size = (file-length in) then (decf size buffer-size)
            while (> size 0) do
              (read-sequence buffer in)
              (write-sequence buffer out :end (min size buffer-size))))))
  (probe-file destination))

(defun object-class (object)
  (declare (ignore object))
  (error "Do not call OBJECT-CLASS which is Allegro dialect.  Use CLASS-OF or TYPE-OF instead."))

(defun delimited-string-to-list (string delimiter-string-or-char)
  (let* ((delimiter-string (string delimiter-string-or-char))
         (length (length delimiter-string)))
    (loop for i = 0 then (and j (+ j length))
        for j = (and i (search delimiter-string string :start2 i))
        until (null i)
        collect (subseq string i j))))

(defun list-to-delimited-string (list delimiter-string-or-char)
  (let* ((delimiter-string (string delimiter-string-or-char)))
    (format nil (concatenate 'string "~{~A~^" delimiter-string "~}") list)))

(defun make-temp-file-name (&optional (prefix "temp") (directory "/tmp"))
  "Returns the namestring of a pathname of a non-existent file in directory. 
   The filename will begin with prefix (and additional characters added after
   prefix will ensure the file does not exist)."
  (assert (probe-directory directory))
  (loop for char = #\a then (code-char (1+ (char-code char)))
      for name = (format nil "~A~A~A" prefix char (get-internal-real-time))
      for pathname = (make-pathname :name name
                                    :directory (pathname-directory (probe-directory directory)))
      unless (probe-file pathname) return pathname))

;;; regular expression

(defmacro re-let (regexp string (&rest args) &body body)
  `(ppcre:register-groups-bind ,(mapcar #'first args)
       (,regexp ,string)
       ,@body))

(defmacro split-re (regexp string)
  `(cl-ppcre:split ,regexp ,string))

(defmacro replace-re (regexp1 regexp2 string)
  `(cl-ppcre::regex-replace ,regexp2 ,regexp1 ,string))

;;; mp

(defun make-process-lock (&key name)
  #+sbcl (sb-thread:make-mutex :name name)
  #+lispworks (mp:make-lock :name name))

#+sbcl
(defmacro with-process-lock ((place &key timeout whostate norecursive)
                             &body body)
  (declare (ignore norecursive timeout whostate))
  `(sb-thread:with-recursive-lock (,place) ,@body))

#+lispworks
(defmacro with-process-lock ((lock &key norecursive timeout whostate) &body forms)
  (declare (ignore norecursive))
  `(mp:with-lock (,lock
                  ,@(when whostate (list :whostate whostate))
                  ,@(when timeout (list :timeout timeout)))
     ,@forms))