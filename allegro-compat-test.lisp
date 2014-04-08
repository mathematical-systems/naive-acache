;;; -*- mode:lisp; coding:utf-8; package:test-allegro-compat -*-

(defpackage test-allegro-compat
  (:nicknames tallegro)
  (:use common-lisp allegro stefil)
  (:shadow deftest)
  (:shadowing-import-from allegro
   directory probe-file pathname-name pathname-directory pathname-type))

(in-package test-allegro-compat)

(defparameter *stefil-allegro-compat-suite* (defsuite stefil-tallegro-suite))

(defmacro deftest (name args &body body)
  `(let ((stefil::*suite* *stefil-allegro-compat-suite*))
     (stefil:deftest ,name ,args
       ,@body)))

;;; テストでのファイルの読み書きで利用するディレクトリ
(defvar *test-tallegro-dirname* "/tmp/tallegro/")

;;; テスト用ディレクトリを用意する
(defixture with-test-work-dir
  (:setup
   (make-directory *test-tallegro-dirname*))
  (:teardown
   (delete-directory-and-files *test-tallegro-dirname*)))

(defun make-empty-file (path)
  (with-open-file (s path :if-does-not-exist :create)))

;;; redefined sbcl functions
(deftest pathname-compat-test ()
  (is (eq (pathname-name ".") nil))
  (is (eq (pathname-name "..") nil))
  (is (eq (pathname-name "./..") nil))
  (is (eq (pathname-name "../.") nil))
  (is (eq (pathname-type ".") nil))
  (is (eq (pathname-type "..") nil))
  (is (eq (pathname-type "./..") nil))
  (is (eq (pathname-type "../.") nil))
  (is (equal (pathname-directory ".") '(:relative)))
  (is (equal (pathname-directory "..") '(:relative :up)))
  (is (equal (pathname-directory "./..") '(:relative :up)))
  (is (equal (pathname-directory "../.") '(:relative :up)))
  (is (equal (pathname-name "/home/kuroda/../foo/../bar")
             "bar"))
  (is (equal (pathname-directory "/home/kuroda/../foo/../bar")
             '(:absolute "home" "kuroda" :up "foo" :up)))
  (is (equal (pathname-directory "/home/kuroda/../foo/.././bar/.")
             '(:absolute "home" "kuroda" :up "foo" :up "bar")))
  (is (probe-file "/etc/passwd"))
  (is (not (probe-file "/etc/passwd/")))
  (is (not (equalp (directory "/tmp") (directory "/tmp/")))))

;;; directory
;;; aclのdirectoryの代替定義
;;; ただし指定されたディレクトリ以下のディレクトリについては、 acl と sbcl で動作が異なる。 
;;;   acl : ファイルと同じように pathname が作られる。
;;;   sbcl: ディレクトリとして pathname が作られる。
(deftest test-directory ()
  "directoryの動作確認"
  (with-fixture with-test-work-dir
    ;; 末尾が「/」でない場合は自身の pathname のみ
    (let ((lst (directory "/tmp")))      
      (is (= (length lst) 1))
      (is (pathnamep (first lst)))
      ;; sbcl版ではディレクトリとしてpathnameが作られる
      #+sbcl (is (null (pathname-name (first lst)))))
    ;; 確認用の一時ファイルを3つ用意する
    (loop for i from 1 to 3
       for file = (merge-pathnames (format nil "~D.txt" i) *test-tallegro-dirname*)
       do (make-empty-file file))
    ;; 確認用の一時ディレクトリを2つ用意する
    (loop for i from 1 to 2
       for dir = (merge-pathnames (format nil "dir-~D" i) *test-tallegro-dirname*)
       do (make-directory dir))
    ;; ディレクトリ以下のファイルとディレクトリを取得
    (let ((lst (directory *test-tallegro-dirname*)))
      (is (= (length lst) 5)))
    ;; 対象が存在ない場合はNIL
    (is (null (directory (make-pathname
                          :directory (append (pathname-directory
                                              (pathname *test-tallegro-dirname*))
                                             '("aaaaa"))))))))

;;; probe-directory
;;;  "This function tests whether pathspec exists and is a directory, 
;;;   returning the result of applying truename to pathspec if it is, 
;;;   and returning nil if it does not exist or is not a directory."
(deftest test-probe-directory ()
  "probe-directoryの動作確認"
  (with-fixture with-test-work-dir
    ;; 存在するなら pathname 
    (is (pathnamep (probe-directory *test-tallegro-dirname*)))
    ;; 存在しない場合は NIL 
    (is (null (probe-directory (make-pathname
                                :directory (append
                                            (pathname-directory (pathname *test-tallegro-dirname*))
                                            '("aaaaa"))))))
    ;; ファイルを用意
    (let ((tmp-fname (merge-pathnames
                      (pathname "1.txt")
                      *test-tallegro-dirname*)))
      (make-empty-file tmp-fname) 
      ;; ファイルの場合は NIL
      (is (and (null (probe-directory tmp-fname)) (probe-file tmp-fname))))))

;;; make-directory
(deftest test-make-directory ()
  "make-directoryの動作確認"
  (with-fixture with-test-work-dir
    (let* ((new-dir (make-pathname :directory (append
                                               (pathname-directory *test-tallegro-dirname*)
                                               '("new")))))
      (is (make-directory new-dir))
      (is (probe-directory new-dir))
      ;; 同じファイルを作ろうとするとエラー
      (signals file-error (make-directory new-dir)))))

;;; pathname-as-directory
;;; aclのexcl:pathname-as-directoryの代替定義
(deftest test-pathname-as-directory ()
  "pathname-as-directoryの動作確認"
  ;; テストパターンはaclのマニュアルから引用
  (is (pathname-match-p (pathname-as-directory "foo") (pathname "foo/")))
  (is (pathname-match-p (pathname-as-directory "foo/") (pathname "foo/")))
  (is (pathname-match-p (pathname-as-directory "foo/bar") (pathname "foo/bar/")))
  (is (pathname-match-p (pathname-as-directory "foo/bar/") (pathname "foo/bar/")))
  #-ccl (is (pathname-match-p (pathname-as-directory ".") (pathname ""))) ; これで好い
  (is (pathname-match-p (pathname-as-directory "..") (pathname "../")))
  (is (pathname-match-p (pathname-as-directory "./foo") (pathname "foo/")))
  (is (pathname-match-p (pathname-as-directory "./foo/../bar") (pathname "foo/../bar/")))
  (is (pathname-match-p (pathname-as-directory "../foo/") (pathname "../foo/")))
  (is (pathname-match-p (pathname-as-directory "../foo/../bar") (pathname "../foo/../bar/"))))

;;; file-directory-p
;;; Returns t if pathname names a directory and nil if it names a file.
(deftest test-file-directory-p ()
  "file-directory-pの動作確認"
  (with-fixture with-test-work-dir
    (is (file-directory-p *test-tallegro-dirname*))
    ;; 対象が存在しない場合は NIL
    (is (null (file-directory-p (make-pathname
                                 :directory (append
                                             (pathname-directory (pathname *test-tallegro-dirname*))
                                             '("aaaaa"))))))
    ;; ファイルなら NIL
    (let ((tmp-fname (merge-pathnames (pathname "1.txt") *test-tallegro-dirname*)))
      (make-empty-file tmp-fname)
      (is (null (file-directory-p tmp-fname))))))

;;; copy-file
;;; aclのsys:copy-fileの代替定義
#-ccl
(deftest test-copy-file ()
  "copy-fileの動作確認"
  (with-fixture with-test-work-dir
    ;; テスト用ディレクトリに新規ファイルを作成し、別名でコピーする
    (let ((from-fname (merge-pathnames (pathname "from.txt") *test-tallegro-dirname*))
          (to-fname (merge-pathnames (pathname "to.txt") *test-tallegro-dirname*))
          (sample-lines '("aaaaaaaaaa" "bbbbbbbbbb" "cccccccccc")))
      (with-open-file (os from-fname :direction :output)
        (dolist (line sample-lines)
          (format os "~a~%" line)))
      (is (copy-file from-fname to-fname))
      ;; ファイルサイズの確認
      (let ((from-file-length (with-open-file (s from-fname)
                                (file-length s)))
            (to-file-length (with-open-file (s to-fname)
                              (file-length s))))
        (is (= from-file-length to-file-length)))
      ;; コピー先のファイルと比較する
      (with-open-file (is to-fname :direction :input)
        (loop for line = (read-line is nil 'eof)
              for n from 0
              until (eq line 'eof)
              do (is (string= (nth n sample-lines) line))))
      ;; コピー先のディレクトリがなければエラー
      (signals file-error
        (copy-file from-fname
                   (merge-pathnames (pathname "to.txt")
                                    (make-pathname
                                     :directory (append
                                                 (pathname-directory (pathname *test-tallegro-dirname*))
                                                 '("aaaaa"))))))
      ;; コピー元がなければエラー
      (signals file-error
        (copy-file (merge-pathnames (pathname "from2.txt") *test-tallegro-dirname*)
                   to-fname)))))

    
;;; object-class
;;; aclのcg-user:object-classの代替定義だったが、こちらでは廃止する。
;;; プログラムを書く場合は代わりに type-of を使うこと。
(deftest test-object-class ()
  (signals error (object-class 1)))

;;; delimited-string-to-list
;;; aclのexcl:delimited-string-to-listの代替定義
;;; テストパターンはaclのマニュアルから引用
(deftest test-delimited-string-to-list ()
  "delimited-string-to-listの動作確認"
  (let ((lst (delimited-string-to-list "one two three" #\space)))
    (is (= (length lst) 3))
    (is (string= (nth 0 lst) "one"))
    (is (string= (nth 1 lst) "two"))
    (is (string= (nth 2 lst) "three")))
  (let ((lst (delimited-string-to-list "one, two, three" ", ")))
    (is (= (length lst) 3))
    (is (string= (nth 0 lst) "one"))
    (is (string= (nth 1 lst) "two"))
    (is (string= (nth 2 lst) "three")))
  (let ((lst (delimited-string-to-list "one, two, three" ",")))
    (is (= (length lst) 3))
    (is (string= (nth 0 lst) "one"))
    (is (string= (nth 1 lst) " two"))
    (is (string= (nth 2 lst) " three")))
  ;; テストパターンを追加  
  (let ((lst (delimited-string-to-list "one. two. three" ".")))
    (is (= (length lst) 3))
    (is (string= (nth 0 lst) "one"))
    (is (string= (nth 1 lst) " two"))
    (is (string= (nth 2 lst) " three"))))

;;; list-to-delimited-string
;;; aclのlist-to-delimited-stringの代替定義
;;; テストパターンはaclのマニュアルから引用
(deftest test-list-to-delimited-string ()
  "list-to-delimited-stringの動作確認"
  (is (string= (list-to-delimited-string '("one" "two" "three") #\space) "one two three"))
  (is (string= (list-to-delimited-string '(:foo :bar) ", ") "FOO, BAR")))
