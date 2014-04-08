;;; -*- mode:lisp; coding:utf-8; package:cl-user -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require 'asdf))

(in-package cl-user)

#-allegro
(progn
  (load "../lib/cl-ppcre-2.0.4/cl-ppcre.asd")
  (asdf:operate 'asdf:load-op 'cl-ppcre))

#-allegro
(asdf:defsystem pdb-test
    :components ((:file "test-init")
                 (:file "allegro-compat")
                 (:file "persistent" :depends-on ("allegro-compat"))
                 (:file "persistent-test"
                        :depends-on ("allegro-compat" "persistent" "test-init"))))

#+allegro
(asdf:defsystem pdb-test
    :components ((:file "test-init")
                 (:file "persistent")
                 (:file "persistent-test" :depends-on ("persistent" "test-init"))))

(asdf:defsystem ieee754-test
    :components ((:file "test-init")
                 (:file "ieee754")
                 (:file "ieee754-test" :depends-on ("ieee754" "test-init"))))

#-allegro
(asdf:defsystem allegro-compat-test
    :components ((:file "test-init")
                 (:file "allegro-compat")
                 (:file "allegro-compat-test" :depends-on ("allegro-compat" "test-init"))))

(eval-when (load eval)
  (format t "~%To build, execute this:~%(asdf:load-system 'pdb-test)~%(asdf:load-system 'ieee754-test)~%(asdf:load-system 'allegro-compat-test)~%And then:~%(in-package tpdb) (stefil-tpdb-suite)~%(in-package t754) (stefil-ieee754-suite)~%(in-package tallegro) (stefil-tallegro-suite)~%"))
