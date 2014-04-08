;;; -*- mode:common-lisp coding:utf-8 -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; Load ~/quicklisp.lisp anyway first.
  ;;; Unless compiler would not know `quicklisp-quickstart:install'
  (unless (probe-file "~/quicklisp.lisp")
    (asdf:run-shell-command "curl http://beta.quicklisp.org/quicklisp.lisp -o ~/quicklisp.lisp"))
  (unless (find-package 'quicklisp-quickstart)
    (load "~/quicklisp.lisp")))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (if (probe-file "~/quicklisp/setup.lisp")
      (load "~/quicklisp/setup.lisp")
    (quicklisp-quickstart:install)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package 'stefil)
    (ql:quickload "stefil")))
