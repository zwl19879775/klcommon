;;;
;;; defpackage.lisp
;;;
(in-package #:cl-user)

(defpackage #:ext-blog
  (:use #:cl #:hunchentoot #:cl-who)
  (:export #:start-services
           #:start-ssl-services))
