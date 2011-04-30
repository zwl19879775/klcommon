;;;
;;; defpackage.lisp
;;;
(in-package #:cl-user)

(defpackage #:ext-blog
  (:use #:cl #:hunchentoot #:cl-who)
  (:export #:start-services
           #:update-blog
           #:start-ssl-services))

(defpackage #:xml-rpc-methods
  (:use
    :common-lisp
    :s-xml-rpc
    :ext-blog)
  (:export
    set-metaweblog-handler 
    |blogger.deletePost|
    |metaWeblog.getRecentPosts|
    |metaWeblog.newMediaObject|
    |metaWeblog.editPost|
    |metaWeblog.newPost|))
