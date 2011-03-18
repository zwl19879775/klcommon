;;;
;;; package.lisp
;;; Kevin Lynx
;;; 3.12.2011
;;;

(defpackage cl-writer
  (:use 
   common-lisp 
   s-xml
   s-xml-rpc)
  (:export
    #:get-recent-posts
    #:new-post
    #:edit-post
    #:get-post
    #:get-blogs
    #:get-categories
    #:get-category-title 
    #:get-category-titles
    #:delete-post
    #:upload-image

    #:open-process-xml
    #:make-cppblog-user
    #:make-user-info

    #:insert-imglist-table 

    #:get-all-category-title 
    #:writer-edit-post
    #:writer-post-new)
  (:documentation "A simple blog client"))

