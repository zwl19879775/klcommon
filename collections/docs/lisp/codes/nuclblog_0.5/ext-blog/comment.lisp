;;;
;;; comment.lisp
;;; Kevin Lynx
;;; 4.22.2011
;;;
(in-package :blog)

(defclass comment ()
  ((entryid :initarg :entryid
            :initform nil
            :accessor comment-entryid)
   (author :initarg :author
           :initform nil
           :accessor comment-author)
   (email :initarg :email
          :initform nil 
          :accessor comment-email)
   (url :initarg :url
          :initform nil 
          :accessor comment-url)
   (desc :initarg :desc
          :initform nil 
          :accessor comment-desc)
   (id :initarg :id
       :initform nil
       :accessor comment-id)
   (time :initarg :time :accessor comment-time)
   (confirmed :initarg :confirmed
              :initform nil
              :accessor comment-confirmed)))

(defparameter *comments-file-lock* (bt:make-lock "comments-file-lock"))
(defparameter *comments-lock* (bt:make-lock "comments-lock"))

(defun get-next-comment-id (blog)
  (let ((ids (mapcar #'comment-id (blog-comments blog))))
    (if ids
      (1+ (reduce #'max ids))
      0)))

(defmethod store-blog-comments ((blog ext-blog))
  (let ((path (blog-comments-storage-path blog)))
    (ensure-directories-exist path)
    (bt:with-lock-held (*comments-file-lock*)
                       (cl-store:store (blog-comments blog) path))))

(defmethod read-blog-comments ((blog ext-blog) &key 
                                               (path (blog-comments-storage-path blog)))
  (when (and path (probe-file path))
    (bt:with-lock-held (*comments-file-lock*)
    (setf (blog-comments blog)
          (cl-store:restore path)))))

(defun create-blog-comment (blog entryid author email url desc &key
                                 (id (get-next-comment-id blog))
                                 (time (get-universal-time)))
  (let ((comment (make-instance 
                   'comment
                   :id id
                   :entryid entryid
                   :author author
                   :email email
                   :url url
                   :desc desc
                   :time time)))
    (bt:with-lock-held 
      (*comments-lock*)
      (setf (blog-comments blog)
            (cons comment (blog-comments blog)))
      (store-blog-comments blog))))

(defun sort-by-time (lst)
  (sort lst #'< :key #'comment-time))

(defun get-entry-comments (blog entryid)
  "Get all confirmed comments associated to the specified entry."
  (sort-by-time 
    (bt:with-lock-held 
      (*comments-lock*)
      (remove-if-not #'(lambda (comment) 
                         (and (= entryid (comment-entryid comment))
                              (comment-confirmed comment)))
                     (blog-comments blog)))))

(defun get-confirmed-entry (blog)
  (bt:with-lock-held
    (*comments-lock*)
    (remove-if-not #'(lambda (c) (comment-confirmed c))
                   (blog-comments blog))))

(defun get-all-comments (blog)
  (bt:with-lock-held
    (*comments-lock*)
    ;; because 'sort' is destructively function
    (blog-comments blog)))

(defun confirm-comment (blog id)
  (let ((comment (find-if #'(lambda (c) (= id (comment-id c))) 
                          (blog-comments blog))))
    (setf (comment-confirmed comment) t)
    (store-blog-comments blog)
    t))

(defun delete-comment (blog id)
  (bt:with-lock-held (*comments-lock*)
    (when (find id (blog-comments blog) :key #'comment-id)
      (setf (blog-comments blog)
            (delete id (blog-comments blog) :key #'comment-id))
      (store-blog-comments blog)
      t)))

