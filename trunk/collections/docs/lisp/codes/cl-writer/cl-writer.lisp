;;;
;;; cl-writer.lisp
;;; cl-writer is a simple blog post client, it can helps you to post articles to
;;; your blog.
;;; Kevin Lynx
;;; 3.12.2011
;;;
(in-package cl-writer)

(defun img-focus-p (name)
  (string-equal name "img"))

(defun append-rel-dir (dir name)
  (format nil "~a/~a" dir name))

(defun attr-preprocess (u dir key value)
  (if (eq key :|src|)
    (let ((file (append-rel-dir dir value))
          (url nil))
      (format t "uploading ~a..." file)
      (setf url (upload-image u file))
      (format t "DONE.~%URL:~a ~%" url)
      url)
      value))

(defmacro read-post-file (u post-file context title &body body)
  `(multiple-value-bind (,context ,title)
      (open-process-xml ,post-file #'(lambda (dir key value)
                                      (attr-preprocess ,u dir key value))
                        #'img-focus-p)
      ,@body))

;; if want to category this post:
;; (writer-post-new u file (get-category-titles (get-categories u) 
;; (list id1 id2)))
(defun writer-post-new (post-file &key (u (get-default-user))(cates))
  (read-post-file u post-file context title
                  (new-post u title context cates)))

(defun writer-edit-post (postid post-file &key (u (get-default-user))
                                (cates))
  (read-post-file u post-file context title
                  (edit-post u postid title context cates)))

(defun get-all-category-title (&optional (u (get-default-user)))
  (mapcar #' (lambda (c) (struct-member c :|title|))
          (get-categories u)))

(defun make-cppblog-user (name password)
  (make-user-info :blogid ""
                  :name name
                  :password password
                  :url (cppblog-rpc-url name)
                  :host (cppblog-host)))

(defun cppblog-host ()
  "www.cppblog.com")

(defun cppblog-rpc-url (uname)
  (format nil "/~a/services/metaweblog.aspx" uname))


