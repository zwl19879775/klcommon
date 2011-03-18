;;;
;;; cl-writer.lisp
;;; cl-writer is a simple blog post client, it can helps you to post articles to
;;; your blog.
;;; Kevin Lynx
;;; 3.12.2011
;;;
(in-package cl-writer)

(defvar *img-map-list* (make-hash-table))

(defun get-imglist-name (file)
  (format nil "~a/~a.lst" (get-pathname-dir file)
          (pathname-name file)))

(defun load-imglist (file)
  (load file :if-does-not-exist nil))

;; hash map does not support string key, so convert the string to a keyword.
(defun convert-key (img)
  (intern img :keyword))

(defun insert-imglist-table (img url)
  (setf (gethash (convert-key img) *img-map-list*) url))

(defun get-imglist-url (img)
  (gethash (convert-key img) *img-map-list*))

(defun append-imglist (file img url)
  (insert-imglist-table img url)
  (with-open-file (out file
                       :direction :output
                       :if-exists :append
                       :if-does-not-exist :create)
    (write-line (format nil "(cl-writer:insert-imglist-table \"~a\" \"~a\")"
                        img url) out)))

(defun img-focus-p (name)
  (string-equal name "img"))

(defun append-rel-dir (dir name)
  (format nil "~a/~a" dir name))

(defun http-path-not-p (file)
  (not (string-equal "http://" file :end2 7)))

(defun attr-preprocess (u imgfile dir key value)
  (if (and (eq key :|src|) (http-path-not-p value))
    (let ((file (append-rel-dir dir value))
          (url (get-imglist-url value)))
      (unless url
        (format t "uploading ~a..." file)
        (setf url (upload-image u file))
        (format t "DONE.~%URL:~a ~%" url)
        (append-imglist imgfile value url))
      url)
      value))

(defmacro read-post-file (u post-file context title &body body)
  `(multiple-value-bind (,context ,title)
     (open-process-xml ,post-file 
                       #'(lambda (dir key value)
                           (attr-preprocess ,u (get-imglist-name ,post-file)
                                            dir key value))
                       #'img-focus-p)
     (load-imglist (get-imglist-name ,post-file))
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


