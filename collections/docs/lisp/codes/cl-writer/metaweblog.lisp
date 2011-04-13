;;;
;;; metaweblog.lisp (depend on s-xml-rpc library)
;;; support metaWeblog api to post blogs.
;;; api is referenced from http://www.cppblog.com/[username]/services/metaweblog.aspx
;;; Kevin Lynx
;;; 3.11.2011
;;;
(in-package cl-writer)

(defstruct user-info 
  blogid
  name
  password
  host
  (port 80)
  url)

;; generate a xml-rpc call
(defmacro rpc-call (u m &rest args)
   `(s-xml-rpc:xml-rpc-call (s-xml-rpc:encode-xml-rpc-call ,m ,@args)
                            :url (user-info-url ,u)
                            :host (user-info-host ,u)
                            :port (user-info-port ,u)))

;;
;; metaWeblog.getRecentPosts returns a cons of a structure.
;;
(defun get-recent-posts (u cnt)
  (rpc-call 
    u
    "metaWeblog.getRecentPosts" 
    (user-info-blogid u)
    (user-info-name u)
    (user-info-password u)
    cnt))

;; helper function to retrieve rpc structure members.
(defun struct-member (obj mem)
  (s-xml-rpc:get-xml-rpc-struct-member obj mem))

(defun struct-members (obj &rest mems)
  (mapcar #'(lambda (mem) (struct-member obj mem))
            mems))

(defun print-post (post)
  (mapcar #' (lambda (m) (format t "~a~%" m))
          (struct-members post :|title|
                          :|postid|
                          :|categories|))
  post)

(defun print-posts (posts)
  (mapcar #' print-post posts))

(defun new-post-struct (title desc cates)
  (let ((s (s-xml-rpc:xml-rpc-struct :|title| title
                                     :|description| desc
                                     :|dateTime| (s-xml-rpc:xml-rpc-time))))
    (unless (null cates)
      (setf (s-xml-rpc:get-xml-rpc-struct-member s :|categories|) cates))
    s))

;; in s-xml-rpc source code, 
;;  ((or (stringp arg) (symbolp arg))) then encode as a stirng,
;; as i tried, (symbolp t) will return true. so this is the reason,
;; t will be encoded as a string even respected a boolean.
;; find something is not a string, either not a symbol, but is (eq t)
;; finally, i modified s-xml-rpc source code.
(defun new-post (u title context &optional (cates))
  (rpc-call 
    u
    "metaWeblog.newPost"
    (user-info-blogid u)
    (user-info-name u)
    (user-info-password u)
    (new-post-struct title context cates)
    t))

;; re-post (edit) a specified post by postid
(defun edit-post (u postid title context &optional (cates))
  (rpc-call 
    u
    "metaWeblog.editPost"
    postid
    (user-info-name u)
    (user-info-password u)
    (new-post-struct title context cates)
    t))

;; get a specified post by postid
(defun get-post (u postid)
  (rpc-call
    u
    "metaWeblog.getPost"
    postid
    (user-info-name u)
    (user-info-password u)))

;; actually a user only has one blog
;; metaWeblog ignores 'appKey', and because there is only 1 blog for a user,
;; the blogid is usually ignored too.
(defun get-blogs (u)
  (rpc-call
    u
    "blogger.getUsersBlogs"
    (user-info-blogid u)
    (user-info-name u)
    (user-info-password u)))

(defun print-blogs (blogs)
  (mapcar #' (lambda (blog) (format t "~a~%~a~%~a~%" 
                                    (struct-member blog :|blogid|)
                                    (struct-member blog :|url|)
                                    (struct-member blog :|blogName|))
               blog)
          blogs))

;; get blog categories
(defun get-categories (u)
  (rpc-call
    u
    "metaWeblog.getCategories"
    (user-info-blogid u)
    (user-info-name u)
    (user-info-password u)))

;; get category title by its id
(defun get-category-title (cates id)
  (if (null cates)
    nil
    (let ((c (car cates)))
      (if (string-equal id (struct-member c :|categoryid|))
        (struct-member c :|title|)
        (get-category-title (cdr cates) id)))))

;; get category titles by a list of id
(defun get-category-titles (cates id-list)
  (if (null id-list)
    nil
    (mapcar #' (lambda (id) (get-category-title cates id))
            id-list)))

(defun print-categories (cates)
  (mapcar #' (lambda (cate) (format t "~a~%~a~%~a~%"
                                    (struct-member cate :|categoryid|)
                                    (struct-member cate :|title|)
                                    (struct-member cate :|description|))
               cate)
          cates)
  t)

;; delete a post, always return true.
(defun delete-post (u postid)
  (rpc-call 
    u
    "blogger.deletePost"
    ""
    postid
    (user-info-name u)
    (user-info-password u)
    t))

(defun new-media-object-struct (filename)
  (multiple-value-bind (bits mime) (read-binary-file filename)
    (s-xml-rpc:xml-rpc-struct :|name| (format-upload-filename filename)
                              :|type| mime
                              :|bits| bits)))

;; upload some media files, return a media-url structure.
(defun new-media-object (u filename)
  (rpc-call 
    u
    "metaWeblog.newMediaObject"
    (user-info-blogid u)
    (user-info-name u)
    (user-info-password u)
    (new-media-object-struct filename)))

;; upload an image, return url.
(defun upload-image (u filename)
  (struct-member (new-media-object u filename) :|url|))


