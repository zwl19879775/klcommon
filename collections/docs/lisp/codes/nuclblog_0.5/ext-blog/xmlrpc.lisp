;;;
;;; xmlrpc.lisp
;;; Use s-xml-rpc to provide nuclblog metaweblog API.
;;; Kevin Lynx
;;; 4.12.2011
;;;

(in-package xml-rpc-methods)

(defparameter *xml-rpc-uri* "/api/metaweblog")
(defparameter *process-blog* nil)

(defun handle-metaweblog-request (blog)
  "Handle metaweblog http request."
  (setf *process-blog* blog)
  (let ((stream (hunchentoot:raw-post-data :want-stream t)))
    (s-xml-rpc::handle-xml-rpc-call stream 0)))

(defun set-metaweblog-handler (blog &key (uri *xml-rpc-uri*))
  "Set metaweblog api uri, use this function instead startup a separate http
  server."
  (setf s-xml-rpc:*xml-rpc-package* (find-package :xml-rpc-methods))
  (nuclblog::define-blog-handler (blog :uri uri)
    ()
    #'handle-metaweblog-request))

;;; nuclblog extend util functions
(defun blog-entry->post (blog entry)
  "Convert a blog entry to a post structure which can be handled by s-xml-rpc."
  (s-xml-rpc:xml-rpc-struct 
    "dateCreated" (s-xml-rpc:xml-rpc-time (nuclblog::blog-entry-time entry))
    "description" (nuclblog::blog-entry-contents entry)
    "title" (nuclblog::blog-entry-title entry)
    "categories" (nuclblog::blog-entry-category entry)
    "link" (nuclblog::make-entry-url blog entry)
    "postid" (nuclblog::blog-entry-number entry)))

(defun get-recent-posts (blog number)
  "Get recent entries and convert them to posts."
  (loop for entry in (nuclblog::sorted-blog-entries blog)
        for i below number
         collect (blog-entry->post blog entry)))

(defun media-object-url (url)
  (s-xml-rpc:xml-rpc-struct "url" url))

(defun save-media-object (media-obj)
  (let* ((name (get-xml-rpc-struct-member media-obj :|name|))
         (path (merge-pathnames name 
                                (ch-asdf:asdf-lookup-path 
                                  "asdf:/ext-blog/ext-blog/static"))))
    (format t "save media file: ~a~%" path)
    (ensure-directories-exist path)
    (with-open-file (file path :element-type '(unsigned-byte 8)
                          :direction :output)
      (write-sequence (get-xml-rpc-struct-member media-obj :|bits|) file))
    (concatenate 'string "/static/" name)))

(defun update-entry (blog id category title content)
  (let ((entry (nuclblog::get-entry id blog)))
    (if entry
      (progn
        (setf (nuclblog::blog-entry-category entry) category)
        (setf (nuclblog::blog-entry-title entry) title)
        (setf (nuclblog::blog-entry-contents entry) content)
        (setf (nuclblog::blog-entry-revised-time entry) (get-universal-time))
        (let ((path (nuclblog::blog-entry-storage-path blog)))
          (when path (nuclblog::store-blog-entries blog path)))
        t)
      nil)))

;;; xml-rpc methods

(defun |blogger.deletePost| (app-key postid username password publish)
  (declare (ignore app-key username password publish))
  (nuclblog::delete-blog-entry *process-blog* (parse-integer postid))
  t)

(defun |metaWeblog.getRecentPosts| (blogid username password number)
  (declare (ignore blogid username password))
  (get-recent-posts *process-blog* number))

(defun |metaWeblog.newPost| (blogid username password post publish)
  (declare (ignore blogid username password publish))
  (let ((entry-lst
          (nuclblog::create-blog-entry 
            *process-blog* 
            (s-xml-rpc:get-xml-rpc-struct-member post :|categories|)
            (s-xml-rpc:get-xml-rpc-struct-member post :|title|)
            (s-xml-rpc:get-xml-rpc-struct-member post :|description|)
            (hunchentoot-auth:session-realm-user 
              (nuclblog::blog-realm *process-blog*)))))
    (if entry-lst
      (format nil "~a" (nuclblog::blog-entry-number (first entry-lst)))
      "")))

(defun |metaWeblog.editPost| (postid username password post publish)
  (declare (ignore username password publish))
  (update-entry 
    *process-blog* 
    (parse-integer postid)
    (s-xml-rpc:get-xml-rpc-struct-member post :|categories|)
    (s-xml-rpc:get-xml-rpc-struct-member post :|title|)
    (s-xml-rpc:get-xml-rpc-struct-member post :|description|)))

(defun |metaWeblog.newMediaObject| (blogid username password media-obj)
  (declare (ignore blogid username password))
  (let ((url (save-media-object media-obj)))
    (media-object-url url)))


