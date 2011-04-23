;;;
;;; ext-blog.lisp
;;; Kevin Lynx
;;; 4.16.2011
;;;
(in-package :ext-blog)

;;;
;;; for debugging hunchentoot errors
#+nil
(progn
  (setf hunchentoot::*show-lisp-backtraces-p* t)
  (setf hunchentoot::*show-lisp-errors-p* t))

(defparameter *blog*
  (make-instance 'blog::ext-blog
                 :owner (cfg-owner-name)
                 :short-name (cfg-short-name)
                 :title (cfg-title)
                 :subtitle (cfg-sub-title)
                 :blog-links (cfg-blog-links)
                 :about-id 0
                 :logo-img-url nil
                 :owner-email (cfg-mail)
                 :page-css (list (cons "ext-blog" "/static/ext-blog.css"))
                 :categories (cfg-categories)
                 :ext-links (cfg-ext-links)
                 :ext-html (cfg-ext-html)
                 :url-root "/blog"
                 :entry-storage-path
                 (merge-pathnames
                   "entries.store"
                   (cfg-storage-path))
                 :comments-storage-path
                 (merge-pathnames
                   "comments.store"
                   (cfg-storage-path))
                 :realm
                 (make-instance 'hunchentoot-auth:realm
                                :user-storage-path
                                (merge-pathnames
                                 "user.store"
                                 (cfg-storage-path))
                                :group-storage-path
                                (merge-pathnames
                                 "group.store"
                                 (cfg-storage-path)))
                 :buttons '((:href-url "https://cyrusharmon.org/projects?project=nuclblog"
                             :id "nuclblog"
                             :alt "nuclblog")
                            (:href-url "http://weitz.de/hunchentoot/"
                             :id "hunchentoot-button"
                             :img-url "/static/hunchentoot-button.png"
                             :alt "hunchentoot")
                            (:href-url "http://www.sbcl.org/"
                             :id "sbclbutton"
                             :img-url "/static/sbclbutton.png"
                             :alt "(get 'sbcl)"))))
    
  

(defparameter *localhost-host*
  (hunchentoot-vhost:make-virtual-host "localhost"
                                       (cfg-virtual-host)))

(defun initialize-blog (blog host)
  (xml-rpc-methods:set-metaweblog-handler *blog*)
  (pushnew (lambda (request)
             (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
             (nuclblog::blog-dispatch request blog))
           (hunchentoot-vhost::virtual-host-dispatch-table host) :test #'equal))

(defun handle-index-page ()
  (hunchentoot:redirect "/blog"))

(defun initialize-server (server)

  ;; add the virtual host to the server
  (hunchentoot-vhost::add-virtual-host *localhost-host* server)

  ;; initialize the blog (that is, associate the blog with the virtual
  ;; host and add a function that will (with the appropriate args)
  ;; call blog-dispatch
  (initialize-blog *blog* *localhost-host*)
    
  (pushnew (hunchentoot:create-regex-dispatcher "^/$" 'handle-index-page)
           (hunchentoot-vhost::virtual-host-dispatch-table *localhost-host*) :test #'equal)

  (pushnew (hunchentoot::create-folder-dispatcher-and-handler
            "/nuclblog-css/"
            (ch-asdf:asdf-lookup-path "asdf:/nuclblog/css"))
           (hunchentoot-vhost::virtual-host-dispatch-table *localhost-host*) :test #'equal)

  (pushnew (hunchentoot::create-folder-dispatcher-and-handler
            "/static/"
            (cfg-rel-path "/static"))
           (hunchentoot-vhost::virtual-host-dispatch-table *localhost-host*) :test #'equal))


(defun start-ssl-services (blog &key (port 4243))
  (let ((key-file (cfg-rel-data-path "/ssl/key-pem"))
        (cert-file (cfg-rel-data-path"/ssl/certificate-pem")))
    (print (list port key-file cert-file))
    (let ((ssl-acceptor (make-instance 'hunchentoot:ssl-acceptor
                                     :ssl-privatekey-file key-file
                                     :ssl-certificate-file cert-file
                                     :port port)))
      (hunchentoot:start ssl-acceptor)
      (initialize-server ssl-acceptor)
      (setf (blog::blog-ssl-port blog) port)
      ssl-acceptor)))

(defun start-services (&key
                       (port 4242)
                       (use-ssl t)
                       ssl-port)
  (let ((access-log-path (cfg-rel-data-path "/log/ext-blog-access-log")))
    (ensure-directories-exist access-log-path)
    (setf hunchentoot:*access-log-pathname*
          access-log-path))
  (let ((message-log-path (cfg-rel-data-path "/log/ext-blog-message-log")))
    (ensure-directories-exist message-log-path)
    (setf hunchentoot:*message-log-pathname*
          message-log-path))
  
  (let ((blog *blog*))
    (setf hunchentoot:*hunchentoot-default-external-format*
          (flex:make-external-format :utf8 :eol-style :lf))
    (let ((acceptor (make-instance 'hunchentoot:acceptor :port port)))
      (hunchentoot:start acceptor)
      (initialize-server acceptor)
      (if use-ssl
          (progn
            (setf (blog::blog-use-ssl-p blog) t)
            (let ((ssl-acceptor (apply #'start-ssl-services blog
                                     (when ssl-port
                                       `(:port ,ssl-port)))))
              (values acceptor ssl-acceptor)))
          acceptor))))


