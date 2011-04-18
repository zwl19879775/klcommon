;;;
;;; ext-blog.asd
;;; Kevin Lynx
;;; 4.16.2011
;;;
(asdf:defsystem ext-blog
  :name "ext-blog"
  :depends-on (:hunchentoot :cl-who :ch-asdf 
      :s-xml-rpc
      :nuclblog :hunchentoot-vhost)
  :components
  ((:module
    :ext-blog
    :components
    ((:cl-source-file "defpackage")
     (:cl-source-file "xmlrpc" :depends-on ("defpackage"))
     (:cl-source-file "config" :depends-on ("defpackage"))
     (:cl-source-file "pages")
     (:cl-source-file "ext-blog" :depends-on ("config")
                      :depends-on ("xmlrpc")
                      :depends-on ("defpackage"))
     (:module "static"
      :components ((:static-file ext-blog-css :pathname #p"ext-blog.css")
                   (:static-file black-css :pathname #p"black.css")
                   (:static-file white-css :pathname #p"white.css")
                   (:static-file sbclbutton-png :pathname #p"sbclbutton.png")))
     (:module "ssl"
      :components ((:static-file "openssl-config"
                    :pathname #p"openssl.config")))))))

(asdf:defsystem ext-blog-data
  :name "ext-blog-data"
  :depends-on (:nuclblog-demo)
  :components
  ((:module
    :ext-blog
    :components
    ((:module "log"
      :components ((:static-file "ext-blog-access-log"
                                 :pathname #p"ext-blog-access-log")
                   (:static-file "ext-blog-message-log"
                                 :pathname #p"ext-blog-message-log")))
     (:module "ssl"
      :components ((:static-file "key-pem"
                    :pathname #p"key.pem")
                   (:static-file "certificate-pem"
                    :pathname #p"certificate.pem")))
     (:module "storage")))))
