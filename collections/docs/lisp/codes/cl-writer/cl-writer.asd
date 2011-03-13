;;;
;;; cl-writer.asd
;;; cl-writer is a simple blog post client, it can helps you to post articles to
;;; your blog.
;;; (asdf:load-system :cl-writer)
;;; Kevin Lynx
;;; 3.12.2011
;;;

(in-package :asdf)

(defsystem :cl-writer
  :name "cl-writer"
  :author "Kevin Lynx (kevinlynx@gmail.com)"
  :version "0.1.0"
  :licence "MIT"
  :description "A simple blog client"
  :components ((:file "package")
               (:file "utils" :depends-on ("package"))
               (:file "metaweblog" :depends-on ("utils"))
               (:file "html-processor" :depends-on ("utils"))
               (:file "cl-writer" :depends-on("html-processor")
                      :depends-on("metaweblog"))
               (:file "config" :depends-on("cl-writer")))
  :depends-on (:s-xml-rpc :s-xml))

