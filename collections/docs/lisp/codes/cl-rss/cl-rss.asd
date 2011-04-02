;;;;
;;;; cl-rss.asd
;;;; cl-rss is a simple rss reader.
;;;; (asdf:load-system :cl-rss)
;;;; Kevin Lynx
;;;; 3.29.2011
;;;;

(in-package :asdf)

(defsystem :cl-rss
  :name "cl-rss"
  :author "Kevin Lynx (kevinlynx@gmail.com)"
  :version "0.1.0"
  :licence "MIT"
  :description "A simple rss reader"
  :components ((:file "package")
               (:file "rss-parser" :depends-on ("package"))
               (:file "parse-date" :depends-on ("package"))
               (:file "rss-output" :depends-on ("rss-parser") 
                      :depends-on ("parse-date"))
               (:file "rss-http" :depends-on ("rss-parser"))
               (:file "pstore" :depends-on ("rss-parser"))
               (:file "cl-rss" :depends-on ("rss-http")
                      :depends-on ("pstore")
                      :depends-on ("rss-output"))
               (:file "web-adapter" :depends-on ("cl-rss")))

  :depends-on (:net-telent-date :s-xml :elephant :hunchentoot))

