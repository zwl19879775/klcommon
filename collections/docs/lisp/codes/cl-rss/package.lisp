;;;;
;;;; package.lisp
;;;; Package definition for cl-rss.
;;;; Kevin Lynx
;;;; 3.29.2011
;;;;

(defpackage cl-rss
  (:use 
   common-lisp 
   s-xml
   date)
  (:export 
    rss channel item
    map-items 
    decode-rss
    decode-rss-file
    write-string-file
    output-simple-html
    decode-rss-from-http)
  (:documentation "A simple RSS reader."))

