;;;;
;;;; package.lisp
;;;; Package definition for cl-rss.
;;;; Kevin Lynx
;;;; 3.29.2011
;;;;

(defpackage cl-rss
  (:use 
   common-lisp 
   sb-ext ;; for timer
   s-xml
   date
   elephant)
  (:export 
    rss channel item
    map-items 
    decode-rss
    decode-rss-file
    write-string-file
    output-simple-html
    decode-rss-from-http

    append-new-rss
    remove-rss
    refresh-rss
    refresh-all-rss
    start-update
    stop-update
    start-server
    stop-server
    open-storage
    close-storage)

  (:documentation "A simple RSS reader."))

