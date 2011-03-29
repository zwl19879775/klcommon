;;;;
;;;; rss-output.lisp
;;;; Output RSS infomation collected by rss-parser to various format.
;;;; Kevin Lynx
;;;; 3.27.2011
;;;;
(in-package :cl-rss)

(defun format-url (url &optional (desc url))
 (with-output-to-string (stream)
   (format stream "<a href=\"~a\"/>~a</a>" url desc)))

(defun write-rss-item (stream rss)
  (map-items rss #'(lambda (item)
                    (format stream "<p><h3>~a</h3>~a<br>~a</p>"
                            (format-url (get-property item :|link|)
                                        (get-property item :|title|))
                            (format-rfc822-time (get-property item :|pubDate|))
                            (get-property item :|description|)))))

(defun output-simple-html (rss)
  "Output a rss to a simple html format and return the result as a string."
  (with-output-to-string (stream)
    (let ((channel (rss-channel rss)))
     (format stream "<html><head><title>~a</title></head>"
             (get-property channel :|title|))
     (format stream "<body><h2>~a</h2><h3>~a</h3>" 
             (format-url (get-property channel :|link|)
                         (get-property channel :|title|))
             (get-property channel :|description|))
     (write-rss-item stream rss)
     (princ "</body></html>" stream))))
 
(defun write-string-file (file str)
  "Write a string to a file."
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede)
    (write-string str out)))

