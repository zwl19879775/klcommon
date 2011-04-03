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

(defmacro with-html-body ((stream title) &body body)
  `(with-output-to-string (,stream)
      (format ,stream "<html><head><title>~a</title></head><body>" ,title)
      ,@body
      (princ "</body></html>" ,stream)))

(defun write-rss-item (stream channel)
  (map-items channel
             #'(lambda (item)
                 (format stream "<p><h3>~a</h3>~a<br>~a</p>"
                         (format-url (get-property item :|link|)
                                     (get-property item :|title|))
                         (format-rfc822-time (get-property item :|pubDate|))
                         (get-property item :|description|)))))

(defun output-simple-html (channel)
  "Output a rss to a simple html format and return the result as a string."
  (with-html-body (stream (get-property channel :|title|))
     (format stream "<h2>~a</h2><h3>~a</h3>" 
             (format-url (get-property channel :|link|)
                         (get-property channel :|title|))
             (get-property channel :|description|))
     (write-rss-item stream channel)))
 
(defun write-string-file (file str)
  "Write a string to a file."
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede)
    (write-string str out)))

(defun convert-url-filename (url)
  "Convert a url to a valid file name."
  (let ((invalid (list #\: #\/ #\? #\=)))
    (substitute-if #\_ #'(lambda (c) 
                           (find c invalid))
                   url)))

(defun get-channel-html-name (channel)
  "Return a channel saved html file name."
  (concatenate 'string (convert-url-filename
                         (get-property channel :|link|))
               ".html"))

(defun write-channel-html (channel)
  "Write a channel to an html."
  (write-string-file (get-channel-html-name channel) 
                     (output-simple-html channel)))

(defun generate-channels-link-page (channels)
  "Generate an index page indexed to all channels."
  (with-html-body (stream "All RSS")
    (mapcar #'(lambda (c)
               (format stream "<h3>~a</h3>"
                (format-url (get-channel-html-name c)
                            (get-property c :|title|))))
            channels)))

