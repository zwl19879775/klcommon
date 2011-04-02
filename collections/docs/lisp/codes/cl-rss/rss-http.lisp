;;;;
;;;; rss-http.lisp
;;;; Request a rss feed from a http server.
;;;; Kevin Lynx
;;;; 3.28.2011
;;;;

(in-package :cl-rss)

(defparameter +crlf+ (make-array 2
                                 :element-type 'character
                                 :initial-contents '(#\return #\linefeed)))
;;; copied from s-xml-rpc library.
(defun format-header (stream headers)
  (mapc #'(lambda (header)
            (cond ((null (rest header)) (write-string (first header) stream) 
                                        (princ +crlf+ stream))
                  ((second header) (apply #'format stream header) 
                                   (princ +crlf+ stream))))
        headers)
  (princ +crlf+ stream))

(defun tokens (string &key (start 0) (separators (list #\space #\return #\linefeed #\tab)))
  "Seperate a string by ``separators`` to a list of token."
  (if (= start (length string))
      '()
      (let ((p (position-if #'(lambda (char) (find char separators)) string :start start)))
        (if p
            (if (= p start)
                (tokens string :start (1+ start) :separators separators)
                (cons (subseq string start p)
                      (tokens string :start (1+ p) :separators separators)))
            (list (subseq string start))))))

(defmacro with-open-socket-stream ((var host port) &body body)
  "Execute body with a bidirectional socket stream opened to host:port"
  #+openmcl
  `(ccl:with-open-socket (,var :remote-host ,host :remote-port ,port)
    ,@body)
  #+lispworks
  `(with-open-stream (,var (comm:open-tcp-stream ,host ,port))
    ,@body)
  #+sbcl
  (let ((socket-object (gensym)))
    `(let ((,socket-object (make-instance 'sb-bsd-sockets:inet-socket
                                          :type :stream
                                          :protocol :tcp)))
      (sb-bsd-sockets:socket-connect ,socket-object
       (car (sb-bsd-sockets:host-ent-addresses
             (sb-bsd-sockets:get-host-by-name ,host))) ,port)
      (let ((,var (sb-bsd-sockets:socket-make-stream ,socket-object
                                                     :element-type 'character
                                                     :input t
                                                     :output t
                                                     :buffering :none)))
        (unwind-protect
             (progn ,@body)
          (close ,var))))))

(defmacro loop-read-line (var stream &body body)
  "Loop in the http-like headers until end."
  `(do ((,var (read-line ,stream nil nil)
              (read-line ,stream nil nil)))
      ((or (null ,var) (= 1 (length ,var))))
      (progn ,@body)))

(defun print-http-stream (stream)
  (loop-read-line line stream
                  (format t "~a~%" line))
  (do ((c (read-char stream nil nil)
          (read-char stream nil nil)))
    ((null c) t)
    (princ c))
  t)

(defmacro skip-headers (stream)
  `(loop-read-line line ,stream))

(defun parse-host-uri (url)
  "Parse host name and uri from an url."
  (let* ((hp (+ 2 (search "//" url)))
         (up (search "/" url :start2 hp)))
    (values (subseq url hp up)
            (subseq url up))))

(defun create-url (host uri)
  (concatenate 'string "http://" host uri))

(defun split-key-value (string)
  "Split key-value string like `Location: url`"
  (let* ((fn #'(lambda (c) (or (char= c #\space) (char= c #\:) 
                               (char= c #\return))))
         (start (position-if fn string))
         (end (position-if-not fn string :start start))
         (cr (position-if-not fn string :from-end t)))
    (values (subseq string 0 start)
            (subseq string end (1+ cr)))))

(defun find-redirect-url (stream)
  "Find a ``Location`` field in a redirect response headers."
  (loop-read-line line stream
    (multiple-value-bind (key value)
      (split-key-value line)
      (when (string-equal key "Location")
        (return-from find-redirect-url 
                     (parse-host-uri value)))))
  (error "not found ``Location`` field in redirect header."))
     
(defun decode-redirect-rss (stream)
  "Redirect to a new url encoded in stream to decode rss feed."
  (let ((url (find-redirect-url stream)))
    (format t "Redirect to ~a~%" url)
    (decode-rss-from-http url)))

(defun process-http-header (stream status-header)
  "Handle response http headers."
  (let* ((toks (tokens status-header))
         (status (second toks)))
    (cond 
      ((string-equal status "200") (skip-headers stream) t)
      ((or (string-equal status "301") 
           (string-equal status "302")) nil)
      (t (format t "~a~%" status-header)
         (print-http-stream stream)
         (error "http-error: ~a~%" status-header)))))

(defun decode-rss-from-http (url &optional (port 80))
  "Decode a rss feed from a http server response."
  (multiple-value-bind (host uri)
    (parse-host-uri url)
    (with-open-socket-stream 
      (stream host port)
      (format-header stream `(("GET ~a HTTP/1.0" ,uri)
                              ("User-Agent: cl-rss")
                              ("Host: ~a" ,host)))
      (finish-output stream)
      (let ((header (read-line stream nil nil))
            (channel))
        (when (null header) (error "no response from server"))
        (if (process-http-header stream header)
          (setf channel (decode-rss stream))
          (setf channel (decode-redirect-rss stream)))
        ;; append the rss url
        (push-property channel :|rssurl| url)
        channel))))


