;;;
;;; httpc.lisp
;;; Never mind the file name, this file only try to spawn many http requests in
;;; a short time, to test an http server response speed.
;;; Kevin Lynx
;;; 4.13.2011
;;;

;;; socket stuff
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

(defmacro with-run-time ((var) &body body)
  `(let ((,var (get-internal-real-time)))
      ,@body
      (setf ,var (- (get-internal-real-time) ,var))))

(defun get-host-desc (host port)
  (if (= port 80)
    (format nil "Host: ~a" host)
    (format nil "Host: ~a:~d" host port)))
 
(defun send-http-request (host &key (port 80) (uri "/"))
  "Start an http request, and return the response time from server."
  (with-open-socket-stream
    (stream host port)
    (format-header stream `(("GET ~a HTTP/1.0" ,uri)
                            (,(get-host-desc host port))))
    (finish-output stream)
    (with-run-time (elapsed)
                   (read-line stream nil nil))))

(defun do-profile-seq (count host &key (port 80) (uri "/"))
  (let ((used-time 0))
    (dotimes (c count)
      (setf used-time (+ used-time (send-http-request host :port port :uri uri))))
    (/ used-time count)))

