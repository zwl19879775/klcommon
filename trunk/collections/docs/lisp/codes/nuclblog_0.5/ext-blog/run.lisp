
(push #P"~/prjs/ch-asdf_0.2.14/" asdf:*central-registry*)
(asdf:load-system :nuclblog)
(asdf:load-system :ext-blog)

(defvar *port* 4000)
(defvar *server* nil)

(defun start-blog (&optional p)
  (when p (setf *port* p))
  (let* ((port *port*)
         (ssl-port (1+ port)))
    (setf *port* (+ 2 *port*))
    (setf *server* 
          (ext-blog:start-services :port port :ssl-port ssl-port))))
                                              
(defun stop-blog ()
  (hunchentoot:stop *server*))

