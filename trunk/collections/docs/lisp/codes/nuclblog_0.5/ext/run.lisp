
(asdf:load-system :nuclblog)
(push #P"~/prjs/ch-asdf_0.2.14/" asdf:*central-registry*)
(asdf:load-system :s-xml-rpc)
(asdf:oos 'asdf:load-op :nuclblog-demo)

(defvar *port* 4000)
(defvar *server* nil)

(defun start-blog ()
  (let* ((port (setf *port* (+ 2 *port*)))
         (ssl-port (1+ port)))
    (setf *server* 
          (nuclblog-demo:start-services :port port :ssl-port ssl-port))))
                                              
(defun stop-blog ()
  (hunchentoot:stop *server*))

