;;;;
;;;; web-adapter.lisp
;;;; Adapter cl-rss to a web server `hunchentoot`.
;;;; Kevin Lynx
;;;; 3.31.2011
;;;;
;;;; see reference(a blog tutorial):
;;;;    http://roeim.net/vetle/docs/cl-webapp-intro/part-1/
;;;;

(in-package :cl-rss)

(defvar *web-acceptor* nil)

(defun get-channel-view-name (channel)
  (concatenate 'string "/view/?" 
               (get-property channel :|rssurl|)))

(defun format-operate-string (channel)
  (let ((refresh
          (format-url
            (format nil "/refresh/?~a" (get-property channel :|rssurl|))
            "Refresh"))
        (remove
          (format-url
            (format nil "/remove/?~a" (get-property channel :|rssurl|))
            "Remove")))
    (concatenate 'string refresh "  " remove)))

(defun generate-index-page ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (with-html-body (stream "All RSS")
    (princ (format-url "/new/" "Add new RSS") stream)
    (mapcar #'(lambda (c)
               (format stream "<h3>~a</h3>~a"
                (format-url (get-channel-view-name c)
                            (get-property c :|title|)) (format-operate-string c)))
            (load-all-channels))))

(defun generate-channel-page ()
  (setf (hunchentoot:content-type*) "text/html; charset=utf-8")
  (let ((url-part (hunchentoot:query-string*))
        (channel))
    (format t "request: ~a~%" url-part)
    (setf channel (s->channel (get-schannel url-part)))
    (output-simple-html channel)))

(defun generate-add-new-page ()
  (cond 
    ((eq (hunchentoot:request-method*) :GET)
     (with-html-body (stream "Add new")
                     (format stream "<form action=\"?add\" method=\"POST\">
                             <input style=\"width: 20em;\" type=\"text\" name=\"rssurl\"/>
                             <input style=\"display: block\" type=\"submit\" value=\"Add\"/>
                             </form>")))
   ((eq (hunchentoot:request-method*) :POST)
    (format t "add-new: ~a~%" (hunchentoot:post-parameter "rssurl"))
    (append-new-rss (hunchentoot:post-parameter "rssurl"))
    (hunchentoot:redirect "/"))))

(defun handle-refresh-channel ()
  (let ((url-part (hunchentoot:query-string*)))
    (format t "refresh: ~a~%" url-part)
    (refresh-rss url-part)
    (hunchentoot:redirect "/")))

(defun handle-remove-channel ()
  (let ((url-part (hunchentoot:query-string*)))
    (format t "remove: ~a~%" url-part)
    (remove-rss url-part)
    (hunchentoot:redirect "/")))

(defun set-request-handlers ()
  (setq hunchentoot:*dispatch-table* 
        (list (hunchentoot:create-regex-dispatcher "^/$" 'generate-index-page)
              (hunchentoot:create-regex-dispatcher "^/new/$" 'generate-add-new-page)
              (hunchentoot:create-regex-dispatcher "^/refresh/$" 'handle-refresh-channel)
              (hunchentoot:create-regex-dispatcher "^/remove/$" 'handle-remove-channel)
              (hunchentoot:create-regex-dispatcher "^/view/$" 'generate-channel-page))))

(defun start-server (&optional (port 8080))
  "Attach cl-rss to the web server, and start the server."
  (set-request-handlers)
  ;; must change to utf8 encode, otherwise the server will not work.
  (setf hunchentoot:*hunchentoot-default-external-format*
          (flex:make-external-format :utf8 :eol-style :lf))
  (setf *web-acceptor* (make-instance 'hunchentoot:acceptor :port port))
  (open-storage)
  ;(start-update) ; the timer seems has some bugs
  (hunchentoot:start *web-acceptor*))

(defun stop-server (&optional (acceptor *web-acceptor*))
  ;(stop-update)
  (close-storage)
  (hunchentoot:stop acceptor))

