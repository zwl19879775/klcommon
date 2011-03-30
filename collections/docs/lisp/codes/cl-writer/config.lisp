;;;
;;; config.lisp
;;; load user config file from home directory .cl-writer.lisp
;;; Kevin Lynx
;;; 3.12.2011
;;;
(in-package cl-writer)

(defvar *default-user* nil)

(defun get-config-name ()
  (make-pathname :name ".cl-writer" :type "lisp"
                 :defaults (user-homedir-pathname)))

(defun get-default-user ()
  *default-user*)

;;
;; in config file, must create a new user and set as *default-user*.
;; i.e: (setf *default-user* (make-cppblog-user "kevinlynx" "password"))
;; i.e: (setf *default-user* (make-user-info :name "kevinynx"
;;                      :password "123" :host "www.cppblog.com"
;;                      :url "/kevinlynx/services/metaweblog.aspx"))
(unless (load (get-config-name) :if-does-not-exist nil)
  (format t "Load config file (~a) failed.~%" (get-config-name)))

