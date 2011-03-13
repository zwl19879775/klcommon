;;;
;;; utils.lisp
;;; Kevin Lynx
;;; 3.11.2011
;;;
(in-package cl-writer)

;; read a file context as a whole string
(defun read-whole-file-string (name)
  (with-open-file (in name)
    (let ((ret (make-sequence 'string (file-length in))))
      (read-sequence ret in)
      ret)))

;; write a file with str
(defun write-string-file (file str)
  (with-open-file (out file
                       :direction :output
                       :if-exists :supersede)
    (write-string str out)))

;; get a file mime description by the file name extension
(defun get-file-mime (name)
  (let ((ext (pathname-type (pathname name))))
    (cond 
      ((or (string-equal ext "jpg") (string-equal ext "jpeg"))
       "image/jpeg")
      ((string-equal ext "gif") "image/gif")
      ((string-equal ext "png") "image/png")
      (t nil))))

;; return file-context and mime type
(defun read-binary-file (name)
  (with-open-file (in name :element-type '(unsigned-byte 8))
      (let ((ret (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (read-sequence ret in)
        (values ret (get-file-mime name)))))

;; dump data (in binary format)
(defun dump-binary-file (name data)
  (with-open-file (out name :element-type '(unsigned-byte 8) 
                            :direction :output)
    (write-sequence data out)))

(defun format-time-string ()
  (multiple-value-bind (s m h d mon y) (get-decoded-time)
    (format nil "~a~a~a~a~a~a" y mon d h m s)))

(defun format-upload-filename (name)
  (format nil "cl-writer/~a/~a.~a" (format-time-string)
          (pathname-name name) (pathname-type name)))

(defun get-pathname-dir (name)
  (let ((dir (pathname-directory name)))
    (if (null dir)
      "."
      (second dir))))


