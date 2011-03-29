;;;;
;;;; parse-date.lisp
;;;; Parse a date string represents in RFC822 date-time format. 
;;;; This file uses net-telent-date library to parse time string.
;;;; Kevin Lynx
;;;; 3.29.2011
;;;;

(in-package :cl-rss)

(defun format-rfc822-time (time-string)
  "Format a rfc822 time string to a displayed format."
  (let ((utime (date:parse-time time-string)))
    (multiple-value-bind (s m h day month year)
      (decode-universal-time utime)
      (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" 
              year month day h m s))))

