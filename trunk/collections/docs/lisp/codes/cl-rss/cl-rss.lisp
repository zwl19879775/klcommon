;;;;
;;;; cl-rss.lisp
;;;; The high level abstraction for cl-rss application.
;;;; Kevin Lynx
;;;; 3.31.2011
;;;;

(in-package :cl-rss)

(defun append-new-rss (url)
  "Append a new rss, get it from http server and store it."
  (store-schannel (decode-rss-from-http url)))

(defun remove-rss (url)
  "Remove a rss from storage."
  (remove-schannel url))

(defun write-all-channels ()
  "Write all channels stored in database to html."
  (let ((channels (load-all-channels)))
    ;; generate all channel context html files.
    (mapcar #'(lambda (c)
                (write-channel-html c))
            channels)
    ;; generate an index page to them.
    (write-string-file "index.html"
                       (generate-channels-link-page channels))) t)

(defun refresh-rss (url)
  "Refresh the rss, update the storage channel, keep the history items."
  (format t "Updating ~a~%" url)
  (merge-schannel (decode-rss-from-http url)))

(defun refresh-all-rss ()
  (map-all-channels #'(lambda (url sc)
                        (declare (ignore sc))
                        (refresh-rss url))))

;;; A global variable to store the timer.
(defvar *timer* nil)

;;;
;;; This function depends on SBCL timer functions.
;;;
(defun start-update (&optional (interval 300))
  "Start to update all rss automatically."
  (let ((tm (make-timer #'(lambda ()
                            (refresh-all-rss)))))
    (schedule-timer tm interval :repeat-interval interval)
    (setf *timer* tm)
    tm))

(defun stop-update (&optional (tm *timer*))
  (unschedule-timer tm))

