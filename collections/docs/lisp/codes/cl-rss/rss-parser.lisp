;;;;
;;;; rss-parser.lisp
;;;; Use S-XML to parse a RSS feed.
;;;; Kevin Lynx
;;;; 3.27.2011
;;;; 

(in-package :cl-rss)

(defclass rss ()
  ((version :accessor rss-version
            :initarg :version
            :initform "2.0")
   (channel :accessor rss-channel
            :initform (make-instance 'channel)))
  (:documentation "Represent a rss feed object, only contains a channel and
   version description."))

(defclass channel ()
  ((plist :accessor channel-plist
          :initarg :plist
          :initform nil)
   (items :accessor channel-items
          :initform nil))
  (:documentation "A channel object has some properties and some items."))

(defmethod add-item (item (c channel))
  (push item (channel-items c)))

(defclass item ()
  ((plist :accessor item-plist
          :initarg :plist
          :initform nil))
  (:documentation "An item is an object only contains some properties."))

(defmethod push-property ((obj item) name value)
  "Push a property to an item object."
  (push (cons name value) (item-plist obj)))

(defmethod push-property ((obj channel) name value)
  "Push a property to a channel object."
  (push (cons name value) (channel-plist obj)))

(defmethod get-property ((obj item) name)
  "Get a property of an item by its name."
  (cdr (assoc name (item-plist obj))))

(defmethod get-property ((obj channel) name)
  "Get a property of a channel by its name."
  (cdr (assoc name (channel-plist obj))))

(defun decode-rss-new-element (name attributes seed)
  "Callback function for s-xml, called when enter a new xml element."
  (case name
    (:|rss| (make-instance 'rss :version (cdr (car attributes))))
    ;; give channel children a channel object.
    (:|channel| (rss-channel seed))
    ;; give item children a new item object.
    (:|item| (make-instance 'item))
    (t seed)))

(defun decode-rss-finish-element (name attributes parent-seed seed)
  "Callback function for s-xml, called when an element parsed done."
  (declare (ignore attributes))
  (case name
    (:|item| (add-item seed parent-seed) parent-seed)
    (:|channel| (setf (channel-items seed) (reverse (channel-items seed))) 
                parent-seed)
    (:|rss| seed)
    (t (push-property parent-seed name seed) parent-seed)))

(defun decode-rss-text (string seed)
  "Callback function for s-xml."
  (declare (ignore seed))
  string)

(defun skip-unicode-bom (stream)
  "Because some UTF-8 file will contain some mark characters at the begin of a
  file, so here is necessary to skip the mark. Lisp can detect this
  automatically!"
  (let ((char (peek-char nil stream nil nil)))
    (when (char= char #\ZERO_WIDTH_NO-BREAK_SPACE)
      (read-char stream))))

(defun decode-rss (stream)
  "Decode a rss feed from a stream."
  (skip-unicode-bom stream)
  (let ((rss
          (s-xml:start-parse-xml 
            stream
            (make-instance 's-xml:xml-parser-state
                           :new-element-hook #'decode-rss-new-element
                           :finish-element-hook #'decode-rss-finish-element
                           :text-hook #'decode-rss-text))))
   (rss-channel rss)))

(defun decode-rss-file (file)
  "Decode a rss feed from a file."
  (with-open-file (stream file)
    (decode-rss stream)))

(defun map-items (channel fn)
  "Travers all the rss items in a rss object."
  (mapc #'(lambda (item) (funcall fn item))
        (channel-items channel)))

;;; TEST utils
(defun print-item (item)
  "(map-items rss #'print-items)"
  (mapcar #'(lambda (p) (format t "~a~%" p))
          (item-plist item))
  (format t "~%"))

