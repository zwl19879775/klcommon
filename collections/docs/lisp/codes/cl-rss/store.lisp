;;;;
;;;; store.lisp
;;;; Implemente RSS channel/item store stuff. I build another abstraction above
;;;; rss-parser, even this is not effictive. But it's more independent.
;;;; This file depends on storage library `Elephant'.
;;;; Kevin Lynx
;;;; 3.30.2011
;;;;
;;;; Depreciated, because it's not effictive and convinent.
;;;;

(in-package :cl-rss)

(defun open-storage (&optional (file "rsscache.db"))
  "Open storage file, remember to close it."
  (open-store `(:clsql (:sqlite3 ,file))))

(defun close-storage ()
  (close-store))

(defun encode-plist (channel btree)
  "Append channel's all properties to the tree."
  (let ((plist (channel-plist channel)))
    (mapcar #'(lambda (p)
                (setf (get-value (car p) btree) (cdr p)))
            plist)))

(defun encode-items (channel btree)
  "Append all items of a channel to the btree."
  (let ((itree (make-btree)))
    (map-items channel #'(lambda (item)
                          (setf (get-value (get-property item :|link|) itree)
                                (item-plist item))))
    (setf (get-value "items" btree) itree)))

(defun get-root-channels ()
  (or (get-from-root "channels")
      (let ((btree (make-btree)))
        (add-to-root "channels" btree)
        btree)))

(defun store-channel (channel)
  "Store a channel in the root. Use the channel link as the key."
  (let ((btree (make-btree))
        (root (get-root-channels)))
    (encode-plist channel btree)
    (encode-items channel btree)
    (setf (get-value (get-property channel :|link|) root) btree)))

(defun decode-items (items-tree)
  (let ((ret))
    (map-btree #'(lambda (key val) (declare (ignore key))
                   (push (make-instance 'item :plist val) ret))
               items-tree)
    ret))

(defun decode-channel (btree)
  "Decode a channel from a btree."
  (let ((channel (make-instance 'channel)))
    (map-btree #'(lambda (key val)
                   (if (string-equal key "items")
                     (setf (channel-items channel) (decode-items val))
                     (push-property channel key val)))
               btree)
    channel))

(defun retrieve-all-channels ()
  "Retrieve all channels from storage."
  (let ((root (get-root-channels))
        (ret))
    (map-btree #'(lambda (link channel)
                   (format t "~a -> ~a~%" link channel)
                   (push (decode-channel channel) ret))
               root)
    ret))

(defun remove-channel (url)
  "Remove a channel from storage."
  (let ((root (get-root-channels)))
    (setf (get-value url root) nil)))

(defun print-all-channels()
  (map-btree #'(lambda (link channel)
                 (format t "~a -> ~a~%" link channel))
             (get-root-channels)))

