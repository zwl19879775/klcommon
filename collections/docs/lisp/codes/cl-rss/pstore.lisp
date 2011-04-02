;;;;
;;;; pstore.lisp
;;;; Use `defpclass` to represent the storage.
;;;; Depends on `Elephant` library
;;;;
;;;; Kevin Lynx
;;;; 3.3.1.2011
;;;;

(in-package :cl-rss)

(defmacro push-b (val lst)
  "Push val at the back of lst."
  `(setf ,lst (append ,lst (cons ,val nil))))

(defun open-storage (&optional (file "rsscache.db"))
  "Open storage file, remember to close it."
  (open-store `(:clsql (:sqlite3 ,file))))

(defun close-storage ()
  (close-store))

(defpclass sitem ()
  ((plist :accessor sitem-plist
          :initarg :plist
          :initform nil))
  (:documentation "Represents storage item"))

(defpclass schannel ()
  ((plist :accessor schannel-plist
          :initform nil
          :initarg :plist)
   (items :accessor schannel-items
          :initform nil))
  (:documentation "Represents storage channel"))

(defmethod get-property ((obj schannel) name)
  (cdr (assoc name (schannel-plist obj))))

(defmethod get-property ((obj sitem) name)
  (cdr (assoc name (sitem-plist obj))))

(defun item->s (item)
  "Convert a normal item object to a storage item."
  (make-instance 'sitem :plist (item-plist item)))

(defun channel->s (channel)
  "Convert a normal channel object to a storage channel."
  (let ((sc (make-instance 'schannel :plist (channel-plist channel))))
    (mapcar #'(lambda (item)
                (push (item->s item) (schannel-items sc)))
            (channel-items channel))
    sc))

(defun s->item (sitem)
  "Convert a storage item to normal item."
  (make-instance 'item :plist (sitem-plist sitem)))

(defun s->channel (schannel)
  "Convert a storage channel to a normal channel."
  (let ((c (make-instance 'channel :plist (schannel-plist schannel))))
    (mapcar #'(lambda (sitem)
                (push (s->item sitem) (channel-items c)))
            (schannel-items schannel))
    c))

(defun get-root-channels ()
  "Get root channel btree."
  (or (get-from-root "channels")
      (let ((btree (make-btree)))
        (add-to-root "channels" btree)
        btree)))

(defun store-schannel (channel)
  "Store a normal channel object."
  (let ((root (get-root-channels)))
    (setf (get-value (get-property channel :|rssurl|) root)
          (channel->s channel))))

(defun remove-schannel (url)
  "Remove a channel."
  (remove-kv url (get-root-channels)))

(defun get-schannel (url)
  "Get a storage channel."
  (get-value url (get-root-channels)))

(defun item-existp (item sitems)
  "Check whether item already exist in storage items."
  (member-if #' (lambda (sitem)
                  ;; If the url is equal, always update the item
                  (string-equal (get-property sitem :|link|)
                                (get-property item :|link|)))
             sitems))

;;; Should i remove these items deleted by the author?
(defun merge-items (channel schannel)
  "Merge channel items to schannel items."
  (mapcar #'(lambda (item)
              (let ((s (item-existp item (schannel-items schannel))))
                (if (null s)
                  (push-b (item->s item) (schannel-items schannel))
                  (setf (car s) (item->s item))))) ; update an exist one
          (channel-items channel)))

(defun merge-schannel (channel)
  "Merge a new channel to an exist storage channel."
  (let ((sc (get-schannel (get-property channel :|rssurl|)))) 
    (when sc
      ;; merge property list.
      (setf (schannel-plist sc) (channel-plist channel))
      ;; merge items then.
      (merge-items channel sc))
    sc))

(defun load-all-channels ()
  "Load all storaged channels to normal channels."
  (let ((root (get-root-channels))
        (ret))
    (map-btree #'(lambda (rssurl schannel) (declare (ignore rssurl))
                   (push (s->channel schannel) ret))
               root)
    ret))

(defun map-all-channels (fn) 
 "Travers all channels stored in database."
 (map-btree fn (get-root-channels)))

(defun print-all-channels ()
  (map-btree #'(lambda (rssurl sc)
                 (format t "~a -> ~a~%" rssurl sc))
             (get-root-channels)))

