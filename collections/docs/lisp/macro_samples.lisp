#|
 1. macros return value is the macro expand string;
 2. , and ,@ just evaluate the expression in a ` ;
|#

(defmacro nil! (var)
  `(setq ,var nil))
    
(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

;; (our-dolist (x '(a b c)) (print x))
(defmacro our-dolist ((var list &optional result) &body body)
  `(progn
     (mapc #' (lambda (,var) ,@body)
           ,list)
     (let ((,var nil))
       ,result)))

(defmacro for ((var start stop) &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
          (,gstop ,stop))
       ((> ,var ,gstop))
       ,@body)))

;; (our-let ((x 1) (y 2)) (+ x y)) =>
;; ((lambda (x y) (+ x y)) 1 2)
;; car return 1st item of a list, cadr return 2nd item,
;; caddr return 3rd item.
;; mapcar collect lambda return values.
(defmacro our-let (binds &body body)
  `((lambda ,(mapcar #' (lambda (x)
                          (if (consp x) (car x) x))
                     binds)
      ,@body)
    ,@(mapcar #' (lambda (x)
                   (if (consp x) (cadr x) nil))
              binds)))

(defmacro when-bind ((var expr) &body body)
  `(let ((,var ,expr))
      (when ,var
        ,@body)))

;; (when-bind* ((x (find-if #'consp '(a (1 2) b)))
;;              (y (find-if #'oddp x)))
;;      (+ y 10))
;; => 11
;; (let ((x (find-if #'consp '(a (1 2) b))))
;;   (if x (when-bind* ((y (find-if #'oddp x))) (+y 10))))
(defmacro when-bind* (binds &body body)
  (if (null binds)
    `(progn ,@body)
    `(let (,(car binds))
        (if ,(caar binds)
            (when-bind* ,(cdr binds) ,@body)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #' (lambda (s)
                       `(,s (gensym)))
            syms)
      ,@body))

(defmacro if3 (test t-case nil-case ?-case)
  `(case ,test
      ((nil) ,nil-case)
      (? ,?-case)
      (t ,t-case)))

;; (in 'a 'a 'b 'c) => t
(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
        (or ,@(mapcar #' (lambda (c) `(eql ,insym ,c))
                         choices)))))

;; (inq 'a a b c) => t
(defmacro inq (obj &rest args)
  `(in ,obj ,@(mapcar #' (lambda (a) 
                            `',a)
                         args)))

(defmacro in-if (fn &rest choices)
  (let ((fnsym (gensym)))
    `(let ((,fnsym ,fn))
        (or ,@(mapcar #' (lambda (c)
                           `(funcall ,fnsym ,c))
                        choices)))))

(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

(defmacro till (test &body body)
  `(do ()
     (,test)
     ,@body))


(defmacro forever (&body body)
  `(do ()
      (nil)
      ,@body))


