;;;
;;; html-processor.lisp
;;; process html file, to replace some tag values.
;;; Kevin Lynx
;;; 3.11.2011
;;;

(in-package cl-writer)

;; when parse xml in another xml parsing process, it's very necessary to do
;;  this. Otherwise, these callback functions 'name' argument will not be a type
;; of keyword, but a symbol. That can make some bugs in s-xml-rpc.
(setf s-xml:*ignore-namespaces* t)

(defstruct process-arg
  stream
  ;; called when parse some attributes, return value will be used as value.
  attr-preprocess-fn
  ;; mark whether a tag should be focused, focused tag will call
  ;; attr-preprocess-fn
  focus-p-fn 
  dir
  title)

(defun default-focus-p (name)
  (declare (ignore name))
  nil)

(defun default-attr-preprocess (dir key value)
  (declare (ignore key dir))
  value)

(defun output-attributes (arg attributes focus)
  (let ((stream (process-arg-stream arg))
        (dir (process-arg-dir arg))
        (fn (process-arg-attr-preprocess-fn arg)))
    (mapcar #'(lambda (pair) 
                (let ((key (car pair)) (value (cdr pair)))
                  (format stream " ~a=\"~a\"" key 
                          (if focus
                            (funcall fn dir key value)
                            value))))
            attributes)))

(defun trace-xml-new-element-hook (arg name attributes seed)
  (let ((new-seed (cons (1+ (car seed)) (1+ (cdr seed))))
        (focus-fn (process-arg-focus-p-fn arg))
        (stream (process-arg-stream arg)))
    (format stream "<~a" name)
    (output-attributes arg attributes (funcall focus-fn name))
    (princ ">" stream)
    ;; ready to read title
    (when (string-equal name "title")
      (setf (process-arg-title arg) nil))
    new-seed))

(defun trace-xml-finish-element-hook (arg name attributes parent-seed seed)
  (declare (ignore parent-seed attributes))
  (let ((new-seed (cons (1- (car seed)) (1+ (cdr seed)))))
    (format (process-arg-stream arg) "</~a>" name)
    new-seed))

(defun trace-xml-text-hook (arg string seed)
  (let ((new-seed (cons (car seed) (1+ (cdr seed)))))
    (format (process-arg-stream arg) string)
    ;; set title
    (when (null (process-arg-title arg))
      (setf (process-arg-title arg) string))
    new-seed))

(defmacro def-hook-fn (fn arg &rest args)
  `(lambda (,@args) (funcall ,fn ,arg ,@args)))

;; in stream
;; damn the dir, because of this, this funciton is not generic any more.
;; return result file string and title.
(defun process-xml (in dir preprocess-fn focus-p-fn)
  (let* ((arg (make-process-arg :stream nil :dir dir :title ""
                                :attr-preprocess-fn preprocess-fn
                                :focus-p-fn focus-p-fn))
         (context 
           (with-output-to-string (stream)
             (setf (process-arg-stream arg) stream)
             (s-xml:start-parse-xml 
               in
               (make-instance 
                 's-xml:xml-parser-state
                 :seed (cons 0 0) 
                 :new-element-hook (def-hook-fn
                                     #'trace-xml-new-element-hook 
                                     arg name attr seed)
                 :finish-element-hook (def-hook-fn 
                                        #'trace-xml-finish-element-hook 
                                        arg name attr ps seed)
                 :text-hook (def-hook-fn
                              #'trace-xml-text-hook 
                              arg string seed))))))
    (values context (process-arg-title arg))))

;; name, filename
(defun open-process-xml (name preprocess-fn focus-p-fn)
  (with-open-file (in name)
    (process-xml in (get-pathname-dir name) preprocess-fn focus-p-fn)))

(defun test-process-html ()
  (multiple-value-bind (context title) (open-process-xml
                                         "test_blog/test-docutils.html"
                                         #'default-attr-preprocess
                                         #'default-focus-p)
    (write-string-file "test_blog/output.html" context)
    title))

