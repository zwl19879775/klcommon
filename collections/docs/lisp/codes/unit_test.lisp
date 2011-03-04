;; a pratical example from <Practical Common Lisp>
;; Kevin Lynx 3.4.2011
;; NOTE: not worked on clisp
(defvar *test-name* nil)

(defun report-result (result form)
    (format t "~:[FAIL~;pass~] ...~a: ~a~%" result *test-name* form)
     result)

(defmacro combine-results (&body forms)
    (with-gensyms (result) 
        `(let ((,result t)) 
           ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
           ,result)))

(defmacro check (&body forms)
    `(combine-results
        ,@(loop for f in forms collect `(report-result ,f ',f))))

(defmacro deftest (name parameters &body body)
    `(defun ,name ,parameters
        (let ((*test-name* '(append *test-name* (list ',name))))
            ,@body)))

(deftest test-+ ()
    (check 
        (= (+ 1 2) 3)
        (= (+1 2 3) 6)))

