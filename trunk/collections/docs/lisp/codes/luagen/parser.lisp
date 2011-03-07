#|
    parser.lisp
    Kevin Lynx
    3.6.2011

    Parser generates grammar tree in a lisp list form:
    (('func func-tok arg-n) ('class class-tok (('func func-tok arg-n) ('func
                func-tok arg-n))))

    And code generator will generate codes recursively.
|#

(defconstant t-func 1)
(defconstant t-class 2)

(defun match-tok (ls tok expect-tok-t)    
    (if (not (token= tok expect-tok-t))
        (error "expected ~a, but got ~a" expect-tok-t (token-tok tok))
        (get-token ls)))

;; skip to the end of the function
(defun skip-func-body (ls now-tok)
    (do ((tok now-tok (get-token ls)))
        ((token= tok tk-rob) 'done)))

;; return the count of arguments.
(defun skip-func-arg (ls now-tok)
    (let ((arg-n 1) (tk-cnt 0))
        (do ((tok now-tok (get-token ls)))
             ((token= tok tk-rb) 
                (if (= tk-cnt 1)
                    0
                    (values arg-n tk-cnt)))
                (if (token= tok tk-comma)
                    (setf arg-n (+ arg-n 1))
                    (setf tk-cnt (+ tk-cnt 1))))))

;; (t-func token arg-number)
(defun parse-func (ls tok)
    (let ((tok-name (match-tok ls tok tk-symbol))) ;skip return-type identifier
        (let* ((tok-next (match-tok ls tok-name tk-symbol))
               (arg-n (skip-func-arg ls tok-next))
               (ret (list t-func tok-name)))
            (let ((tok-body (get-token ls)))
                (if (token= tok-body tk-sem)
                    (append ret (list arg-n)) ;function declaration
                    (progn (skip-func-body ls tok-body) ;function definition
                        (append ret (list arg-n))))))))

;; parse function list in '{' and '}
(defun parse-func-list (ls tok)
    (if (token= tok tk-rob)
        nil
        (append (list (parse-func ls tok)) 
            (parse-func-list ls (get-token ls)))))

;; parse a class definition
;; (t-class token (func func func))
(defun parse-class (ls tok)
    (let* ((tok-name (match-tok ls tok tk-class)) ;skip 'class' keyword  
           (ret (list t-class tok-name))
           (tok-next (match-tok ls tok-name tk-symbol)))
        (if (token= tok-next tk-sem)
            ret 
            (progn (setf tok-next (match-tok ls tok-next tk-lob))
                (append ret (list (parse-func-list ls tok-next)))))))

;; currently i assume the processed file only contains class definitions
#|
  -- test.h
  class A {
      some functions
  };
  class B {
      some functions
  };
|#
(defun parse-class-list (ls)
    (let ((tok (get-token ls)))
        (if (not (token= tok tk-class))
            nil
            (let ((ret (list (parse-class ls tok)))
                  (tok-next (get-token ls))) ; tk-sem expected
                    (append ret (parse-class-list ls))))))


