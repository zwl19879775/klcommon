#|
    main.lisp
    a program to generate lua codes from some simple c++ definitions.
    Kevin Lynx
    3.6.2011
|#

;; read source string from a support code file.
(defun read-file-string (file)
    (with-open-file (in file)
        (do ((s "" (read-line in nil nil))
             (ret ""))
            ((null s) ret)
            (setf ret (concat-string ret s)))))

;; write translated string.
(defun write-gen-string (file str)
    (with-open-file (out file
                        :direction :output
                        :if-exists :supersede)
        (write-string str out)))

;; the proram entry
(defun main (in out)
    (let* ((ls (make-lex-state :source (read-file-string in) :pos 0))
           (classlst (parse-class-list ls))
           (gen-s (concat-string (gen-file-header)
                    (gen-class-list classlst))))
        (write-gen-string out gen-s)
        gen-s))

    
