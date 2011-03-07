
(load "scanner.lisp")
(load "parser.lisp")
(load "gen.lisp")

(defvar *test-string* "class Lisp { void func(int a, char b) { printf(\"func\"); } }")
(defvar *test-string2* "void func ( int a, char bc, int c = 1 ) { printf(\"func\"); }")
(defvar *func-decl* "void func (int a) ; int func2 (int b);")
(defvar *func-decl2* " { void func (int a) ; int func2 (int b); }")
(defvar *class-def* " class Lisp { void func (int a) ; int func2 (int b); }")
(defvar *file-def* " class Lisp { void func (int a, int b); void func2 (int a);
    void func3(); }; class Haskell { void name(int c); }; ")

(defun test-get-token (&optional (s *test-string*))
    (let ((ls (make-lex-state :source s :pos 0)))
        (do ((tk (get-token ls) (get-token ls)))
            ((= (token-tok tk) tk-end) 'done)
            (format t "~a~%" tk))))

(defun test-create-ls (&optional (s *test-string*))
    (make-lex-state :source s :pos 0))

