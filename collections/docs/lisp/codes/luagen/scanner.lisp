#|
    scanner.lisp
    Kevin Lynx
    3.5.2011

class-def:
    class Symbol {
        function-def
    }

function-def:
    type Symbol ( type a1, type a2, type a3 ) {
    }

symbol:
    [a-zA-Z_][a-zA-Z0-9_]*
|#
    
(defun maketok-list (toks)
    (loop for tk in toks
        for i from 1 to (list-length toks)
        collecting `(defconstant ,tk ,i)))

; i donot know how to expand a string like: (defconstant A 1) (defconstant B 2),
; it cannot splice the outside '(' whatever you done. So, this macro does not
; work now.
(defmacro deftoken (&body toks)
    `(,@(maketok-list toks)))

(defconstant tk-class 255) ;"class"
(defconstant tk-symbol 256) ;symbol
(defconstant tk-end  0)
(defconstant tk-lb (char-code #\()) ;'('
(defconstant tk-rb (char-code #\))) ;')'
(defconstant tk-lob (char-code #\{)) ;'{'
(defconstant tk-rob (char-code #\})) ;'}'
(defconstant tk-comma (char-code #\,)) ;','
(defconstant tk-sem (char-code #\;)) ;';'
(defconstant char-end (code-char 0))

;; check whether a character is [a-zA-Z_].
(defun symbols-p (c) 
    (or (alpha-char-p c)
        (char= #\_ c)))

;; check whether a character is [a-zA-Z0-9_].
(defun symbol-p (c)
    (or (symbols-p c)
        (digit-char-p c)))

;; lexical state
;; (setf p (make-lex-state :source src :pos 0))
(defstruct lex-state
    source
    pos)

(defstruct token
    tok
    str)

(defun token= (tok tok-t)
    (= (token-tok tok) tok-t))

(defun is-end (ls &optional p)
    (let ((pos (or p (lex-state-pos ls))))
        (>= pos (length (lex-state-source ls)))))

(defun now-char (ls &optional p)
    (let ((pos (or p (lex-state-pos ls))))
        (if (is-end ls pos)
            char-end
            (char (lex-state-source ls) pos))))

(defun get-char (ls)
    (let ((c (now-char ls)))
        (if (not (char= c char-end))
            (setf (lex-state-pos ls) (+ (lex-state-pos ls) 1)))
        c))

(defun unget-char (ls)
    (setf (lex-state-pos ls) (- (lex-state-pos ls) 1)))

;; (let ((s "a") (c #\b)) (setf s (append-string s c)) (format t "~a" s))
(defun append-string (s c)
    (concatenate 'string s (string c)))

(defun concat-string (s1 s2)
    (concatenate 'string s1 s2))

(defun make-symbol-token (s)
    (cond ((string= "class" s) (make-token :tok tk-class :str s))
        (t (make-token :tok tk-symbol :str s))))   

(defun symbol-token (ls nowc)
    (let ((s ""))
        (do ((c nowc (get-char ls)))
            ((not (symbol-p c)) (progn 
                (unget-char ls)
                (make-symbol-token s)))
            (setf s (append-string s c)))))

(defun space-p (c)
    (or (char= #\Space c)
        (char= #\Tab c)))

(defun skip-space (ls)
    (do ((c #\Space (get-char ls)))
        ((not (space-p c))
            (progn (if (not (char= c char-end))
                        (unget-char ls))
                    'done))))

(defun get-token (ls)
    (let ((c (get-char ls)))
        (cond 
            ((char= c char-end) (make-token :tok tk-end))
            ((space-p c) (progn
                (skip-space ls)
                (get-token ls)))
            ((char= c #\() (make-token :tok tk-lb))
            ((char= c #\)) (make-token :tok tk-rb))
            ((char= c #\{) (make-token :tok tk-lob))
            ((char= c #\}) (make-token :tok tk-rob))
            ((char= c #\,) (make-token :tok tk-comma))
            ((char= c #\;) (make-token :tok tk-sem))
            ((symbols-p c) (symbol-token ls c))
            (t (get-token ls))))) ; eat unknown character

