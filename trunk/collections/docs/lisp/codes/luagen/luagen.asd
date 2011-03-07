;; like a makefile in ASDF program in LISP.
;; Kevin Lynx
;; 3.7.2011
;;
;; (asdf:load-system :luagen)
;;

(defsystem :luagen
    :version "0.1.0"
    :components ((:file "scanner")
                 (:file "parser" :depends-on ("scanner"))
                 (:file "gen" :depends-on ("scanner") :depends-on ("parser"))
                 (:file "main" :depends-on ("scanner") :depends-on ("parser")
                    :depends-on ("gen"))))



