
; load asdf and cl-ppcre library in clisp
; (load "clpcre.lisp")
(load "~/prjs/asdf.lisp")
(push "/media/work/libs/cl-ppcre-2.0.3/" asdf:*central-registry*)
(asdf:load-system :cl-ppcre)

