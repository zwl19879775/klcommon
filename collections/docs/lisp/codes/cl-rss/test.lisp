;;;;
;;;; test.lisp
;;;; test cl-rss.
;;;; To test cl-rss, first load cl-rss: (asdf:load-system :cl-rss), then load
;;;; this file: (load "test.lisp"), and (cl-rss-test:test-rss-http).
;;;; Kevin Lynx
;;;; 3.29.2011
;;;;
(defpackage :cl-rss-test
 (:use :common-lisp :cl-rss)
 (:export test-rss-http
          test-rss-file))

(in-package :cl-rss-test)

;;; TEST utils
#|
Tested sites:
 host                   url
www.cppblog.com         /kevinlynx/rss.aspx
www.cppblog.com         /rss.aspx
cd.qq.com               /news/newshot/hotnewsrss.xml
blog.sina.com.cn        /rss/wangmomo.xml 
coolshell.cn            /feed
coolshell.cn            /?feed=rss2 (support redirect now)
|#
(defun test-rss-http (&key (host "www.cppblog.com") 
                      (uri "/kevinlynx/rss.aspx"))
  (write-string-file "output.html" 
                     (output-simple-html 
                       (decode-rss-from-http uri
                                             host))))

(defun test-rss-file (file)
  (write-string-file "output.html" 
                     (output-simple-html 
                       (decode-rss-file file))))

