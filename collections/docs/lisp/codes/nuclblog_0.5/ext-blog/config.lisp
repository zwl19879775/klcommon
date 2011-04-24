;;;
;;; config.lisp
;;; Config my ext-blog.
;;; Kevin Lynx
;;; 4.16.2011
;;;
(in-package :ext-blog)

(defun cfg-rel-path (rel)
  (ch-asdf:asdf-lookup-path
    (format nil "asdf:/ext-blog/ext-blog~a" rel)))

(defun cfg-rel-data-path (rel)
  (ch-asdf:asdf-lookup-path
    (format nil "asdf:/ext-blog-data/ext-blog~a" rel)))

(defun cfg-storage-path ()
  (cfg-rel-data-path "/storage"))

(defun cfg-owner-name ()
  "Kevin Lynx")

;; MUST not contain 'www', because google can NOT search that.
(defun cfg-blog-links ()
  "codemacro.com")

(defun cfg-title ()
  "loop_in_codes")

(defun cfg-short-name ()
  "导航")

(defun cfg-sub-title ()
  "Kevin Lynx's blog -> http://codemacro.com 
  (测试阶段，点击左侧导航'关于'了解更多)")

(defun cfg-mail ()
  "kevinlynx@gmail.com")

(defun cfg-categories ()
  (list "C/C++"
        "Lua"
        "Lisp"
        "编译原理"
        "模块架构"
        "通用编程"
        "网络编程"
        "游戏开发"
        "Others"))

(defun cfg-ext-links ()
  '((:url "http://www.cppblog.com/kevinlynx"
     :desc "我在CPPBLOG的博客")
    (:url "http://blog.csdn.net/kevinlynx"
     :desc "我在CSDN的博客(08年以前)")
    (:url "http://edge2d.googlecode.com/"
     :desc "edge2d(2d游戏引擎,08年)")
    (:url "http://code.google.com/p/luafeiq/"
     :desc "luafeiq(兼容飞秋的局域网IM)")
    (:url "http://www.cppblog.com/kevinlynx/archive/2008/05/14/49783.html"
     :desc "05-08年做的小游戏")))

(defun cfg-ext-html ()
  "<script language='javascript' type='text/javascript' src='http://js.users.51.la/4670235.js'></script>
   <noscript><a href='http://www.51.la/?4670235' target='_blank'><img alt='&#x6211;&#x8981;&#x5566;&#x514D;&#x8D39;&#x7EDF;&#x8BA1;'
   src='http://img.users.51.la/4670235.asp' style='border:none' /></a></noscript>")

(defun cfg-virtual-host ()
  '("localhost" "127.0.0.1"
    "www.codemacro.com"
    "codemacro.com"))


