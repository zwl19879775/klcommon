ext-blog更新
==========================

:Author: Kevin Lynx
:Date: 12.3.2011
:Contact: kevinlynx at gmail dot com

前些天有个同学给我发邮件，告知我目前的ext-blog在最新的quicklisp库中运行不了了。因为我订阅了quicklisp的开发博客，虽然不太关注，但大概还是了解到，quicklisp作者一直在更新这个软件，同时也会对其软件库做不定期的更新。ext-blog不经意间已经依赖了很多lisp库，所以其依赖库有更新的话，出问题也就很正常了。

主要的问题主要集中于hunchentoot1.2.1相比我之前使用的1.1.1，修改了access-log-path和message-log-path的设置方式。而hunchentoot项目网站上的文档也没有同步更新，大概看了下源码，我简单地修正这个问题::

    (defun get-acceptor (port)
      (find port restas::*acceptors* :key #'hunchentoot:acceptor-port))

    (defun set-log (acceptor)
      (let ((access-log-path "log/ext-blog-access-log")
            (message-log-path "log/ext-blog-message-log"))
        (ensure-directories-exist access-log-path)
        (ensure-directories-exist message-log-path)
        (setf (hunchentoot:acceptor-access-log-destination acceptor) access-log-path)
        (setf (hunchentoot:acceptor-message-log-destination acceptor) message-log-path)))

当然set-log函数也就必须放在restas:start的调用之后。

同样，更新quicklisp可以简单地使用以下方式::

    (ql:update-client)
    (ql:update-all-dists)

最好重新下载一次依赖库::

    (load "dep.lisp")

ext-blog至今没有被加入quicklisp的软件库。主要原因是我太懒，懒得折腾。因为要加入quicklisp，需要让ext-blog至少支持若干种common lisp编译器。此外，还需要修正一些小问题，例如支持可以在任意目录载入ext-blog（目前必须在ext-blog根目录载入）。

最后，以后ext-blog的更新全部在以下github地址，原来我发布的地址不再更新::

    git://github.com/kevinlynx/ext-blog.git


