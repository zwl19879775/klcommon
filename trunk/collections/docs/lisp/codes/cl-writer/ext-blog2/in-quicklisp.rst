ext-blog被加入quicklisp
==========================

原来我以为要加入quicklisp软件库，需要支持quicklisp支持的所有Common Lisp编译器，甚至，秉着负责任的态度，还得保证在三大OS平台上well tested。我想这对我而言是多么不可能完成的事情，最起码的，我没有Apple OS，当然Windows也是非正版的。

但后来试着在其他Common Lisp编译器上测试ext-blog时发现，quicklisp收录的好些库都无法编译通过。或者某些编译器本身提供的特性就不够，例如clisp的多线程实现，就让hunchentoot多少有点问题。

于是我懂了。这些跨平台的言辞，都是个幌子啊。我只需要说，ext-blog没有使用任何编译器相关的API，所以， **理论上** 它是跨平台的----这当然是事实。

然后在quicklisp的github页面上提交新的 issue_ ，然后就是和quicklisp作者交流一些问题。期间，最大的问题就是我拆分了ext-blog依赖的三个库：image, kl-verify, restas.file-publisher。image不是我写的，其他两个是我写的。由于一些原因，我需要把这3个库随ext-blog一起打包，并且由于另一个原因导致我使用了一个trick来自动载入依赖。这种trick为人诟病，结果就是，这三个库也被加入quicklisp软件库。

ext-blog最后在2012年1月份的dist里被加入：http://blog.quicklisp.org/2012/01/january-dist-and-client-updates-now.html

现在，最爽的事情就是获取ext-blog变得非常简单，你再也不用手动去下载那些依赖库了::

    (ql:quickload 'ext-blog)

Enjoy it!

.. _issue: https://github.com/quicklisp/quicklisp-projects/issues/242#issuecomment-3381542

