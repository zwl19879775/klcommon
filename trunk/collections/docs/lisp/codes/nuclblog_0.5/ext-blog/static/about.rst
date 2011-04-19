
关于本站
---------------

本站是一个完全采用Common Lisp实现的个人博客站点。虽然使用了nginx作为Web前端服务器
但这完全是出于基本的效率考虑，在可预见的未来我将保持所有逻辑均由Lisp编写。之所以
使用Common Lisp，纯碎是出于个人兴趣。

本站的实现包括的软件有：

* SBCL_: 作为Common Lisp的实现
* hunchentoot_: 作为后端Web服务器，之所以不直接使用它作为前端服务，而使用nginx，
  主要是出于并发效率考虑
* nuclblog_: 一个1000行Lisp代码的简易博客系统
* ext-blog: 基于 nuclblog_ ，用于增强其功能，由我自己开发:D
* cl-writer_: 使用Lisp开发的博客客户端，本站由该客户端发表文章，由我开发:D

除此之外，以上Common Lisp软件还基于很多优秀的Common Lisp库，在此不详细列举。

关于作者
--------------

Kevin Lynx

在职MMO游戏服务器端程序员，半调子计算机科学爱好者。广泛涉猎各种计算机技术，对编
程语言、模块设计尤为爱好。高中开始自学编程，由电子词典上的GVBasic开始，从此开始
不务正业。

大学开始自写各类 小游戏_ ，代码越写越多，对计算机的理解却不见长。毕业后从业于网
游行业，默默无闻至今。青春不再，还望自己努力。

INTEREST
--------------

使用Common Lisp建立本站，会发生很多趣事。详细过程见此文：

http://codemacro.com/blog/display?id=3

.. _SBCL: http://www.sbcl.org
.. _hunchentoot: http://weitz.de/hunchentoot/
.. _nuclblog: https://cyrusharmon.org/projects?project=nuclblog
.. _小游戏: http://www.cppblog.com/kevinlynx/archive/2008/05/14/49783.html
.. _cl-writer: http://www.cppblog.com/kevinlynx/archive/2011/03/13/141713.aspx
