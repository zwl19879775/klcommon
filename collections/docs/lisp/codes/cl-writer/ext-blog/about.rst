关于
-----------

关于本站
~~~~~~~~~~~~~~~

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

本站大部分功能，可以说已经由我自己写的ext-blog支撑。ext-blog目前并不开源，
因为它在我对博客系统毫无经验的情况下，在不断地堆砌功能中已经糟糕得不行。但这并不
意味着我会忽略掉你索要源码的邮件:D。关于ext-blog的开发过程，参考如下：

http://codemacro.com/blog/display?id=5

关于作者
~~~~~~~~~~~~~

`Kevin Lynx`_

在职MMO游戏服务器端程序员，半调子计算机科学爱好者。广泛涉猎各种计算机技术，对编
程语言、模块设计尤为爱好。高中开始自学编程，由电子词典上的GVBasic开始，从此开始
不务正业。

大学开始自写各类 小游戏_ ，代码越写越多，对计算机的理解却不见长。大三开始进入游
戏业，摸爬滚打至今。青春不再，还望自己努力。

**和我联系**

原则上不使用IM交流，索要我以前那些小游戏源代码的 请点这里_ (真不敢相信这么多年了
依然能间断收到索要这些源码的邮件-__-!)。同时更愿意与各位进行各类技术交流，无论是
网上还是网外。

Email: kevinlynx at gmail dot com, kevinlynx at vip dot qq dot com

ENDS.(本页面不定期更新)

.. _SBCL: http://www.sbcl.org
.. _hunchentoot: http://weitz.de/hunchentoot/
.. _nuclblog: https://cyrusharmon.org/projects?project=nuclblog
.. _小游戏: http://www.cppblog.com/kevinlynx/archive/2008/05/14/49783.html
.. _cl-writer: http://www.cppblog.com/kevinlynx/archive/2011/03/13/141713.aspx
.. _Kevin Lynx: http://codemacro.com
.. _请点这里: http://www.cppblog.com/kevinlynx/archive/2008/05/14/49783.html

