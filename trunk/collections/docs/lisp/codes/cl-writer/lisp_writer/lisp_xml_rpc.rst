.. -*- coding: utf-8 -*-

用lisp开发博客客户端
====================

:Author: Kevin Lynx
:Date: 3.13.2011

最近一直在学习Lisp这门语言。回头一看，基本上接近1个月了。刚开始接触Lisp是因为看
了<Lisp本质>，然后我发现有很多人宗教般地忠诚这门语言，于是就来了兴趣。

    .. image:: imgs/lisp_believer.png

当然并不是每次因为某篇写得很geek技术文章就去学习某个新的技术点。一个月时间对我来
说还是很珍贵了。但是Lisp绝对是大部分程序员都值得一学的语言（就像Haskell一样）。
我能给出的简单理由包括：

* 大部分程序员只会命令式语言（C/C++/C Like etc)，缺乏函数式语言解决编程问题的思
  想（当然Lisp不是纯函数式)
* Lisp是仅次于Fortran的古老语言，很多优秀的语言设计思想在现代的一些语言里都找得
  到
* 装B党必备

另一方面，结合我一个月以来的读书和两个练习工程的实践经历，我觉得也有些理由值得你
不去学习Lisp：

* 你会Haskell或者其他函数式语言
* 我目前还是觉得Lisp学习曲线高(大概是因为我读到的书都在应用语法层兜圈子，事实上
  Lisp的语法之统一，全特么的是s-expression)，你不愿意花费这些成本
* you are too old bo to be a B

关于这篇文档
------------

这篇博客我使用reStructuredText格式编写，然后用docutls导出为html，再然后使用这回
用lisp开发的基于metaweblog API的博客客户端，自动发布到CPPBLOG。

他们怎么说Lisp
--------------

我就摘录些书上的观点(历史)：

* 1958年，John McCarthy和他的学生搞出了Lisp，包括其第一个实现，最初貌似也是以一
  篇论文起头
* Lisp可以让你做其他语言里无法做的事情(<ANSI common Lisp>)
* 大部分编程语言只会告诉你不能怎样做，这限制了你解决问题的思路，Lisp not (<ANSI
  Common Lisp>)
* Lisp让你以Lisp的思维思考问题，换到其他语言你会说：为什么X语言就不支持这个特性
  呢(Once you've leanred Lisp, you'll even dream in Lisp) (<Land Of Lisp>)
* Lisp代码更清晰地体现你的想法(<Practical Common Lisp>)

And my opinion
--------------

我可还没到把Lisp捧上天的地步。如果Lisp如此之好，为什么用的人不多？<Land Of Lisp>
里作者恰好对这个问题做了回答(bla bla bla，懒得细读)。

* Lisp也是一门杂和型风格的语言，函数式、命令式、面向对象，以及最被人吹捧的宏编程
  --程序自己写自己
* Lisp的语句全部以(xxx xxx)的形式出现，被称为s-expression，我看称为括号表达式还
  差不多
* Lisp每条语句都有返回值，没基础过函数式编程的同学，if语句也是有返回值的
* 函数式编程语言的一个重要特性就是闭包(closure)，这个东西用来避免全局变量实在太
  geek了

开始学习Lisp
------------

Lisp不像有些语言，有个直接的机构来维护。感觉它更像C/C++一样，只有个标准，然后有
若干编译器（解释器）实现。Lisp在几十年的发展中，产生了很多种方言。方言也就是形变
神不变的语言变种，本文说的Lisp均指Lisp的方言Common Lisp。另一个比较有名的方言是
Scheme，关于各个方言的特点，<Land Of Lisp>里也给了一个图片：

    .. image:: imgs/dialect.png

其中，最左边那只wolf就是Common Lisp，右边那只sheep就是Scheme。

要学习Lisp，首先就是选择方言。然后最重要的就是选择一个编译器实现。世界上知名的有
十几种实现（也许更多）。一些商业版本非常强大，甚至能编译出很小的本地代码执行文件
，不过价格也不菲。当然也有很多开源免费的实现，例如CLISP、SBCL。我选用的是SBCL。

SBCL交互式命令行不支持括号匹配，甚至没有输入历史。要实现这两个功能，可以装一个
lisp工具：linedit。在lisp的世界中，要获得一个lisp的库实在不是件方便的事。尤其是
这些免费的编译器实现，并不像有些语言一样，直接随编译器带个几十M的库。

然后就有了quicklisp这个工具。该工具就像Ubuntu系统里的软件管理器一样，你可以在
lisp里直接获取某个库。quicklisp检查该库是否存在，不存在直接从它的服务器上下载人
然后自动安装。

此外，在lisp的世界里，写出来的程序不再是跨OS。OS的差异由编译器实现来解决。但是，
写lisp程序却需要考虑跨编译器实现（egg hurt）。这也是个无比伤神的事，比跨OS更伤
神。因为OS就那么几个，但lisp的编译器实现，流行的也有好几个。

lisp的世界里，工程组织也有特殊的一套，就像makefile一样，这就是asdf。

博客客户端如何实现
------------------

像我们这种基本没接触过Web开发的人，可能完全没有思路去实现一个博客客户端。事实上
实现起来非常简单。

使用过其他博客客户端（例如Windows Live writer）的人肯定知道metaweblog API，在配
置客户端的时候需要填入。例如CPPBLOG的这个地址就是
http://www.cppblog.com/kevinlynx/services/metaweblog.aspx。这个页面展示了一些API
说明。这些API就是博客客户端和服务器进行操作通信的接口。意思是说，服务器端提供这
这些接口，我们的客户端调用这些接口即可。例如::

    blogger.deletePost，调用该接口即可删除一篇博客文章

但是客户端如何调用到这个接口呢？这需要通过一种新的技术（或者说标准），即 **xml rpc**
。rpc大家应该清楚，xml rpc其实说白了， **就是把接口调用的细则塞进** **http
请求发给web服务器，服务器接收请求完成操作后再把结果以http回应的形式丢给客户端，
即完成了一次接口调用** 。

至于http请求回应的细则就不提了，无非就是一些特殊格式的数据，通过tcp连接与服务器
交互这些数据。

所以，基本上，整个过程还是非常简单。如何来将调用细节塞进http请求，则是以xml rpc
标准来做，其格式正好是xml格式。举个例子吧::

    <?xml version='1.0'?>
    <methodCall>
        <methodName>title_or_id</methodName>
            <params>
            </params>
    </methodCall

当然这部分数据之前就是若干http请求的数据。服务器回应也是以xml格式组织::

    <?xml version='1.0'?>
    <methodResponse>
        <params>
            <param>
                <value><string>Welcome to Zope.org</string></value>
            </param>
        </params>
    </methodResponse>

我们的博客客户端所要做的，就是把这些博客发布相关的操作封装起来提供给使用者。底层
实现主要包括http请求、xml-rpc的组织等。何况，这两部分在各个语言里都有大量的库存
在，lisp自然也有。

我这里直接选取了lisp的一个xml-rpc库：s-xml-rpc，基本上百来行代码就可以把各个功
能跑一遍。例如以下lisp代码就实现了通过s-xml-rpc删除CPPBLOG的一篇文章::

    (defun delete-post (postid)
      (rpc-call 
        "blogger.deletePost"
        postid
        "kevinlynx"
        "password"
        t))

发布博客也很简单，根据metaweblog API接口的说明，发布博客时需要填充一个结构体。但
主要涉及到的数据仅包括：文章内容、文章标题、文章分类（可选）::

    (defun new-post (title context &optional (cates))
      (rpc-call 
        "metaWeblog.newPost"
        ""
        "kevinlynx"
        "password"
        (new-post-struct title context cates)
        t))

值得注意的是，如果文章中有贴图，则需要事先将图片文件上传到服务器。CPPBLOG的
metaweblog API里恰有API提供::

    (defun new-media-object (filename)
      (rpc-call 
        "metaWeblog.newMediaObject"
        ""
        "kevinlynx"
        "password"
        (new-media-object-struct filename)))

该函数读入图片文件，然后调用metaWeblog.newMediaObject接口，即可完成上传。上传成
功后，服务器会返回该图片的URL。然后在我们的文章中就可以使用该图片了。

完整实现方案
------------

仅仅将metaweblog的一些接口做封装，对于一个可以使用的博客客户端来说还远远不够。大
部分同类工具都有一个友好的GUI编辑界面。我并不打算弄一个编辑界面出来，吃力不讨好
的事情。

我的打算是先用其他工具对文章做排版处理，最后导出为html格式。因为CPPBLOG支持直接
发布一个html文件。然后在用这个lisp工具将整个文件作为博客文章内容发布。

恰好公司最近打算用reStructureText(rst)格式来编辑文档，作为熟悉手段，我决定拿这个
来练手。rst格式非常简单，同wiki命令很相似。在vim里编辑该文件非常合适，因为默认支
持。见图:

    .. image:: imgs/rst.png

由图即可看出，rst是一种半所见即所得的格式。即：它遵循你在编辑器里的排版，同时也
通过一些tag（例如image）来控制更丰富的输出。

rst有很多前端工具，可以将rst文件输出，例如rst2html.py就可以输出为html。好吧，最
最终我们得到了html格式的博客文章。

但是如果文章中出现了图片，而图片基本上在本地，转成html后也是相对路径。我需要我的
lisp writer(cl-writer)能自动扫描文章，发现有图片的地方，就自动将图片上传。最恶心
的是上传后还得替换图片引用路径。这个工作可以在rst格式上做，也可以在结果格式html
上做。通过xml解析库解析html比直接解析rst格式更简单，并且在扩展性上更好。

最终这个html中图片路径替换工作只消耗了不到100行lisp代码。这在很大程度上也依赖于
s-xml库的接口设计。

最终封装好的发布接口如下，从这里也可以看出，函数式语言锻炼我们写出功能单一代码度
短小的接口::

    (defun writer-post-new (post-file &key (u (get-default-user))(cates))
      (read-post-file u post-file context title
                      (new-post u title context cates)))

END
-----

别指望我发布的代码能够让你一键在你的博客上留下"this is a test"，你甚至别指望它能
能够工作。但如果你本来就是一个资深的lisper，或者虽然不是lisper但却执意想看看结果
。这里我就简要说说如何让这些代码欢乐起来:

1. OS Ubuntu10.04，下载安装SBCL，不会有问题；
2. 下载安装quicklisp，官方文档hand by hand，简单不会有问题；
3. SBCL交互环境中使用quicklisp安装s-xml-rpc::

        (ql:quickload "s-xml-rpc")

4. 装载我的代码::

        (asdf:load-system :cl-writer)

5. 在home下添加配置文件.cl-writer.lisp，配置你博客信息，例如::

        (in-package cl-writer)
        (setf *default-user* (make-cppblog-user "账户名" "密码"))

   如果你的博客不在CPPBLOG，虽然也许也是metaweblog，但我不能保证成功，配置文件则
   要复杂点::

        (setf *default-user* (make-user-info :name "帐户名"
                              :password "密码" :host "www.cppblog.com"
                              :url "/kevinlynx/services/metaweblog.aspx"))

6. SBCL交互环境下测试::

        (in-package cl-writer)
        (new-post (get-default-user) "this is a test" "title")


下载代码_

.. _下载代码: http://www.cppblog.com/Files/kevinlynx/cl-writer.tar.gz

最后，终于敲完这篇文章，我需要通过以下步骤来发表它::

    in shell:

    rst2html.py lisp_xml_rpc.rst lisp_xml_rpc.html

    in SBCL:

    (writer-post-new "lisp_xml_rpc.html")

;;EOF;;
~~~~~~~~

