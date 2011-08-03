ext-blog
=============

ext-blog是一个使用Common Lisp开发的博客系统。在这个版本之前，我曾基于nuclblog扩展修改过一个博客系统，也称为ext-blog，那是一个简陋的几乎硬编码的玩具。目前这个ext-blog不同于它，新的ext-blog最大的特性就是将前台渲染和逻辑彻底分离开，这使得它支持主题（theme）。

ext-blog完全开源免费，并且我将在不确定未来时间内提供邮件支持（但不要问我如何安装SBCL）。无论如何，ext-blog最起码是你学习Common Lisp语言的一个范例。

特性(features)
-----------------

ext-blog包含以下特性：

* 基本的博客系统特性：发文章、显示文章、评论
* 支持后台管理页面
* 支持自定义主题，定制前端显示
* 支持RSS订阅
* 支持metaweblog API，可使用博客客户端发表文章
* 自带若干主题（从WordPress移植）

获取ext-blog
---------------

目前，我仅仅将ext-blog放在我的一个github项目里，因为我还没来得及编写足够多的英文文档将其正式开源。你可以如下获取完整源代码::

    git clone git://github.com/kevinlynx/klprj.git

或者你不想获取其他乱七八糟的东西，那么尝试访问下这个页面::

    https://github.com/kevinlynx/klprj/tree/master/ext-blog

安装
------------

目前ext-blog仅仅能在SBCL上运行，当然我非常欢迎你提交任何一个Common Lisp编译器的patch给我。ext-blog依赖的库可以通过载入dep.lisp来获取。这个文件使用quicklisp自动下载并安装依赖库，如下::

    in shell:

    cd ext-blog-path
    sbcl

    in sbcl:

    (load "dep.lisp")

当然，在之前，你需要安装SBCL，以及quicklisp。安装完依赖库之后，即可载入ext-blog，并运行之::

    (asdf:load-system :ext-blog)
    (ext-blog:start)

这样，ext-blog则已经在8080端口上开启了web服务。尝试在浏览器里访问一下::

    http://localhost:8080

第一次运行ext-blog时，其主页将要求你进行一些初始设置。最基本的设置仅包括管理你博客的用户名和密码。在初始化页面完成这些设置后，浏览器将转入ext-blog的管理后台，这里你可以进行更多的设置，例如主题、博客标题等。然后一切OK。

使用主题(theme)
--------------------

主题是一个依赖于ext-blog的软件模块，它们使用asdf组织。ext-blog中的主题分为后台管理主题和前台主题。前者用于渲染后台管理页面，后者用于博客的前端显示。

要使用一个主题，只需将整个主题放置于./theme文件夹下即可。ext-blog在启动时会自动载入该文件夹下的所有主题。对于前台主题，可以直接在后台管理页面进行选择。

主题的编写（或者从WordPress主题移植）将在以后描述。

主题的编写
--------------

coming soon

