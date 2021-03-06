History
----------

使用Lisp来开发博客系统，会发生很多趣事，在此偶尔记录。

**4.30.2011**

* 增加留言功能，使用了评论功能的部分代码
* 修改rststyle.css，改代码块背景色为灰色
* 将主导航改为横向，由增加banner :after实现，主要还是通过css来控制
* 将搜索框改到主导航右侧，进一步给左侧导航留出空间
* 限制最新文章仅显示5篇
* 添加公告导航框

**4.24.2011**

修正使用google搜索站内无内容的问题。 传给google的地址里，domains和sitesearch都包
含了域名，domains可以为任意标示，但是sitesearch则必须为codemacro.com。

到目前为止，主要功能基本都有了。服务器和SBCL都算稳定。

**4.23.2011**

现在没心思花在代码结构方面，一方面我不想以其他语言（C/C++）的模块组织方式去规划
Lisp，Lisp有自己的方式；另一方面基于不想修改nuclblog源代码的基础上增加功能，本身
就是个吃力不讨好的事情。索性，先完成功能。也许将来头一热，就自己重头写个。

* 完成评论审核功能，加入后台评论管理，包括“删除评论“、”审核评论“
* 加入“最新评论“栏
* 修改了导航栏宽度，悲剧地修改了nuclblog的css文件
* 修改about页面直接使用博客文章
* 加入“统计“导航，方便查看是否有待验证留言

**4.22.2011**

原始的nuclblog功能实在过于简陋，包括评论功能我都得自己写。关键是我对WEB前端完全无
经验更无多少积累。评论功能最麻烦的就是对垃圾信息的过滤，思来想去，还是直接人工审
核算了。

nuclblog在一开启的时候，就载入了所有的文章。这在内存如此有限的VPS上，实在是种
残酷。包括评论也一样。要节约内存，只有等需要显示的时候，才载入。一想头皮就发麻，
想想自己未来好长一段业余时间，都要扑在这个非主流语言的博客系统上，就为时间而痛心。

Anyway，今天依然把这个简朴的评论功能发上来。为了让人测试，直接不过滤了。

**4.20.2011**

目前ext-blog还存在很多BUG，或者说很多不完美的地方。所以目前这个博客网站也算是处
于测试阶段。现在上班期间干不了这事，所以只有晚上的几个小时能做。今天修正的问题包
括:

* 修正XMLRPC发布文章时分类不正确问题
* 修正XMLRPC发布文章无法显示作者问题(session-realm-user其实返回的是验证过的用户名)
* 修正XMLRPC调用metaweblog api时没有进行用户验证的问题(check-password)
* 修正当文章数为单页显示数整数倍时，页数多计算一页的问题
* 修正RSS中关于channel的link错误问题，因为目前hunchentoot使用了nginx作为代理服务
  器，而nuclblog内部又是使用http request中的Host域来生成RSS channel link，所以就
  会得到http://127.0.0.1:xxx之类的url。解决办法就是在nginx的配置里重新设置http请
  求头中的Host::

    # nginx configure
    localtion / {
        proxy_set_header Host $host;
    }

* 增加virtual host，以正确响应不同域名请求，这个主要是nuclblog使用了virtual host
  导致

**4.19.2011**

使用SSH登录VPS，启动SBCL，然后在其交互模式下开启hucnhentoot。然后断开SSH后，SBCL
也会退出。解决方法可以简单地使用screen程序来完成，其具体原理暂时没时间去探究::

    screen -D -m -S nucblog sbcl --eval "(load \"ext-blog/wrap-run.lisp\") --dynamic-space 90

**4.18.2011**

因为SBCL运行的OS貌似其locale不是utf8，所以SBCL在load包含中文的代码文件时，直接
报错，大概就是不可识别某个字符：external-format :asci。使用如下方式载入代码文件
是最简单直接的方法::

    (load "ext-blog/run.lisp" :external-format :utf-8)

**4.17.2011**

VPS的内存非常有限，仅128M。网上关于SBCL运行于VPS上会出现的问题也非常多。似乎加上
一些显示的内存大小参数，可以解决问题，这个当然得有待实验::

    #sbcl --dynamic-space 90

90M是我在本机上实验的最小数量，也许直接使用编译好的lisp文件可以减少此数值。

