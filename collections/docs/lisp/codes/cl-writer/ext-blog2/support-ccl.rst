ext-blog支持Clozure CL
============================

ext-blog本身并没有使用编译器提供的API，另一方面它依赖的库大部分也是跨平台的，所以理论上ext-blog也是跨平台的。但因为世界上有十几种相对主流的Common Lisp编译器实现，如果要挨个让ext-blog在这些实现上跑一遍，实在麻烦。当然，更别说在Linux/MacOS/Windows三个操作系统平台测试了。

所以，我一直没去做这事。这也直接导致了ext-blog不能被加入到quicklisp库列表。如果你突然发现ext-blog能在你的Common Lisp编译器里运行，你可以发邮件告诉我。

很早前就有同学希望ext-blog支持Clozure CL。这回加上另一个同学的原因，我总算完成了这件事。实际上做起来很简单，测试了下发现只有依赖的image库使用了SBCL特定的API，而且仅一个函数。image库用于ext-blog生成验证码，这个库代码文件里甚至都看不到作者信息，更别说版权信息了。所以，我直接修改了其源码，将依赖的SBCL函数换成flexi-stream里对应的接口（神奇般的函数名和参数完全一样）。然后，其他都OK了。

测试的Clozure CL下载自：

ftp://ftp.clozure.com/pub/release/1.7/ccl-1.7-linuxx86.tar.gz

最新的ext-blog代码可从如下地址获取:

git://github.com/kevinlynx/ext-blog.git

另外，可以让Clozure CL使用之前SBCL里的quicklisp。quicklisp默认安装在$HOME下。在$HOME下新建ccl-init.lisp，添加::

    ;;; The following lines added by ql:add-to-init-file:
    #-quicklisp
    (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                           (user-homedir-pathname))))
      (when (probe-file quicklisp-init)
        (load quicklisp-init)))

然后启动ccl即可。


