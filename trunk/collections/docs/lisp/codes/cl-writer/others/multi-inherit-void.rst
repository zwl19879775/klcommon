多重继承和void*的糗事
========================

:Author: Kevin Lynx
:Date: 4.30.2011

C++为了兼容C，导致了不少语言阴暗面。Bjarne Stroustrup在<D&E>一书里也常为此表现出无奈。另一方面，强制转换也是C++的一大诟病。但是，因为我们的应用环境总是那么“不
纯”，所以也就常常导致各种问题。

本文即描述了一个关于强制转换带来的问题。这个问题几年前我曾遇到过(<`多线程下vc2003,vc2005对虚函数表处理的BUG？`_>)，当时没来得及深究。深究C++的某些语法，实在是件辛苦事。所以，这里也不提过于诡异的用法。

问题
------------

考虑下面非常普通的多重继承代码::

    class Left {
    public:
        virtual void ldisplay () {
            printf ("Left::ldisplay\n");
        }
    };

    class Right {
    public:
        virtual void rdisplay () {
            printf ("Right::rdisplay\n");
        }
    };

    class Bottom : public Left, public Right {
    public:
        virtual void ldisplay () {
            printf ("Bottom::ldisplay\n");
        }
    };

这样子的代码在我们的项目中很容易就会出现，例如::

    class BaseObject;
    class EventListener;
    class Player : public BaseObject, public EventListener

别紧张，我当然不会告诉你这样的代码是有安全隐患的。但它们确实在某些时候会出现隐患。在我们的C++项目中，也极有可能会与一些纯C模块打交道。在C语言里，极有肯能出现以
下的代码::

    typedef void (*allocator) (void *u); 
    void set_allocator (allocator alloc, void *u);

之所以使用回调函数，是出于对模块的通用性的考虑。而在调用回调函数时，也通常会预留一个user data的指针，用于让应用层自由地传递数据。

以上关于多重继承和void*的使用中，都属于很常规的用法。但是当它们遇到一起时，事情就悲剧了。考虑下面的代码::

    Bottom *bobj = new Bottom(); // we HAVE a bottom object
    Right *robj = bobj; // robj point to bobj?
    robj->rdisplay(); // display what ?
    void *vobj = bobj; // we have a VOID* pointer
    robj = (Right*) vobj; // convert it back
    robj->rdisplay(); // display what?

这里的输出结果是什么呢？::

    Right::rdisplay 
    Bottom::ldisplay // !!!!

由void*转回来的robj调用rdisplay时，却调用了莫名其妙的Bottom::ldisplay！

多重继承类的内存布局
-----------------------

类对象的内存布局，并不属于C++标准。这里仅以vs2005为例。上面例子中，Bottom类的内存布局大概如下::

    +-------------+
    | Left_vptr   |
    +-------------+
    | Left data   |
    +-------------+
    | Right_vptr  |
    +-------------+
    | Right data  |
    +-------------+
    | Bottom data |
    +-------------+

与单继承不同的是，多重继承的类里，可能会包含多个vptr。当一个Bottom对象被构造好时，其内部的两个vptr也被正确初始化，其指向的vtable分别为::

    Left_vptr --->  +---------------------+
                    | 0: Bottom::ldisplay |
                    +---------------------+

    Right_vptr ---> +---------------------+
                    | 0: Right::rdisplay  |
                    +---------------------+


转换的内幕
---------------

**类体系间的转换**

隐式转换相比强制转换而言，一定算是优美的代码。考虑如下代码的输出::

    Bottom *bobj = new Bottom();
    printf ("%p\n", bobj);
    Right *robj = bobj;
    printf ("%p\n", robj);

其输出结果可能为::

    003B5DA0
    003B5DA4

**结论就是，Right *robj = bobj;时，编译器返回了bobj的一个偏移地址。** 从语言角度看，就是这个转换，返回了bobj中Right*的那一部分的起始地址。但编译器并不总是在bobj上加一个偏移，例如::

    bobj = NULL;
    Right *robj = bobj;

编译器不会傻到给你一个0x00000004的地址，这简直比NULL更无理。

**void*转换**

编译器当然有理由做上面的偏移转换。那是因为在编译阶段，编译器就知道bobj和Right之间的关系。这个偏移量甚至不需要在运行期间动态计算，或是从某个地方取。如果你看过上面代码对应的汇编指令，直接就是::

    add eax, 4 ; 直接加 sizeof(Left)，记住，Right在Left之后

void*就没那么幸运了。void*和Bottom没有任何关系，所以::

    void *vobj = bobj; // vobj的地址和bobj完全相同

然后当你将vobj转换到一个Right*使用时::

    robj = (Right*) vobj;  // 没有偏移转换，robj == vobj == bobj
    robj->rdisplay();

robj指向的是Bottom的起始地址，天啊，在我们学习C++时，我们可以说Bottom就是一个Left，也是一个Right，所谓的is kind of。但这里的悲剧在于，按照上面的逻辑，我们在使用Right时，其实应该使用Bottom里Right那一部分。 **但现在这个转换，却让robj指向了Bottom里Left那一部分。**

当调用 ``robj->rdisplay`` 时，编译器当然按照Right的内存布局，生成一个虚函数的调用指令，大概就是::

    mov vptr, robj->[0] ;; vptr在robj起始地址处
    mov eax, vptr[0] ;; rdisplay在vtable中位于第一个
    mov ecx, robj
    call eax

总而言之， ``robj->rdisplay`` 就是使用偏移0处的值作为vptr，然后使用vptr指向的vtable中第一个函数作为调用。

但，robj正指向bobj的起始地址，这个地址是放置Left_vptr的地方。这个过程，使用了Left_ptr，而Left_ptr指向的vtable中，第一个函数是什么呢？::

    Left_vptr --->  +---------------------+
                    | 0: Bottom::ldisplay |
                    +---------------------+

正是Bottom::ldisplay！到这里，整个问题的原因就被梳理出来了。

;;END;;

.. _多线程下vc2003,vc2005对虚函数表处理的BUG？: http://www.cppblog.com/kevinlynx/archive/2008/04/24/48001.html

