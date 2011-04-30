传递Lua函数到C/C++中
---------------------

问题
~~~~~~~

在Lua中，因为函数也是第一类值，所以会出现将函数作为另一个函数的参数，或者函数作
为函数的返回值。这种机制在很多地方都能代码更灵活更简洁，例如::

   table.sort(table [,comp])

这里的comp就要求传入一个函数，我们在调用时，大概会有如下形式::

   table.sort(t, comp) -- 直接写函数名
   table.sort(t, local_comp) -- 某个局部函数
   table.sort(t, function (a, b) xxx end ) -- 临时构造一个匿名函数

其中最后一种方式最为灵活，任意时候在需要的时候构造一个匿名函数。这种在Lua自身的
环境中使用，自然没有问题。但是，当我们在C/C++中注册一些函数到Lua环境中，而这些
函数也需要使用函数参数的时候，问题就出来了。

Lua本身是不支持将Lua函数作为函数参数传入C/C++的，不管这个想要传入的函数是全局的
、局部的、或者匿名的（匿名的本质上也算局部的）。一般情况下，我们唯一的交互方式，
不是传入一个函数，而是一个全局函数名。C/C++保存这个函数名，在需要回调Lua的时候，
就在Lua全局表中找到这个函数（根据函数名），然后再调用之。情况大致如下::

    function lua_func () xxx end
    cfunc(lua_func) -- wrong
    cfunc("lua_func") -- right

我们这回的脚本模块，策划会大量使用需要回调函数的C/C++函数。显然，创建大量的全局
函数，先是从写代码的角度看，就是很伤神的。

解决
~~~~~~

我们最终需要的方式，大概如下::

    cfunc(lua_func) -- ok
    cfunc(function () xxx end) -- ok
    local xxx = function () xxx end
    cfunc(xxx) -- ok

要解决这个问题，我的思路是直接在Lua层做一些包装。因为C/C++那边仅支持传入一个全局
函数名（当然不一定得全局的，根据实际情况，可能在其他自己构造的表里也行），也就是
一个字符串，所以我的思路就是将Lua函数和一个唯一的字符串做映射。::

    function wrap (fn)
        local id = generate_id()
        local fn_s = "__callback_fn"..id
        _G[fn_s] = fn
        return fn_s
    end

这个wrap函数，就是将一个函数在全局表里映射到一个字符串上，那么在使用时::

    cfunc(wrap(function () xxx end))
    cfunc(const char *fn_name, xxx); -- cfunc的原型

cfunc是C/C++方注册进Lua的函数，它的原型很中规中矩，即：只接收一个函数名，一个字
符串，如之前所说，C/C++要调用这个回调函数时，就根据这个字符串去查找对应的函数。
脚本方在调用时，如果想传入一个匿名函数了，就调用wrap函数包装一下即可。

一个改进
~~~~~~~~~

上面的方法有个很严重的问题，在多次调用wrap函数后，将导致全局表也随之膨胀。我们需
要想办法在C/C++完成回调后，来清除wrap建立的数据。这个工作当然可以放到C/C++来进行
，例如每次发生回调后，就设置下全局表。但这明显是不对的，因为违背了接口的设计原则
，这个额外的机制是在Lua里添加的，那么责任也最好由Lua来负。要解决这个问题，就可以
使用Lua的metamethods机制。这个机制可以在Lua内部发生特定事件时，让应用层得到通知。
这里，我们需要关注__call事件。

Lua中只要有__call metamethod的值，均可被当作函数调用。例如::

    ab(1, 2) 

这里这个函数调用形式，Lua就会去找ab是否有__call metamethod，如果有，则调用它。这
个事实暗示我们，一个table也可以被调用。一个改进的wrap函数如下::

    local function create_callback_table (fn, name)
        local t = {}
        t.callback = fn
        setmetatable (t, {__call =  -- 关注__call
            function (func, ...) -- 在t(xx)时，将调用到这个函数
                func.callback (...) -- 真正的回调
                del_callback (name) -- 回调完毕，清除wrap建立的数据
            end })
        return t
    end
    
    function wrap (fn)
        local id = generate_func_id() -- 产生唯一的id
        local fn_s = "_callback_fn"..id
        _G[fn_s] = create_callback_table(fn, fn_s) -- _G[fn_s]对应的是一个表
        return fn_s
    end

在我们的C/C++程序中，依然如往常一样，先是从_G里取出函数名对应的对象。虽然这个对
象现在已经是一个table。然后lua_call。

上面的代码是否会在原有基础上增加不可接受的性能代价？虽然我没有做过实际测试，但是
从表明看来，排除meta table在Lua里的代价，也就多了几次Lua函数调用。

最后，感叹一下，Lua里的table及metatable机制，实在非常强大。这种强大不是功能堆砌
出来的强大，而是简单东西组合出来的强大。其背后的设计思想，着实让人佩服。

**4.26.2011 Update**

之前的文中说“Lua本身是不支持将Lua函数作为函数参数传入C/C++的“，这句话严格来说不
正确（由某网友评论）。假设函数cfun由c/c++注册，我们是可以编写如下代码的::

    cfunc(print) -- 传入Lua函数

但是问题在于，我们无法取出这个函数并保存在c/c++方。Lua提供了一些接口用于取cfunc
的参数，例如luaL_checknumber（封装lua_tonumber）。但没有类似luaL_checkfunction的
接口。Lua中的table有同样的问题。究其原因，主要是Lua中的函数没有直接的c/c++数据结
构对应。


;; END

