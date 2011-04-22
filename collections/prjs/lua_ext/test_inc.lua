-- test_inc.lua
-- require lua-ext.cpp
-- Kevin Lynx
-- 4.22.2011
require ("sys")

-- register a callback function into c/c++, and later when something happen,
-- c/c++ will call the callback function registered here.
register_fn (sys._callback(function (s1, s2) print(s1, s2) end),
            "hello", "world")

