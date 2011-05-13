-- test ref-callback implementation
-- Kevin Lynx
-- 5.9.2011
--
local fn = function (s1, s2) print (s1, s2) end

--register_fn (function (s1, s2) print (s1, s2) end, "Hello", "World")
register_fn (fn, "Hello", "World")
