-- test ParamTable in Lua
local pt = ParamTable.new()
pt.age = 11
print (pt.age)
pt.name = "Hello world"
print (pt.name)
pt.istest = true
print (pt.istest)
local sub_pt = ParamTable.new()
sub_pt.sname = "sub name"
pt.sub = sub_pt
print (pt.sub.sname)

local s = pt.sub
print (s.sname)
if pt.sub == sub_pt then
    print ("equal")
else
    print ("not equal")
end
print (pt)

print ("+------------Traverse-----------------+")
for k, v in ParamTable.values (pt) do
    print (k, v)
end
print ("+-------------------------------------+")
