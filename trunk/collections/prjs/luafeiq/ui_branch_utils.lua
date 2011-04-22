--[[
  Some utitlity for tree control in IUP.
  Kevin Lynx
  1.28.2011
--]]

function branch_add(tree, name)
    tree.value = "ROOT"
    tree.addbranch = name
end

function branch_addleaf_at(tree, id, leafname, arg)
    local p = "addleaf" .. id
    tree[p] = leafname
    if arg ~= nil then
        iup.TreeSetTableId(tree, id + 1, arg)
    end
end

function branch_find(tree, name, kind)
    local i = 1
    local ret = false
    while true do 
        local namei = "name" .. i
        local kindi = "kind" .. i
        if tree[namei] == nil then
            break
        elseif tree[namei] == name and tree[kindi] == kind then
            ret = true
            break
        end
        i = i + 1
    end
    if not ret then i = 0 end
    return i 
end

function branch_dumpall(tree)
    local i = 1
    logd("--------------------------------------------------------------------------------")
    while true do
        local namei = "name" .. i
        local kindi = "kind" .. i
        if tree[namei] == nil then
            break
        end
        logd(string.format("%s(%s)", tree[namei], tree[kindi]))
        i = i + 1
    end
    logd("--------------------------------------------------------------------------------")
end

function branch_addleaf(tree, bname, leafname, arg)
    local i = branch_find(tree, bname, "BRANCH")
    branch_addleaf_at(tree, i, leafname, arg)
end

function branch_delleaf(tree, leafname)
    local i = branch_find(tree, leafname, "LEAF")
    if i > 0 then
        local marked = "marked" .. i
        local del = "delnode" .. i
        tree[marked] = "YES"
        tree[del] = "MARKED"
    end
end
