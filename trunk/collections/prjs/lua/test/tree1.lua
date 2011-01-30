-- IupTree Example in IupLua 
-- Creates a tree with some branches and leaves. 
-- Two callbacks are registered: one deletes marked nodes when the Del key 
-- is pressed, and the other, called when the right mouse button is pressed, 
-- opens a menu with options.

require( "iuplua" )
require( "iupluacontrols" )

print(iup.GetGlobal("UTF8AUTOCONVERT"))
iup.SetGlobal("UTF8AUTOCONVERT", "NO")
print(iup.GetGlobal("UTF8AUTOCONVERT"))

dofile("chat_dlg.lua")
--------------------------------------------------------------------------------

function branch_add(tree, name)
    tree.value = "ROOT"
    tree.addbranch = name
end

function branch_addleaf_at(tree, id, leafname)
    local p = "addleaf" .. id
    tree[p] = leafname
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

function branch_addleaf(tree, bname, leafname)
    local i = branch_find(tree, bname, "BRANCH")
    branch_addleaf_at(tree, i, leafname)
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
--------------------------------------------------------------------------------

tree = iup.tree{}

code_group = "Code"
test_group = "Test"

function init_tree_atributes()
    tree.font = "COURIER_NORMAL_10"
    tree.name = "All"
    tree.addexpanded = "NO"
    branch_add(tree, code_group)
    branch_addleaf(tree, code_group, "程序")
    branch_add(tree, test_group)
    branch_addleaf(tree, code_group, "coderb")
    branch_addleaf(tree, test_group, "testera")
    tree.redraw = "YES"
end

-- Creates menu displayed when the right mouse button is pressed
addleaf = iup.item {title = "Add Leaf"}
addbranch = iup.item {title = "Add Branch"}
renamenode = iup.item {title = "Rename Node"}
menu = iup.menu{addleaf, addbranch, renamenode}

-- Callback of the right mouse button click
function tree:rightclick_cb(id)
  tree.value = id
  menu:popup(iup.MOUSEPOS,iup.MOUSEPOS)
  return iup.DEFAULT
end

function tree:executeleaf_cb(id)
    print("executeleaf_cb")
    local wnd = chatdlg_create()
    chatdlg_show(wnd)
end

-- Callback called when a node will be renamed
function tree:renamenode_cb(id)
  return iup.DEFAULT
end

function tree:k_any(c)
  if c == 339 then tree.delnode = "MARKED" end
  return iup.DEFAULT
end

-- Callback called when a leaf is added
function addleaf:action()
  --branch_addleaf(tree, code_group, "coderc")
  branch_delleaf(tree, "codera")
  tree.redraw = "YES"
  return iup.DEFAULT
end

-- Callback called when a branch is added
function addbranch:action()
  branch_add(tree, "Art")
  tree.redraw = "YES"
  return iup.DEFAULT
end

-- Callback called when a branch will be renamed
function renamenode:action()
  tree:renamenode_cb(tree.value)
  tree.redraw = "YES"
  return iup.DEFAULT
end

function init_tree_atributes2()
  tree.font = "COURIER_NORMAL_10"
  tree.name = "Figures"
  tree.addbranch = "B3D"
  tree.addbranch = "B2D"
  tree.addbranch1 = "parallelogram"
  tree.addleaf2 = "diamond"
  tree.addleaf2 = "square"
  tree.addbranch1 = "triangle"
  tree.addleaf2 = "scalenus"
  tree.addleaf2 = "isoceles"
  tree.value = "6"
  tree.ctrl = "YES"
  tree.shift = "YES"
  tree.addexpanded = "NO"
  tree.redraw = "YES"
end

dlg = iup.dialog{tree; title = "IupTree", size = "QUARTERxTHIRD"} 
dlg:showxy(iup.CENTER,iup.CENTER)
init_tree_atributes()

if (not iup.MainLoopLevel or iup.MainLoopLevel()==0) then
  iup.MainLoop()
end

