--[[
  The main dialog show user list.
  Kevin Lynx
  1.28.2011
--]]
require( "iuplua" )
require( "iupluacontrols" )

dofile("luafeiq.lua")
dofile("ui_chat_dlg.lua")
dofile("ui_branch_utils.lua")

OTHER_GROUP_NAME = "Others"

local tree = iup.tree{}
function tree:executeleaf_cb(id)
    local arg = iup.TreeGetTable(tree, id)
    local wnd = chatdlg_create(arg)
    chatdlg_show(wnd)
end

function idle_cb(udp)
    return function () 
        udp_runonce(udp)
        return iup.DEFAULT
    end
end

function add_group(tree, groupname)
    if string_empty(groupname) then
        groupname = OTHER_GROUP_NAME
    end
    local i = branch_find(tree, groupname, "BRANCH")
    if i == 0 then
        branch_add(tree, groupname)
    end
end

function add_user(tree, user)
    local groupname = user.groupname
    if string_empty(groupname) then
        groupname = OTHER_GROUP_NAME
    end
    branch_addleaf(tree, groupname, user.nickname, user)
end

local user_listener = {}
user_listener.onadd = function(user)
    add_group(tree, user.groupname)
    add_user(tree, user)
end

user_listener.onremove = function(user)
end

tree.font = "COURIER_NORMAL_10"
tree.name = "All"
local dlg = iup.dialog{tree; title = "luaFeiq", size = "QUARTERxTHIRD"} 
dlg:showxy(iup.CENTER,iup.CENTER)

user_setlistener(user_listener)

udp = luafeiq_init()
iup.SetIdle(idle_cb(udp))

iup.MainLoop()

-- release 

