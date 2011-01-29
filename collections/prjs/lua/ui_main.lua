--[[
  The main dialog, show user list.
  Kevin Lynx
  1.28.2011
--]]
require( "iuplua" )
-- since IUP 3.3, it does not require iupluacontrols
--require( "iupluacontrols" )

dofile("luafeiq.lua")
dofile("ui_chat_dlg.lua")
dofile("ui_branch_utils.lua")
dofile("ui_unread_message.lua")
dofile("ui_grouplist.lua")

OTHER_GROUP_NAME = "Others"
UI_FONT = "COURIER_NORMAL_10" 

local tree = iup.tree{}
function tree:executeleaf_cb(id)
    local arg = iup.TreeGetTable(tree, id)
    local wnd = chatdlg_create(arg, WINDOW_DATA_USER)
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
		tree.redraw = "YES"
    end
end

function add_user(tree, user)
    local groupname = user.groupname
    if string_empty(groupname) then
        groupname = OTHER_GROUP_NAME
    end
    branch_addleaf(tree, groupname, user.nickname, user)
	tree.redraw = "YES"
end

function remove_user(tree, user)
	branch_delleaf(tree, user.nickname)
	tree.redraw = "YES"
end

local user_listener = {}
user_listener.onadd = function(user)
    add_group(tree, user.groupname)
    add_user(tree, user)
end

user_listener.onremove = function(user)
	remove_user(tree, user)
end

function ui_on_recv_privatemsg(user, text)
	if not chatdlg_handle_recvmsg(user, WINDOW_DATA_USER, text, user) then
		-- put the message in the unread window
		unreaddlg_append_private(user, text)
	end
end

function ui_on_recv_groupmsg(user, group, text)
	if not chatdlg_handle_recvmsg(group, WINDOW_DATA_GROUP, text, user) then
		-- put the message in the unread window
		unreaddlg_append_group(group, text)
	end
end

function set_ui_listener()
	user_setlistener(user_listener)
	luafeiq.on_recv_privatemsg = ui_on_recv_privatemsg
	luafeiq.on_recv_groupmsg = ui_on_recv_groupmsg
end

function wrap_in_tabs()
	tree.tabtitle = "Users"
	-- the size of the tree seems some bugs.
	-- here must set the correct rastersize, can make it works fine.
	tree.rastersize = "10x10"
	local unread = unreaddlg_create()
	unread.tabtitle = "Unread"
	local grouplist = groupwnd_create()
	grouplist.tabtitle = "Groups"
	return iup.tabs { tree, grouplist, unread }
end

tree.font = UI_FONT 
tree.name = "All"
local dlg = iup.dialog
	{
		wrap_in_tabs(),
		title = "luaFeiq", 
		size = "180x320",
		maxbox = "NO"		
	} 
dlg:showxy(iup.CENTER,iup.CENTER)

set_ui_listener()
local udp = luafeiq_init()
iup.SetIdle(idle_cb(udp))
iup.MainLoop()
luafeiq_release()

