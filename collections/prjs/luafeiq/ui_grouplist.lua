--[[ 
  UI to display group list
  Kevin Lynx
  1.29.2011
--]]
grouplist_window = {}

function groupwnd_onclick(tree, id)
    local arg = iup.TreeGetTable(tree, id)
    local wnd = chatdlg_create(arg, WINDOW_DATA_GROUP)
    chatdlg_show(wnd)
end

function groupwnd_create_listener(tree, listener)
	listener.onadd = function(group)
		logi(string.format("add a group (%s) to group list window", group.number))
		branch_addleaf(tree, tree.name, group_format_name(group), group)
		tree.redraw = "YES"
	end
end

function groupwnd_create()
	grouplist_window.tree = iup.tree { 
		name = "Groups", 
		font = UI_FONT,
		rastersize = "10x10"
	}
	grouplist_window.tree.executeleaf_cb = groupwnd_onclick
	grouplist_window.listener = {}
	groupwnd_create_listener(grouplist_window.tree, grouplist_window.listener)
	group_setlistener(grouplist_window.listener)
	return grouplist_window.tree
end

