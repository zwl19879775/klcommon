--[[
  Implement a dialog to store these unread messages
  Kevin Lynx
  1.29.2011
--]]

unread_msg_window = {}

function unreaddlg_on_dblclick_cb(list, pos, text)
	local idtext = unreaddlg_get_idtext(text)
	local messages = unread_msg_window.messages
	local entry = messages[idtext]
	if not entry.group then
		-- want to view a private message
		local user = entry.arg
		local wnd = chatdlg_create(user, WINDOW_DATA_USER)
		chatdlg_show(wnd)
	else
		-- want to view group message
		local group = entry.arg
		local wnd = chatdlg_create(group, WINDOW_DATA_GROUP)
		chatdlg_show(wnd)
	end
	messages[idtext] = nil
	list.removeitem = pos
end

function unreaddlg_create()
	unread_msg_window.list = iup.list { expand="YES" }
	unread_msg_window.list.dblclick_cb = unreaddlg_on_dblclick_cb
	unread_msg_window.messages = {}
	return iup.frame { unread_msg_window.list, title = "Unread messages",
		font = UI_FONT }
end

function unreaddlg_private_idtext(user)
	return string.format("%s:%s", user.nickname, user.ip)
end

function unreaddlg_group_idtext(groupnum)
	return "Group:" .. groupnum
end

-- append a text into the history list(update/new)
function unreaddlg_append_private(user, text)
	local messages = unread_msg_window.messages
	local idtext = unreaddlg_private_idtext(user)
	local entry = messages[idtext]
	if entry == nil then
		entry = unreaddlg_new_entry(idtext, text, false, user)
	else 
		entry = unreaddlg_update_entry(idtext, entry, text, user)
	end
	messages[idtext] = entry
end

function unreaddlg_append_group(group, text)
	local messages = unread_msg_window.messages
	local idtext = unreaddlg_group_idtext(group.number)
	local entry = messages[idtext]
	if entry == nil then
		entry = unreaddlg_new_entry(idtext, text, true, group)
	else 
		entry = unreaddlg_update_entry(idtext, entry, text, group)
	end
	messages[idtext] = entry
end

function unreaddlg_formattext(idtext, entry)
	return string.format("%s(%d)%s", idtext, entry.count, entry.text)
end

function unreaddlg_new_entry(idtext, text, groupflag, arg)
	local entry = {}
	entry.text = text
	entry.count = 1
	entry.group = groupflag
	entry.arg = arg
	local list = unread_msg_window.list
	list[list.count+1] = unreaddlg_formattext(idtext, entry)
	return entry
end

function unreaddlg_get_idtext(text)
	print(text)
	return readin(text, '%(', 0)
end

function unreaddlg_finditem(idtext)
	local list = unread_msg_window.list
	local cnt = 1
	local maxcnt = tonumber(list.count)
	while cnt <= maxcnt do
		if unreaddlg_get_idtext(list[cnt]) == idtext then
			return cnt
		end
		cnt = cnt + 1
	end
	return 0
end

function unreaddlg_update_entry(idtext, entry, text, arg)
	entry.text = text
	entry.count = entry.count + 1
	entry.arg = arg
	local id = unreaddlg_finditem(idtext)
	if id == 0 then
		logw(string.format("not found idtext : %s in the list", idtext))
	else
		unread_msg_window.list[id] = unreaddlg_formattext(idtext, entry)
	end
	return entry
end

