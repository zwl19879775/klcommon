--[[
  Implements group chat dialog.
  Kevin Lynx
  1.28.2011
--]]
-- <ip>-<window>
chat_windows = {}
CHAT_FROM_ME = "ME"
WINDOW_DATA_USER = 1
WINDOW_DATA_GROUP = 2

-- attach a user to this window, that means to make this window
-- to be a private chat window.
function chatdlg_attach_user(window, user)
	window.data = user 
	window.data_t = WINDOW_DATA_USER
end

-- attach a group info to this window, that means to make this window
-- to be a group chat window.
function chatdlg_attach_group(window, group)
	window.data = group
	window.data_t = WINDOW_DATA_GROUP
end

function chatdlg_attach(window, data, data_t)
	if data_t == WINDOW_DATA_USER then
		chatdlg_attach_user(window, data)
		window.dlg.title = string.format("Chat with %s", data.nickname)
	else
		chatdlg_attach_group(window, data)
		window.dlg.title = string.format("Group Chat in %s", group_format_name(data))
	end
end

function chatdlg_retrieve_key(data, data_t)
	if data_t == WINDOW_DATA_USER then
		return data.ip
	else
		return data.number
	end
end

function chatdlg_push(window)
	local key = chatdlg_retrieve_key(window.data, window.data_t)
    chat_windows[key] = window
end

function chatdlg_pop(window)
	local key = chatdlg_retrieve_key(window.data, window.data_t)
    chat_windows[key] = nil
end

-- listener, called after recv private or group messages.
function chatdlg_handle_recvmsg(data, data_t, text, user)
	local key = chatdlg_retrieve_key(data, data_t)
	local window = chat_windows[key]
	if window == nil then
		logd(string.format("not found listener window on %s", key))
		return false 
	end
	-- append a new message.
	chatdlg_append_history(window, text, user.nickname)
	return true
end

function chatdlg_append_history(window, text, from)
	window.text_history.append = string.format("%s:\n%s\n", from, text)
	window.text_history.caret = "1000,1" -- scroll to the end
end

function chatdlg_send_msg(window, text)
	if window.data_t == WINDOW_DATA_USER then
		-- send private message
		send_chat_msg(luafeiq_udp(), window.data.ip, DEST_PORT, text)
	else
		-- send group chat message
		send_group_msg(luafeiq_udp(), window.data.number, text)
	end
end

function chatdlg_set_btncb(window)
    window.btn_send.action = function(btn)
        local text = window.text_body.value
		if not string_empty(text) then
			window.text_body.value = ""
			chatdlg_send_msg(window, text)
			chatdlg_append_history(window, text, CHAT_FROM_ME)
			-- append message log sent by myself
			chatlog_insert(window.data.message, text, CHAT_FROM_ME)
		end
		iup.SetFocus(window.text_body)
    end
end

function chatdlg_create(data, data_t)
    local window = {}
    window.text_body = iup.multiline{expand = "HORIZONTAL", size="x50", wordwrap="YES"}
    window.text_history = iup.multiline{expand = "YES", readonly="YES", wordwrap="YES"}
    window.btn_send = iup.button{title = "Send"}
    window.dlg = iup.dialog{iup.vbox 
        {
            window.text_history,
            window.text_body,
            iup.hbox { iup.fill{}, window.btn_send, iup.fill{} },
			gap="10",
			margin="10x10",
        }, 
        size = "300x300",
		font = UI_FONT
    }
    window.dlg.close_cb = function(dlg)
        chatdlg_pop(window)
    end
	chatdlg_attach(window, data, data_t)
    chatdlg_set_btncb(window)
    return window
end

function chatdlg_retrieveall(window)
	chatlog_iterate(window.data.message, function(entry) 
		logd(string.format("append history message %s:%s", entry.from, entry.text))
		chatdlg_append_history(window, entry.text, entry.from)
	end)
end

function chatdlg_show(window)
    window.dlg:showxy(iup.CENTER, iup.CENTER)
    chatdlg_push(window)
	chatdlg_retrieveall(window)
	iup.SetFocus(window.text_body)
end

