--[[
  Store some focused group
  Kevin Lynx
  1.29.2011
--]]
GROUPLIST = {}
grouplist_listener = nil

-- flags, whether enable receive other group message.
GROUP_RECV_OTHERS = true

function group_setlistener(listener)
	logd("set group list listener")
	grouplist_listener = listener
end

function group_create(groupnum)
	local group = {}
	group.number = groupnum
	group.message = chatlog_create()
	return group
end

function group_insert(group)
	GROUPLIST[group.number] = group
	if grouplist_listener ~= nil then
		grouplist_listener.onadd(group)
	end
end

function group_exist(groupnum)
	return group_get(groupnum) ~= nil
end

function group_get(groupnum)
	return GROUPLIST[groupnum]
end

