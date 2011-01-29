--[[
  Store all valid users
  Kevin Lynx
  1.26.2011
--]]

USERLIST = { }
userlist_listener = nil

function user_dump(user)
	return string.format("user(ip:%s,port:%s,nick:%s,group:%s)",
		user.ip, user.port, user.nickname, user.groupname)
end

function user_create(ip, port, nickname, groupname)
	local user = { }
	user.ip = ip
	user.port = port
	user.nickname = check_string(nickname)
	user.groupname = check_string(groupname)
	user.message = chatlog_create()
	return user
end

function user_setlistener(listener)
    logi("set user listener")
    userlist_listener = listener 
end

function user_add(user)
	if USERLIST[user.ip] then
		logw(string.format("user %s already exist, remove the exist one", user.ip))
		user_remove(user.ip)
	end
	USERLIST[user.ip] = user
	logi(string.format("add user: %s", user_dump(user)))
    if userlist_listener ~= nil then
        userlist_listener.onadd(user)
    end
end

function user_equal(u1, u2)
    return u1.ip == u2.ip
end

function user_get(ip)
	return USERLIST[ip]
end

function user_remove(ip)
    if USERLIST[ip] ~= nil and userlist_listener ~= nil then
        userlist_listener.onremove(USERLIST[ip])
    end
	logi(string.format("remove user %s", ip))
	USERLIST[ip] = nil
end

