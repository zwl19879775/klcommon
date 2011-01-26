--[[
  Store all valid users
  Kevin Lynx
  1.26.2011
--]]

USERLIST = { }

function user_dump(user)
	return string.format("user(ip:%s,port:%s,nick:%s,group:%s)",
		user.ip, user.port, user.nickname, user.groupname)
end

function user_create(ip, port, nickname, groupname)
	local user = { }
	user.ip = ip
	user.port = port
	user.nickname = nickname
	user.groupname = groupname
	return user
end

function user_add(user)
	if USERLIST[user.ip] then
		logd(string.format("user %s already exist", user.ip))
	end
	USERLIST[user.ip] = user
	logd(string.format("add user: %s", user_dump(user)))
end

function user_get(ip)
	return USERLIST[ip]
end

function user_remove(ip)
	USERLIST[ip] = nil
end

