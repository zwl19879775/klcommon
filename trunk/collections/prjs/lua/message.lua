--[[
  Parse message.
  Kevin Lynx
  1.26.2011
--]]

require("blowfish") -- to decrypt/encrypt messages

function skipto(s, c, i)
   local s,_ = string.find(s, c, i) 
   return s
end

function readin(str, c, cnt)
    local s = 0
    while cnt > 0 do
        s = skipto(str, c, s)
		if s == nil then 
			return nil 
		end
		s = s + 1
		cnt = cnt - 1
    end
    local e = skipto(str, c, s)
    if e == nil then
        return string.sub(str, s)
    else
		e = e - 1
        return string.sub(str, s, e)
    end
end

function msg_read_feiqheader(fullheader)
	return readin(fullheader, ':', 0)
end

function msg_read_fullheader(msg)
	local c = 5
	local s = 0
	while c > 0 do
		c = c - 1
		s = skipto(msg, ':', s) + 1
	end
	return string.sub(msg, 0, s - 1), s
end

function msg_read_username(fullheader)
    return readin(fullheader, ':', 2)
end

function msg_read_pcname(fullheader)
    return readin(fullheader, ':', 3)
end

function msg_read_body(msg, pos)
	return string.sub(msg, pos)
end

function msg_read_mac(feiqheader)
	return readin(feiqheader, '#', 2)
end

function msg_read_bodysize(feiqheader)
	return readin(feiqheader, '#', 5)
end

function msg_read_type(fullheader)
	return readin(fullheader, ':', 4)
end

function msg_decrypt_body(body, bodys, mac)
	local bf = blowfish.create(mac, string.len(mac))
	local decrypt = blowfish.decrypt(bf, body, bodys)
	blowfish.destroy(bf)
	return decrypt
end

function msg_encrypt_body(body, mac)
	local bf = blowfish.create(mac, string.len(mac))
	local encrypt = blowfish.encrypt(bf, body)
	blowfish.destroy(bf)
	return encrypt
end

function msg_create_feiqheader(bodys)
    local h = string.format("1_lbt4_0#128#%s#0#0#%d", MAC_ADDRESS, bodys)
    return h
end

function msg_create_fullheader(feiqheader, cmd)
    local h = string.format("%s:%d:%s:%s:%d:", feiqheader, os.time(), 
        USERNAME, PCNAME, cmd)
    return h
end

function msg_create(fullheader, body)
    return fullheader .. body
end

function msg_create_group_body(text, group_num, mac)
    local body = string.format("QUNMSGMARK#%s#", group_num)
    body = body .. text
    local encrypt_body = msg_encrypt_body(body, mac)
    return encrypt_body
end

function msg_read_groupnum(groupmsg)
	return readin(groupmsg, '#', 1)
end

function msg_read_groupbody(groupmsg)
	return readin(groupmsg, '#', 2)
end

