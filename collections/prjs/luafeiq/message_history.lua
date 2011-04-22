--[[
  Store chat message history.
  Kevin Lynx
  1.28.2011
--]]

function chatlog_create()
	local message = {}
	message.cnt = 0
	message.entrys = {}
	return message
end

function chatlog_insert(message, text, from)
	-- entry represents one message
	local entry = {}
	entry.text = text
	entry.from = from
	entry.time = os.time()
	message.cnt = message.cnt + 1
	message.entrys[message.cnt] = entry
	logd(string.format("chatlog insert %s:%s, count=%d", from, text, message.cnt))
	return entry
end

function chatlog_clear(message)
	message.cnt = 0
	message.entrys = {}
end

-- callback = function(entry)
function chatlog_iterate(message, callback)
	for i, v in ipairs(message.entrys) do
		callback(v)
	end
end

function chatlog_dump(message)
	chatlog_iterate(message, function(entry)
		logd(string.format("%s:\n%s\n%s\n", entry.from,
			entry.text, os.date("%c", entry.time)))
	end)
end

