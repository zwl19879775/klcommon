--[[
  Provide some logger functions
  Kevin Lynx
  1.26.2011
--]]
require("logging")
require("logging.file")

logger = logging.file("luafeiq_%s.log", "%Y-%m-%d")
_consoleLvl = logging.INFO
logger:info("=========================start log=========================")

function check_print(s, l)
	if _consoleLvl <= l then
		print(s)
	end
end

function logd(s)
	logger:debug(s)
	check_print(s, logging.DEBUG)
end

function logi(s)
	logger:info(s)
	check_print(s, logging.INFO)
end

function logw(s)
	logger:warn(s)
	check_print(s, logging.WARN)
end

function loge(s)
	logger:error(s)
	check_print(s, logging.ERROR)
end

