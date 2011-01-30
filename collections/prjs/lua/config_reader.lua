--[[
  Read config.
  Kevin Lynx
  1.30.2011
--]]
require("LuaXML")

CONFIG = {}
CONFIG_TAGS = { "loginname", "pcname", "nickname", "groupname", "macaddress" }
CONFIG_GROUP_TAGS = { "number", "name" }

function config_loginname()
    return CONFIG[CONFIG_TAGS[1]]
end

function config_pcname()
    return CONFIG[CONFIG_TAGS[2]]
end

function config_nickname()
    return CONFIG[CONFIG_TAGS[3]]
end

function config_groupname()
    return CONFIG[CONFIG_TAGS[4]]
end

function config_macaddress()
    return CONFIG[CONFIG_TAGS[5]]
end

function config_iterate_group(callback)
    local groups = CONFIG.groups
    if groups ~= nil then
        local number = CONFIG_GROUP_TAGS[1]
        local name = CONFIG_GROUP_TAGS[2]
        for i, v in ipairs(groups) do
            callback(v[number], v[name])
        end
    end
end

function config_get_groupname(groupnum)
    local groups = CONFIG.groups
    if groups ~= nil then
        local number = CONFIG_GROUP_TAGS[1]
        local name = CONFIG_GROUP_TAGS[2]
        for i, v in ipairs(groups) do
            if v[number] == groupnum then
                return v[name]
            end
        end
    end
    return nil
end

function config_load()
    local filename = "data/luafeiq.xml"
    local file = xml.load(filename)
    if file == nil then
        logw(string.format("load config file (%s) failed.", filename))
        return false
    end
    local root = file:find("luafeiq")
    for i,v in ipairs(CONFIG_TAGS) do
        local t = root:find(v)
        if t == nil then
            logw(string.format("config file %s node does not configed", v))
        else
            CONFIG[v] = t.value
            logi(string.format("load %s as %s", v, t.value))
        end
    end
    local t = root:find("focusedgroups")
    if t ~= nil then
        CONFIG.groups = config_loadgroups(t)
    end
    return true
end

function config_loadgroups(t)
    local i = 1
    local groups = {}
    local number = CONFIG_GROUP_TAGS[1]
    local name = CONFIG_GROUP_TAGS[2]
    while t[i] ~= nil do
        local tag = t[i]
        local group = {}
        group[number] = string.lower(tag[number])
        group[name] = tag[name]
        logi(string.format("load group %s, %s", group[number], group[name]))
        groups[i] = group
        i = i + 1
    end
    return groups
end

