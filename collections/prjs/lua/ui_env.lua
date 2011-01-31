--[[
  Luckily the UI library IUP can check the OS we are running,
  so we can use it to check something,like string encoding.
  kevin Lynx
  1.31.2011
--]]
env = {}
OS_LINUX = 1
OS_WINDOWS = 2
OS_UNKNOWN = 3

function env_init_strencode()
    if env.os == OS_LINUX then
        -- to communicate with these windows users, i should
        -- translate these string encoding.
        require('stringconv')
    end
end

function env_init()
    local os_s = iup.GetGlobal("SYSTEM")
    if os_s == "Linux" then
        env.os = OS_LINUX
    elseif os_s == "WinXP" or os_s == "Win7" or
        os_s == "Vista" or os_s == "Win2K" then
        env.os = OS_WINDOWS
    else
        env.os = OS_UNKNOWN
    end
    logi("detect run in " .. os_s)
    env_init_strencode()
end

-- convert utf8 to gbk
function env_s_u2g(s)
    if env.os == OS_LINUX then
        return stringconv.u2g(s)
    end
    return s
end

-- convert gbk to utf8
function env_s_g2u(s)
    if env.os == OS_LINUX then
        return stringconv.g2u(s)
    end
    return s
end

