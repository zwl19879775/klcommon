--[[
  Implements a chat dialog.
  Kevin Lynx
  1.28.2011
--]]
chat_windows = {}

function chatdlg_push(window, user)
    chat_windows[user.ip] = window
end

function chatdlg_pop(window)
    chat_windows[window.user.ip] = nil
end

function chatdlg_set_btncb(window)
    window.btn_send.action = function(btn)
        local text = window.text_body.value
        window.text_body.value = ""
        -- TODO: udp object
        send_chat_msg(udp, window.user.ip, BIND_PORT, text)
        window.text_history.append = string.format("%s:\n%s", "ME", text)
    end
end

function chatdlg_create(user)
    local window = {}
    window.user = user
    window.text_history = iup.multiline{expand = "YES"}
    window.text_body = iup.multiline{expand = "YES"}
    window.btn_send = iup.button{title = "Send"}
    window.dlg = iup.dialog{iup.vbox 
        {
            window.text_history,
            window.text_body,
            iup.hbox { iup.fill{}, window.btn_send, iup.fill{} }
        }, 
        title=string.format("Chat with %s", user.nickname),
        size="QUARTERxTHIRD"
    }
    window.dlg.close_cb = function(dlg)
        logd(string.format("dialog %s closed", window.user.nickname))
        chatdlg_pop(window)
    end
    chatdlg_set_btncb(window)
    logd(string.format("create dialog %s", user.nickname))
    return window
end

function chatdlg_show(window)
    window.dlg:showxy(iup.CENTER, iup.CENTER)
    chatdlg_push(window, window.user)
end

