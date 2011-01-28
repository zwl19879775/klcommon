--[[
  Implements a chat dialog.
  Kevin Lynx
  1.28.2011
--]]
function chatdlg_create()
    local window = {}
    window.text_history = iup.multiline{expand = "YES"}
    window.text_body = iup.multiline{expand = "YES"}
    window.btn_send = iup.button{title = "Send"}
    window.dlg = iup.dialog{iup.vbox 
        {
            window.text_history,
            window.text_body,
            iup.hbox { iup.fill{}, window.btn_send, iup.fill{} }
        }, 
        title="Chat with somebody",
        size="QUARTERxTHIRD"
    }
    return window
end

function chatdlg_show(window)
    window.dlg:showxy(iup.CENTER, iup.CENTER)
end

