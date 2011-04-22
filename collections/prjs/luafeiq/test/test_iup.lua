require 'lfs'
require 'iuplua'
require( "iupluacontrols" )


local append = table.insert

function get_dir (path)
    local files = {}
    local dirs = {}
    for f in lfs.dir(path) do
        if f ~= '.' and f ~= '..' then
            if lfs.attributes(path..'/'..f,'mode') == 'file' then
                append(files,f)
            else
                append(dirs,f)
            end
        end
    end
    return files,dirs
end

tree = iup.tree {}

function set (id,value,attrib)
    iup.TreeSetTableId(tree,id,{value,attrib})
    print(string.format("id=%d, name=%s", id, tree.value))
end

function get(id)
    return iup.TreeGetTable(tree,id)
end

function fill (path,id)
    local files,dirs = get_dir(path)
    id = id + 1
    local state = "STATE"..id
    for i = #files,1,-1 do -- put the files in reverse order!
        tree.addleaf = files[i]
        set(id,path..'/'..files[i])
    end
    for i = #dirs,1,-1 do -- ditto for directories!
        tree.addbranch = dirs[i]
        set(id,path..'/'..dirs[i],'dir')
        tree[state] = "COLLAPSED"
    end
end

function tree:branchopen_cb(id)
    tree.value = id
    local t = get(id)
    if t[2] == 'dir' then
        fill(t[1],id)
        set(id,t[1],'xdir')
    end
end

dlg = iup.dialog{tree; title = "IupTree", size = "QUARTERxTHIRD"} 
dlg:showxy(iup.CENTER,iup.CENTER)

fill("c:\\", 0)

iup.MainLoop()


