
local hexView=getMemoryViewForm().HexadecimalView
local m=hexView.PopupMenu
local mi=createMenuItem(m)
mi.Caption="Copy as text to clipboard"
mi.ImageIndex=15
mi.Shortcut="Ctrl+Alt+C"
mi.OnClick=function(sender)
  if hexView.HasSelection then
    local l=hexView.SelectionStop-hexView.SelectionStart+1
    local bytes=readBytes(hexView.SelectionStart, l,true)

    local s
    if (l>1) and (bytes[2]==0) then --widestring (todo: better detection, especially for those non-english languages)
      s=byteTableToWideString(bytes)
    else
      s=byteTableToString(bytes)
    end

    writeToClipboard(s)
  end
end



m.items.insert(getMemoryViewForm().Cut1.MenuIndex,mi)