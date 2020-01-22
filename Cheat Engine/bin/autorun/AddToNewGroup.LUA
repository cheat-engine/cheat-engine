local pm=AddressList.PopupMenu
local pmAddToNewGroup=createMenuItem(pm)
pmAddToNewGroup.Caption=translate('Add to new group')
pmAddToNewGroup.ImageIndex=MainForm.CreateGroup.ImageIndex
pm.Items.insert(MainForm.CreateGroup.MenuIndex, pmAddToNewGroup)

local oldOnPopup=AddressList.PopupMenu.OnPopup
AddressList.PopupMenu.OnPopup=function(s)
  if oldOnPopup then
    oldOnPopup(s)
  end
  pmAddToNewGroup.Visible=AddressList.SelCount>=1
end

pmAddToNewGroup.OnClick=function(s)
  local i
  local count=0
  local selcount=0
  local withAddress=false
  local hasAddressSupport=false

  if AddressList.SelCount==0 then
    messageDialog('Please select at least one entry first', mtError, mbOK)
    return
  end

  hasAddressSupport=AddressList[0].IsAddressGroupHeader~=nil

  for i=0,AddressList.Count-1 do
    if AddressList[i].IsGroupHeader then
      count=count+1
    end
  end


  local groupname=translate(string.format('Group %d',count+1))
  if (isKeyPressed(VK_CONTROL)==false) then
    groupname=InputQuery(translate('Groups'), translate('What do you want the groupname to be?'), groupname)
    if groupname then
      if hasAddressSupport then
        withAddress=messageDialog(translate('Do you want "address" version?'), mtConfirmation, mbYes, mbNo)==mrYes
      end
    else
      return
    end
  end


  --create a new group and add all selected records to the list
  local header=AddressList.createMemoryRecord()
  header.IsGroupHeader=true
  header.IsAddressGroupHeader=withAddress
  header.Description=groupname

  records={}

  for i=0,AddressList.Count-1 do
    if AddressList[i].Selected then
      local selectedparent=false
      local p=AddressList[i].Parent
      while p do
        if p.Selected then selectedparent=true end
        p=p.Parent
      end

      if selectedparent==false then
        table.insert(records,AddressList[i])
      end
    end
  end

  for i=1,#records do
    records[i].Parent=header
  end
end
