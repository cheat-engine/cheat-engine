local c=createComboBox(MainForm.gbScanOptions)

c.Style='csDropDownList'
c.Items.add('All')
c.ItemIndex=0
c.Name='ScanOptionsModuleList'  


c.Align=alTop
c.BorderSpacing.Left=6
c.BorderSpacing.Right=6
c.BorderSpacing.Bottom=2

local modulelist

function FillList()
  local is64bit=targetIs64Bit()
  local op
  if is64bit then
    op='32'
  else
    op='64'
  end
  synchronize(function()  
    while c.Items.Count>1 do
      c.Items.delete(1)
    end
  end)

  modulelist=enumModules()
  
  synchronize(function()
    if modulelist then
      local i
      for i=1, #modulelist do
        modulelist[i].OriginalName=modulelist[i].Name
        if modulelist[i].Is64Bit ~= is64bit then
          modulelist[i].OriginalName='_'..modulelist[i].OriginalName
          modulelist[i].Name=modulelist[i].Name..' ('..op..'-bit)'
        end

        c.Items.Add(modulelist[i].Name)
      end
    end
  end)
end

c.OnMouseEnter=function(d)  
  if c.Items.Count<=1 then
    --print("enter")
    if FillListThread==nil then
      FillListThread=createThread(function(t)
        FillList()
        FillListThread=nil
      end)
      c.ItemIndex=0
    end
  end
end

c.OnDropDown=function(d)
  if FillListThread==nil then
    FillListThread=createThread(function(t)
      FillList()
      FillListThread=nil
    end) 
  end
    
  while FillListThread do
    checkSynchronize(50)
  end
end

c.OnSelect=function(d)
  if c.ItemIndex>=1 then
    MainForm.FromAddress.Text=string.format("%.16x",modulelist[c.ItemIndex].Address)
    if modulelist[c.ItemIndex].Size==nil then
      modulelist[c.ItemIndex].Size=getModuleSize(modulelist[c.ItemIndex].OriginalName)
    end
    if modulelist[c.ItemIndex].Size then
      MainForm.ToAddress.Text=string.format("%.16x",modulelist[c.ItemIndex].Address+modulelist[c.ItemIndex].Size)
    else
      MainForm.ToAddress.Text="7fffffffffffffff"
    end
  else
    MainForm.FromAddress.Text="0000000000000000"
    MainForm.ToAddress.Text="7fffffffffffffff"
  end
end

c.Enabled=false
