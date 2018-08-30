--This extension makes use of the capabilitie of adding custom fields to objects
--Note that function callbacks do not return the same object if they are passed, but it's still useful for a global object

--States
--0=ProcessPick
--1=First scan, select value type
--2=First scan, initial scan options
--3=Next scan

if (iknowwhatthiscodedoes==nil) then return nil end

BabyCE=createPanel(MainForm.Panel5)

BabyCE.AnchorSideTop.Control=MainForm.foundcountlabel
BabyCE.AnchorSideTop.Side=asrBottom
BabyCE.AnchorSideLeft.Control=MainForm.FoundList3
BabyCE.AnchorSideLeft.Side=asrRight
BabyCE.AnchorSideRight.Control=MainForm.Panel5
BabyCE.AnchorSideRight.Side=asrRight
BabyCE.AnchorSideBottom.Control=MainForm.FoundList3
BabyCE.AnchorSideBottom.Side=asrBottom
BabyCE.BevelOuter='bvNone'
BabyCE.Caption=''

BabyCE.Font.Size=15 
MainForm.FoundList3.Font.Size=15


BabyCE.Anchors="[akLeft,akRight,akTop,akBottom]"
MainForm.LogoPanel.Visible=false

BabyCE.UpdateState=function(newstate)
  BabyCE.State=newstate  

  --clear the screen
  while BabyCE.ComponentCount>0 do
    BabyCE.Component[0].destroy()
  end

  BabyCE.DoState[newstate]()
end

BabyCE.DoState={}
BabyCE.DoState[0]=function()
  --processlist.  I want it in the same order as CE. Using the table version will be randomized so using the stringlist

  --local 
  local hint=createLabel(BabyCE)
  hint.Caption="Doubleclick the process"
  hint.align="alTop"
  hint.Alignment="taCenter"
 
  MainForm.FoundList3.Visible=false 
  
  
  
  
  ProcessList=createComponentClass("TListView",BabyCE)
  ProcessList.Parent=BabyCE
  ProcessList.Align="alClient"
  ProcessList.ViewStyle="vsReport"
  ProcessList.ReadOnly=true
  ProcessList.HideSelection=false
  ProcessList.RowSelect=true
  ProcessList.AutoWidthLastColumn=true
  local cPid=ProcessList.Columns.add()  
  cPid.Caption='PID'
  local cName=ProcessList.Columns.add()  
  cName.Caption='Name'
   
  local list=createStringList()
  getProcessList(list)

  ProcessList.beginUpdate()

  
  maxwidth=0
  
  for i=list.Count-1,0,-1 do
    --because some babies like having it as a decimal value, convert the hex pid to decimal    
    local sep=string.find(list[i],'-')
    if sep then      
      local pid=string.sub(list[i],1,sep-1)
      local name=string.sub(list[i],sep+1)
      
      local li=ProcessList.Items.Add()
      li.Caption=tonumber("0x"..pid)
      li.SubItems.Add(name)     

      local w=BabyCE.Canvas.getTextWidth(li.Caption+4)
      if w>maxwidth then
        maxwidth=w
      end
    end
  end 
  list.destroy() 

  cPid.Width=maxwidth;
  
  ProcessList.endUpdate()  
  if ProcessList.Items.Count>0 then
    ProcessList.ItemIndex=0
  end
  
  ProcessList.OnDblClick=function(pl)
    local li=pl.Items[pl.ItemIndex]
    local pid=tonumber(li.caption)
    OpenProcess(pid)
    
    BabyCE.UpdateState(1)
  end 
end

BabyCE.DoState[1]=function()
  local indexer={}
  indexer[vtByte]=1
  indexer[vtWord]=2
  indexer[vtDword]=3
  indexer[vtQword]=4
  indexer[vtSingle]=5
  indexer[vtDouble]=6
  indexer[vtByteArray]=8
  indexer[10]=9
    
  local typeButtonClick=function(b)
    print("b.tag="..b.tag.." which means an index of "..indexer[b.tag])
    
    MainForm.VarType.itemindex=indexer[b.tag]
    MainForm.VarType.OnChange(MainForm.VarType)

    local t=createTimer() --for some reason this is needed...
    t.Interval=1
    t.OnTimer=function(t)
      t.destroy()
      BabyCE.UpdateState(2) --select scan options
    end
    
  end

  MainForm.FoundList3.Visible=true
  --first scan, select value type

  local btnAll=createButton(BabyCE)
  btnAll.Caption='ALL'
  btnAll.Align="alTop"
  btnAll.AutoSize=true
  btnAll.Tag=10  --vtAll is 10, that typedef missing in 6.8.1 and earlier
  
  btnAll.OnClick=function()
    local s=getSettings()
    --make sure the user has set the most important types (it's baby CE, don't expect the user to not mess up the settings)
    s.Value['AllDWord']=1
    s.Value['AllFloat']=1
    s.Value['AllDouble']=1
    reloadSettingsFromRegistry()
    
    typeButtonClick(btnAll) 
  end
  
  local hint=createLabel(BabyCE)
  hint.Caption="Select the value type to scan"
  hint.align="alTop"
  hint.Alignment="taCenter" 


  buttonPanel=createPanel(BabyCE)
  buttonPanel.align="alClient"
  buttonPanel.BevelOuter="bvNone"
  buttonPanel.Caption=""
  buttonPanel.ChildSizing.Layout="cclLeftToRightThenTopToBottom"
  buttonPanel.ChildSizing.ControlsPerLine=3
  buttonPanel.ChildSizing.EnlargeHorizontal="crsHomogenousChildResize"  
  
  local btnByte=createButton(buttonPanel)
  btnByte.Caption='1 Byte'
  btnByte.OnClick=typeButtonClick
  btnByte.Tag=vtByte
  
  local btnWord=createButton(buttonPanel)
  btnWord.Caption='2 Bytes';
  btnWord.OnClick=typeButtonClick
  btnWord.Tag=vtWord
  
  local btnDword=createButton(buttonPanel)
  btnDword.Caption='4 Bytes';
  btnDword.OnClick=typeButtonClick
  btnDword.Tag=vtDword
  
  local btnQword=createButton(buttonPanel)
  btnQword.Caption='8 Bytes'
  btnQword.OnClick=typeButtonClick
  btnQword.Tag=vtQword
  
  local btnFloat=createButton(buttonPanel)  
  btnFloat.Caption='Float'
  btnFloat.OnClick=typeButtonClick
  btnFloat.Tag=vtSingle
  
  local btnDouble=createButton(buttonPanel)
  btnDouble.Caption='Double'
  btnDouble.OnClick=typeButtonClick
  btnDouble.Tag=vtDouble
  
  local btnAOB=createButton(buttonPanel)
  btnAOB.Caption='AOB'
  btnAOB.OnClick=typeButtonClick
  btnAOB.Tag=vtByteArray
end

BabyCE.DoState[2]=function()  
  local hint=createLabel(BabyCE)
  local btnClick=function(b)
    MainForm.ScanType.ItemIndex=b.tag
    MainForm.ScanType.OnChange(MainForm.ScanType)
    
    local t=createTimer() --for some reason this is needed...
    t.Interval=1
    t.OnTimer=function(t)
      t.destroy()
      BabyCE.UpdateState(3) --select scan options
    end    
  end
  hint.Caption="Choose your scan options"
  hint.align="alTop"
  hint.Alignment="taCenter" 
  
  --first scan, initial scan options
  
  --parse the scan options from the MainForm.ScanType
  buttonPanel=createPanel(BabyCE)
  buttonPanel.align="alClient"
  buttonPanel.BevelOuter="bvNone"
  buttonPanel.Caption=""
  buttonPanel.ChildSizing.Layout="cclLeftToRightThenTopToBottom"
  buttonPanel.ChildSizing.ControlsPerLine=2
  buttonPanel.ChildSizing.HorizontalSpacing=4
  buttonPanel.ChildSizing.VerticalSpacing=4
  
  buttonPanel.ChildSizing.EnlargeHorizontal="crsHomogenousChildResize"  

  local i 
  local items=MainForm.ScanType.Items
  for i=0,items.Count-1 do
    local btn=createButton(buttonPanel)    
    btn.Caption=items[i]
    btn.Tag=i  
    btn.OnClick=btnClick
  end  
  
end

BabyCE.DoState[3]=function()
  --next scan
end


--init:
if getOpenedProcessID()==0 then
  BabyCE.UpdateState(0)
else
  if MainForm.btnNextScan.Enabled==false then
    BabyCE.UpdateState(2)
  else
    BabyCE.UpdateState(1)
  end
end
