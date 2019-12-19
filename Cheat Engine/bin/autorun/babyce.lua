--This extension makes use of the capabilitie of adding custom fields to objects
--Note that function callbacks do not return the same object if they are passed, but it's still useful for a global object

--States
--0=ProcessPick
--1=First scan, select value type
--2=First/next scan: scan options
--3=First/next scan: secondary options (like input field), else goto 4
--4=Do Scan (First or Next scan) and when done, goto 2

if (iknowwhatthiscodedoes==nil) then return nil end

local oldAddTab=MainForm.miAddTab.OnClick
local oldFoundListFontSize

MainForm.miAddTab.OnClick=function(m)
  if messageDialog('Tabs are scary, are you sure?',mtWarning, mbYes, mbNo)==mrYes then
    --babies don't use tabs
    BabyCE.Visible=false
    if oldFoundListFontSize then
      MainForm.FoundList3.Font.Size=oldFoundListFontSize
    end
    
    MainForm.miAddTab.OnClick=oldAddTab
    oldAddTab(m)
  end
end



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
  
  local ProcessList=createComponentClass("TListView",BabyCE)
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

  --ProcessList.beginUpdate()

  
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
 -- list.destroy() 

  cPid.Width=maxwidth;
  
  --ProcessList.endUpdate() --(needs ce 6.8.2+)  
  if ProcessList.Items.Count>0 then
    ProcessList.ItemIndex=0
  end
  
  MainForm.Width=MainForm.Width-1 --triggers an update
  MainForm.Width=MainForm.Width+1
  
  ProcessList.OnDblClick=function(pl)
    if pl.ItemIndex==-1 then return end
    
    local li=pl.Items[pl.ItemIndex]
    local pid=tonumber(li.caption)
    
    pl.destroy()
    
    OpenProcess(pid)
    
    local t=createTimer() --for some reason this is needed...
    t.Interval=1
    t.OnTimer=function(t)
      t.destroy()     
      
      BabyCE.UpdateState(1) --select scan options
    end
    
    --BabyCE.UpdateState(1)
    
    
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
    --print("b.tag="..b.tag.." which means an index of "..indexer[b.tag])
    
    MainForm.VarType.itemindex=indexer[b.tag]
    MainForm.VarType.OnChange(MainForm.VarType)

    local t=createTimer() --for some reason this is needed...
    t.Interval=1
    t.OnTimer=function(t)
      t.destroy()
      BabyCE.UpdateState(2) --select scan options
    end
    
  end

  oldFoundListFontSize=MainForm.FoundList3.Font.Size
  MainForm.FoundList3.Font.Size=15
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
  
  local btnBack=createButton(BabyCE)
  btnBack.AnchorSideRight.Control=BabyCE
  btnBack.AnchorSideRight.Side=asrRight
  btnBack.AnchorSideBottom.Control=BabyCE
  btnBack.AnchorSideBottom.Side=asrBottom
  btnBack.Anchors="[akBottom,akRight]"
  btnBack.Caption="Back"
  btnBack.AutoSize=true
  btnBack.OnClick=function(s)
    local t=createTimer() --for some reason this is needed...
    t.Interval=1
    t.OnTimer=function(t)
      t.destroy()
      BabyCE.UpdateState(0) --select scan options
    end       
  end
    
end

BabyCE.DoState[2]=function() 
  --scan options 
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
  
  --check that all the buttons are visible, else shrink the foundlist
  local cw=buttonPanel.ClientWidth
  local maxX=cw;
  for i=0,buttonPanel.ComponentCount-1 do  
    local x,w
    x=buttonPanel.Component[i].Left
    w=buttonPanel.Component[i].Width  

    if (x+w)>maxX then
      maxX=x+w
    end    
  end
  
  if maxX>cw then
    MainForm.FoundList3.Width=MainForm.FoundList3.Width-(maxX-cw)
  end
  
  buttonPanel.Component[MainForm.ScanType.ItemIndex].setFocus()
  
  if MainForm.btnNextScan.Enabled then
    MainForm.foundlistpopup.OnPopup(MainForm.foundlistpopup)
    if MainForm.miForgotScan.visible and MainForm.miForgotScan.enabled then
      local btnForgotScan=createButton(buttonPanel)    
      btnForgotScan.Caption='Forgot/Reload'
      btnForgotScan.Hint="Click this if you forgot what the previous scan was"
      btnForgotScan.ShowHint=true      
      btnForgotScan.OnClick=function(s)
        MainForm.miForgotScan.doClick() --the OnScanDone is already configured to go to state 2        
      end
    end
  end
  
  
  local newScan=createButton(BabyCE)
  newScan.AnchorSideRight.Control=BabyCE
  newScan.AnchorSideRight.Side=asrRight
  newScan.AnchorSideBottom.Control=BabyCE
  newScan.AnchorSideBottom.Side=asrBottom
  newScan.Anchors="[akBottom,akRight]"
  newScan.Caption="New scan"
  newScan.AutoSize=true
  newScan.OnClick=function(s)
    if messageDialog("Are you sure? Your current scan results will be erased",mtConfirmation,mbYes,mbNo)==mrYes then
      MainForm.btnNewScan.doClick()
      local t=createTimer() --for some reason this is needed...
      t.Interval=1
      t.OnTimer=function(t)
        t.destroy()
        BabyCE.UpdateState(1) --select scan options
      end       
    end
  end
end

BabyCE.DoState[3]=function()
  --secondary options
  local edtValue
  local edtValue2
  local hasOptions=false
  local startScan=function()
    --start the scan
    if edtValue then
      MainForm.ScanValue.Text=edtValue.Text
    end
    
    if edtValue2 then
      MainForm.ScanValue2.Text=edtValue2.Text
    end
    
    local t=createTimer() --for some reason this is needed...
    t.Interval=1
    t.OnTimer=function(t)
      t.destroy()
      BabyCE.UpdateState(4) --select scan options
    end       
  end

  
  local hint=createLabel(BabyCE)
  hint.Caption=MainForm.ScanType.Text
  hint.align="alTop"
  hint.Alignment="taCenter" 
  
  buttonPanel=createPanel(BabyCE)
  buttonPanel.align="alClient"
  buttonPanel.BevelOuter="bvNone"
  buttonPanel.Caption=""
  buttonPanel.ChildSizing.Layout="cclLeftToRightThenTopToBottom"
  buttonPanel.ChildSizing.ControlsPerLine=1
  buttonPanel.ChildSizing.HorizontalSpacing=4
  buttonPanel.ChildSizing.VerticalSpacing=64
  buttonPanel.ChildSizing.EnlargeHorizontal="crsScaleChilds"    
  buttonPanel.BorderWidth=4
    
    
  if MainForm.ScanValue.Visible then
    hasOptions=true
    edtValue=createComponentClass("TEdit",buttonPanel) --using a TEdit since it has a nice TextHint property
    edtValue.parent=buttonPanel
    edtValue.TextHint="Value to scan"
    edtValue.Text=MainForm.ScanValue.Text
    
    if edtValue.Text~='' then
      edtValue.selectAll()
    end
    
    edtValue.setFocus()
  end
  
  if MainForm.ScanValue2.Visible then
    buttonPanel.ChildSizing.ControlsPerLine=3
    
    andLabel=createComponentClass("TLabel",buttonPanel)
    andLabel.caption=MainForm.andlabel.caption
    andLabel.parent=buttonPanel
    andLabel.Alignment='taCenter'
  
    edtValue2=createComponentClass("TEdit",buttonPanel)
    edtValue2.parent=buttonPanel
    edtValue2.TextHint="Value to scan" 
    edtValue2.Text=MainForm.ScanValue2.Text 
    
    MainForm.ScanValue2.sendToBack() --for some reason it pops up over BabyCE
  end 
  
  --start the scan
  if hasOptions then
    --create a button to start the scan
    btnStartScan=createButton(buttonPanel)
    btnStartScan.Caption="Scan"
    btnStartScan.OnClick=startScan
    --not a direct child
    btnStartScan.AnchorSideTop.Control=edtValue
    btnStartScan.AnchorSideTop.Side=asrBottom
    btnStartScan.BorderSpacing.Top=64
    
    btnStartScan.AnchorSideLeft.Control=buttonPanel
    btnStartScan.AnchorSideLeft.Side=asrLeft
    btnStartScan.AnchorSideRight.Control=buttonPanel
    btnStartScan.AnchorSideRight.Side=asrRight
    
    btnStartScan.Anchors="[akLeft, akTop, akRight]"    
    btnStartScan.Font.Size=24
    btnStartScan.Font.Style="[fsBold]"
    btnStartScan.AutoSize=true
    
    btnStartScan.Default=true
    
    local btnBack=createButton(BabyCE)
    btnBack.AnchorSideRight.Control=BabyCE
    btnBack.AnchorSideRight.Side=asrRight
    btnBack.AnchorSideBottom.Control=BabyCE
    btnBack.AnchorSideBottom.Side=asrBottom
    btnBack.Anchors="[akBottom,akRight]"
    btnBack.Caption="Back"
    btnBack.AutoSize=true
    btnBack.OnClick=function(s)
      local t=createTimer() --for some reason this is needed...
      t.Interval=1
      t.OnTimer=function(t)
        t.destroy()
        BabyCE.UpdateState(2) --select scan options
      end       
    end
    
  else
    startScan()  
  end     
  
  --If Unknown initial value, start scan and skip to 4  
end

BabyCE.DoState[4]=function()
  local hint=createLabel(BabyCE)
  hint.Caption=MainForm.ScanType.Text.." scan in progress. Please wait"
  hint.WordWrap=true
  hint.align="alTop"
  hint.Alignment="taCenter" 
  
  getCurrentMemscan().OnScanDone=function()
    BabyCE.UpdateState(2) --scan options
  end

  if MainForm.btnNextScan.Enabled then  
    MainForm.btnNextScan.doClick()
  else
    MainForm.btnNewScan.doClick()
  end
  
  BabyCE.bringToFront()
  
  --now wait till the scan is done, and capture errors...
  
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
