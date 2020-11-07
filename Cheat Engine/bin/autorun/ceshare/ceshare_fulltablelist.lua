local DPIMultiplier=(getScreenDPI()/96)
local ThumbnailWidth=240*DPIMultiplier
local ThumbnailHeight=80*DPIMultiplier

local DummyBitmap=createBitmap()
DummyBitmap.Canvas.Font.Size=12

local getListItemData,getThumbnail,generateListItemBitmap,getListItemBitmap
local cleanPage, setPage,getFullProcessList,filterList

--[[
ceshare.FullProcessList is the downloaded list which contains all entries
ceshare.FullProcessListView is the searchresult

--]]

local backgroundcolor

local darkMode=1
local windowColor,listColor,searchFieldColor,fontColor, fontSize, linkColor, highlightColor
if darkMode==1 then
  listColor=clBlack
  searchFieldColor=clBlack
  fontColor=clWhite
  windowColor=clBlack
  linkColor=0x0000ff
  highlightColor=0x00ff00
else 
  listColor=clDefault
  searchFieldColor=clDefault
  fontColor=clDefault
  windowColor=clDefault
  linkColor=0xff0000
  highlightColor=clDefault
end

fontSize=12



function getListItemData(index)
  local width
  local height
  local data=ceshare.FullProcessListView[index]

  local text=data.process..' - '..data.title
  local width=2*DPIMultiplier+DummyBitmap.Canvas.getTextWidth(text) 
  local height=2*DPIMultiplier+DummyBitmap.Canvas.getTextHeight(text) 
  
  local text2
  if data.count>1 then      
    text2=string.format(translate('  and %d more tables...'),data.count)
    local w=2*DPIMultiplier+DummyBitmap.Canvas.getTextWidth(text2)       
    if w>width then width=w end      
    height=height+2*DPIMultiplier+DummyBitmap.Canvas.getTextHeight(text2) 
  end 
  
  if data.thumbnail then
    if ThumbnailWidth>width then width=ThumbnailWidth end
    height=height+ThumbnailHeight
  end
  
  return width,height, text,text2
end

function getThumbnail(data)


  if data.thumbnail then
    if ceshare.ThumbnailCache==nil then
      ceshare.ThumbnailCache={}
    end
    
    --check cache
    local p=ceshare.ThumbnailCache[data.thumbnail]
    
    if p and isKeyPressed(VK_CONTROL) then --force reload      
      p.destroy() 
      ceshare.ThumbnailCache[data.thumbnail]=nil
      p=nil
    end
    
    if p==nil then --not cached yet
      local thumbnailtext=ceshare.getInternet().getURL(data.thumbnail)
      local ss=createStringStream(thumbnailtext)
      p=createPicture()
      if p.loadFromStream(ss)==false then
        --X  (not found/valid)
        p.Bitmap.Width=10
        p.Bitmap.Heigth=10
        p.Bitmap.Canvas.Width=10
        p.Bitmap.Canvas.Heigth=10
        p.Bitmap.Canvas.Pen.Color=0x0000ff
        p.Bitmap.Canvas.Brush.Color=0xffffff
        p.Bitmap.Canvas.clear()
        p.Bitmap.Canvas.line(0,0,10,10)
        p.Bitmap.Canvas.line(10,0,0,10)        
      end
      
      ss.destroy()
      
      ceshare.ThumbnailCache[data.thumbnail]=p 
    end    
    return p
  end
end

function generateListItemBitmap(index)
  --generate a bitmap
  local data=ceshare.FullProcessListView[index]
  local result
  if data then
    local w,h,text,text2=getListItemData(index)
    local thumbnail=getThumbnail(data)
    local y=0
    
    result=createBitmap(w,h)    
    if backgroundcolor==nil then
      backgroundcolor=ceshare.AllTableForm.List.RGBColor         
    end
    result.Canvas.Brush.Color=backgroundcolor --0xff00
    result.Canvas.fillRect(0,0,w,h)
    
    result.Canvas.Font.Size=12
    result.Canvas.Font.Color=fontColor
    
    if thumbnail then
      local r={}
      r.Top=0
      r.Left=0
      r.Right=ThumbnailWidth
      r.Bottom=ThumbnailHeight
      result.Canvas.stretchDraw(r,thumbnail.Bitmap) 
      --result.Canvas.draw(0,0, thumbnail.Bitmap) --works
      --result.Canvas.copyRect(0,0,ThumbnailWidth,ThumbnailHeight,thumbnail.PNG,0,0,thumbnail.PNG.Width,thumbnail.PNG.Height)
      y=ThumbnailHeight
    end
    result.Canvas.textOut(0,y,text)
    if text2 and text2~='' then
      y=y+result.Canvas.getTextHeight(text)
      result.Canvas.textOut(0,y,text2)
    end
  end
  
  return result
end


function getListItemBitmap(index)
  --generates or gets a previously generated bitmap for the given listitem
  local data=ceshare.FullProcessListView[index] 
  if data then
    if data.bitmap==nil then
      data.bitmap=generateListItemBitmap(index)
    end
    
    return data.bitmap
  else
    print("index "..index.." is not in the list");
  end  
end

function cleanPage(index)
  --cleans a single page (usually the previous page)
  for i=1+index*25,math.min(1+index*25+25, #ceshare.FullProcessListView) do
    if ceshare.FullProcessListView[i].bitmap then
      ceshare.FullProcessListView[i].bitmap.destroy()
      ceshare.FullProcessListView[i].bitmap=nil
    end
  end  
end

function setPage(index)
  if ceshare.AllTableForm.Page then
    cleanPage(ceshare.AllTableForm.Page)
  end

  if index<0 then index=0 end
  if 1+index*25>#ceshare.FullProcessListView then
    index=(#ceshare.FullProcessListView / 25) 
  end
  ceshare.FullProcessListPage=index
  
  --add ceshare.FullProcessListView items to the list  
  ceshare.AllTableForm.List.Items.clear()
  ceshare.AllTableForm.Page=index
  local start=1+index*25
  local stop=math.min(index*25+25, #ceshare.FullProcessListView) 
  local i
  for i=start,stop do
    ceshare.AllTableForm.List.items.add(ceshare.FullProcessListView[i].process..' - '..ceshare.FullProcessListView[i].title)
  end
  
  ceshare.AllTableForm.lblCurrentPage.Caption=(math.floor(index+1))..'/'..(math.floor(1+#ceshare.FullProcessListView/25)) 
  
  ceshare.AllTableForm.lblNext.Enabled=#ceshare.FullProcessListView>1+index*25+25
  ceshare.AllTableForm.lblPrev.Enabled=index>0
  
  if ceshare.AllTableForm.lblNext.Enabled then
    ceshare.AllTableForm.lblNext.Cursor=crHandPoint   
  else
    ceshare.AllTableForm.lblNext.Cursor=crDefault
  end
  
  if ceshare.AllTableForm.lblPrev.Enabled then
    ceshare.AllTableForm.lblPrev.Cursor=crHandPoint   
  else
    ceshare.AllTableForm.lblPrev.Cursor=crDefault
  end  
end



function getFullProcessList()
  --clean up the entries with a bitmap first
  local i
  if ceshare.FullProcessList then  
    for i=1,#ceshare.FullProcessList do
      if ceshare.FullProcessList[i].bitmap then
        ceshare.FullProcessList[i].bitmap.destroy()
        ceshare.FullProcessList[i].bitmap=nil
      end
    end  
  end
  
  ceshare.FullProcessList={}
  ceshare.FullProcessListView={}
  fullProcessListText=ceshare.getInternet().getURL(ceshare.base..'fullprocesslist.txt')
  sl=createStringList()
  sl.Text=fullProcessListText
  local i
  for i=0, sl.Count-1 do
    local a=sl[i]
    local e={}
    local count,thumbnail,process,title=a:split(' #-# ')
    
    e.count=tonumber(count)
    if thumbnail~='' then
      e.thumbnail=thumbnail
    end  
   
    
    
    e.process=process
    e.title=title
    
    table.insert(ceshare.FullProcessList,e)
  end
  
  sl.destroy()  
  filterList()
end

function filterList()
  local searchvalue=ceshare.AllTableForm.SearchField.Text:upper()
  if searchvalue=='' then
    ceshare.FullProcessListView=ceshare.FullProcessList
  else
    ceshare.FullProcessListView={} --assign it a new table
    
    --scan the FullProcessList and only add those where the process or title match
    local i
    for i=1, #ceshare.FullProcessList do
      if ceshare.FullProcessList[i].process:upper():find(searchvalue,1,true) or 
         ceshare.FullProcessList[i].title:upper():find(searchvalue,1,true) then
        table.insert(ceshare.FullProcessListView, ceshare.FullProcessList[i])
      end
    end    
  end
  
  setPage(0)  
end

function ceshare.ViewAllTablesClick()
  local atf=ceshare.AllTableForm
  local list
  
  if atf==nil then 
    ceshare.AllTableForm=createForm(false)        
    atf=ceshare.AllTableForm
    atf.Name='ceshare_AllTableForm'
    atf.Caption=translate('All tables')
    atf.Color=windowColor
    
    
    local panel=createPanel(atf)
    panel.color=windowColor
    panel.align=alTop
      local searchfieldlabel=createLabel(panel)
      searchfieldlabel.Caption=translate('Search:')
      local searchfield=createEdit(atf)
      
      searchfieldlabel.AnchorSideTop.Side=asrTop
      searchfieldlabel.AnchorSideTop.Control=panel
      searchfieldlabel.AnchorSideLeft.Side=asrLeft
      searchfieldlabel.AnchorSideLeft.Control=panel
      searchfieldlabel.Font.Color=fontColor
      
      searchfield.AnchorSideTop.Side=asrBottom
      searchfield.AnchorSideTop.Control=searchfieldlabel
      searchfield.AnchorSideLeft.Side=asrLeft
      searchfield.AnchorSideLeft.Control=panel
      searchfield.Name='SearchField' 
      searchfield.Text=''      
      searchfield.OnChange=filterList
      searchfield.Parent=panel
      
      searchfield.Constraints.MinWidth=atf.Canvas.getTextWidth('Space for a gamename')*2
      searchfield.BorderSpacing.Bottom=4*DPIMultiplier      
      searchfield.Color=searchFieldColor
      searchfield.Font.Color=fontColor
      searchfield.Font.Size=fontSize
      
      

      local prevnext=createPanel(panel)
      prevnext.Color=windowColor
      prevnext.AutoSize=true
      prevnext.ChildSizing.Layout='cclLeftToRightThenTopToBottom'
      prevnext.ChildSizing.ControlsPerLine=3
      prevnext.ChildSizing.HorizontalSpacing=5*DPIMultiplier
      prevnext.BevelOuter='bvNone'
      
      local lblPrev=createLabel(atf)      
      local lblCurrentPage=createLabel(atf)      
      local lblNext=createLabel(atf)
      lblPrev.Caption=translate('Previous (25)')
      lblPrev.Name='lblPrev'
      lblPrev.Font.Style='[fsUnderline]'
      lblPrev.Font.Color=linkColor
      lblCurrentPage.Caption='1/xx'
      lblCurrentPage.Name='lblCurrentPage'
      lblCurrentPage.Font.Color=fontColor
      lblNext.Caption=translate('Next (25)')
      lblNext.Name='lblNext'
      lblNext.Font.Style='[fsUnderline]'
      lblNext.Font.Color=linkColor      
      
      lblPrev.OnMouseDown=function() setPage(ceshare.AllTableForm.Page-1) end
      lblNext.OnMouseDown=function() setPage(ceshare.AllTableForm.Page+1) end
      
      lblPrev.Parent=prevnext
      lblCurrentPage.Parent=prevnext
      lblNext.Parent=prevnext
      
      prevnext.AnchorSideTop.Side=asrBottom
      prevnext.AnchorSideTop.Control=searchfield
      prevnext.AnchorSideRight.Side=asrRight
      prevnext.AnchorSideRight.Control=panel
      prevnext.Anchors='[akBottom, akRight]'
      


    panel.AutoSize=true
    
    list=createListBox(atf)
    list.align=alClient      
    list.name='List'
    list.OnMeasureItem=function(sender, index, height)
      local realindex=ceshare.AllTableForm.Page*25+index+1
      
      w,h,t1,t2=getListItemData(realindex)
      --print("measureitem "..index.." h="..h)
      
      return h
    end
    
    list.OnDrawItem=function (sender, index, rect, state)
      --print("drawing "..index)
      local realindex=ceshare.AllTableForm.Page*25+index+1
      
      local bm=getListItemBitmap(realindex)
      if bm then
        sender.Canvas.Brush.Color=backgroundcolor        
        sender.Canvas.Pen.Color=backgroundcolor
        sender.Canvas.fillRect(rect.Left, rect.Top, rect.Right,rect.Bottom)
        sender.Canvas.rect(rect.Left, rect.Top, rect.Right,rect.Bottom)  
        sender.Canvas.draw(rect.Left,rect.Top,bm)
        if state:find('odSelected') then
          sender.Canvas.Brush.Color=highlightColor   
          sender.Canvas.Pen.Color=highlightColor
          sender.Canvas.Brush.Style='bsClear'
          sender.Canvas.Pen.Color=highlightColor
          sender.Canvas.Pen.Width=2*DPIMultiplier
          sender.Canvas.rect(rect.Left, rect.Top, rect.Right,rect.Bottom)        
        end       
      else
        print("error "..index)
      end      
    end
    
    list.OnDblClick=function()
      local realindex=ceshare.AllTableForm.Page*25+ceshare.AllTableForm.List.ItemIndex+1
      if realindex>=0 then      
        ceshare.CheckForCheatsClick(nil, ceshare.FullProcessListView[realindex].process)
      end    
    end
    
    list.Style='lbOwnerDrawVariable'
    list.Color=listColor    
    

    atf.BorderStyle='bsSizeable'
    if atf.loadFormPosition()==false then
      --set initial size    
      atf.Width=600*DPIMultiplier
      atf.Height=400*DPIMultiplier
      atf.Position=poScreenCenter
    end   
    
    
    atf.OnClose=function() return caHide end
    
    atf.OnDestroy=function(f)
      f.saveFormPosition()
    end
    
    atf.PopupMode='pmNone'
  end
  
  atf.searchfield.Text=''
  
  getFullProcessList()
  ceshare.FullProcessListView=ceshare.FullProcessList
  setPage(0)
  
  atf.show()
end