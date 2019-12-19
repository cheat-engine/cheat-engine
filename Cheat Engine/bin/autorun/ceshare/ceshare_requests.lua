function ceshare.QueryProcessRequests(processname, startindex)
  local result=nil
  if processname==nil or processname=='' then return end
  
  local parameters='processname='..ceshare.url_encode(processname)
  if startindex then
    parameters=parameters..'startindex='..startindex
  end
  s=ceshare.QueryXURL('QueryProcessRequests.php', parameters)
  if s then
    if s.RequestList then
      result={}
      
      for i=1, s.RequestList:numChildren() do
        local request=s.RequestList:children()[i]
        local entry={}
        entry.ID=tonumber(request["@ID"])
        entry.Username=request["@username"]
        entry.Message=request["@message"]
        entry.Score=tonumber(request["@score"])
        entry.Time=request["@time"]
        
        table.insert(result, entry)
      end      
    end
  end
  
  return result
end

function ceshare.createRequestPanel(request)
  local panel=createPanel(ceshare.RequestsFrm.MessageBox)
  panel.Align='alTop'  
  panel.Tag=request.ID  
  
  local pnlScore=createPanel(panel)
  pnlScore.align='alRight'
  
  local lblScore=createLabel(pnlScore)
  lblScore.Caption='Score='..request.Score
  
  local cbMeToo=createCheckBox(pnlScore)
  cbMeToo.Caption='+1'  
  cbMeToo.Checked=ceshare.settings.Value['requestvoted'..request.ID]=='1'
    

  lblScore.AnchorSideLeft.Control=pnlScore
  lblScore.AnchorSideLeft.Side=asrLeft
  
  lblScore.AnchorSideTop.Control=pnlScore
  lblScore.AnchorSideTop.Side=asrCenter
  lblScore.BorderSpacing.Left=8
  
  cbMeToo.AnchorSideRight.Control=pnlScore
  cbMeToo.AnchorSideRight.Side=asrRight
  cbMeToo.AnchorSideTop.Control=lblScore
  cbMeToo.Anchors='[akTop,akRight]'
  cbMeToo.OnClick=function(s)
    local adjustby
    if s.Checked then
      adjustby=1
    else
      adjustby=-1
    end
      
    r=ceshare.QueryXURL('SetRequestScore.php', 'id='..request.ID..'&adjustscoreby='..adjustby)
    if r then
      request.Score=request.Score+adjustby --I could have used a return value from the server, but just showing of an example of a server sided value showing up as if it worked... (And yes, I am talking to you)
      lblScore.Caption='Score='..request.Score
      if s.Checked then
        ceshare.settings.Value['requestvoted'..request.ID]='1'
      else
        ceshare.settings.Value['requestvoted'..request.ID]='0'
      end
    else
      s.Checked=false
    end
  end
  
  pnlScore.Width=cbMeToo.Width+8+pnlScore.Canvas.getTextWidth('Score=100')
  
  
  local pnlMessage=createPanel(panel)
  pnlMessage.align='alClient'
  
  local lblUsername=createLabel(pnlMessage)
  lblUsername.Caption=request.Username
  lblUsername.Align='alTop'
  lblUsername.Font.Style='[fsBold]'
  local romemo=createMemo(pnlMessage)  
  romemo.readOnly=true
  romemo.ScrollBars='ssAutoBoth'
  romemo.Lines.Text=request.Message
  romemo.Align='alClient'
  
  panel.Height=panel.Canvas.getTextHeight('Qwertyuiopjkl')*4+lblUsername.height
  panel.BorderSpacing.Bottom=8
end

function ceshare.RequestForCheatsClick(s)
  ceshare.CurrentRequests=ceshare.QueryProcessRequests(process)
    
  
  if ceshare.CurrentRequests then
    --show the Request window
    if ceshare.RequestsFrm==nil then
      --create the request form
      local f=createFormFromFile(ceshare.formpath..'CommentsOrRequests.FRM')
      ceshare.RequestsFrm=f
      f.Name='RequestsFrm'
      f.Caption='Requests'
      
      f.btnSend.OnClick=function(s)
        r=ceshare.QueryXURL('AddRequest.php', 'processname='..ceshare.url_encode(process)..'&note='..ceshare.url_encode(ceshare.RequestsFrm.mMessage.Lines.Text))
        if r then        
          ceshare.RequestForCheatsClick(s)          
        end      
      end
    end
    
    --ceshare.RequestsFrm.lblPrevious25.Visible=#ceshare.CurrentRequests>25
    --ceshare.RequestsFrm.lblNext25.Visible=#ceshare.CurrentRequests>25
    
    --ceshare.RequestsFrm.lblPrevious25.Enabled=false --starts at the top
    --ceshare.RequestsFrm.lblNext25.Enabled=#ceshare.CurrentRequests>25
    ceshare.RequestsFrm.lblPrevious25.Visible=false
    ceshare.RequestsFrm.lblNext25.Visible=false

    --build gui controls for the requests and put them in the scrollbox  
    while ceshare.RequestsFrm.MessageBox.ControlCount>0 do
      ceshare.RequestsFrm.MessageBox.Control[0].destroy()
    end
    
    
    --#1 is the most recent one and goes down from there
    for i=#ceshare.CurrentRequests,1,-1 do
      ceshare.createRequestPanel(ceshare.CurrentRequests[i])
    end    
    
    ceshare.RequestsFrm.show()
    ceshare.RequestsFrm.Position='poScreenCenter'
    
    
  
  end 
  
  
  
end

