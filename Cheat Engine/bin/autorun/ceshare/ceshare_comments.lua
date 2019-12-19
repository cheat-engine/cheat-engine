function ceshare.QueryTableComments(entry, startindex)
  local result=nil
  local parameters='id='..entry.ID
  if startindex then
    parameters=parameters..'startindex='..startindex
  end
  s=ceshare.QueryXURL('QueryTableComments.php', parameters)
  if s then
    if s.Comments then
      result={}
      
      for i=1, s.Comments:numChildren() do
        local comment=s.Comments:children()[i]
        local entry={}
        entry.ID=tonumber(comment["@ID"])
        entry.Username=comment["@username"]
        entry.Message=comment["@message"]
        entry.Time=comment["@time"]
        table.insert(result, entry)
      end      
    end
  end  
  return result
end


function ceshare.createCommentPanel(comment)
  local panel=createPanel(ceshare.CommentsFrm.MessageBox)
  panel.Align='alTop'  
  panel.Tag=comment.ID  
   
  local pnlMessage=createPanel(panel)
  pnlMessage.align='alClient'
  
  local lblUsername=createLabel(pnlMessage)
  lblUsername.Caption=comment.Username
  lblUsername.Align='alTop'
  lblUsername.Font.Style='[fsBold]'
  local romemo=createMemo(pnlMessage)  
  romemo.readOnly=true
  romemo.ScrollBars='ssAutoBoth'
  romemo.Lines.Text=comment.Message
  romemo.Align='alClient'
  
  panel.Height=panel.Canvas.getTextHeight('Qwertyuiopjkl')*4+lblUsername.height
  panel.BorderSpacing.Bottom=8
end

function ceshare.ViewComments(entry)
  if entry then
    ceshare.CurrentComments=ceshare.QueryTableComments(entry)
    
  
    if ceshare.CurrentComments then
      --show the Comments window
      if ceshare.CommentsFrm==nil then
        --create the comments form
        local f=createFormFromFile(ceshare.formpath..'CommentsOrRequests.FRM')
        ceshare.CommentsFrm=f
        f.Name='CommentsFrm'
        f.Caption='Comments'
        
        f.btnSend.OnClick=function(s)
          r=ceshare.QueryXURL('AddComment.php', 'id='..entry.ID..'&comment='..ceshare.url_encode(ceshare.CommentsFrm.mMessage.Lines.Text))
          if r then        
            ceshare.ViewComments(entry)
          end      
        end
      end
      
      --ceshare.CommentsFrm.lblPrevious25.Visible=#ceshare.CurrentRequests>25
      --ceshare.CommentsFrm.lblNext25.Visible=#ceshare.CurrentRequests>25
      
      --ceshare.CommentsFrm.lblPrevious25.Enabled=false --starts at the top
      --ceshare.CommentsFrm.lblNext25.Enabled=#ceshare.CurrentRequests>25
      
      ceshare.CommentsFrm.lblPrevious25.Visible=false
      ceshare.CommentsFrm.lblNext25.Visible=false      

      --build gui controls for the requests and put them in the scrollbox  
      while ceshare.CommentsFrm.MessageBox.ControlCount>0 do
        ceshare.CommentsFrm.MessageBox.Control[0].destroy()
      end
      
      
      --#1 is the most recent one and goes down from there
      for i=#ceshare.CurrentComments,1,-1 do
        ceshare.createCommentPanel(ceshare.CurrentComments[i])
      end    
      
      ceshare.CommentsFrm.show()
      ceshare.CommentsFrm.Position='poScreenCenter'
      
    
    end 
  end
end