
function ceshare.login(username,password)
  local i=ceshare.getInternet()
  local parameters='';
  if username then
    parameters=parameters..'username='..ceshare.url_encode(username)
  end 
  
  if password then
    parameters=parameters..'&password='..ceshare.url_encode(password)
  end
  
  local r=i.postURL(ceshare.base..'login.php',parameters)
  if (r:sub(1,2)=='<?') then
    local s=xmlParser:ParseXmlText(r)
    if s then
      if s.Valid then
        ceshare.LoggedIn=true
        return true        
      else
        if s.error then
          ceshare.showError(s.error:value())
        end
      end
    else
      ceshare.showError(r)
    end
  else
    ceshare.showError(r);
  end
end

function ceshare.logout()
  print('logout')
  local i=ceshare.getInternet()
  i.postURL(ceshare.base..'logout.php')
  ceshare.LoggedIn=false
end

function ceshare.spawnLoginDialog() --I could also use a frm for this, but just showing how to use pure lua for a dialog (designer is still easier though)
  --check if already logged in (cookies)
  if ceshare.login() then return true end
  
  if ceshare.loginform==nil then 
    f=createForm(false)
    lblUsername=createLabel(f)
    edtUsername=createEdit(f)
    lblPassword=createLabel(f)
    edtPassword=createEdit(f)
    lblRegister=createLabel(f)
    cbLogoutWhenLoadingTables=createCheckBox(f)

    pnlBtns=createPanel(f)
    pnlBtns.BevelOuter='bvNone'
    btnOk=createButton(pnlBtns)
    btnOk.Caption='OK';
    btnOk.ModalResult=mrOK
    btnOk.Default=true

    btnCancel=createButton(pnlBtns)
    btnCancel.Caption='Cancel'
    btnCancel.ModalResult=mrCancel
    btnCancel.Cancel=true

    f.Caption='CEShare Login';
    f.BorderStyle='bsSingle';
    f.Position='poScreenCenter';

    lblUsername.Caption='Username'
    lblUsername.AnchorSideTop.Control=f
    lblUsername.AnchorSideTop.Side=asrTop
    lblUsername.AnchorSideLeft.Control=f
    lblUsername.AnchorSideLeft.Side=asrLeft
    lblUsername.BorderSpacing.Left=4
    lblUsername.BorderSpacing.Right=4

    edtUsername.AnchorSideTop.Control=lblUsername
    edtUsername.AnchorSideTop.Side=asrBottom
    edtUsername.AnchorSideLeft.Control=lblUsername
    edtUsername.AnchorSideLeft.Side=asrLeft
    edtUsername.AnchorSideRight.Control=f
    edtUsername.AnchorSideRight.Side=asrRight
    edtUsername.Constraints.MinWidth=f.Canvas.getTextWidth('   USERNAME   ')
    edtUsername.TextHint='Username'
    edtUsername.BorderSpacing.Right=4
    edtUsername.Anchors='[akTop, akLeft, akRight]'
    edtUsername.name='username'
    edtUsername.Text=''


    lblPassword.Caption='Password'
    lblPassword.AnchorSideTop.Control=edtUsername
    lblPassword.AnchorSideTop.Side=asrBottom
    lblPassword.AnchorSideLeft.Control=lblUsername
    lblPassword.AnchorSideLeft.Side=asrLeft

    edtPassword.AnchorSideTop.Control=lblPassword
    edtPassword.AnchorSideTop.Side=asrBottom
    edtPassword.AnchorSideLeft.Control=lblUsername
    edtPassword.AnchorSideLeft.Side=asrLeft
    edtPassword.AnchorSideRight.Control=f
    edtPassword.AnchorSideRight.Side=asrRight
    edtPassword.Constraints.MinWidth=f.Canvas.getTextWidth('   PASSWORD   ')
    edtPassword.BorderSpacing.Right=4
    edtPassword.Anchors='[akTop, akLeft, akRight]'
    edtPassword.PasswordChar=string.byte('*')
    edtPassword.TextHint='Password'
    edtPassword.name='password';
    edtPassword.Text=''
    
    lblRegister.Caption='Register'
    lblRegister.Font.Color=0xff0000
    lblRegister.Font.Style='[fsUnderline]'
    lblRegister.Cursor=-21
    lblRegister.AnchorSideTop.Control=edtPassword
    lblRegister.AnchorSideTop.Side=asrBottom
    lblRegister.BorderSpacing.Top=4
    lblRegister.BorderSpacing.Bottom=4    
    lblRegister.AnchorSideLeft.Control=edtPassword
    lblRegister.AnchorSideLeft.Side=asrCenter    
    lblRegister.OnMouseDown=function()
      shellExecute(ceshare.base..'RegisterPage.php')
    end
    
    

    pnlBtns.AnchorSideTop.Control=lblRegister
    pnlBtns.AnchorSideTop.Side=asrBottom
    pnlBtns.AnchorSideLeft.Control=f
    pnlBtns.AnchorSideLeft.Side=asrCenter

    pnlBtns.ChildSizing.ControlsPerLine=2
    pnlBtns.ChildSizing.HorizontalSpacing=4
    pnlBtns.ChildSizing.EnlargeHorizontal='crsHomogenousChildResize'
    pnlBtns.ChildSizing.Layout='cclLeftToRightThenTopToBottom'
    pnlBtns.AutoSize=true

    pnlBtns.BorderSpacing.Top=6
    pnlBtns.BorderSpacing.Bottom=4
    
    cbLogoutWhenLoadingTables.Caption='Logout when loading ceshare tables'
    cbLogoutWhenLoadingTables.Name='cbLogoutWhenLoadingTables'
    cbLogoutWhenLoadingTables.AnchorSideTop.Control=pnlBtns
    cbLogoutWhenLoadingTables.AnchorSideTop.Side=asrBottom    
    cbLogoutWhenLoadingTables.AnchorSideLeft.Control=lblUsername
    cbLogoutWhenLoadingTables.AnchorSideLeft.Side=asrLeft
    
    cbLogoutWhenLoadingTables.Hint='When checked you will get logged out when loading a ceshare table. This to prevent malicious tables from using your account';
    cbLogoutWhenLoadingTables.ShowHint=true    
    cbLogoutWhenLoadingTables.Checked=ceshare.settings.Value['logoutWhenLoadingTables']~='0'
    

    f.OnClose=nil --by default forms created with createForm will have an OnClose that will free themself on close. That's not needed in this case where the form needs to be reused
    
    f.BorderIcons='[]'
    f.AutoSize=true
    ceshare.loginform=f
  end
  
  if ceshare.loginform.showModal()==mrOK then
    local username=ceshare.loginform.username.text
    local password=ceshare.loginform.password.text

    ceshare.loginform.username.text=''
    ceshare.loginform.password.text=''    
    
    local v
    if ceshare.loginform.cbLogoutWhenLoadingTables.checked then    
      ceshare.settings.Value['logoutWhenLoadingTables']='1'
    else
      ceshare.settings.Value['logoutWhenLoadingTables']='0'
    end
    
    return ceshare.login(username, password)  
  end
end




function ceshare.ClearCredentials()
  --print("Forgetting who you are")
  ceshare.logout()  --in case some idiot tablemaker wants to make 

  if ceshare.loginform then
    ceshare.loginform.password.Text=''
    ceshare.loginform.username.Text=''
    ceshare.loginform.destroy()
    ceshare.loginform=nil
  end
end