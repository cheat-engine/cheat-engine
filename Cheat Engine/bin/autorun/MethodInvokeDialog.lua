--[[
Just a generic form for filling in fields. The caller is responsible for the callbacks
Can be other types besides .NET  so no .net/mono specific code here  (just generic OOP things)

createMethodInvokeDialog returns true on ok, false on cancel

invokeDialogParams:{
name
nonmodal: boolean - When true the form won't close on clicking ok, but instead calls an OnOKClick function
isStatic: boolean (true/false/nil) - When true the instance address field will be hidden
allowCustomAddress: boolean (true/false/nil) - When true the instance address field will be editable
address: integer - address of the instance
parameters[]={varname=xxx, isObject=true/false/nil}, whatever else you wish, value=undefined} : After the call returns, the element named 'value' will be filled in by the users input
onCreateInstanceClick: function(dialog, invokeDialogParams, parameterindex) : string - Called when the + button is clicked. Return a string with the address or 'other' constructor code if you created an object (up to the caller)
onOKClick: function(dialog, invokeDialogParams, output) --called when nonmodal is true . Output is a strings object of the listbox used to show results to the user
}


--]]


local dpiscale=getScreenDPI()/96

local function createMethodInvokeDialogEx(invokeDialogParams)

  local midinfo={}
  midinfo.mid=createForm(false)
  midinfo.mid.position='poScreenCenter'
  midinfo.mid.borderStyle='bsSizeable'
  midinfo.mid.Caption=translate('Invoke ')..invokeDialogParams.name
  midinfo.mid.Name='MethodInvokeDialog'

  if (not invokeDialogParams.isStatic) and invokeDialogParams.allowCustomAddress then
    midinfo.lblInstanceAddress=createLabel(midinfo.mid)
    midinfo.lblInstanceAddress.Caption=translate('Instance address')
    midinfo.edtInstance=createEdit(midinfo.mid)

    midinfo.edtInstance.Text=string.format("%x",invokeDialogParams.address)
  end

  midinfo.gbParams=createGroupBox(midinfo.mid)
  midinfo.gbParams.Caption=translate('Parameters')

  midinfo.gbParams.AutoSize=true

  midinfo.pnlButtons=createPanel(midinfo.mid)
  midinfo.pnlButtons.ChildSizing.ControlsPerLine=2
  midinfo.pnlButtons.ChildSizing.Layout='cclLeftToRightThenTopToBottom'

  midinfo.pnlButtons.BevelOuter='bvNone'
  midinfo.pnlButtons.BorderSpacing.Top=5
  midinfo.pnlButtons.BorderSpacing.Bottom=5
  midinfo.pnlButtons.ChildSizing.HorizontalSpacing=8


  midinfo.btnOk=createButton(midinfo.mid)
  midinfo.btnCancel=createButton(midinfo.mid)

  midinfo.btnOk.Parent=midinfo.pnlButtons
  midinfo.btnCancel.Parent=midinfo.pnlButtons

  midinfo.pnlButtons.AutoSize=true

  midinfo.btnOk.caption=translate('OK')
  midinfo.btnCancel.caption=translate('Cancel')
  midinfo.btnCancel.Cancel=true

  midinfo.pnlButtons.AnchorSideTop.Control=midinfo.gbParams
  midinfo.pnlButtons.AnchorSideTop.Side=asrBottom
  midinfo.pnlButtons.AnchorSideLeft.Control=midinfo.mid
  midinfo.pnlButtons.AnchorSideLeft.Side=asrCenter
  midinfo.pnlButtons.Anchors='[akLeft, akTop]'


  midinfo.output=createListBox(midinfo.mid)
  midinfo.output.Visible=false
  midinfo.output.AnchorSideBottom.Control=midinfo.mid
  midinfo.output.AnchorSideBottom.Side=asrBottom
  midinfo.output.AnchorSideTop.Control=midinfo.pnlButtons
  midinfo.output.AnchorSideTop.Side=asrBottom
  midinfo.output.AnchorSideLeft.Control=midinfo.mid
  midinfo.output.AnchorSideLeft.Side=asrLeft
  midinfo.output.AnchorSideRight.Control=midinfo.mid
  midinfo.output.AnchorSideRight.Side=asrRight

  midinfo.output.Anchors='[akLeft, akRight, akBottom, akTop]'
  --midinfo.output.Anchors='[akLeft, akRight, akBottom]'





 -- midinfo.pnlButtons.Color=clRed


  if (not invokeDialogParams.isStatic) and invokeDialogParams.allowCustomAddress then
    midinfo.lblInstanceAddress.AnchorSideTop.Control=midinfo.mid
    midinfo.lblInstanceAddress.AnchorSideTop.Side=asrTop
    midinfo.lblInstanceAddress.AnchorSideTop.Left=midinfo.mid
    midinfo.lblInstanceAddress.AnchorSideTop.Side=asrLeft
    midinfo.edtInstance.AnchorSideTop.Control=midinfo.lblInstanceAddress
    midinfo.edtInstance.AnchorSideTop.Side=asrBottom

    midinfo.edtInstance.AnchorSideLeft.Control=midinfo.mid
    midinfo.edtInstance.AnchorSideLeft.Side=asrLeft
    midinfo.edtInstance.AnchorSideRight.Control=midinfo.mid
    midinfo.edtInstance.AnchorSideRight.Side=asrRight
    midinfo.edtInstance.Anchors='[akLeft, akRight, akTop]'
    midinfo.edtInstance.BorderSpacing.Left=2*dpiscale
    midinfo.edtInstance.BorderSpacing.Right=2*dpiscale

    midinfo.gbParams.AnchorSideTop.Control=midinfo.edtInstance
    midinfo.gbParams.AnchorSideTop.Side=asrBottom
  else
    midinfo.gbParams.AnchorSideTop.Control=midinfo.mid
    midinfo.gbParams.AnchorSideTop.Side=asrTop
  end

  midinfo.gbParams.AnchorSideLeft.Control=midinfo.mid
  midinfo.gbParams.AnchorSideLeft.Side=asrLeft
  midinfo.gbParams.AnchorSideRight.Control=midinfo.mid
  midinfo.gbParams.AnchorSideRight.Side=asrRight


  midinfo.gbParams.Anchors='[akLeft, akRight, akTop]'

  midinfo.gbParams.autoSize=true

  --midinfo.mid.AutoSize=true

  midinfo.parameters={}

  local parameters=invokeDialogParams.parameters

  if #parameters==0 then
    midinfo.gbParams.visible=false
  else
    midinfo.gbParams.TabOrder=0
  end

  local i
  for i=1, #parameters do
    local lblVarName=createLabel(midinfo.mid)
    local edtVarText=createEdit(midinfo.mid)
    local btnCreateInstance=createButton(midinfo.mid)

    lblVarName.Parent=midinfo.gbParams
    edtVarText.Parent=midinfo.gbParams
    btnCreateInstance.Parent=midinfo.gbParams
    btnCreateInstance.AutoSize=true

    btnCreateInstance.AnchorSideRight.Control=midinfo.gbParams
    btnCreateInstance.AnchorSideRight.Side=asrRight

    lblVarName.AnchorSideLeft.Control=midinfo.gbParams
    lblVarName.AnchorSideLeft.Side=asrLeft
    lblVarName.BorderSpacing.Left=2*dpiscale

    lblVarName.AnchorSideTop.Control=edtVarText
    lblVarName.AnchorSideTop.Side=asrCenter


    if i==1 then


      edtVarText.AnchorSideTop.Control=midinfo.gbParams
      edtVarText.AnchorSideTop.Side=asrTop
      edtVarText.AnchorSideLeft.Control=midinfo.gbParams
      edtVarText.AnchorSideLeft.Side=asrLeft
      
      

      --borderspacing.Left will set the position
    else


      edtVarText.AnchorSideTop.Control=midinfo.parameters[i-1].edtVarText
      edtVarText.AnchorSideTop.Side=asrBottom
      edtVarText.AnchorSideLeft.Control=midinfo.parameters[i-1].edtVarText
      edtVarText.AnchorSideLeft.Side=asrLeft --same position as the top (which gets set later)
    end
    
    edtVarText.TabOrder=i-1

    btnCreateInstance.AnchorSideTop.Control=edtVarText
    btnCreateInstance.AnchorSideTop.Side=asrCenter

    edtVarText.BorderSpacing.Top=2*dpiscale
    edtVarText.AnchorSideRight.Control=btnCreateInstance
    edtVarText.AnchorSideRight.Side=asrLeft
    edtVarText.BorderSpacing.Right=2*dpiscale


    edtVarText.Anchors='[akLeft, akRight, akTop]'

    btnCreateInstance.caption='+'
    btnCreateInstance.Anchors='[akTop, akRight]'


    midinfo.parameters[i]={}


    if type(parameters[i])=='table' then
      lblVarName.Caption=parameters[i].varname:trim()
      btnCreateInstance.visible=parameters[i].isObject
    elseif type(parameters[i])=='string' then --old code
      lblVarName.Caption=parameters[i]:trim()
      btnCreateInstance.visible=false
    end


    midinfo.parameters[i].lblVarName=lblVarName
    midinfo.parameters[i].edtVarText=edtVarText

    lblVarName.BorderSpacing.CellAlignVertical='ccaCenter'

    if btnCreateInstance.Visible then
      --return the parameters field provided in this call (the caller is responsible for adding enough extra data to be able to create an instance using that)
      btnCreateInstance.OnClick=function(sender)
        if invokeDialogParams.onCreateInstanceClick then
          edtVarText.Text=invokeDialogParams.onCreateInstanceClick(midinfo.mid, invokeDialogParams, i)
        end
      end
    end
  end

  midinfo.btnOk.OnClick=function(sender)
    local r={}

    if invokeDialogParams.isStatic==false then
      invokeDialogParams.address=getAddressFromName(midinfo.edtInstance.Text)
      if invokeDialogParams.address==nil or invokeDialogParams.address==0 then
        messageDialog(midinfo.edtInstance.Text..translate(' is not a valid address'),mtError)
        return
      end
    end

    for i=1,#invokeDialogParams.parameters do
      invokeDialogParams.parameters[i].value=midinfo.parameters[i].edtVarText.Text
    end


    if invokeDialogParams.nonmodal and invokeDialogParams.onOKClick then
      local output=midinfo.output.Items
      invokeDialogParams.onOKClick(midinfo.mid, invokeDialogParams, output)
      if output.Count>0 then
        if midinfo.output.Visible==false then
          midinfo.output.Visible=true
          midinfo.mid.height=midinfo.mid.height+midinfo.mid.canvas.getTextHeight("Xx")*4
        end
      end
    else
      midinfo.mid.ModalResult=mrOK --setting modalresult closes a modal form
    end
  end

  midinfo.btnCancel.OnClick=function(b) midinfo.mid.close() end


  midinfo.mid.OnShow=function(s)
    midinfo.mid.autoSize=false
    midinfo.mid.width=midinfo.mid.canvas.getTextWidth(midinfo.mid.Caption)+180*dpiscale


    idf=s
    if #midinfo.parameters>0 then
      local labelwidth=0
      local i
      for i=1,#parameters do --get the min width needed
        labelwidth=math.max(labelwidth, midinfo.parameters[i].lblVarName.Width)
      end

      midinfo.parameters[1].edtVarText.BorderSpacing.Left=labelwidth+7*dpiscale
    end

    midinfo.mid.position='poDesigned' --recenters the form
    midinfo.mid.position='poScreenCenter'
  end

  midinfo.mid.onClose=function(f)
    return caFree
  end

  midinfo.mid.onDestroy=function(f)
    --destroy all objects
    midinfo.btnOk.destroy()
    midinfo.btnOk=nil

    midinfo.btnCancel.destroy()
    midinfo.btnCancel=nil

    if midinfo.edtInstance then
      midinfo.edtInstance.destroy()
      midinfo.edtInstance=nil
    end

    midinfo.gbParams.destroy()
    midinfo.gbParams=nil

    --midinfo.mid.saveFormPosition()

    midinfo=nil
  end

  midinfo.mid.AutoSize=true
  if invokeDialogParams.nonmodal then
    midinfo.mid.show()
  else
    return midinfo.mid.showModal()==mrOK
  end
end

function createMethodInvokeDialog(name, parameters, okclickfunction, customAddress, static)

  if type(name)=='table' then
    --called with the params for createMethodInvokeDialogEx
    return createMethodInvokeDialogEx(name)
  else
    print("Warning: Obsolete method of calling createMethodInvokeDialog")


    --pack the parameters into an invokeDialogParams block
    local params={}
    params.name=name
    params.isStatic=static
    params.allowCustomAddress=customAddress
    params.parameters={}
    for i=1,#parameters do
      if type(parameters[i])=='string' then
        params.parameters[i]={}
        params.parameters[i].varname=parameters
      elseif type(parameters[i])=='table' then
        params.parameters[i]=parameters[i]
      end
    end
    local r=createMethodInvokeDialogEx(params)

    if r and okclickfunction then
      local r={}
      for i=1,#params.parameters do
        r[i]=params.parameters[i].value
      end

      okclickfunction(nil, r)
    end

  end

end

createMethodInvokedialog=createMethodInvokeDialog --backward compatibility