local dpiscale=getScreenDPI()/96

function createMethodInvokeDialog(name, parameters, okclickfunction, customAddress, address)
  local midinfo={}
  midinfo.mid=createForm(false)
  midinfo.mid.position='poScreenCenter'
  midinfo.mid.borderStyle='bsSizeable'
  midinfo.mid.Caption=translate('Invoke ')..name
  midinfo.mid.Name='MethodInvokeDialog'

  if customAddress then
    midinfo.lblInstanceAddress=createLabel(midinfo.mid)
    midinfo.lblInstanceAddress.Caption=translate('Instance address')
    midinfo.cbInstance=createComboBox(midinfo.mid)
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


  midinfo.pnlButtons.AnchorSideBottom.Control=midinfo.mid
  midinfo.pnlButtons.AnchorSideBottom.Side=asrBottom
  midinfo.pnlButtons.AnchorSideLeft.Control=midinfo.mid
  midinfo.pnlButtons.AnchorSideLeft.Side=asrCenter
  midinfo.pnlButtons.Anchors='[akLeft, akBottom]'
 -- midinfo.pnlButtons.Color=clRed


  if customAddress then
    midinfo.lblInstanceAddress.AnchorSideTop.Control=midinfo.mid
    midinfo.lblInstanceAddress.AnchorSideTop.Side=asrTop
    midinfo.lblInstanceAddress.AnchorSideTop.Left=midinfo.mid
    midinfo.lblInstanceAddress.AnchorSideTop.Side=asrLeft
    midinfo.cbInstance.AnchorSideTop.Control=midinfo.lblInstanceAddress
    midinfo.cbInstance.AnchorSideTop.Side=asrBottom

    midinfo.cbInstance.AnchorSideLeft.Control=midinfo.mid
    midinfo.cbInstance.AnchorSideLeft.Side=asrLeft
    midinfo.cbInstance.AnchorSideRight.Control=midinfo.mid
    midinfo.cbInstance.AnchorSideRight.Side=asrRight
    midinfo.cbInstance.Anchors='[akLeft, akRight, akTop]'
    midinfo.cbInstance.BorderSpacing.Left=2*dpiscale
    midinfo.cbInstance.BorderSpacing.Right=2*dpiscale

    midinfo.gbParams.AnchorSideTop.Control=midinfo.cbInstance
    midinfo.gbParams.AnchorSideTop.Side=asrBottom
  else
    midinfo.gbParams.AnchorSideTop.Control=midinfo.mid
    midinfo.gbParams.AnchorSideTop.Side=asrTop
  end

  midinfo.gbParams.AnchorSideLeft.Control=midinfo.mid
  midinfo.gbParams.AnchorSideLeft.Side=asrLeft
  midinfo.gbParams.AnchorSideRight.Control=midinfo.mid
  midinfo.gbParams.AnchorSideRight.Side=asrRight
  midinfo.gbParams.AnchorSideBottom.Control=midinfo.pnlButtons
  midinfo.gbParams.AnchorSideBottom.Side=asrTop

  midinfo.gbParams.Anchors='[akLeft, akRight, akTop, akBottom]'

  --midinfo.mid.AutoSize=true

  midinfo.parameters={}
  local i
  for i=1, #parameters do
    local lblVarName=createLabel(midinfo.mid)
    local edtVarText=createEdit(midinfo.mid)

    lblVarName.Parent=midinfo.gbParams
    edtVarText.Parent=midinfo.gbParams

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
    edtVarText.BorderSpacing.Top=2*dpiscale
    edtVarText.AnchorSideRight.Control=midinfo.gbParams
    edtVarText.AnchorSideRight.Side=asrRight
    edtVarText.BorderSpacing.Right=2*dpiscale

    edtVarText.Anchors='[akLeft, akRight, akTop]'



    lblVarName.Caption=parameters[i]:trim()

    midinfo.parameters[i]={}
    midinfo.parameters[i].lblVarName=lblVarName
    midinfo.parameters[i].edtVarText=edtVarText

    lblVarName.BorderSpacing.CellAlignVertical='ccaCenter'
  end

  midinfo.btnOk.OnClick=function(sender) 
    local r={}
    for i=1,#parameters do      
      r[i]=midinfo.parameters[i].edtVarText.Text
    end
    okclickfunction(sender, r) 
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

    if midinfo.cbInstance then
      midinfo.cbInstance.destroy()
      midinfo.cbInstance=nil
    end

    midinfo.gbParams.destroy()
    midinfo.gbParams=nil

    --midinfo.mid.saveFormPosition()

    midinfo=nil
  end

  midinfo.mid.AutoSize=true
  
  return midinfo
end

createMethodInvokedialog=createMethodInvokeDialog --backward compatibility