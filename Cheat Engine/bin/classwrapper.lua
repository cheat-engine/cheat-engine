--Include(require) this lua script and you'll be able to 


--------------OBJECT--------------
Object=class(function(a,ceobject)
  a.o=ceobject
end)

function Object:GetClassName()
  return object_getClassName(self.o)
end;

function Object:Destroy()
         object_destroy(self.o)
end


--------------COMPONENT--------------
Component=class(Object, function(c, ceobject)
  Object.init(c, ceobject)
end)

function Component:GetComponentCount()
  return component_getComponentcount(self.o)
end

function Component:GetComponent(index)
  return component_getComponent(self.o, index)
end

function Component:GetName()
  return component_getName(self.o)
end

function Component:SetName(name)
         component_setName(self.o, name)
end

function Component:GetTag()
  return component_getTag(self.o)
end

function Component:SetTag(tag)
         component_setTag(self.o, tag)
end

function Component:GetOwner()
  return component_getOwner(self.o)
end

--------------CONTROL--------------
Control=class(Component, function(c, ceobject)
  Component.init(c, ceobject)
end)

function Control:SetCaption(caption)
         control_setCaption(self.o, caption)
end

function Control:GetCaption()
  return control_getCaption(self.o)
end

function Control:SetPosition(x,y)
         control_setPosition(self.o, x,y)
end

function Control:GetPosition()
  return control_getPosition(self.o)
end

function Control:SetSize(x,y)
         control_setPosition(self.o, x,y)
end

function Control:GetSize()
  return control_getPosition(self.o)
end

function Control:SetAlign(align)
         control_setAlign(self.o, align)
end

function Control:GetAlign()
  return control_getAlign(self.o)
end

function Control:SetEnabled(state)
         control_setEnabled(self.o, state)
end

function Control:GetEnabled()
  return control_getEnabled(self.o)
end


function Control:SetVisible(state)
  control_setVisible(self.o, state)
end

function Control:GetVisible()
  return control_getVisible(self.o)
end

function Control:SetColor(c)
         control_setColor(self.o, c)
end

function Control:GetColor()
  return control_getColor(self.o)
end

function Control:SetParent(control)
         control_setParent(self.o, control.o)
end

function Control:GetParent()
  return WinControl(control_getParent(self.o))
end

function Control:OnClick(f)
  control_onClick(self.o, f)
end

--------------WINCONTROL--------------
WinControl=class(Control, function(c,ceobject)
  Control.init(c,ceobject)
end)

function WinControl:GetControlCount()
  return wincontrol_getControlCount(self.o)
end

function WinControl:GetControl(index)
  return Control(wincontrol_getControl(self.o, index))
end

function WinControl:CanFocus()
  return wincontrol_canFocus(self.o)
end

function WinControl:Focused()
  return wincontrol_focused(self.o)
end

function WinControl:SetFocus()
         wincontrol_setFocus(self.o)
end

function WinControl:OnEnter(f)
  return wincontrol_onEnter(self.o,f)
end

function WinControl:OnExit(f)
  return wincontrol_onExit(self.o,f)
end

--------------STRINGS--------------
Strings=class(Object, function(c,ceobject)
  Object.init(c,ceobject)
end)

function Strings:Clear()
         strings_clear(self.o)
end

function Strings:GetCount()
  return strings_getCount(self.o)
end

function Strings:Count()
  return strings_getCount(self.o)
end



function Strings:Add(string)
         strings_add(self.o, string)
end

function Strings:Delete(index)
         strings_delete(self.o, index)
end

function Strings:Remove(string)
         strings_remove(self.o, string)
end

function Strings:GetText()
  return strings_getText(self.o)
end

function Strings:IndexOf(string)
  return strings_indexOf(self.o, string)
end

function Strings:Insert(index, string)
         strings_insert(self.o, index, string)
end

function Strings:LoadFromFrile(filename)
         strings_loadFromFile(self.o, filename)
end

function Strings:SaveToFile(filename)
         strings_saveToFile(self.o, filename)
end

function Strings:GetString(index)
  return strings_getString(self.o, index)
end

function Strings:SetString(index, string)
  return strings_setString(self.o, index)
end

--------------Stringlist--------------
StringList=class(Strings, function(c,ceobject)
  Strings.init(c,ceobject)
end)

function StringList:GetDuplicates()
  return StringList_getDuplicates(self.o)
end

function StringList:SetDuplicates(d)
         StringList_setDuplicates(self.o,d)
end

function StringList:GetSorted()
  return StringList_getDuplicates(self.o)
end

function StringList:SetSorted(d)
         StringList_setSorted(self.o,d)
end

function StringList:GetCaseSensitive()
  return StringList_getCaseSensitive(self.o)
end

function StringList:SetCaseSensitive(d)
         StringList_setCaseSensitive(self.o,d)
end


--------------CUSTOMCONTROL--------------
CustomControl=class(WinControl, function(c,ceobject)
  WinControl.init(c,ceobject)
end)

--no functions exported for now

--------------SCROLLINGWINCONTROL--------------
ScrollingWinControl=class(CustomControl, function(c,ceobject)
  CustomControl.init(c,ceobject)
end)

--no functions exported for now


--------------FORM--------------
Form=class(ScrollingWinControl, function(c,ceobject)
  ScrollingWinControl.init(c,ceobject)
end)

function Form:CenterScreen()
         form_centerScreen(self.o)
end

function Form:Hide()
         form_hide(self.o)
end

function Form:Show()
         form_show(self.o)
end

function Form:ShowModal()
  return form_showModal(self.o)
end

function Form:IsForegroundWindow()
  return form_isForegroundWindow(self.o)
end

function Form:OnClose(f)
  return form_onClose(self.o, f)
end

--------------GRAPHICCONTROL--------------
GraphicControl=class(Control, function(c,ceobject)
  Control.init(c,ceobject)
end)

--------------LABEL--------------
Label=class(GraphicControl, function(c,ceobject)
  GraphicControl.init(c,ceobject)
end)

--------------SPLITTER--------------
Splitter=class(CustomControl, function(c,ceobject)
  CustomControl.init(c,ceobject)
end)

--------------PANEL--------------
Panel=class(CustomControl, function(c,ceobject)
  CustomControl.init(c,ceobject)
end)

function Panel:GetAlignment()
  return panel_getAlignment(self.o)
end

function Panel:SetAlignment(a)
         panel_setAlignment(self.o,a)
end

function Panel:GetBevelInner()
  return panel_getBevelInner(self.o)
end

function Panel:SetBevelInner(b)
         panel_setBevelInner(self.o, b)
end

function Panel:GetBevelOuter()
  return panel_getBevelOuter(self.o)
end

function Panel:SetBevelOuter(b)
         panel_setBevelOuter(self.o, b)
end

function Panel:GetBevelWidth()
  return panel_getBevelWidth(self.o)
end

function Panel:SetBevelWidth(w)
         panel_setBevelWidth(self.o, w)
end

function Panel:GetFullRepaint()
  return panel_getFullRepaint(self.o)
end

function Panel:SetFullRepaint(b)
         panel_setFullRepaint(self.o, b)
end

--------------IMAGE--------------
Image=class(GraphicControl, function(c,ceobject)
  GraphicControl.init(c,ceobject)
end)

function Image:LoadImageFromFile(fn)
         image_loadImageFromFile(self.o, fn)
end

function Image:Stretch(b)
         image_stretch(self.o, b)
end

function Image:Transparent(b)
         image_transparent(self.o, b)
end

--------------EDIT--------------
Edit=class(WinControl, function(c,ceobject)
  WinControl.init(c,ceobject)
end)

function Edit:Clear()
         edit_clear(self.o)
end

function Edit:SelectAll()
         edit_selectAll(self.o)
end

function Edit:ClearSelection()
         edit_clearSelection(self.o)
end

function Edit:CopyToClipboard()
         edit_copyToClipboard(self.o)
end

function Edit:CutToClipboard()
         edit_cutToClipboard(self.o)
end

function Edit:PasteFromClipboard()
         edit_pasteFromClipboard(self.o)
end

function Edit:OnChange(f)
         edit_onChange(self.o,f)
end

--------------MEMO--------------
Memo=class(Edit, function(c,ceobject)
  Memo.init(c,ceobject)
end)

function Memo:Append(s)
         memo_append(self.o, s)
end

function Memo:GetLines()
  return memo_getLines(self.o)
end

function Memo:GetWordWrap()
  return memo_getWordWrap(self.o)
end

function Memo:SetWordWrap(b)
         memo_setWordWrap(self.o, b)
end

function Memo:GetWantTabs()
  return memo_getWantTabs(self.o)
end

function Memo:SetWantTabs(b)
         memo_setWantTabs(self.o, b)
end

function Memo:GetWantReturns()
  return memo_getWantReturns(self.o)
end

function Memo:SetWantReturns(b)
         memo_setWantReturns(self.o, b)
end

function Memo:GetScrollbars()
  return memo_getScrollbars(self.o)
end

function Memo:SetScrollbars(b)
         memo_setScrollbars(self.o, b)
end



