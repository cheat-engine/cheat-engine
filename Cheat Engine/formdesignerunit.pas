unit formdesignerunit;

{$mode DELPHI}


interface

uses
  LCLIntf, LCLStrConsts,strutils, Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, Menus, JvDesignSurface,
  JvDesignImp, JvDesignUtils, typinfo, PropEdits, ObjectInspector, LResources,
  maps, ExtDlgs, PopupNotifier, IDEDialogs, ceguicomponents, LMessages, luacaller,
  luahandler, cefuncproc, ListViewPropEdit, TreeViewPropEdit, AnchorEditor,
  LCLType, GraphicPropEdit, GraphPropEdits, registry, math;





type

  { TFormDesigner }

  TFormDesigner = Class(TForm)
    FindDialog: TToolButton;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    miAnchorEditor: TMenuItem;
    miMenuSep: TMenuItem;
    miMenuMoveUp: TMenuItem;
    miMenuMoveDown: TMenuItem;
    miAddSubMenu: TMenuItem;
    miSetupMainMenu: TMenuItem;
    miAddTab: TMenuItem;
    miAddItems: TMenuItem;
    miDelete: TMenuItem;
    miSave: TMenuItem;
    miSaveLFM: TMenuItem;
    miLoad: TMenuItem;
    miLoadLFM: TMenuItem;
    miBringToFront: TMenuItem;
    miSendToBack: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    controlPopup: TPopupMenu;
    SaveDialog1: TSaveDialog;
    ToolBar1: TToolBar;
    CEButton: TToolButton;
    CELabel: TToolButton;
    NoSelection: TToolButton;
    CEPanel: TToolButton;
    CEMemo: TToolButton;
    CEEdit: TToolButton;
    CEToggleBox: TToolButton;
    CECheckbox: TToolButton;
    CEGroupBox: TToolButton;
    CERadioGroup: TToolButton;
    CEListBox: TToolButton;
    CEComboBox: TToolButton;
    ToolButton1: TToolButton;
    CETimer: TToolButton;
    CEOpenDialog: TToolButton;
    CESavedialog: TToolButton;
    CEProgressbar: TToolButton;
    CETrackBar: TToolButton;
    CEListView: TToolButton;
    CESplitter: TToolButton;
    PaintBox: TToolButton;
    CETreeview: TToolButton;
    CEPageControl: TToolButton;
    MainMenu: TToolButton;
    PopupMenu: TToolButton;
    Calendar: TToolButton;
    SelectDirectoryDialog: TToolButton;
    RadioButton: TToolButton;
    ScrollBox: TToolButton;
    ToolButton6: TToolButton;
    CEImage: TToolButton;
    procedure controlPopupPopup(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure foundlist3Data(Sender: TObject; Item: TListItem);
    procedure miAddItemsClick(Sender: TObject);
    procedure miAddSubMenuClick(Sender: TObject);
    procedure miAddTabClick(Sender: TObject);
    procedure miAnchorEditorClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miLoadClick(Sender: TObject);
    procedure miLoadLFMClick(Sender: TObject);
    procedure miMenuMoveDownClick(Sender: TObject);
    procedure miMenuMoveUpClick(Sender: TObject);
    procedure miSaveClick(Sender: TObject);
    procedure miSaveLFMClick(Sender: TObject);
    procedure miBringToFrontClick(Sender: TObject);
    procedure miSendToBackClick(Sender: TObject);
    procedure miSetupMainMenuClick(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure TBClick(Sender: TObject);
    procedure CELabelClick(Sender: TObject);
    procedure NoSelectionClick(Sender: TObject);
  private
    { private declarations }

    componentToAdd: string;

     saved: tmemorystream;


    SurfaceList: Tlist;

    fOnClose2: TCloseEvent;
    loadedfromsave: boolean;

    methodlist: tstringlist;
    lastupdate: uint64;

    ComponentTreeWindowProc: TWndMethod;

    //anchorEditor: TAnchorDesigner;

    procedure UpdateMethodListIfNeeded;

    procedure OIDDestroy(sender: Tobject);
    function MethodExists(const Name: String; TypeData: PTypeData; var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean;
    function CompatibleMethodExists(const Name: String; InstProp: PInstProp; var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean;

    procedure OnComponentRenamed(AComponent: TComponent);
    procedure onRefreshPropertyValues;
    procedure setFormName;
    procedure mousedownhack(var TheMessage: TLMessage);
  public
    { public declarations }
    oid:TObjectInspectorDlg;
    procedure DesignerGetAddClass(Sender: TObject; var ioClass: string);
    procedure DesignerSelectionChange(sender: tobject);
    procedure DesignerChange(sender: TObject);
    procedure ObjectInspectorSelectionChange(sender: tobject);

    procedure surfaceOnChange(sender: tobject);


    function IDESelectDirectory(const Title, InitialDir: string): string;
    procedure InitIDEFileDialog (AFileDialog: TFileDialog);
    function IDEMessageDialog(const aCaption, aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; const HelpKeyword: string = ''): Integer;
    function IDEQuestionDialog(const aCaption, aMsg: string; DlgType: TMsgDlgType; Buttons: array of const; const HelpKeyword: string = ''): Integer;
    procedure Modified(Sender: TObject);


    procedure DeletePersistent(var APersistent: TPersistent);

    procedure oidOnDelete(sender: TObject);
    procedure oidComponentTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    function GetDesignerForm(APersistent: TPersistent): TCustomForm;


    procedure onRenameMethod(const CurName, NewName: String);
    procedure onShowMethod(const Name: String);
    function onCreateMethod(const Name: ShortString; ATypeInfo: PTypeInfo; APersistent: TPersistent; const APropertyPath: string): TMethod;
    function ogm(const Method: TMethod; CheckOwner: TObject): String;
    procedure OnGetMethods(TypeData: PTypeData; Proc: TGetStrProc);
    procedure OnGetCompatibleMethods(InstProp: PInstProp; const Proc: TGetStrProc);

    procedure OnWriteMethod(Writer: TWriter; Instance: TPersistent; PropInfo: PPropInfo; const MethodValue, DefMethodValue: TMethod; var Handled: boolean);


    procedure ofm(Reader: TReader; const MethodName: string; var Address: Pointer; var Error: Boolean);


    procedure SAD(sender: tobject);
    procedure designForm(f: tceform);
    procedure CheckBoxForbooleanClick(sender: tobject);
    property OnClose2: TCloseEvent read fOnClose2 write fOnClose2;
  end; 

var
  FormDesigner: TFormDesigner;
  globalcounter: integer;

implementation

{$R *.lfm}

{ TFormDesigner }


uses mainunit;

resourcestring
  rsInvalidObject = '{Invalid object}';
  rsFormDesignerCaption = 'Form Designer';
  rsFormFilesFrmFRM = 'Form files(*.frm)|*.FRM';
  rsFormFilesLfmLFM = 'Form files(*.lfm)|*.LFM';
  rsShowCheckboxesForBoolean = 'Show checkboxes for boolean';


procedure TFormDesigner.setFormName;
begin
  if (GlobalDesignHook.LookupRoot<>nil) and (GlobalDesignHook.LookupRoot is TComponent) then
    caption:=rsFormDesignerCaption+':'+TComponent(GlobalDesignHook.LookupRoot).name;
end;

procedure TFormDesigner.foundlist3Data(Sender: TObject; Item: TListItem);
begin
  item.caption:=inttostr(item.index);
  item.SubItems.Add(inttostr(globalcounter*(1+item.index)));
end;

procedure TFormDesigner.miAddItemsClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to oid.PropertyGrid.RowCount-1 do
    if oid.PropertyGrid.Rows[i].Name='Items' then
    begin
      oid.PropertyGrid.Rows[i].Editor.Edit;
      exit;
    end;

end;

procedure TFormDesigner.miAddSubMenuClick(Sender: TObject);
var m: TMenuItem;

  mi: TMenuitem;
begin
  m:=TMenuItem(oid.selection[0]);

  mi:=TMenuItem.Create(TCEForm(GlobalDesignHook.LookupRoot));
  mi.name:=DesignUniqueName(m, 'TMenuItem');

  m.Add(mi);

  TCEForm(GlobalDesignHook.LookupRoot).designsurface.Change;
end;

procedure TFormDesigner.miAddTabClick(Sender: TObject);
var ts: TTabSheet;
  pc: TCEPageControl;

var b: tbutton;
begin
  pc:=TCEPageControl(controlpopup.PopupComponent);


  ts:=TTabSheet.Create(TCEForm(GlobalDesignHook.LookupRoot));
  //ts:=TTabSheet.Create(pc);
  ts.PageControl:=pc;
  ts.name:=DesignUniqueName(pc, 'TTabSheet');



  TCEForm(GlobalDesignHook.LookupRoot).designsurface.Messenger.DesignComponent(ts,true);
  TCEForm(GlobalDesignHook.LookupRoot).designsurface.Change;
end;

procedure TFormDesigner.miAnchorEditorClick(Sender: TObject);
begin
  if AnchorDesigner=nil then
  begin
    AnchorDesigner:=TAnchorDesigner.Create(self);
    AnchorDesigner.show;
  end
  else
    AnchorDesigner.Show;
end;

procedure TFormDesigner.miDeleteClick(Sender: TObject);
begin
  if GlobalDesignHook.LookupRoot is TCEForm then
    TCEForm(GlobalDesignHook.LookupRoot).designsurface.DeleteComponents;

end;

procedure TFormDesigner.miLoadClick(Sender: TObject);
var f: TCeform;
begin
  OpenDialog1.DefaultExt := '.FRM';
  OpenDialog1.Filter := rsFormFilesFrmFRM;
  if (GlobalDesignHook.LookupRoot<>nil) and (GlobalDesignHook.LookupRoot is TCEForm) and (OpenDialog1.Execute) then
  begin
    f:=TCEForm(GlobalDesignHook.LookupRoot);

    f.LoadFromFile(UTF8ToAnsi(OpenDialog1.filename));
    setFormName;
  end;
end;

procedure TFormDesigner.miLoadLFMClick(Sender: TObject);
var f: TCeform;
begin
  OpenDialog1.DefaultExt := '.LFM';
  OpenDialog1.Filter := rsFormFilesLfmLFM;
  if (GlobalDesignHook.LookupRoot<>nil) and (GlobalDesignHook.LookupRoot is TCEForm) and (OpenDialog1.Execute) then
  begin
    f:=TCEForm(GlobalDesignHook.LookupRoot);

    f.LoadFromFileLFM(UTF8ToAnsi(OpenDialog1.filename));
    setFormName;
  end;
end;

procedure TFormDesigner.miMenuMoveDownClick(Sender: TObject);
var
  mi: TMenuItem;
  p: TMenuItem;
  i: integer;
begin
  mi:=TMenuItem(oid.selection[0]);

  p:=mi.parent;

  if p<>nil then
  begin
    i:=p.IndexOf(mi);

    if i+1<p.Count then
    begin
      //swap
      p.Delete(i);
      p.Insert(i+1, mi);

      TCEForm(GlobalDesignHook.LookupRoot).designsurface.Change;
      oid.ComponentTree.RebuildComponentNodes;
    end;

  end;
end;

procedure TFormDesigner.miMenuMoveUpClick(Sender: TObject);
var
  mi: TMenuItem;
  p: TMenuItem;
  i: integer;
begin
  mi:=TMenuItem(oid.selection[0]);

  p:=mi.parent;

  if p<>nil then
  begin
    i:=p.IndexOf(mi);
    if i>0 then
    begin
      //swap
      p.Delete(i);
      p.Insert(i-1, mi);

      TCEForm(GlobalDesignHook.LookupRoot).designsurface.Change;
      oid.ComponentTree.RebuildComponentNodes;
    end;

  end;
end;

procedure TFormDesigner.miSaveClick(Sender: TObject);
var f: TCeform;
begin
  SaveDialog1.DefaultExt := '.FRM';
  SaveDialog1.Filter := rsFormFilesFrmFRM;
  if (GlobalDesignHook.LookupRoot<>nil) and (GlobalDesignHook.LookupRoot is TCEForm) and (SaveDialog1.Execute) then
  begin
    f:=TCEForm(GlobalDesignHook.LookupRoot);

    f.SaveToFile(Utf8ToAnsi(Savedialog1.filename));
  end;
end;

procedure TFormDesigner.miSaveLFMClick(Sender: TObject);
var f: TCeform;
begin
  SaveDialog1.DefaultExt := '.LFM';
  SaveDialog1.Filter := rsFormFilesLfmLFM;
  if (GlobalDesignHook.LookupRoot<>nil) and (GlobalDesignHook.LookupRoot is TCEForm) and (SaveDialog1.Execute) then
  begin
    f:=TCEForm(GlobalDesignHook.LookupRoot);

    f.SaveToFileLFM(Utf8ToAnsi(Savedialog1.filename));
  end;
end;

procedure TFormDesigner.miBringToFrontClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to oid.Selection.Count-1 do
  begin
    if (oid.Selection.Items[i] is TControl) then
      tcontrol(oid.Selection.Items[i]).BringToFront;
  end;
end;

procedure TFormDesigner.miSendToBackClick(Sender: TObject);
var i: integer;
begin
  for i:=0 to oid.Selection.Count-1 do
  begin
    if (oid.Selection.Items[i] is TControl) then
      tcontrol(oid.Selection.Items[i]).SendToBack;
  end;
end;

procedure TFormDesigner.miSetupMainMenuClick(Sender: TObject);
var m: TMainMenu;

  mi: TMenuitem;
begin
  m:=TMainMenu(oid.selection[0]);

  mi:=TMenuItem.Create(TCEForm(GlobalDesignHook.LookupRoot));
  mi.name:=DesignUniqueName(m, 'TMenuItem');

  m.Items.Add(mi);

  TCEForm(GlobalDesignHook.LookupRoot).designsurface.Change;
end;

procedure TFormDesigner.PopupMenu1Popup(Sender: TObject);
begin
  miSetupMainMenu.visible:=(oid.Selection.Count>0) and ((oid.selection[0] is TMainMenu) or (oid.selection[0] is TPopupMenu));
  miAddSubMenu.visible:=(oid.Selection.Count>0) and (oid.selection[0] is TMenuItem);

  miBringToFront.visible:=(oid.Selection.Count>0) and (oid.selection[0] is TControl);
  miSendToBack.visible:=miBringToFront.visible;

  miMenuSep.visible:=miAddSubMenu.visible;
  miMenuMoveUp.visible:=miMenuSep.visible;
  miMenuMoveDown.visible:=miMenuSep.visible;
end;



procedure TFormDesigner.TBClick(Sender: TObject);
begin
  //give it the name of the clicked component
  componentToAdd:='T'+(sender as TToolbutton).name;
end;

procedure TFormDesigner.CELabelClick(Sender: TObject);
begin
end;

procedure TFormDesigner.NoSelectionClick(Sender: TObject);
begin
  componentToAdd:='';
end;



function TFormDesigner.IDESelectDirectory(const Title, InitialDir: string): string;
begin
  result:='';
end;

procedure TFormDesigner.InitIDEFileDialog (AFileDialog: TFileDialog);
begin

end;

function TFormDesigner.IDEMessageDialog(const aCaption, aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; const HelpKeyword: string = ''): Integer;
begin
  result:=messagedlg(acaption, amsg, dlgtype, buttons, HelpKeyword);
end;

function TFormDesigner.IDEQuestionDialog(const aCaption, aMsg: string; DlgType: TMsgDlgType; Buttons: array of const; const HelpKeyword: string = ''): Integer;
begin
  result:=messagedlg(acaption, amsg, dlgtype, [mbok], HelpKeyword);
end;


procedure TFormDesigner.modified(Sender: TObject);
begin
  if (GlobalDesignHook.LookupRoot<>nil) and
     (TCEform(GlobalDesignHook.LookupRoot).designsurface<>nil) then
     TCEform(GlobalDesignHook.LookupRoot).designsurface.UpdateDesigner;
end;

procedure TFormDesigner.DeletePersistent(var APersistent: TPersistent);
begin

end;

procedure TFormDesigner.oidOnDelete(sender: TObject);
var ol: array of tobject;
  i: integer;
begin
 { if (GlobalDesignHook.LookupRoot<>nil) and
     (TCEform(GlobalDesignHook.LookupRoot).designsurface<>nil) then
     begin


       TCEform(GlobalDesignHook.LookupRoot).designsurface.DeleteComponents;

     end;   }

  setlength(ol, oid.Selection.Count);
  for i:=0 to length(ol)-1 do
    ol[i]:=oid.selection[i];

  for i:=0 to length(ol)-1 do
  begin
    if not (ol[i] is TCustomForm) then
      ol[i].free;
  end;

  TCEform(GlobalDesignHook.LookupRoot).designsurface.ClearSelection;
  TCEform(GlobalDesignHook.LookupRoot).designsurface.UpdateDesigner;


  oid.ComponentTree.RebuildComponentNodes;

end;

procedure TFormDesigner.oidComponentTreeKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key=vk_delete then
    oidOnDelete(nil);
end;

function TFormDesigner.GetDesignerForm(APersistent: TPersistent): TCustomForm;
begin
  result:=nil;

  if (GlobalDesignHook.LookupRoot<>nil) then
    result:=TCustomForm(GlobalDesignHook.LookupRoot);
end;

function TFormDesigner.MethodExists(const Name: String; TypeData: PTypeData; var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean;
begin
  {
  Just say it exists. If it doesn't now, it might exist later on
  }
  MethodIsCompatible:=true;
  MethodIsPublished:=true;
  IdentIsMethod:=true;
  result:=true;
end;

function TFormDesigner.CompatibleMethodExists(const Name: String; InstProp: PInstProp; var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean;
begin
  result:=MethodExists(name, nil, MethodIsCompatible, MethodIsPublished, IdentIsMethod);
end;

procedure TFormDesigner.OnComponentRenamed(AComponent: TComponent);
begin
  if (AComponent is TCustomForm) then
  begin
    Lua_RegisterObject(AComponent.name, AComponent);
    setFormName;
  end
  else
    Lua_RegisterObject(TCustomForm(GlobalDesignHook.LookupRoot).name +'_'+AComponent.Name, AComponent);
end;


procedure TFormDesigner.onRefreshPropertyValues;
begin
  //refresh
 // showmessage('weee');
end;

procedure TFormDesigner.FormCreate(Sender: TObject);
var h: TPropertyEditorHook;
  gc: TOICustomPropertyGrid;
  i: integer;
  r: TOIPropertyGridRow;
  x: array of integer;
begin
  OnGetDesignerForm:=GetDesignerForm;
  LazIDESelectDirectory:=IDESelectDirectory;
  idedialogs.InitIDEFileDialog:=self.InitIDEFileDialog;
  idedialogs.StoreIDEFileDialog:=self.InitIDEFileDialog;
  idedialogs.IDEMessageDialog:=self.IDEMessageDialog;
  idedialogs.IDEQuestionDialog:=self.IDEQuestionDialog;

  SurfaceList:=tlist.create;

  GlobalDesignHook:=TPropertyEditorHook.Create;
  GlobalDesignHook.AddHandlerCreateMethod(onCreateMethod);
  GlobalDesignHook.AddHandlerGetMethodName(ogm);
  GlobalDesignHook.AddHandlerGetMethods(onGetMethods);

  //new lazarus version doesn't seem to use GetMethods...
  GlobalDesignHook.AddHandlerGetCompatibleMethods(OnGetCompatibleMethods);


//  GlobalDesignHook.addhandler
  GlobalDesignHook.AddHandlerModified(Modified);
 // GlobalDesignHook.AddHandlerDeletePersistent(DeletePersistent);

  GlobalDesignHook.AddHandlerShowMethod(onShowMethod);
  GlobalDesignHook.AddHandlerRenameMethod(onRenameMethod);

  GlobalDesignHook.AddHandlerMethodExists(MethodExists);
  GlobalDesignHook.AddHandlerCompatibleMethodExists(CompatibleMethodExists);


  GlobalDesignHook.AddHandlerComponentRenamed(OnComponentRenamed);

  GlobalDesignHook.AddHandlerRefreshPropertyValues(onRefreshPropertyValues);

  setlength(x,0);
  loadedfromsave:=loadformposition(self, x);

  methodlist:=tstringlist.create;
  UpdateMethodListIfNeeded;
end;


procedure TFormDesigner.OIDDestroy(sender: Tobject);
begin
  saveformposition(TObjectInspectorDlg(sender),[]);
end;

procedure TFormDesigner.FormDestroy(Sender: TObject);
begin
  saveformposition(self,[]);
  if methodlist<>nil then
    freeandnil(methodlist);
end;

procedure TFormDesigner.FormShow(Sender: TObject);
begin
  self.clientheight:=toolbar1.height;
end;


procedure TFormDesigner.DesignerGetAddClass(Sender: TObject; var ioClass: string);
begin
  ioclass:=componentToAdd;
  componentToAdd:='';
  NoSelection.down:=true;
end;

procedure TFormDesigner.ObjectInspectorSelectionChange(sender: tobject);
//update the selection in the design surface
var s: TPersistentSelectionList;
  i: integer;
  surface: TJvDesignSurface;
begin
  if GlobalDesignHook.LookupRoot<>nil then
  begin
    surface:=TCEform(GlobalDesignHook.LookupRoot).designsurface;

    if surface<>nil then
    begin
      if surface.active then
      begin
        surface.onselectionchange:=nil;
        surface.Selector.ClearSelection;

        s:=oid.Selection;

        for i:=0 to s.Count-1 do
          surface.Selector.AddToSelection(tcontrol(s[i]));

        surface.onselectionchange:=designerSelectionChange;


      end;

    end;
  end;

end;


procedure TFormDesigner.DesignerChange(sender: TObject);
begin
//  showmessage('changed');
end;

procedure TFormDesigner.DesignerSelectionChange(sender: tobject);
var s: TJvDesignObjectArray;
  i: integer;

  dr: trect;
  c: tcontrol;
  it: pinterfacetable;

  surface: TJvDesignSurface;

  sl: TPersistentSelectionList;
begin
  //oid.
  if GlobalDesignHook=nil then exit;

  surface:=TJvDesignSurface(sender);

  if GlobalDesignHook.LookupRoot<>nil then
  begin
    if GlobalDesignHook.LookupRoot<>surface.Container then //deselect the components on the other surface
    begin
      if (TCEform(GlobalDesignHook.LookupRoot).designsurface<>nil) and (TCEform(GlobalDesignHook.LookupRoot).designsurface.Selector<>nil) then
        TCEform(GlobalDesignHook.LookupRoot).designsurface.Selector.ClearSelection;
    end;

  end;


  GlobalDesignHook.LookupRoot:=surface.Container;

  surface.OnSelectionChange:=nil;


 // sl:=TPersistentSelectionList.Create;
  s:=Surface.Selected;
  if oid<>nil then
  begin

    oid.Selection.Clear;
    if length(s)>0 then
    begin
      for i:=0 to length(s)-1 do
      begin
        oid.Selection.Add(TPersistent(s[i]));
       // sl.Add(TPersistent(s[i]));
      end;
    end
    else
      oid.selection.add(GlobalDesignHook.LookupRoot);

    oid.RefreshSelection;
  end;


  oid.RefreshComponentTreeSelection;
  oid.RefreshPropertyValues;

  if AnchorDesigner<>nil then
    GlobalDesignHook.SetSelection(oid.Selection);

  surface.OnSelectionChange:=DesignerSelectionChange;

//  sl.free;

  setFormName;
end;

procedure TFormDesigner.surfaceOnChange(sender: tobject);
begin
  oid.RefreshPropertyValues;

  oid.RefreshComponentTreeSelection;

  if GlobalDesignHook=nil then exit;

  if (GlobalDesignHook.LookupRoot<>nil) and (GlobalDesignHook.LookupRoot is TCEForm) then
    TCEForm(GlobalDesignHook.LookupRoot).ResyncWithLua;



end;

function TFormDesigner.onCreateMethod(const Name: ShortString; ATypeInfo: PTypeInfo; APersistent: TPersistent; const APropertyPath: string): TMethod;
var f: TLuaCaller;
  z: procedure of object;
    td: PTypeData;

  old: TMethod;

  pn: string;
  i: integer;

  NeedsToBeCreated: boolean;
  header: tstringlist;
begin
  f:=TLuaCaller.create;
  f.luaroutine:=name;
  f.owner:=APersistent;

  try
    pn:=APropertyPath;
    i:=pos('.',pn);
    while i>0 do
    begin
      pn:=copy(pn,i+1, length(pn));
      i:=pos('.',pn)
    end;


    old:=GetMethodProp(APersistent, pn);
    if (old.code<>nil) and (tobject(old.Data) is TLuaCaller) then
      TLuaCaller(old.data).free;

  except
    //failed to get the propertyname
  end;

  i:=methodlist.IndexOf(name);
  NeedsToBeCreated:=i=-1;

  header:=tstringlist.create;
  result:=luacaller_getFunctionHeaderAndMethodForType(ATypeInfo, f, name, header);

  if NeedsToBeCreated then
  begin
    mainform.frmLuaTableScript.assemblescreen.Lines.AddStrings(header);
    header.add('');
  end;

  header.free;

  onShowMethod(Name);

end;

procedure TFormDesigner.onShowMethod(const Name: String);
var i: integer;
begin
  UpdateMethodListIfNeeded;

  //check if this method exists
  i:=methodlist.IndexOf(name);
  if i<>-1 then
  begin
    //go there
    mainform.frmLuaTableScript.Show;

    mainform.frmLuaTableScript.assemblescreen.SelStart:=integer(methodlist.Objects[i])+1;
    mainform.frmLuaTableScript.assemblescreen.SelEnd:=integer(methodlist.Objects[i])+1;
    mainform.frmLuaTableScript.assemblescreen.CaretY:=mainform.frmLuaTableScript.assemblescreen.CaretY+1;

    mainform.frmLuaTableScript.assemblescreen.SetFocus;
  end;

end;


procedure TFormDesigner.onRenameMethod(const CurName, NewName: String);
var i: integer;
  c: integer;
  wp: tpoint;
begin
  UpdateMethodListIfNeeded;

  //check if this method exists
  i:=methodlist.IndexOf(name);
  if i<>-1 then
  begin
    c:=integer(methodlist.objects[i]);

    mainform.frmLuaTableScript.assemblescreen.SelStart:=c+2;
    mainform.frmLuaTableScript.assemblescreen.SelEnd:=c+2;

    wp:=mainform.frmLuaTableScript.assemblescreen.NextWordPos;
    mainform.frmLuaTableScript.assemblescreen.CaretXY:=wp;
    mainform.frmLuaTableScript.assemblescreen.SelectWord;
  end;

end;

function TFormDesigner.ogm(const Method: TMethod; CheckOwner: TObject): String;
begin
  if method.code=nil then
    result:=''
  else
  begin
    if tobject(method.data) is TLuaCaller then
      result:=TLuaCaller(method.Data).luaroutine
    else
      result:=rsInvalidObject;
  end;
end;

procedure TFormDesigner.UpdateMethodListIfNeeded;
var s: string;
  i: integer;
  z: pchar;
  sp: TStringSearchOptions;
  sd: TSysCharSet;
  fn: string;
begin
  sd:=WordDelimiters-['_'];

  if lastupdate<MainForm.frmLuaTableScript.assemblescreen.ChangeStamp then
  begin
    lastupdate:=MainForm.frmLuaTableScript.assemblescreen.ChangeStamp;

    //get the list
    methodlist.clear;

    s:=MainForm.frmLuaTableScript.assemblescreen.Text;

    z:=nil;
    sp:=[soDown, soWholeWord];
    z:=pchar(s);
    repeat
      z:=SearchBuf(z, length(z), 0,0, 'function',sp);
      if z<>nil then
      begin
        fn:=ExtractWord(2,z,sd);
        methodlist.AddObject(fn, tobject(z-pchar(s))); //save the name and the character this function starts at
        inc(z,9); //next
      end;

    until z=nil;
  end;
end;

procedure TFormDesigner.OnGetMethods(TypeData: PTypeData; Proc: TGetStrProc);
var i: integer;
begin
  //TypeData.ParamCount

  //get the function list (look for "function","functionname", "("   )
  UpdateMethodListIfNeeded;

  for i:=0 to methodlist.count-1 do
    proc(methodlist[i]);

end;

procedure TFormDesigner.OnGetCompatibleMethods(InstProp: PInstProp; const Proc: TGetStrProc);
begin
 // InstProp.PropInfo.
  OnGetMethods(nil, proc);
end;

procedure TFormDesigner.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if oid<>nil then
    FreeAndNil(oid);

  if AnchorDesigner<>nil then
    FreeAndNil(AnchorDesigner);

  if GlobalDesignHook<>nil then
    FreeAndNil(GlobalDesignHook);

  if assigned(fOnClose2) then
    fOnClose2(sender,CloseAction);

end;

procedure TFormDesigner.controlPopupPopup(Sender: TObject);
begin
  miDelete.visible:=not (controlPopup.PopupComponent is TCustomForm);
  miAnchorEditor.Visible:=miDelete.visible;

  miAddItems.visible:=controlpopup.PopupComponent is TCETreeview;
  miAddTab.visible:=controlpopup.PopupComponent is TCEPageControl;

end;



procedure TFormDesigner.OnWriteMethod(Writer: TWriter; Instance: TPersistent; PropInfo: PPropInfo; const MethodValue, DefMethodValue: TMethod; var Handled: boolean);
begin
  if (MethodValue.data<>nil) and (tobject(MethodValue.data) is TLuaCaller)  then
  begin
    writer.Driver.BeginProperty(propinfo.Name);
    writer.Driver.WriteMethodName(TLuaCaller(MethodValue.data).luaroutine);
    writer.Driver.EndProperty;
  end;
  //Writer.DefineProperty('bla', nil, TWriterProc, true);
   handled:=true;
end;

procedure tformdesigner.ofm(Reader: TReader; const MethodName: string; var Address: Pointer; var Error: Boolean);
begin
 // showmessage('OnFindMethod:'+MethodName);
  address:=nil;
  error:=false;
end;



procedure TFormDesigner.mousedownhack(var TheMessage: TLMessage);
var
  bm: ^TLMRButtonDown;
  n: ttreenode;
begin
  if TheMessage.msg=LM_RBUTTONDOWN then
  begin
    bm:=@TheMessage;

    n:=oid.ComponentTree.GetNodeAt(bm.XPos, bm.YPos);
    if n<>nil then
      oid.ComponentTree.Items.SelectOnlyThis(n);


  end;

  if assigned(ComponentTreeWindowProc) then
    ComponentTreeWindowProc(TheMessage);
end;


procedure TFormDesigner.SAD(sender: tobject);
begin
  if AnchorDesigner=nil then
    AnchorDesigner:=TAnchorDesigner.create(self);

  AnchorDesigner.show;
end;

//{$define OLDLAZARUS11}
procedure TFormDesigner.CheckBoxForbooleanClick(sender: tobject);
var reg: tregistry;
begin

  {
  If you're wondering why this code is giving an error, then update to
  lazarus 1.6 or remove the comments from the above $define line (or add
  OLDLAZARUS11 to your defines)
  }
  {$ifndef OLDLAZARUS11}

  oid.GridControl[oipgpProperties].CheckboxForBoolean:=tmenuitem(sender).Checked;
  oid.RebuildPropertyLists;

  reg:=tregistry.create;
  try
    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine',true) then
      reg.WriteBool('FormDesigner CheckboxForBoolean', oid.GridControl[oipgpProperties].CheckboxForBoolean);
  finally
    reg.free;
  end;
  {$endif}
end;

procedure TFormDesigner.designForm(f: tceform);
var x: array of integer;
  r: trect;

  m: tmethod;

  miChangeCheckboxSetting: TMenuItem;
  reg: Tregistry;
  i: integer;
begin
  GlobalDesignHook.LookupRoot:=f;

  setFormName;

  if oid=nil then //no oid yet
  begin
    oid:=TObjectInspectorDlg.Create(self);
    oid.AutoSize:=false;
    oid.PropertyEditorHook:=GlobalDesignHook; //needs to be created
    oid.ShowFavorites:=false;
    oid.ComponentTree.PopupMenu:=popupmenu1; //nil;


    {$ifndef OLDLAZARUS11}
    reg:=tregistry.create;
    try
      Reg.RootKey := HKEY_CURRENT_USER;
      if Reg.OpenKey('\Software\Cheat Engine',false) then
      begin
        if reg.ValueExists('FormDesigner CheckboxForBoolean') then
          oid.GridControl[oipgpProperties].CheckboxForBoolean:=reg.ReadBool('FormDesigner CheckboxForBoolean')
        else
          oid.GridControl[oipgpProperties].CheckboxForBoolean:=true;
      end;
    finally
      reg.free;
    end;

    miChangeCheckboxSetting:=tmenuitem.create(oid.MainPopupMenu);
    miChangeCheckboxSetting.caption:=rsShowCheckboxesForBoolean;
    miChangeCheckboxSetting.checked:=oid.GridControl[oipgpProperties].CheckboxForBoolean;
    miChangeCheckboxSetting.OnClick:=CheckBoxForbooleanClick;
    miChangeCheckboxSetting.AutoCheck:=true;

    oid.MainPopupMenu.Items.Add(miChangeCheckboxSetting);
    {$endif}

//    AnchorDesigner:=TAnchorDesigner.Create(oid);


    ShowAnchorDesigner:=SAD; //panda       (I wanted to call it ShowAnchorDesigner but that was causing 'issues')


    ComponentTreeWindowProc:=oid.ComponentTree.WindowProc;


    oid.ComponentTree.WindowProc:=mousedownhack;

    oid.OnSelectPersistentsInOI:=ObjectInspectorSelectionChange;

    oid.DeletePopupmenuItem.OnClick:=oidOnDelete;
    oid.ComponentTree.OnKeyDown:=oidComponentTreeKeyDown;

    oid.Selection.Add(f);

    setlength(x,0);
    if not loadformposition(oid, x) then
    begin
      oid.left:=0;
      oid.top:=0;
    end;

    oid.show;
    {
    oipgpProperties,
    oipgpEvents,
    oipgpFavorite,
    oipgpRestricted
    }



    oid.DefaultItemHeight:=max(oid.DefaultItemHeight, oid.Canvas.TextHeight('QFDZj')+2); //make sure the itemheight fits the current dpi
    oid.OnDestroy:=OIDDestroy;


    if not loadedfromsave then
    begin
      //first time show or the user isn't saving form positions
      LCLIntf.GetWindowRect(oid.handle, r);
      left:=r.Right+5;
      top:=0;
    end;
  end;


  f.active:=true;

  f.designsurface.PopupMenu:=controlPopup;
  f.show;
end;



initialization
{$i formdesignerunit.lrs}

{$R designerimages.res}

end.

