unit formdesignerunit;

{$mode DELPHI}

{
Note:
I edited laz_xmlstreaming
I changed TXMLObjectWriter.WriteBinary to
procedure TXMLObjectWriter.WriteBinary(const Buffer; Count: Longint);
var
  s: string;
  hex: pchar;
begin
  getmem(hex, count*2+1);
  BinToHex(pchar(@buffer), hex, count);
  hex[count*2]:=#0;
  s:=hex;
  freemem(hex);

  GetPropertyElement('binary')['value'] := s;
end;

and
procedure TXMLObjectReader.ReadBinary(const DestData: TMemoryStream);
var
  Value: String;
  binarydata: pchar;
  count: integer;
begin
  Value:=FElement['value'];
  if Value<>'' then
  begin
    count:=length(value) div 2;
    getmem(binarydata, count);
    HexToBin(pchar(value), binarydata, count);

    DestData.Write(binarydata[0],count);
    DestData.Position:=0;
  end;

  ReadValue;
  //writeln('TXMLObjectReader.ReadBinary ');
end;

also added the following line to the function TXMLObjectReader.ReadNextValue(Stay: Boolean): TValueType;
else if FElement.NodeName='binary' then
          result:=vaBinary


}

interface

uses
  LCLIntf, LCLStrConsts,strutils, Classes, SysUtils, FileUtil, Forms, Controls, Graphics,
  Dialogs, ComCtrls, StdCtrls, ExtCtrls, Buttons, Menus, JvDesignSurface,
  JvDesignImp, JvDesignUtils, typinfo, PropEdits, ObjectInspector, LResources,
  maps, ExtDlgs, PopupNotifier, IDEDialogs, ceguicomponents, LMessages, luacaller,
  luahandler, cefuncproc;





type

  { TFormDesigner }

  TFormDesigner = Class(TForm)
    ImageList1: TImageList;
    miBringToFront: TMenuItem;
    miSendToBack: TMenuItem;
    PopupMenu1: TPopupMenu;
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
    CEListbox: TToolButton;
    CECombobox: TToolButton;
    ToolButton1: TToolButton;
    CETimer: TToolButton;
    CEOpenDialog: TToolButton;
    CESavedialog: TToolButton;
    CEProgressbar: TToolButton;
    CETrackBar: TToolButton;
    CEListView: TToolButton;
    CESplitter: TToolButton;
    ToolButton6: TToolButton;
    CEImage: TToolButton;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure foundlist3Data(Sender: TObject; Item: TListItem);
    procedure miBringToFrontClick(Sender: TObject);
    procedure miSendToBackClick(Sender: TObject);
    procedure TBClick(Sender: TObject);
    procedure CELabelClick(Sender: TObject);
    procedure NoSelectionClick(Sender: TObject);
  private
    { private declarations }

    componentToAdd: string;


    ps:TPersistentSelectionList;

    testf: tform;
     saved: tmemorystream;


    SurfaceList: Tlist;

    fOnClose2: TCloseEvent;
    loadedfromsave: boolean;

    methodlist: tstringlist;
    lastupdate: uint64;
    procedure UpdateMethodListIfNeeded;

    procedure OIDDestroy(sender: Tobject);
    function MethodExists(const Name: String; TypeData: PTypeData; var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean;
  public
    { public declarations }
    oid:TObjectInspectorDlg;
    procedure DesignerGetAddClass(Sender: TObject; var ioClass: string);
    procedure DesignerSelectionChange(sender: tobject);
    procedure ObjectInspectorSelectionChange(sender: tobject);
    procedure surfaceOnChange(sender: tobject);


    function IDESelectDirectory(const Title, InitialDir: string): string;
    procedure InitIDEFileDialog (AFileDialog: TFileDialog);
    function IDEMessageDialog(const aCaption, aMsg: string; DlgType: TMsgDlgType; Buttons: TMsgDlgButtons; const HelpKeyword: string = ''): Integer;
    function IDEQuestionDialog(const aCaption, aMsg: string; DlgType: TMsgDlgType; Buttons: array of const; const HelpKeyword: string = ''): Integer;
    procedure Modified(Sender: TObject);
    procedure oidOnDelete(sender: TObject);
    function GetDesignerForm(APersistent: TPersistent): TCustomForm;



    function ocm(const Name: ShortString; ATypeInfo: PTypeInfo; APersistent: TPersistent; const APropertyPath: string): TMethod;
    function ogm(const Method: TMethod; CheckOwner: TObject): String;
    procedure OnGetMethods(TypeData: PTypeData; Proc: TGetStrProc);

    procedure OnWriteMethod(Writer: TWriter; Instance: TPersistent; PropInfo: PPropInfo; const MethodValue, DefMethodValue: TMethod; var Handled: boolean);


    procedure ofm(Reader: TReader; const MethodName: string; var Address: Pointer; var Error: Boolean);
    procedure orsp(Sender:TObject; const Instance: TPersistent; PropInfo: PPropInfo; var Content:string);
    procedure occ(Reader: TReader; ComponentClass: TComponentClass; var Component: TComponent);


    procedure designForm(f: tceform);
    property OnClose2: TCloseEvent read fOnClose2 write fOnClose2;
  end; 

var
  FormDesigner: TFormDesigner;
  globalcounter: integer;

implementation

{$R *.lfm}

{ TFormDesigner }


uses mainunit;


procedure TFormDesigner.foundlist3Data(Sender: TObject; Item: TListItem);
begin
  item.caption:=inttostr(item.index);
  item.SubItems.Add(inttostr(globalcounter*(1+item.index)));
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

procedure TFormDesigner.oidOnDelete(sender: TObject);
begin
  if (GlobalDesignHook.LookupRoot<>nil) and
     (TCEform(GlobalDesignHook.LookupRoot).designsurface<>nil) then
     TCEform(GlobalDesignHook.LookupRoot).designsurface.DeleteComponents;

end;

function TFormDesigner.GetDesignerForm(APersistent: TPersistent): TCustomForm;
begin
  result:=nil;

  if (GlobalDesignHook.LookupRoot<>nil) then
    result:=TCustomForm(GlobalDesignHook.LookupRoot);
end;

function TFormDesigner.MethodExists(const Name: String; TypeData: PTypeData; var MethodIsCompatible,MethodIsPublished,IdentIsMethod: boolean):boolean;
begin
  MethodIsCompatible:=true;
  MethodIsPublished:=true;
  IdentIsMethod:=true;
  result:=true;
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
  GlobalDesignHook.AddHandlerCreateMethod(ocm);
  GlobalDesignHook.AddHandlerGetMethodName(ogm);
  GlobalDesignHook.AddHandlerGetMethods(onGetMethods);
  GlobalDesignHook.AddHandlerModified(Modified);


  GlobalDesignHook.AddHandlerMethodExists(MethodExists);

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
  clientheight:=toolbar1.height;
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

procedure TFormDesigner.DesignerSelectionChange(sender: tobject);
var s: TJvDesignObjectArray;
  i: integer;

  dr: trect;
  c: tcontrol;
  it: pinterfacetable;

  surface: TJvDesignSurface;
begin
  //oid.
  if GlobalDesignHook=nil then exit;

  surface:=TJvDesignSurface(sender);


  if GlobalDesignHook.LookupRoot<>nil then
  begin
    if GlobalDesignHook.LookupRoot<>surface.Container then //deselect the components on the other surface
    begin
      if TCEform(GlobalDesignHook.LookupRoot).designsurface<>nil then
        TCEform(GlobalDesignHook.LookupRoot).designsurface.Selector.ClearSelection;
    end;

  end;


  GlobalDesignHook.LookupRoot:=surface.Container;


  surface.OnSelectionChange:=nil;

  s:=Surface.Selected;
  if oid<>nil then
  begin
    oid.Selection.Clear;
    if length(s)>0 then
    begin
      for i:=0 to length(s)-1 do
        oid.Selection.Add(TPersistent(s[i]));
    end
    else
      oid.selection.add(GlobalDesignHook.LookupRoot);



    oid.RefreshSelection;

  end;

  surface.OnSelectionChange:=DesignerSelectionChange;

  oid.RefreshComponentTreeSelection;


  oid.RefreshPropertyValues;

end;

procedure TFormDesigner.surfaceOnChange(sender: tobject);
begin
  oid.RefreshPropertyValues;

  oid.RefreshComponentTreeSelection;
end;






function TFormDesigner.ocm(const Name: ShortString; ATypeInfo: PTypeInfo; APersistent: TPersistent; const APropertyPath: string): TMethod;
var f: TLuaCaller;
  z: procedure of object;
    td: PTypeData;

  old: TMethod;

  pn: string;
  i: integer;
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

  if ATypeInfo.Name ='TNotifyEvent' then
    result:=TMethod(TNotifyEvent(f.NotifyEvent))
  else
  if ATypeInfo.Name ='TCloseEvent' then
    result:=TMethod(TCloseEvent(f.CloseEvent))
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
      result:='{Invalid object}';
  end;
end;

procedure TFormDesigner.UpdateMethodListIfNeeded;
var s: string;
  i: integer;
  z: pchar;
  sp: TStringSearchOptions;
begin
  if lastupdate<MainForm.frmLuaTableScript.assemblescreen.ChangeStamp then
  begin
    lastupdate:=MainForm.frmLuaTableScript.assemblescreen.ChangeStamp;

    //get the list
    methodlist.clear;

    s:=MainForm.frmLuaTableScript.assemblescreen.Text;

    z:=nil;
    sp:=[soDown, soWholeWord];
    z:=SearchBuf(pchar(s), length(s), 0,length(s), 'function', sp);
    if z<>nil then
    begin
      showmessage(z);

    end;

   // s:=lowercase(s);
 {
    repeat
      i:=pos('function',s);
      if i>0 then
      begin

      end;
    until i=0;
   // i:=pos('function',s);
   // while i>0 do
                      }





  end;
end;

procedure TFormDesigner.OnGetMethods(TypeData: PTypeData; Proc: TGetStrProc);
begin
  //TypeData.ParamCount

  //get the function list (look for "function","functionname", "("   )
  UpdateMethodListIfNeeded;
 {
  proc('bla1');
  proc('bla2');
  proc('bla3'); }

end;

procedure TFormDesigner.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if oid<>nil then
  begin
    oid.free;
    oid:=nil;
  end;

  if GlobalDesignHook<>nil then
  begin
    GlobalDesignHook.Free;
    GlobalDesignHook:=nil;
  end;

  if assigned(fOnClose2) then
    fOnClose2(sender,CloseAction);

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
  address:=0;
  error:=false;
end;

procedure TFormDesigner.orsp(Sender: TObject; const Instance: TPersistent;
  PropInfo: PPropInfo; var Content: string);
begin

end;

procedure TFormDesigner.occ(Reader: TReader; ComponentClass: TComponentClass;
  var Component: TComponent);
begin

end;








procedure TFormDesigner.designForm(f: tceform);
var x: array of integer;
  r: trect;
begin
  GlobalDesignHook.LookupRoot:=f;

  if oid=nil then //no oid yet
  begin
    oid:=TObjectInspectorDlg.Create(self);
    oid.AutoSize:=false;
    oid.PropertyEditorHook:=GlobalDesignHook; //needs to be created
    oid.ShowFavorites:=false;
    oid.ComponentTree.PopupMenu:=popupmenu1; //nil;
    oid.OnSelectPersistentsInOI:=ObjectInspectorSelectionChange;

    oid.Selection.Add(f);

    if not loadformposition(oid, x) then
    begin
      oid.left:=0;
      oid.top:=0;
    end;
    oid.show;
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



  f.show;
  f.BringToFront;
end;


initialization
{$i formdesignerunit.lrs}

LazarusResources.Add('laz_cut','','');
LazarusResources.Add('laz_copy','','');
LazarusResources.Add('laz_paste','','');
LazarusResources.Add('laz_add','','');
LazarusResources.Add('laz_delete','','');
LazarusResources.Add('arrow_up','','');
LazarusResources.Add('arrow_down','','');
LazarusResources.Add('delete_selection','','');
LazarusResources.Add('menu_environment_options','','');
LazarusResources.Add('order_move_front','','');
LazarusResources.Add('order_move_back','','');
LazarusResources.Add('order_forward_one','','');
LazarusResources.Add('order_back_one','','');

end.

