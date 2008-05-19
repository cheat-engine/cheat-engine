unit FormsExtra;
{
This unit will contain some extensions to the normal form like a stay on top system menu option
It will override the original TForm class for units that include this to the uses
}

interface
uses windows,classes,forms,messages,dialogs;

const togglefront=WM_USER+$4000;

type
  TForm = class(forms.TForm)
  private
    originalformstyle: Tformstyle;
    fStayOnTop: boolean;
    procedure setSysMenu;
    procedure setStayOnTop(newstate: boolean); 
    procedure WMSysCommand(var Msg: TWMSysCommand) ; message WM_SYSCOMMAND;
  public
    procedure togglestayonfront;
    property StayOnTop: boolean read fStayOnTop write setStayOnTop;
  protected
    SMenu: HMenu;


    procedure DoCreate; override;

  end;

implementation

procedure TForm.setSysMenu;
var param: dword;
begin
  if stayontop then
    param:=MF_STRING or MF_CHECKED
  else
    param:=MF_STRING or MF_UNCHECKED;

  SMenu:=GetSystemMenu(handle,true);
  SMenu:=GetSystemMenu(handle,false);
  AppendMenu(SMenu,MF_SEPARATOR,0,'') ;
  AppendMenu(SMenu, param		, togglefront, 'Stay on top') ;
end;

procedure TForm.DoCreate;

begin
  inherited DoCreate;
  setSysMenu;

end;

procedure TForm.setStayOnTop(newstate: boolean);
var mii : TMenuItemInfo;
begin
{
//great, I spend a hour figuring this out to find then out that setting formstyle reserts the sysmenu as well...
  ZeroMemory(@mii,sizeof(mii));
  mii.cbSize := SizeOf(TMenuItemInfo);
  mii.fMask := MIIM_STATE;
  if newState then
    mii.fState := MFS_CHECKED
  else
    mii.fState := MFS_UNCHECKED;

  SetMenuItemInfo(SMenu,togglefront, false, mii);

  }
  fStayOnTop:=newState;
  if fStayOnTop then
  begin
    originalFormStyle:=formstyle;
    formstyle:=fsStayOnTop;
  end else formstyle:=originalFormStyle;

  setSysMenu;
end;

procedure TForm.togglestayonfront;
begin
  StayOntop:=not StayOnTop;
end;


procedure TForm.WMSysCommand(var Msg : TWMSysCommand);
begin
  if msg.CmdType=togglefront then
    togglestayonfront
  else
    inherited;
end;


end.
