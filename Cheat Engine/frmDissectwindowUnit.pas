unit frmDissectwindowUnit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,StdCtrls,CEFuncProc, ExtCtrls, LResources, Menus;

type TCETimerhookdata=record
  processed: boolean;
  returnhandle: thandle;
  window: thandle;
  timerid: dword;
  timerproc: dword;
  targetprocess: dword;
end;

type

  { TfrmdissectWindow }

  TfrmdissectWindow = class(TForm)
    TreeView1: TTreeView;
    Button1: TButton;
    Panel1: TPanel;
    Button2: TButton;
    Button3: TButton;
    Button5: TButton;
    Button4: TButton;
    Button6: TButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
  private
    { Private declarations }
    timerhook: thandle;
    cehookdll:thandle;

  public
    { Public declarations }
    CETimerHook:pointer;
    CETIMERHOOKDATA: ^TCETimerhookdata;   
  end;

var
  frmdissectWindow: TfrmdissectWindow;

implementation

uses frmCapturedTimersUnit, ProcessHandlerUnit, Parsers, LazUTF8;

//uses frmCapturedTimersUnit;


resourcestring
  rsInvis = 'Invis';
  rsRemoved = 'Removed';
  rsDissectWindows = 'Dissect Windows';
  rsGiveTheNewTextForThisWindow = 'Give the new text for this window';


procedure TfrmdissectWindow.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  frmdissectWindow:=nil;
end;

procedure TfrmdissectWindow.FormCreate(Sender: TObject);
var title:pchar;
    classname:pchar;
    winhandle,h:thandle;
    winprocess:dword;
    i,err: integer;
 //   processid: dword;


begin
  //fill the treeview with stuff
  //find the windows that have this processid as owner
//  processid:=getcurrentprocessid;
  {$ifdef windows}
  winhandle:=getwindow(getforegroundwindow,GW_HWNDFIRST);

  getmem(title,101);
  getmem(classname,101);

  while winhandle<>0 do
  begin
    GetWindowThreadProcessId(winhandle,addr(winprocess));
    title[0]:=#0;
    classname[0]:=#0;
    getwindowtext(winhandle,title,100);
    GetClassName(winhandle,classname,100);
    classname[100]:=#0;
    title[100]:=#0;

    if winprocess=processid then
      if iswindowvisible(winhandle) then
        treeview1.Items.Add(nil,IntToHex(winhandle,8)+'-'+wincptoutf8(title)+' - ('+wincptoutf8(classname)+')')
      else
        treeview1.Items.Add(nil, IntToHex(winhandle, 8)+'-'+wincptoutf8(title)+' - ('+wincptoutf8(classname)+') ('+rsInvis+')');


    winhandle:=getwindow(winhandle,GW_HWNDNEXT);
  end;

  i:=0;
  while i<treeview1.Items.Count-1 do
  begin
    err:=pos('-',treeview1.items[i].Text);

    try
      h:=StrToQWordEx('$'+copy(treeview1.items[i].Text, 1, err-1));

      winhandle:=getwindow(h,GW_CHILD);

      while winhandle<>0 do
      begin
        GetWindowThreadProcessId(winhandle,addr(winprocess));
        title[0]:=#0;
        classname[0]:=#0;
        getwindowtext(winhandle,title,100);
        GetClassName(winhandle,classname,100);
        classname[100]:=#0;
        title[100]:=#0;

        if iswindowvisible(winhandle) then
          treeview1.Items.Addchild(treeview1.items[i],IntToHex(winhandle,8)+'-'+wincptoutf8(title)+' - ('+wincptoutf8(classname)+')')
        else
          treeview1.Items.Add(nil, IntToHex(winhandle, 8)+'-'+wincptoutf8(title)+' - ('+wincptoutf8(classname)+') ('+rsInvis+')');


        winhandle:=getwindow(winhandle,GW_HWNDNEXT);
      end;


    except
      continue;
    end;
    inc(i);
  end;

  freememandnil(title);
  freememandnil(classname);
  {$endif}
end;

procedure TfrmdissectWindow.Button2Click(Sender: TObject);
var h:Thandle;
    err:integer;
    title,classname: pchar;
begin
  {$ifdef windows}
  //get the handle
  getmem(title,101);
  getmem(classname,101);

  if treeview1.Selected<>nil then
  begin
    err:=pos('-',treeview1.selected.Text);

    try
      h:=StrToQWordEx('$'+copy(treeview1.selected.Text, 1, err-1));

      title[0]:=#0;
      classname[0]:=#0;
      getwindowtext(h,title,100);
      GetClassName(h,classname,100);
      classname[100]:=#0;
      title[100]:=#0;

      if iswindowvisible(h) then
        showwindow(h,sw_hide)
      else
        showwindow(h,sw_show);

      if iswindowvisible(h) then
        treeview1.selected.text:=IntToHex(h,8)+'-'+wincptoutf8(title)+' - ('+wincptoutf8(classname)+')'
      else
        treeview1.selected.text:=IntToHex(h, 8)+'-'+wincptoutf8(title)+' - ('+wincptoutf8(classname)+') ('+rsInvis+')';

    except
    end;
  end;

  freememandnil(classname);
  freememandnil(title);
  {$endif}
end;

procedure TfrmdissectWindow.Button1Click(Sender: TObject);
var w_exstyle,w_style,w_wndproc,w_hinstance,w_hwndParent,w_id,w_USERDATA: dword;
    h:Thandle;
    err:integer;
begin
//get info like title, windowclass structure, etc...
{  if treeview1.Selected<>nil then
  begin
    val('$'+treeview1.Selected.Text,h,err);

    w_exstyle:=GetWindowLong(h,GWL_EXSTYLE);
    w_style:=GetWindowLong(h,GWL_STYLE);
    w_wndproc:=GetWindowLong(h,GWL_WNDPROC);
    w_hinstance:=GetWindowLong(h,GWL_HINSTANCE);
    w_hwndparent:=GetWindowLong(h,GWL_HWNDPARENT);
    w_id:=GetWindowLong(h,GWL_ID);
    w_USERDATA:=GetwindowLong(h,GWL_USERDATA);

    showmessage(
    'EXSTYLE='+IntToHex(w_exstyle,8)+#13#10+
    'STYLE='+IntToHex(w_style,8)+#13#10+
    'WNDPROC='+IntToHex(w_WNDPROC,8)+#13#10+
    'HINSTANCE='+IntToHex(W_hinstance,8)+#13#10+
    'HWNDPARENT='+IntToHex(w_hwndparent,8)+#13#10+
    'ID='+IntToHex(w_id,8)+#13#10+
    'USERDATA='+IntToHex(w_userdata,8)
    );

    frmDissectwindowextra:=TFrmDissectWindowExtra.create(self);
    with frmDissectwindowextra do
    begin
      label1.Caption:='EXSTYLE='+IntToHex(w_exstyle,8);
      label2.Caption:='STYLE='+IntToHex(w_style,8);
      label3.Caption:='WNDPROC='+IntToHex(w_WNDPROC,8);
      label4.Caption:='HINSTANCE='+IntToHex(W_hinstance,8);
      label5.Caption:='HWNDPARENT='+IntToHex(w_hwndparent,8);
      label6.Caption:='ID='+IntToHex(w_id,8);
      label7.Caption:='USERDATA='+IntToHex(w_userdata,8);
      showmodal;
    end;
  end;  }
end;

procedure TfrmdissectWindow.Button4Click(Sender: TObject);
{var
  winhandle,possiblewinhandle: thandle;
  winprocess: dword;
  winthreadid: dword;
  err:integer;
  TimerhookdataMapping:thandle;  }

begin
  raise exception.Create(rsRemoved);
  {
  raise exception.Create('I said Removed dickwad');

  CEHOOKDLL:=LoadLibrary('CEHook.dll');
    if CEHOOKDLL=0 then exit;

    //still here so the dll is loaded
    CETimerHook:=GetProcAddress(CEHOOKDLL,'MyTimerHook');
    if (CETimerHook=nil) then exit;

    TimerhookdataMapping:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,sizeof(tscansettings),'CETIMERHOOKDATA');
    if TimerhookdataMapping=0 then
    begin
      FreeLibrary(CEHOOKDLL);
      exit;

    end;


    CETIMERHOOKDATA:=MapViewOfFile(TimerhookdataMapping,FILE_MAP_ALL_ACCESS,0,0,0);
    if CETIMERHOOKDATA=nil then
    begin
      closehandle(TimerhookdataMapping);
      FreeLibrary(CEHOOKDLL);
      exit;
    end;

    frmCapturedtimers:=TFrmCapturedTimers.create(self);
    CETIMERHOOKDATA.returnhandle:=frmCapturedTimers.Handle;
    CETIMERHOOKDATA.processed:=true;
    CETIMERHOOKDATA.targetprocess:=processid;

    TimerHook:=setwindowshookex(WH_GETMESSAGE,CETimerHook,CEHOOKDLL,0);
    sendmessage(windowhandle,wm_user+666,dword($87654321),TimerHook);

    frmCapturedTimers.windowhandle:=windowhandle;
    frmCapturedTimers.showmodal;

    sendmessage(windowhandle,wm_user+666,$12345678,0);
    UnhookWindowsHookEx(TimerHook);
    FreeLibrary(CEHOOKDLL);
    UnmapViewOfFile(CETIMERHOOKDATA);
    closehandle(TimerhookdataMapping);
    }
end;

procedure TfrmdissectWindow.Button3Click(Sender: TObject);
var h: thandle;
    err,i: integer;
begin
  {$ifdef windows}
  if treeview1.Selected<>nil then
  begin
    err:=pos('-',treeview1.selected.Text);
    try
      h:=StrToQWordEx('$'+copy(treeview1.selected.Text, 1, err-1));
      closewindow(h);
    except

    end;
  end;
  {$endif}
end;

procedure TfrmdissectWindow.Button6Click(Sender: TObject);

    {$ifdef windows}
var
    oldname:pchar;
    h:hwnd;
    err: integer;
    name:string;
    title,classname: pchar;
    {$endif}

begin
  {$ifdef windows}
  if treeview1.Selected=nil then exit;

  err:=pos('-',treeview1.selected.Text);
  try
    h:=StrToQWordEx('$'+copy(treeview1.selected.Text, 1, err-1));



    getmem(oldname,255);
    try
      GetWindowText(h,oldname,254);
      oldname[254]:=#0; //make sure
      name:=oldname;

      if inputquery(rsDissectWindows, rsGiveTheNewTextForThisWindow, name) then
      begin
        SetWindowText(h,pchar(name));

        getmem(title,101);
        getmem(classname,101);
        try
          title[0]:=#0;
          classname[0]:=#0;
          getwindowtext(h,title,100);
          GetClassName(h,classname,100);
          classname[100]:=#0;
          title[100]:=#0;

          if iswindowvisible(h) then
            treeview1.selected.text:=IntToHex(h,8)+'-'+wincptoutf8(title)+' - ('+wincptoutf8(classname)+')'
          else
            treeview1.selected.text:=IntToHex(h, 8)+'-'+wincptoutf8(title)+' - ('+wincptoutf8(classname)+') ('+rsInvis+')';
        finally
          freememandnil(title);
          freememandnil(classname);
        end;
      end;

    finally
      freememandnil(oldname);
    end;
  except

  end;
  {$endif}
end;


initialization
  {$i frmDissectwindowUnit.lrs}

end.
