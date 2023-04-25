unit frmEnumerateDLLsUnit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport, lclproc,
  {$endif}
  {$ifdef windows}
  windows, imagehlp,
  {$endif}

  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs,CEFuncProc, StdCtrls, ComCtrls, ExtCtrls, ActnList,
  Menus, LResources,symbolhandler, symbolhandlerstructs, FindDialogFix,
  commonTypeDefs, strutils, ProcessHandlerUnit, Clipbrd, betterControls;

type tenumthread=class(tthread)
  public
    symbolcount: integer;
    moduletext: string;
    symbolname: array [1..25] of string;
    x: TTreenode;
    procedure AddModule;
    procedure AddSymbol;
    procedure Done;
    procedure execute; override;
end;

type

  { TfrmEnumerateDLLs }

  TfrmEnumerateDLLs = class(TForm)
    CopySymbolName: TAction;
    edImageList: TImageList;
    Label2: TLabel;
    CopySymbolName1: TMenuItem;
    miFindNext: TMenuItem;
    TreeView1: TTreeView;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    FindDialog1: TFindDialog;
    ActionList1: TActionList;
    Find: TAction;
    pmSymbol: TPopupMenu;
    Find1: TMenuItem;
    procedure CopySymbolNameExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FindDialog1Close(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure miFindNextClick(Sender: TObject);
    procedure TreeView1CustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure TreeView1DblClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FindExecute(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
  private
    { Private declarations }
    enumthread: tenumthread;
    findpos: tpoint;

  public
    { Public declarations }

    procedure Enumerate;

  end;

var
  frmEnumerateDLLs: TfrmEnumerateDLLs;

implementation

uses MemoryBrowserFormUnit, Parsers;

resourcestring
  rsNothingFound = 'nothing found';

var canceled: boolean; //global var for only this unit

procedure tenumthread.Done;
begin
  if frmEnumerateDLLs<>nil then
  begin
    frmEnumerateDLLs.button2.visible:=false;
    if x<>nil then frmEnumerateDLLs.treeview1.EndUpdate;
  end
  else canceled:=true;


end;

procedure tenumthread.addsymbol;
var i: integer;
begin

  if frmEnumerateDLLs<>nil then
  begin
    for i:=1 to symbolcount do
      frmEnumerateDLLs.treeview1.items.addchild(x,symbolname[i]);
  end else canceled:=true;

  symbolcount:=0;
end;

procedure tenumthread.AddModule;
begin
  if frmEnumerateDLLs<>nil then
  begin
    if x<>nil then frmEnumerateDLLs.treeview1.EndUpdate;

    x:=frmEnumerateDLLs.treeview1.items.add(nil,moduletext);
    frmEnumerateDLLs.treeview1.BeginUpdate;
  end else canceled:=true;
end;


procedure tenumthread.execute;
var ml: Tstringlist;
  i,j: integer;
  mi: TModuleInfo;
  sl: Tstringlist;
begin
  freeonterminate:=true;
  symbolcount:=0;
  Priority:=tpLower;

 // symhandler.waitforsymbolsloaded;

  if not canceled then
  begin
    sl:=tstringlist.create;
    ml:=tstringlist.create;
    try
      symhandler.getModuleList(ml);
      for i:=0 to ml.count-1 do
      begin
        if symhandler.getmodulebyaddress(ptruint(ml.Objects[i]), mi) then
        begin
          moduletext:=IntToHex(mi.baseaddress,8)+' - '+mi.modulename;
          Synchronize(addmodule);
        end;


        sl.clear;
        symhandler.GetSymbolList(mi.baseaddress, sl);
        for j:=0 to sl.count-1 do
        begin
          inc(symbolcount);
          symbolname[symbolcount]:=IntToHex(ptruint(sl.objects[j]),8)+' - '+sl[j];

          if canceled then break;

          if symbolcount=25 then
            Synchronize(addsymbol);
        end;


        if canceled then break;

        if symbolcount>0 then
          synchronize(addsymbol);
      end;

    finally
      ml.free;
      sl.free;
    end;

  end;

  if symbolcount>0 then
    synchronize(addsymbol);

  synchronize(done);
end;

procedure TfrmEnumerateDLLs.Enumerate;
var crashcount: integer;
begin
  treeview1.items.Clear;

  canceled:=false;
  enumthread:=tenumthread.create(false);

  frmEnumerateDLLs.TreeView1.SortType:=stText;
end;

procedure TfrmEnumerateDLLs.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmEnumerateDLLs.CopySymbolNameExecute(Sender: TObject);
begin
  if Treeview1.Selected<>nil then
    Clipboard.AsText:=TreeView1.Selected.Text;
end;

procedure TfrmEnumerateDLLs.FindDialog1Close(Sender: TObject);
begin
  findpos.x:=finddialog1.left;
  findpos.y:=finddialog1.top;
end;

procedure TfrmEnumerateDLLs.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  canceled:=true;
  action:=cafree;
  frmEnumerateDLLS:=nil;
end;

procedure TfrmEnumerateDLLs.FormCreate(Sender: TObject);
var x: TWindowPosArray;
begin
  LoadFormPosition(self,x);
  if length(x)>=2 then
  begin
    findpos.x:=x[0];
    findpos.y:=x[1];
  end;


  {$ifdef darwin}
  Find.Shortcut:=TextToShortCut('Meta+F');
  CopySymbolName.Shortcut:=TextToShortCut('Meta+C');
  {$endif}
end;

procedure TfrmEnumerateDLLs.FormDestroy(Sender: TObject);
var x: TWindowPosArray;
begin
  setlength(x,2);
  x[0]:=findpos.x;
  x[1]:=findpos.y;
  SaveFormPosition(self,x);
end;

procedure TfrmEnumerateDLLs.FormShow(Sender: TObject);
begin
  button2.autosize:=true;
  button2.height:=canvas.TextHeight(button2.caption)+4;
  treeview1.font.height:=GetFontData(font.Handle).Height;

end;

procedure TfrmEnumerateDLLs.miFindNextClick(Sender: TObject);
begin
  FindDialog1Find(Find1);
end;

procedure TfrmEnumerateDLLs.TreeView1CustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
    i,j: integer;
    s: string;
    a: ptruint;
    p: pchar;
    x: ptruint;
begin
  defaultdraw:=true;
  s:=node.text;
  i:=RPos(' --> ',s);
  j:=length(s);

  if (i=j-4) then
  begin
    i:=pos(' - ',s);
    s:=copy(s,1,i);
    try
      a:=StrToQWordEx('$'+s);


      if i>0 then
      begin
        getmem(p,128);
        x:=0;
        readprocessmemory(processhandle, pointer(a),p,127,x);
        p[x]:=#0;
        node.text:=node.text+p;
        freememandnil(p);
      end;

    except
    end;
  end;
end;

procedure TfrmEnumerateDLLs.TreeView1DblClick(Sender: TObject);
var address: ptrUint;
    i: integer;
    s: string;
begin
  if Treeview1.Selected<>nil then
  begin
    if treeview1.Selected.Level=1 then
    begin
      //showmessage('dblclick: '+treeview1.Selected.Text);
      s:='';
      for i:=1 to length(treeview1.Selected.Text)-1 do
        if not (treeview1.Selected.Text[i] in ['0'..'9','a'..'f','A'..'F'] ) then
        begin
          s:=copy(treeview1.Selected.Text,1,i-1);
          break;
        end;


      if s='' then //should never happen
        s:=treeview1.Selected.Text;

      address:=StrToQWordEx('$'+s);
      { val('$'+s,address,i); fpc 2.4.1 doesn't handle this correctly }

      i:=pos(' --> ',treeview1.Selected.Text);
      if i>0 then
      begin
        memorybrowser.hexview.Address:=address;
        s:=copy(treeview1.Selected.Text,i+5,length(treeview1.Selected.Text));

        try
          memorybrowser.disassemblerview.SelectedAddress:=symhandler.getAddressFromName(s);
        except
        end;
      end
      else
        memorybrowser.disassemblerview.SelectedAddress:=address;
     //showmessage('s='+s+' address='+inttohex(address,8));

    end;
  end;
end;

procedure TfrmEnumerateDLLs.Button2Click(Sender: TObject);
begin
  canceled:=true;
end;

procedure TfrmEnumerateDLLs.FindExecute(Sender: TObject);
var p: TPoint;
begin
  finddialog1.Execute;

  if (findpos.x<>0) or (findpos.y<>0) then
  begin
    finddialog1.left:=findpos.x;
    finddialog1.top:=findpos.y;
  end
  else
  begin
    finddialog1.Left:=left+((width div 2)-(finddialog1.width div 2));
    finddialog1.Top:=top+((height div 2)-(finddialog1.height div 2));
  end;

end;

procedure TfrmEnumerateDLLs.FindDialog1Find(Sender: TObject);
var current: ttreenode;
    i,j: integer;

begin
  miFindNext.Enabled:=true;
  if treeview1.Selected=nil then
    current:=treeview1.Items.GetFirstNode
  else
    current:=treeview1.Selected;

  i:=current.AbsoluteIndex;
  if frFindNext in finddialog1.Options then
    inc(i);

  for j:=i to treeview1.Items.Count-1 do
  begin
    if pos(uppercase(finddialog1.FindText),uppercase(treeview1.Items[j].Text))>0 then
    begin
      treeview1.Selected:=treeview1.Items[j];
      exit;
    end;
  end;

  errorbeep;
  //showmessage(rsNothingFound);
end;

initialization
  {$i frmEnumerateDLLsUnit.lrs}

end.
