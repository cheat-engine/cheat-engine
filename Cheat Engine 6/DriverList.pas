unit DriverList;

{$MODE Delphi}

interface

uses
  jwaWindows, windows, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,{psapi,} Menus,imagehlp,CEFuncProc,NewKernelHandler, LREsources;

type
  TfrmDriverlist = class(TForm)
    ListBox1: TListBox;
    Button1: TButton;
    FindDialog1: TFindDialog;
    PopupMenu1: TPopupMenu;
    Find1: TMenuItem;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure Find1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var frmDriverlist: TfrmDriverlist;

implementation





procedure TfrmDriverlist.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmDriverlist.FormCreate(Sender: TObject);
var need:dword;
    x: PPointerArray;
    i: integer;
    count: integer;
    drivername: pchar;
begin
  EnumDevicedrivers(nil,0,need);
  getmem(x,need);
  try
    if enumDevicedrivers(@x[0],need,need) then
    begin
      count:=need div sizeof(pointer);
      getmem(drivername,200);
      try
        for i:=0 to count-1 do
        begin
          GetDevicedriverBaseNameA(x[i],drivername,200);
          listbox1.items.add(inttohex(ptrUint(x[i]),8)+' - '+drivername);
        end;


      finally
        freemem(drivername);
      end;
    end;
  finally
    freemem(x);
  end;

  listbox1.Sorted:=true;
end;

procedure TfrmDriverlist.FindDialog1Find(Sender: TObject);
var i,j: integer;
begin
  i:=listbox1.itemindex;
  inc(i);

  for j:=i to listbox1.Items.Count-1 do
  begin
    if pos(uppercase(finddialog1.FindText),uppercase(listbox1.Items[j]))>0 then
    begin
      listbox1.Selected[j]:=true;
      exit;
    end;
  end;
  showmessage('nothing found');
end;

procedure TfrmDriverlist.Find1Click(Sender: TObject);
begin
  finddialog1.Execute;
end;

procedure TfrmDriverlist.Button1Click(Sender: TObject);
begin
  close;
end;

initialization
  {$i DriverList.lrs}


finalization

end.


