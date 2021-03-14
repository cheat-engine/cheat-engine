unit frmCR3SwitcherUnit;

//for runtime switching between CR3 targets

{$mode delphi}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus, maps;

type

  { TfrmCR3Switcher }

  TfrmCR3Switcher = class(TForm)
    btnSetCR3: TButton;
    edtNewCR3: TEdit;
    Label1: TLabel;
    lbCR3List: TListBox;
    MenuItem1: TMenuItem;
    pmCR3List: TPopupMenu;
    procedure btnSetCR3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lbCR3ListDblClick(Sender: TObject);
    procedure lbCR3ListSelectionChange(Sender: TObject; User: boolean);
    procedure MenuItem1Click(Sender: TObject);
  private
    fOnCR3Change: TNotifyEvent;
    fCR3: qword;
  public
    procedure addCR3ToList(v: qword);
    property CR3: qword read fCR3 write fCR3;
    property OnCR3Change: TNotifyEvent read fOnCR3Change write fOnCR3Change;
  end;

implementation

{ TfrmCR3Switcher }

uses CEFuncProc, vmxfunctions;

resourcestring
  rsDBVMCR3Log = 'DBVM CR3 Log';
  rsDescribeDBVMRoutine = 'DBVM will record all CR3 values it encounters up to 512 unique ones). How long should it wait? (In seconds)';
  rsLongwaitWarning = 'Are you sure you wish to wait %d seconds?';

procedure TfrmCR3Switcher.FormCreate(Sender: TObject);
begin
  loadformposition(self);
end;

procedure TfrmCR3Switcher.addCR3ToList(v: qword);
var i: integer;
begin
 i:=lbCR3List.Items.IndexOf(inttohex(v,8));
 if i<>-1 then
   lbCR3List.Items.Move(i,0)
 else
   lbCR3List.Items.Insert(0,inttohex(v,8));
end;

procedure TfrmCR3Switcher.btnSetCR3Click(Sender: TObject);
var
  newcr3: qword;
begin
  newcr3:=0;
  if edtNewCR3.text<>'' then
  begin
    try
      newcr3:=StrToInt64('$'+edtNewCR3.text);
    except
      showmessage('invalid value');
      exit;
    end;
  end;

  fcr3:=newcr3;
  fOnCR3Change(self);

  //add to the list if not there already, and if it is, bring to the top
  addCR3ToList(newcr3);
end;

procedure TfrmCR3Switcher.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmCR3Switcher.FormShow(Sender: TObject);
begin
  edtNewCR3.ClientWidth:=canvas.TextWidth(' DDDDDDDDDDDDDDDD ');

  if autosize then
  begin
    autosize:=false;

    clientheight:=edtNewCR3.Height*12;

  end;
end;

procedure TfrmCR3Switcher.lbCR3ListDblClick(Sender: TObject);
begin
  if lbCR3List.ItemIndex<>-1 then
  begin
    edtNewCR3.Text:=lbCR3List.Items[lbCR3List.ItemIndex];
    btnSetCR3.Click;
  end;
end;

procedure TfrmCR3Switcher.lbCR3ListSelectionChange(Sender: TObject;
  User: boolean);
begin
  if lbCR3List.ItemIndex<>-1 then
    edtNewCR3.Text:=lbCR3List.Items[lbCR3List.ItemIndex];
end;

procedure TfrmCR3Switcher.MenuItem1Click(Sender: TObject);
var
  v: string;
  t: integer;
  cr3log: array [0..511] of qword;
  i: integer;
begin
  v:='5';
  if InputQuery(rsDBVMCR3Log, rsDescribeDBVMRoutine, v) then
  begin
    t:=strtoint(v);
    if (t<60) or (MessageDlg(Format(rsLongwaitWarning, [t]), mtConfirmation, [mbyes, mbno], 0)=mryes) then
    begin
      if dbvm_log_cr3values_start then
      begin
        sleep(t*1000);
        if dbvm_log_cr3values_stop(@cr3log[0]) then
        begin
          lbCR3List.Items.BeginUpdate;
          try
            lbCR3List.Clear;

            for i:=0 to 511 do
            begin
              if cr3log[i]=0 then break;

              lbCR3List.Items.Add(inttohex(cr3log[i],8));
            end;
          finally
            lbCR3List.Items.EndUpdate;
          end;
        end;
      end;
    end;
  end;
end;

initialization
  {$I frmCR3SwitcherUnit.lrs}

end.

