unit formProcessInfo;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CEFuncProc, NewKernelHandler, LResources, ExtCtrls,
  memorybrowserformunit;

type

  { TfrmProcessInfo }

  TfrmProcessInfo = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    lbidt: TListBox;
    lblcr0: TLabel;
    lblcr3: TLabel;
    lblcr4: TLabel;
    lblGdt: TLabel;
    lblisvalid: TLabel;
    lblPEPROCESS: TLabel;
    lblSdt: TLabel;
    lblSsdt: TLabel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lblclick(Sender: TObject);
    procedure lbidtDblClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure lblPEPROCESSClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmProcessInfo: TfrmProcessInfo;

implementation


uses {$ifdef windows}threadlistexfrm, dbk32functions,{$endif} ProcessHandlerUnit, parsers;

procedure TfrmProcessInfo.FormCreate(Sender: TObject);
var cr3: QWORD;
    idt: array [0..31] of qword;
    i,j: integer;
    limit: word;
begin

  {$ifdef windows}
  lblPEPROCESS.caption:=inttohex(GetPEProcess(processid),8);
  lblisvalid.caption:=booltostr(isvalidhandle(processhandle),true);

  if getcr3(processhandle,cr3) then
    lblcr3.caption:=inttohex(cr3,8)
  else
    lblcr3.Caption:='---';


  lblcr0.caption:=inttohex(getcr0(),8);
  lblcr4.caption:=inttohex(getcr4(),8);
  i:=getidts(@idt[0],32);
  for j:=0 to i-1 do
    lbidt.Items.Add(inttohex(idt[j],8));

  lblsdt.caption:=inttohex(getsdt(),8);
  lblssdt.caption:=inttohex(GetSDTShadow(),8);

  lblgdt.Caption:=inttohex(GetGDT(@limit),8);

  {$endif}

end;

procedure TfrmProcessInfo.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmProcessInfo.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
   {$ifdef windows}
  action:=cafree;
  if frmThreadlistEx<>nil then
    frmThreadlistEx.close;
  frmThreadlistEx:=nil;
  {$endif}
end;

procedure TfrmProcessInfo.lblclick(Sender: TObject);
begin

  memorybrowser.memoryaddress:=StrToInt64('$'+tlabel(sender).caption);

  if not memorybrowser.visible then
    memorybrowser.show;

end;

procedure TfrmProcessInfo.lbidtDblClick(Sender: TObject);
begin

  if lbidt.ItemIndex<>-1 then
  begin
    memorybrowser.memoryaddress:=StrToQWordEx('$'+lbidt.Items[lbidt.itemindex]);

    if not memorybrowser.visible then
      memorybrowser.show;
  end;
end;

procedure TfrmProcessInfo.Button2Click(Sender: TObject);
begin

  {$ifdef windows}
  if frmThreadlistEx=nil then
  begin
    frmThreadlistEx:=tfrmThreadlistEx.create(self);
    frmThreadlistEx.left:=left+width;
    frmThreadlistEx.Top:=top;
    frmThreadlistEx.show;
  end;

  frmThreadlistEx.updatelist;
  {$endif}
end;

procedure TfrmProcessInfo.lblPEPROCESSClick(Sender: TObject);
begin

end;

initialization
  {$i formProcessInfo.lrs}

end.
