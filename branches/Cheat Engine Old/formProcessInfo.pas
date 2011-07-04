unit formProcessInfo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,cefuncproc,newkernelhandler,memorybrowserformunit;

type
  TfrmProcessInfo = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblcr3: TLabel;
    lblcr4: TLabel;
    lblPEPROCESS: TLabel;
    lbidt: TListBox;
    Button1: TButton;
    Label8: TLabel;
    lblisvalid: TLabel;
    Button2: TButton;
    Label5: TLabel;
    lblSdt: TLabel;
    Label6: TLabel;
    lblSsdt: TLabel;
    Label7: TLabel;
    lblGdt: TLabel;
    Label9: TLabel;
    lblcr0: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure lblclick(Sender: TObject);
    procedure lbidtDblClick(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmProcessInfo: TfrmProcessInfo;

implementation

{$R *.dfm}

uses threadlistexfrm;

procedure TfrmProcessInfo.FormCreate(Sender: TObject);
var cr3: dword;
    idt: array [0..31] of dword;
    i,j: integer;
    limit: word;
begin
  lblPEPROCESS.caption:=inttohex(GetPEProcess(processid),8);
  lblisvalid.caption:=booltostr(isvalidhandle(processhandle),true);

  if getcr3(processhandle,cr3) then
    lblcr3.caption:=inttohex(cr3,8)
  else
    lblcr3.Caption:='---';

  lblcr0.caption:=inttohex(getcr0,8);
  lblcr4.caption:=inttohex(getcr4,8);
  i:=getidts(@idt[0],32);
  for j:=0 to i-1 do
    lbidt.Items.Add(inttohex(idt[j],8));

  lblsdt.caption:=inttohex(getsdt,8);
  lblssdt.caption:=inttohex(GetSDTShadow,8);

  lblgdt.Caption:=inttohex(GetGDT(limit),8);

end;

procedure TfrmProcessInfo.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmProcessInfo.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=cafree;
  if frmThreadlistEx<>nil then
    frmThreadlistEx.close;
  frmThreadlistEx:=nil;
end;

procedure TfrmProcessInfo.lblclick(Sender: TObject);
begin
  memorybrowser.memoryaddress:=strtoint('$'+tlabel(sender).caption);

  if not memorybrowser.visible then
    memorybrowser.show;

  memorybrowser.RefreshMB;
end;

procedure TfrmProcessInfo.lbidtDblClick(Sender: TObject);
begin
  if lbidt.ItemIndex<>-1 then
  begin
    memorybrowser.memoryaddress:=strtoint('$'+lbidt.Items[lbidt.itemindex]);

    if not memorybrowser.visible then
      memorybrowser.show;

    memorybrowser.RefreshMB;
  end;
end;

procedure TfrmProcessInfo.Button2Click(Sender: TObject);
begin
  if frmThreadlistEx=nil then
  begin
    frmThreadlistEx:=tfrmThreadlistEx.create(self);
    frmThreadlistEx.left:=left+width;
    frmThreadlistEx.Top:=top;
    frmThreadlistEx.show;
  end;

  frmThreadlistEx.updatelist;
end;

end.
