unit frmAssemblyScanUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CEFuncProc, Parsers, symbolhandler, ProcessHandlerUnit;

type

  { TfrmAssemblyScan }

  TfrmAssemblyScan = class(TForm)
    btnOk: TButton;
    edtFrom: TEdit;
    edtTo: TEdit;
    lblInputAssemblyCode: TLabel;
    lblFrom: TLabel;
    lblTo: TLabel;
    mAssemblerSearch: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    startaddress: ptruint;
    stopaddress: ptruint;
  end;

var frmAssemblyScan: TfrmAssemblyScan;

implementation

uses LCLType, LCLIntf;

{$R *.lfm}

{ TfrmAssemblyScan }

procedure TfrmAssemblyScan.btnOkClick(Sender: TObject);
begin
  try
    startaddress:=StrToQWordEx('$'+edtfrom.text);
  except
    startaddress:=symhandler.getAddressFromName(edtfrom.text);
  end;

  try
    stopaddress:=StrToQWordEx('$'+edtto.text);
  except
    stopaddress:=symhandler.getAddressFromName(edtto.text);
  end;

  if startaddress>stopaddress then
  begin  //xor swap
    startaddress:=startaddress xor stopaddress;
    stopaddress:=stopaddress xor startaddress;
    startaddress:=startaddress xor stopaddress;
  end;

  modalresult:=mrok;
end;

procedure TfrmAssemblyScan.FormShow(Sender: TObject);
var DC: hDC;
    rect: TRect;
begin
  if processhandler.is64bit then
  begin
    //init just once if needed
    if (edtto.Text = '') or (edtfrom.Text = '') then   // if not initialized
     begin
        edtto.text:='7FFFFFFFFFFFFFFF';
        edtfrom.Text:='0000000000000000';
     end;
  end
  else
  begin
    //init just once if needed
    if (edtto.Text = '') or (edtfrom.Text = '') then   // if not initialized
    begin
       edtto.text:='7FFFFFFF';
       edtfrom.Text:='00000000';
    end;
  end;

  edtfrom.Constraints.MinWidth:=canvas.GetTextWidth('XXXXXXXXXXXXXXXX');
  edtTo.Constraints.MinWidth:=edtfrom.Constraints.MinWidth;

  DC := lblInputAssemblyCode.canvas.GetUpdatedHandle([csFontValid]);
  rect:=TRect.Create(0, 0, 0, 0);
  DrawText(DC, pchar(lblInputAssemblyCode.caption), Length(lblInputAssemblyCode.caption), rect, DT_CALCRECT);

  constraints.minwidth:=rect.Width+edtFrom.width+8;
  constraints.MinHeight:=btnOk.top+btnok.Height;


  doautosize;
  panel1.autosize:=false;
  panel2.autosize:=false;

end;
end.
