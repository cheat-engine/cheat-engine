unit frmAssemblyScanUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CEFuncProc, Parsers, symbolhandler;

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
    Splitter1: TSplitter;
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
begin
  panel2.autosize:=false;
end;
end.
