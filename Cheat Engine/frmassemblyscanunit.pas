unit frmAssemblyScanUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CEFuncProc, Parsers;

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

uses symbolhandler;

procedure TfrmAssemblyScan.btnOkClick(Sender: TObject);
begin
  startaddress:=symhandler.getAddressFromName(edtFrom.Text);
  stopaddress:=symhandler.getAddressFromName(edtFrom.Text);
  modalresult:=mrok;
end;

end.

