unit frmAssemblyScanUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, CEFuncProc;

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

implementation

{$R *.lfm}

{ TfrmAssemblyScan }

procedure TfrmAssemblyScan.btnOkClick(Sender: TObject);
begin
  startaddress:=StrToQWordEx('$'+edtFrom.Text);
  stopaddress:=StrToQWordEx('$'+edtTo.Text);
  modalresult:=mrok;
end;

end.

