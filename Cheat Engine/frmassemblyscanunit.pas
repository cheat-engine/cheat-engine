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
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
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

uses symbolhandler,windows;

procedure TfrmAssemblyScan.btnOkClick(Sender: TObject);
begin
  startaddress:=symhandler.getAddressFromName(edtFrom.Text);
  stopaddress:=symhandler.getAddressFromName(edtTo.Text);
  modalresult:=mrok;
end;

procedure TfrmAssemblyScan.FormCreate(Sender: TObject);
begin

end;

procedure TfrmAssemblyScan.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if(key=VK_ESCAPE)then
    self.close;
end;

procedure TfrmAssemblyScan.FormShow(Sender: TObject);
begin
  panel2.autosize:=false;
end;

end.

