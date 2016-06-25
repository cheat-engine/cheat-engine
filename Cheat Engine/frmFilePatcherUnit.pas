unit frmFilePatcherUnit;

{Not implemented as I currently don't see a real use for it}


{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn, PEInfoFunctions, PEInfounit;

type

  { TfrmFilePatcher }

  TfrmFilePatcher = class(TForm)
    Button1: TButton;
    edtBaseAddress: TEdit;
    filename: TFileNameEdit;
    lblFilename: TLabel;
    lblBaseAddress: TLabel;
    lbModules: TListBox;
    rbAuto: TRadioButton;
    rbManual: TRadioButton;
    procedure AutoOrManual(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
    fmodule: string;
    procedure setModule(m: string);
  public
    { public declarations }
    property module: string read fmodule write setModule;
  end;

var
  frmFilePatcher: TfrmFilePatcher;

implementation

{$R *.lfm}

resourcestring
  rsFilePatcher = 'File Patcher: ';

procedure TfrmFilePatcher.setModule;
begin
  fmodule:=m;
  caption:=rsFilePatcher+m;
end;

procedure TfrmFilePatcher.AutoOrManual(Sender: TObject);
begin
  lblFilename.enabled:=rbManual.checked;
  filename.enabled:=rbManual.checked;
  lblBaseAddress.enabled:=rbManual.checked;
  edtBaseAddress.enabled:=rbManual.checked;
  lbModules.enabled:=rbAuto.checked;
end;

procedure TfrmFilePatcher.Button1Click(Sender: TObject);
begin
  //get the filename to patch
  //get the base address to read from
end;



end.

