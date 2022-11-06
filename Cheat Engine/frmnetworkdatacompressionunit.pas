unit frmNetworkDataCompressionUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, betterControls;

type

  { TfrmNetworkDataCompression }

  TfrmNetworkDataCompression = class(TForm)
    Label1: TLabel;
    lblMaxCompression: TLabel;
    lblNone: TLabel;
    tbCompressionLevel: TTrackBar;
    procedure FormShow(Sender: TObject);
    procedure tbCompressionLevelChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmNetworkDataCompression: TfrmNetworkDataCompression;

implementation

{$R *.lfm}

{ TfrmNetworkDataCompression }

uses networkConfig;

procedure TfrmNetworkDataCompression.tbCompressionLevelChange(Sender: TObject);
begin
  networkcompression:=tbCompressionLevel.Position;
end;

procedure TfrmNetworkDataCompression.FormShow(Sender: TObject);
begin
  width:=(screen.PixelsPerInch div 96)*320;
  height:=(screen.PixelsPerInch div 96)*200;
end;

end.

