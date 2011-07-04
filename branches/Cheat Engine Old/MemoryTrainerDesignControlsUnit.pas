unit MemoryTrainerDesignControlsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, ComCtrls, ToolWin, ImgList;

type
  TfrmTrainerDesignControls = class(TForm)
    Button6: TButton;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ImageList1: TImageList;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    closeisok: boolean;
  end;

var
  frmTrainerDesignControls: TfrmTrainerDesignControls;

implementation

uses MemoryTrainerDesignUnit;

{$R *.dfm}

procedure TfrmTrainerDesignControls.Button1Click(Sender: TObject);
begin
  frmTrainerDesigner.addItem:=twincontrol(sender).Tag;
end;

procedure TfrmTrainerDesignControls.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  canclose:=closeisok;
end;

end.
