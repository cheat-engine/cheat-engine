unit frmHelpUnit;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TfrmHelp }

  TfrmHelp = class(TForm)
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Image1Click(Sender: TObject);
  private
    attachedform: TCustomForm;
    ftutorialtag: string;
    procedure fixsize;
    procedure attachedformboundschange(Sender: TObject);
  public
    procedure Attach(f: TCustomForm; tutorialtag: string);
  end;

var
  frmHelp: TfrmHelp;

implementation

{ TfrmHelp }

procedure TfrmHelp.attachedformboundschange(Sender: TObject);
var bw: integer;
begin
  bw:=GetSystemMetrics(SM_CXEDGE);

  left:=attachedform.left+attachedform.Width+bw*2;
  top:=attachedform.top;
end;

procedure TfrmHelp.fixsize;
begin
  clientwidth:=ScaleX(64,96);
  clientheight:=ScaleY(64,96);
end;

procedure TfrmHelp.Attach(f: TCustomForm; tutorialtag: string);
begin
  attachedform:=f;
  ftutorialtag:=tutorialtag;
  f.OnChangeBounds:=@attachedformboundschange;
  attachedformboundschange(self);

  show;
end;

procedure TfrmHelp.FormCreate(Sender: TObject);
var
  key: dword;
begin
  //only works on forms in windows 7 and earlier, but also works on child components in windows 8 and later
  if SetWindowLong(handle, GWL_EXSTYLE, GetWindowLong(handle, GWL_EXSTYLE) or WS_EX_LAYERED)=0 then
    exit; //not supported, go look at the green background

  key:=clLime;
  SetLayeredWindowAttributes(handle, clLime,0, LWA_COLORKEY);


  fixsize;
  //ShowInTaskBar:=stNever;
end;

procedure TfrmHelp.FormShow(Sender: TObject);
begin

  fixsize;
end;

procedure TfrmHelp.Image1Click(Sender: TObject);
begin
  ShellExecute(0, PChar('open'), PChar('https://cheatengine.org/tutorial.php?tutorial='+ftutorialtag),PChar(''), PChar(''), SW_SHOW);
end;

initialization
  {$I frmHelpUnit.lrs}

end.

