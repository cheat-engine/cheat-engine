unit PasteTableentryFRM;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources;

type
  TfrmPasteTableentry = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    edtFind: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    edtReplace: TEdit;
    Label3: TLabel;
    edtOffset: TEdit;
    Button1: TButton;
    Button2: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmPasteTableentry: TfrmPasteTableentry;

implementation


initialization
  {$i PasteTableentryFRM.lrs}

end.
