unit frmTracerConfigUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TfrmTracerConfig }

  TfrmTracerConfig = class(TForm)
    Button1: TButton;
    Button2: TButton;
    cbDereferenceAddresses: TCheckBox;
    edtMaxTrace: TEdit;
    edtCondition: TEdit;
    Label1: TLabel;
    Label2: TLabel;
  private
    { private declarations }
  public
    { public declarations }
  end; 


implementation

initialization
  {$I frmTracerConfigUnit.lrs}

end.

