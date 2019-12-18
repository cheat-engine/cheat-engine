unit APIhooktemplatesettingsfrm;

{$MODE Delphi}

interface

uses
  LCLIntf, Lmessages, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources, ExtCtrls;

type

  { TfrmAPIhookTemplateSettings }

  TfrmAPIhookTemplateSettings = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label6: TLabel;
    Button1: TButton;
    Button2: TButton;
    Label5: TLabel;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation


{ TfrmAPIhookTemplateSettings }

procedure TfrmAPIhookTemplateSettings.FormCreate(Sender: TObject);
begin

end;

procedure TfrmAPIhookTemplateSettings.FormShow(Sender: TObject);
begin
  button1.autosize:=false;
  button2.autosize:=false;
  if button1.width<button2.width then
    button1.width:=button2.width else button2.width:=button1.width;
end;

initialization
  {$i APIhooktemplatesettingsfrm.lrs}

end.
