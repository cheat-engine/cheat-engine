unit Changeoffsetunit;

interface

uses
  Windows, Messages, SysUtils, classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls,symbolhandler;

type
  TChangeOffset = class(TForm)
    TabControl1: TTabControl;
    Change: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    procedure ChangeClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TabControl1Changing(Sender: TObject;
      var AllowChange: Boolean);
    procedure TabControl1Change(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    offset: Integer;
    FromAddress: dword;
    toAddress: dword;
    error: integer;
  end;

var
  ChangeOffset: TChangeOffset;

implementation

{$R *.dfm}


procedure TChangeOffset.ChangeClick(Sender: TObject);
var temp: dword;
begin
  if tabcontrol1.TabIndex=0 then
  begin
    if checkbox1.checked then val('$'+edit1.Text,offset,error) else
                              val(edit1.Text,offset,error);
  end
  else
  begin
    try
      temp:=symhandler.getAddressFromName(edit1.text);
      offset:=temp-fromaddress;
    except
      error:=1;
    end;
  end;
end;

procedure TChangeOffset.FormShow(Sender: TObject);
begin
  if tabcontrol1.TabIndex=0 then
  begin
    if checkbox1.Checked then edit1.Text:=IntToHex(toaddress-fromaddress,8) else
                              edit1.Text:=IntToStr(integer(toaddress-fromaddress));
  end
  else
  begin
    edit1.Text:=IntToHex(toaddress,8);
  end;
end;

procedure TChangeOffset.TabControl1Changing(Sender: TObject;
  var AllowChange: Boolean);
var controle: integer;
begin
  if tabcontrol1.TabIndex=0 then
  begin
    if checkbox1.checked then val('$'+edit1.Text,offset,controle) else
                              val(edit1.text,offset,controle);
  end else
  begin
    val('$'+edit1.text,toaddress,controle);
  end;

  if controle=0 then allowchange:=true else
  begin
    allowchange:=false;
    raise exception.Create('This is not an valid value');
  end;

end;

procedure TChangeOffset.TabControl1Change(Sender: TObject);
begin
  if tabcontrol1.TabIndex=0 then
  begin
    checkbox1.Visible:=true;
    if checkbox1.Checked then edit1.Text:=IntToHex(toaddress-fromaddress,8) else
                              edit1.Text:=IntToStr(integer(toaddress-fromaddress));
  end
  else
  begin
    checkbox1.visible:=false;
    edit1.Text:=IntToHex(toaddress,8);
  end;
end;

procedure TChangeOffset.CheckBox1Click(Sender: TObject);
begin
  if tabcontrol1.TabIndex=0 then
  begin
    if checkbox1.Checked then edit1.Text:=IntToHex(toaddress-fromaddress,8) else
                              edit1.Text:=IntToStr(integer(toaddress-fromaddress));
  end;
end;                        

end.
