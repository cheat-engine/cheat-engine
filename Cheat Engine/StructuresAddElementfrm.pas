unit StructuresAddElementfrm;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, LResources;

resourcestring
  rsSAE4Bytes = '4 Bytes';
  rsSAE2Bytes = '2 Bytes';

type

  { TfrmStructuresAddElement }

  TfrmStructuresAddElement = class(TForm)
    cbType: TComboBox;
    cbPointerto: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    edtDescription: TEdit;
    edtByteSize: TEdit;
    Label2: TLabel;
    edtOffset: TEdit;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure cbTypeChange(Sender: TObject);
    procedure edtByteSizeChange(Sender: TObject);
    procedure edtOffsetChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    bytesize: integer;
  end;

var
  frmStructuresAddElement: TfrmStructuresAddElement;

implementation


procedure TfrmStructuresAddElement.cbTypeChange(Sender: TObject);
var s: string;
begin
  if visible then
  begin
    s:=edtDescription.Text;
    if s='Dword' then s:=rsSAE4Bytes;
    if s='Word' then s:=rsSAE2Bytes;
    if cbType.Items.IndexOf(s)<>-1 then
      edtDescription.Text:=cbType.Items[cbType.itemindex];


    bytesize:=integer(cbtype.Items.Objects[cbtype.itemindex]);
    edtbytesize.Text:=inttostr(bytesize);
    edtByteSize.enabled:=(cbtype.ItemIndex=13) or (cbtype.itemindex=14);
  end;
end;

procedure TfrmStructuresAddElement.Button1Click(Sender: TObject);
begin

end;

procedure TfrmStructuresAddElement.edtByteSizeChange(Sender: TObject);
begin
  try
    bytesize:=strtoint(edtbytesize.text);

    if bytesize>0 then
      cbtype.Items.Objects[cbtype.itemindex]:=pointer(ptrint(bytesize));
  except
  end;
end;

procedure TfrmStructuresAddElement.edtOffsetChange(Sender: TObject);
var x: integer;
begin
  button1.Enabled:=TryStrToInt('$'+edtOffset.Text,x);
end;

procedure TfrmStructuresAddElement.FormCreate(Sender: TObject);
begin

end;

procedure TfrmStructuresAddElement.FormShow(Sender: TObject);
begin
  cbTypeChange(self);
end;

initialization
  {$i StructuresAddElementfrm.lrs}

end.
