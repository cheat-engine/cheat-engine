unit symbolconfigunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,symbolhandler, ComCtrls;

type
  TfrmSymbolhandler = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    edtSymbolname: TEdit;
    Label3: TLabel;
    edtAddress: TEdit;
    Label2: TLabel;
    ListView1: TListView;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure SymUpdate(var message:TMessage); message wm_user+1;

  public
    { Public declarations }
    procedure refreshlist;
  end;

var
  frmSymbolhandler: TfrmSymbolhandler;

implementation

{$R *.dfm}

procedure SymbolUpdate;
begin
  if frmsymbolhandler<>nil then
    postmessage(frmsymbolhandler.handle,wm_user+1,0,0);  //in case of multithreading
end;

procedure TfrmSymbolhandler.SymUpdate(var message: tmessage);
begin
  refreshlist;
end;


procedure TfrmSymbolhandler.refreshlist;
var sl: tstringlist;
    i: integer;
    li: tlistitem;
begin
  listview1.Items.Clear;
  sl:=tstringlist.create;
  try
    symhandler.EnumerateUserdefinedSymbols(sl);

    for i:=0 to sl.Count-1 do
    begin
      li:=listview1.Items.Add;
      li.Caption:=sl[i];
      li.SubItems.Add(inttohex(dword(sl.objects[i]),8));
    end;
  finally
    sl.free;
  end;
end;

procedure TfrmSymbolhandler.FormShow(Sender: TObject);
begin
  refreshlist;
end;

procedure TfrmSymbolhandler.Button1Click(Sender: TObject);
var symbolname:string;
    address: dword;
    li: tlistitem;
begin
  symbolname:=edtsymbolname.Text;
  address:=strtoint('$'+edtaddress.Text);
  symhandler.AddUserdefinedSymbol(address,symbolname);

  li:=listview1.Items.Add;
  li.Caption:=symbolname;
  li.SubItems.Add(inttohex(address,8));
end;

procedure TfrmSymbolhandler.ListView1DblClick(Sender: TObject);
begin
  if listview1.ItemIndex<>-1 then
  begin
    if messagedlg('Are you sure you want to remove this symbol from the list?',mtconfirmation,[mbyes,mbno],0)=mryes then
    begin
      symhandler.DeleteUserdefinedSymbol(listview1.Items[listview1.ItemIndex].Caption);
      listview1.Items[listview1.ItemIndex].Delete;
    end;
  end;
end;

procedure TfrmSymbolhandler.ListView1Click(Sender: TObject);
var li: tlistitem;
begin
  if listview1.ItemIndex<>-1 then
  begin
    li:=listview1.Items[listview1.itemindex];

    edtSymbolname.Text:=li.Caption;
    edtAddress.text:=li.SubItems[0];
  end;
end;

procedure TfrmSymbolhandler.FormCreate(Sender: TObject);
begin
  symhandler.RegisterUserdefinedSymbolCallback(@symbolupdate);
end;

end.
