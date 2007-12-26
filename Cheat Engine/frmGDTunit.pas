unit frmGDTunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,cefuncproc,newkernelhandler;

type
  TfrmGDTinfo = class(TForm)
    TreeView1: TTreeView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmGDTinfo: TfrmGDTinfo;

implementation

{$R *.dfm}

procedure TfrmGDTinfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmGDTinfo.FormCreate(Sender: TObject);
var limit: word;
    address: dword;
    x: packed array of record
      limit_low: word;    //0-15
      base_low: word;     //\
      base_middle: byte;  /// 16-39
      access: byte;
      granularity: byte;
      base_high: byte;
    end;
    i: integer;
    br:dword;
    t: ttreenode;

    dpl: integer;
    accessed: integer;
    descriptor_type: integer;
    descriptor_bit: integer;
    dlimit: dword;

    descriptor: int64;
begin
  address:=getgdt(limit);

  setlength(x,limit);
  newkernelhandler.readprocessmemory(processhandle,pointer(address),x,limit,br);

  if br>0 then
  begin
    for i:=0 to (br div 8)-1 do
    begin
      descriptor:=pint64(@x[i])^;

      t:=treeview1.Items.Add(nil,inttohex(8*i,4)+': '+inttohex(descriptor,16));
      treeview1.Items.Addchild(t,'base='+inttohex(x[i].base_high,2)+inttohex(x[i].base_middle,2)+inttohex(x[i].base_low,4)  );

      accessed:=x[i].access;

      if (x[i].access shr 7=1) then
        treeview1.Items.addchild(t,'present')
      else
        treeview1.Items.addchild(t,'not present');

      accessed:=accessed and $1;
      treeview1.items.addchild(t,'accessed='+inttostr(accessed));

      descriptor_type:=(accessed shr 1) and $7;
      treeview1.items.addchild(t,'descriptor type='+inttostr(descriptor_type));

      descriptor_bit:=(accessed shr 4) and $7;
      treeview1.items.addchild(t,'Descriptor bit='+inttostr(descriptor_bit));

      dpl:=x[i].access;
      dpl:=(dpl and $7f) shr 5;
      treeview1.items.addchild(t,'dpl='+inttostr(dpl));

      dlimit:=descriptor and $ffff;
      dlimit:=dlimit+ ((descriptor shr 48) and $f) shl 16;
      treeview1.Items.addchild(t,'limit='+inttohex(dlimit,5));


    end;

  end else showmessage('Read error');

end;

end.
