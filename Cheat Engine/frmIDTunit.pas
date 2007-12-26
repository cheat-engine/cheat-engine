unit frmIDTunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,cefuncproc,newkernelhandler, StdCtrls;

type
  TfrmIDT = class(TForm)
    tvIDT: TTreeView;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure dissectIDTentry(entry: uint64; var offset_0_15: word; var segmentselector: word; var idttype: byte; var dpl: byte; var p: byte; var offset_16_31: word);
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

procedure TfrmIDT.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmIDT.dissectIDTentry(entry: uint64; var offset_0_15: word; var segmentselector: word; var idttype: byte; var dpl: byte; var p: byte; var offset_16_31: word);
begin
  offset_0_15:=entry and $ffff;
  segmentselector:=(entry shr 16) and $ffff;
  idttype:=(entry shr (32+8)) and $1f;
  dpl:=(entry shr (32+13)) and $3;
  p:=(entry shr (32+15)) and 1;
  offset_16_31:=(entry shr (32+16)) and $ffff;
end;

procedure TfrmIDT.FormCreate(Sender: TObject);
var limit: word;
    address: dword;
    x: Puint64Array;
    i: integer;
    br: dword;

    offset_0_15: word;
    segmentselector: word;
    idttype: byte;
    dpl: byte;
    p: byte;
    offset_16_31: word;

    title: string;
begin
  getidts(@address,1);
  limit:=256;

  getmem(x,8*limit);
  try
    newkernelhandler.kernelreadprocessmemory(processhandle,pointer(address),@x[0],limit*8,br);

    i:=0;
    for i:=0 to 255 do
    begin
      dissectIDTentry(x[i], offset_0_15, segmentselector, idttype, dpl, p, offset_16_31);

      title:=inttostr(i)+' : ';
      if p=0 then  //if it's not present then only say it's not present
        title:=title+'not present'
      else
      case idttype of
        5: title:=title+'task gate'; //task gate
        6: title:=title+'16-bit interrupt gate'; //16 bit interrupt gate
        7: title:=title+'16-bit trap gate'; //16 bit trap gate
       14: title:=title+'32-bit interrupt gate'; //32-bit interrupt gate
       15: title:=title+'32-bit trap gate'; //32-bit trap gate
       else title:=title+'unknown IDT'; //unknown type
      end;



      if p=1 then //there's more
      begin
        title:=title+' ('+inttohex(segmentselector,4)+':';
        if idttype=5 then
          title:=title+'xxxxxxxx)'
        else
          title:=title+inttohex(offset_0_15+(offset_16_31 shl 16),8)+')';

        title:=title+' DPL='+inttostr(DPL);
      end;
      tvidt.Items.Add(nil,title);
    end;
  finally
    freemem(x);
  end;
end;

end.
