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
    procedure dissectGDTentry(entry: uint64; var segmentlimit_0_15: word; var baseaddress_0_23: dword; var segmenttype: byte; var dpl: byte; var p: byte; var segmentlimit_16_19: byte; var AVL: byte; var bigordefault: byte; var gran: byte; var baseaddress_24_31: byte );
  public
    { Public declarations }
  end;


implementation

{$R *.dfm}

procedure TfrmGDTinfo.dissectGDTentry(entry: uint64; var segmentlimit_0_15: word; var baseaddress_0_23: dword; var segmenttype: byte; var dpl: byte; var p: byte; var segmentlimit_16_19: byte; var AVL: byte; var bigordefault: byte; var gran: byte; var baseaddress_24_31: byte );
begin
  segmentlimit_0_15:=entry and $ffff;
  baseaddress_0_23:=(entry shr 16) and $ffffff;
  segmenttype:=(entry shr (32+8)) and $1f;
  dpl:=(entry shr (32+13)) and 3;
  p:=(entry shr (32+15)) and 1;
  segmentlimit_16_19:=(entry shr (32+16)) and $f;
  avl:=(entry shr (32+20)) and 1;
  bigordefault:=(entry shr (32+22)) and 1;
  gran:=(entry shr (32+23)) and 1;
  baseaddress_24_31:=(entry shr (32+24)) and $ff;
end;


procedure TfrmGDTinfo.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  action:=cafree;
end;

procedure TfrmGDTinfo.FormCreate(Sender: TObject);
var limit: word;
    address: dword;

    x: puint64array;

    i: integer;
    br:dword;
    t: ttreenode;


    segmentlimit_0_15: word;
    baseaddress_0_23: dword;
    segmenttype: byte;
    dpl: byte;
    p: byte;
    segmentlimit_16_19: byte;
    AVL: byte;
    bigordefault: byte;
    gran: byte;
    baseaddress_24_31: byte;

    seglimit: dword;
    baseaddress: dword;

    segtype: integer;

    title: string;
begin
  address:=getgdt(limit);

  getmem(x,limit*8);
  try
    newkernelhandler.kernelreadprocessmemory(processhandle,pointer(address),x,limit,br);

    if br>0 then
    begin
      for i:=0 to (br div 8)-1 do
      begin
        dissectGDTentry(x[i],segmentlimit_0_15,baseaddress_0_23, segmenttype, dpl,p, segmentlimit_16_19,avl,bigordefault,gran, baseaddress_24_31);

        title:=inttohex(8*i,4)+': ';
        if p=1 then
        begin
          if (segmenttype shr 4)=1 then
          begin
            if ((segmenttype shr 3) and 1)=1 then
            begin
              title:=title+'Code Segment';
              segtype:=1;
            end
            else
            begin
              segtype:=0;
              title:=title+'Data Segment';
            end;

          end else
          begin
            segtype:=2;
            title:=title+'System Segment';
          end;



          baseaddress:=baseaddress_0_23+(baseaddress_24_31 shl 24);
          seglimit:=segmentlimit_0_15+(segmentlimit_16_19 shl 16);
          if gran=1 then
            seglimit:=seglimit*4096+$FFF;

          title:=title+' ('+inttohex(baseaddress,8)+' - '+inttohex(baseaddress+seglimit,8)+')';
        end
        else title:='Not present';


        t:=treeview1.Items.Add(nil,title);

        if p=1 then
        begin
          if segtype in [0,1] then
            treeview1.items.addchild(t,'Accessed='+inttostr(segmenttype and 1));

          if segtype = 0 then //data
          begin
            treeview1.items.addchild(t,'Writable='+inttostr((segmenttype shr 1) and 1));
            treeview1.items.addchild(t,'Expansion direction='+inttostr((segmenttype shr 2) and 1));
          end;

          if segtype = 1 then //code
          begin
            treeview1.items.addchild(t,'Readable='+inttostr((segmenttype shr 1) and 1));
            treeview1.items.addchild(t,'Conforming='+inttostr((segmenttype shr 2) and 1));
          end;

          treeview1.items.addchild(t,'DPL='+inttostr(dpl));
          treeview1.items.addchild(t,'AVL='+inttostr(AVL));
        end;
      end;

    end else showmessage('Read error');

  finally
    freemem(x);
  end;

end;

end.
