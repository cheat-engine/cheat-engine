unit frmIDTunit;

{$MODE Delphi}

interface

uses
  LCLIntf, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls,CEFuncProc,NewKernelHandler, StdCtrls, LResources, commonTypeDefs;

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

uses DBK32functions, ProcessHandlerUnit;

resourcestring
  rsIDTNotPresent = 'not present';
  rsIDTTaskGate = 'task gate';
  rsIDT16bitInterruptGate = '16-bit interrupt gate';
  rsIDT16BiyTrapGate = '16-bit trap gate';
  rsIDT32bitInterruptGate = '32-bit interrupt gate';
  rsIDT32BiyTrapGate = '32-bit trap gate';
  rsIDTUnkownIDT = 'unknown IDT';
  rsIDTDPL = ' DPL=';

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
    address: qword;
    x: Puint64Array;
    i: integer;
    br: PtrUInt;

    offset_0_15: word;
    segmentselector: word;
    idttype: byte;
    dpl: byte;
    p: byte;
    offset_16_31: word;

    title: string;
begin
  {$ifdef windows}
  getidts(@address,1);
  limit:=256;

  getmem(x,8*limit);
  try
    dbk32functions.rpm(processhandle,pointer(address),@x[0],limit*8,br);

    i:=0;
    for i:=0 to 255 do
    begin
      dissectIDTentry(x[i], offset_0_15, segmentselector, idttype, dpl, p, offset_16_31);

      title:=inttostr(i)+' : ';
      if p=0 then  //if it's not present then only say it's not present
        title:=title+rsIDTNotPresent
      else
      case idttype of
        5: title:=title+rsIDTTaskGate; //task gate
        6: title:=title+rsIDT16bitInterruptGate; //16 bit interrupt gate
        7: title:=title+rsIDT16BiyTrapGate; //16 bit trap gate
       14: title:=title+rsIDT32bitInterruptGate; //32-bit interrupt gate
       15: title:=title+rsIDT32BiyTrapGate; //32-bit trap gate
       else title:=title+rsIDTUnkownIDT; //unknown type
      end;



      if p=1 then //there's more
      begin
        title:=title+' ('+inttohex(segmentselector,4)+':';
        if idttype=5 then
          title:=title+'xxxxxxxx)'
        else
          title:=title+inttohex(offset_0_15+(offset_16_31 shl 16),8)+')';

        title:=title+rsIDTDPL+inttostr(DPL);
      end;
      tvidt.Items.Add(nil,title);
    end;
  finally
    freememandnil(x);
  end;
  {$endif}
end;

initialization
  {$i frmIDTunit.lrs}

end.
