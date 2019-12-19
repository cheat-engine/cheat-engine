unit frmChangedAddressesCommonalityScannerUnit;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  windows,
  {$endif}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, formChangedAddresses;

type

  { TfrmChangedAddressesCommonalityScanner }

  TfrmChangedAddressesCommonalityScanner = class(TForm)
    lvRegisters: TListView;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lvRegistersDblClick(Sender: TObject);
  private
    { private declarations }

    group: array [1..2] of array of TAddressEntry;
  public
    { public declarations }

    procedure setGroup(groupnr: integer; const grouplist: array of TAddressEntry);
    procedure initlist;
  end;


implementation

{$R *.lfm}

uses cefuncproc, ProcessHandlerUnit, frmstructurecompareunit;

resourcestring
  rsDblClickLaunchComp = 'Doubleclick to launch structure compare';
  rsShowResults = 'Doubleclick to show scanner/results';

type
  TRegisterInfo=class
  private
    owner: TfrmChangedAddressesCommonalityScanner;
    regnr: integer;
    contextoffset: dword;
    g1same, g2same: boolean;
    validaddress: boolean;
  public
    running: boolean;
    done: boolean;
    scanner: TfrmStructureCompare;

    values: array [1..2] of array of ptruint;

    oldscannerdestroy: TNotifyEvent;

    procedure scannerdestroy(sender: tobject);

    constructor create(o: TfrmChangedAddressesCommonalityScanner; r: integer);
    destructor destroy; override;
  end;

procedure TRegisterInfo.scannerdestroy(sender: tobject);
begin
  if self=nil then exit;
  scanner:=nil;
  oldscannerdestroy(sender);
end;

destructor TRegisterInfo.destroy;
begin
  if scanner<>nil then
    scanner.OnDestroy:=oldscannerdestroy;

  inherited destroy;
end;

constructor TRegisterInfo.create(o: TfrmChangedAddressesCommonalityScanner; r: integer);
var
  x: TContext;
  v: ptruint;
  i,j: integer;
begin
  owner:=o;
  regnr:=r;


  case regnr of
    0: contextoffset:=ptruint(@PContext(nil)^.{$ifdef cpu64}Rax{$else}eax{$endif});
    1: contextoffset:=ptruint(@PContext(nil)^.{$ifdef cpu64}Rbx{$else}ebx{$endif});
    2: contextoffset:=ptruint(@PContext(nil)^.{$ifdef cpu64}Rcx{$else}ecx{$endif});
    3: contextoffset:=ptruint(@PContext(nil)^.{$ifdef cpu64}Rdx{$else}edx{$endif});
    4: contextoffset:=ptruint(@PContext(nil)^.{$ifdef cpu64}Rsi{$else}esi{$endif});
    5: contextoffset:=ptruint(@PContext(nil)^.{$ifdef cpu64}Rdi{$else}edi{$endif});
    6: contextoffset:=ptruint(@PContext(nil)^.{$ifdef cpu64}Rbp{$else}ebp{$endif});
    7: contextoffset:=ptruint(@PContext(nil)^.{$ifdef cpu64}Rsp{$else}esp{$endif});
    {$ifdef cpu64}
    8: contextoffset:=ptruint(@PContext(nil)^.r8);
    9: contextoffset:=ptruint(@PContext(nil)^.r9);
    10: contextoffset:=ptruint(@PContext(nil)^.r10);
    11: contextoffset:=ptruint(@PContext(nil)^.r11);
    12: contextoffset:=ptruint(@PContext(nil)^.r12);
    13: contextoffset:=ptruint(@PContext(nil)^.r13);
    14: contextoffset:=ptruint(@PContext(nil)^.r14);
    15: contextoffset:=ptruint(@PContext(nil)^.r15);
    {$endif}
  end;

  g1same:=true;
  g2same:=true;
  for i:=1 to 2 do
  begin
    setlength(values[i], length(owner.group[i]));
    for j:=0 to length(values[i])-1 do
    begin
      values[i][j]:=pptruint(ptruint(@owner.group[i][j].context)+contextoffset)^;
      if (j>0) and (values[i][j]<>values[i][0]) then //check if in the current group (i) the value of this entry (j) is the same as the first one in this group
        if i=1 then g1same:=false else g2same:=false;
    end;
  end;

  g1same:=g1same and (length(values[1])>1);
  g2same:=g2same and (length(values[2])>1);

  if g1same and g2same and (values[1][0]=values[2][0]) then //This check shouldn't be necesary as initlist has done this already
  begin
    //if both are the same, none are the same
    g1same:=false;
    g2same:=false;
  end;


end;

procedure TfrmChangedAddressesCommonalityScanner.FormDestroy(Sender: TObject);
var i,j: integer;
  r: TRegisterInfo;
begin
  saveformposition(self);

  //tell all the scanners to stop if they where active
  for i:=0 to lvRegisters.items.count-1 do
  begin
    r:=lvRegisters.items[i].data;
    if (r<>nil) and (r.scanner<>nil) then
    begin
      if (r.scanner.Visible) then  //if visible, make it free itself when it's done
        r.scanner.donotfreeonclose:=false
      else
        freeandnil(r.scanner); //else free it now
    end;

    freeandnil(r);
    lvRegisters.items[i].data:=nil;
  end;

  lvRegisters.Clear;

  //free memory
  for i:=1 to 2 do
    for j:=0 to length(group[i])-1 do
    begin
      if group[i][j]<>nil then
        freeandnil(group[i][j]);
    end;
end;

procedure TfrmChangedAddressesCommonalityScanner.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  closeaction:=cafree;
end;

procedure TfrmChangedAddressesCommonalityScanner.FormCreate(Sender: TObject);
begin
  loadformposition(self);
end;

procedure TfrmChangedAddressesCommonalityScanner.lvRegistersDblClick(Sender: TObject);
var
  r: TRegisterInfo;

  i,j: integer;
  address: ptruint;
  shadow: ptruint;
  shadowsize: integer;

  a: pointer;
  x: ptruint;
begin
  if lvRegisters.Selected<>nil then
  begin
    r:=TRegisterInfo(lvRegisters.Selected.data);

    if r.scanner=nil then
    begin
      //create it
      r.scanner:=TfrmStructureCompare.Create(application);
      r.scanner.donotfreeonclose:=true;
      r.oldscannerdestroy:=r.scanner.OnDestroy;
      r.scanner.OnDestroy:=r.scannerdestroy;

      r.scanner.SaveDialog1.FileName:='reg'+lvRegisters.Selected.caption+'.sptr';
      r.scanner.caption:=r.scanner.caption+' : '+ lvRegisters.Selected.caption;

      for i:=1 to 2 do
      begin
        for j:=0 to length(group[i])-1 do
        begin
          shadow:=0;
          shadowsize:=0;
          address:=r.values[i][j];

          if r.regnr=7 then
          begin
            //create a shadow
            if group[i][j].stack.stack<>nil then
            begin
              a:=VirtualAllocEx(processhandle, nil, group[i][j].stack.savedsize, MEM_COMMIT or MEM_RESERVE, PAGE_READWRITE);
              if a<>nil then
              begin
                if writeprocessmemory(processhandle, a, group[i][j].stack.stack, group[i][j].stack.savedsize, x) then
                begin
                  shadow:=ptruint(a);
                  shadowsize:=group[i][j].stack.savedsize;
                end;
              end;
            end;
          end;
          r.scanner.addAddress(address, shadow, shadowsize, i-1);
        end;
      end;
    end;

    r.scanner.show;
  end;
end;


procedure TfrmChangedAddressesCommonalityScanner.setGroup(groupnr: integer; const grouplist: array of TAddressEntry);
var i: integer;
begin
  //copy the data so it won't be freed when the previous window closes
  if (groupnr<1) or (groupnr>2) then raise exception.create('invalid parameter');

  setlength(group[groupnr], length(grouplist));

  for i:=0 to length(grouplist)-1 do
  begin
    group[groupnr][i]:=TAddressEntry.Create;
    group[groupnr][i].address:=grouplist[i].address;
    group[groupnr][i].context:=grouplist[i].context;
    if grouplist[i].stack.stack<>nil then
    begin
      getmem(group[groupnr][i].stack.stack, grouplist[i].stack.savedsize);
      CopyMemory(group[groupnr][i].stack.stack, grouplist[i].stack.stack, grouplist[i].stack.savedsize);
      group[groupnr][i].stack.savedsize:=grouplist[i].stack.savedsize;
    end;
  end;
end;

procedure TfrmChangedAddressesCommonalityScanner.initlist;
var
  registers: record //list to keep track of which registers are different
    RAX: boolean;
    RBX: boolean;
    RCX: boolean;
    RDX: boolean;
    RSI: boolean;
    RDI: boolean;
    RBP: boolean;
    R8: boolean;
    R9: boolean;
    R10: boolean;
    R11: boolean;
    R12: boolean;
    R13: boolean;
    R14: boolean;
    R15: boolean;
  end;

  li: TListItem;
  ri: TRegisterInfo;
  i,j,k: integer;

  s: string;
  allreadable: boolean;
  b: byte;
  x: ptruint;
begin
  //todo for next version
  //check the register values in g1 and g2 for commonalities
  ZeroMemory(@registers, sizeof(registers));

  for i:=0 to length(group[1])-1 do
  begin
    for j:=0 to length(group[2])-1 do
    begin
      if (group[1][i].context.{$ifdef cpu64}RAX{$else}EAX{$endif}=group[2][j].context.{$ifdef cpu64}RAX{$else}EAX{$endif}) then registers.rax:=true;
      if (group[1][i].context.{$ifdef cpu64}RBX{$else}EBX{$endif}=group[2][j].context.{$ifdef cpu64}RBX{$else}EBX{$endif}) then registers.rbx:=true;
      if (group[1][i].context.{$ifdef cpu64}RCX{$else}Ecx{$endif}=group[2][j].context.{$ifdef cpu64}RCX{$else}Ecx{$endif}) then registers.rcx:=true;
      if (group[1][i].context.{$ifdef cpu64}RDX{$else}Edx{$endif}=group[2][j].context.{$ifdef cpu64}RDX{$else}Edx{$endif}) then registers.rdx:=true;
      if (group[1][i].context.{$ifdef cpu64}RSI{$else}Esi{$endif}=group[2][j].context.{$ifdef cpu64}RSI{$else}Esi{$endif}) then registers.rsi:=true;
      if (group[1][i].context.{$ifdef cpu64}RDI{$else}Edi{$endif}=group[2][j].context.{$ifdef cpu64}RDI{$else}Edi{$endif}) then registers.rdi:=true;
      if (group[1][i].context.{$ifdef cpu64}RBP{$else}Ebp{$endif}=group[2][j].context.{$ifdef cpu64}RBP{$else}Ebp{$endif}) then registers.rbp:=true;
      {$ifdef cpu64}
      if (group[1][i].context.R8=group[2][j].context.r8) then registers.r8:=true;
      if (group[1][i].context.R9=group[2][j].context.r9) then registers.r9:=true;
      if (group[1][i].context.R10=group[2][j].context.r10) then registers.r10:=true;
      if (group[1][i].context.R11=group[2][j].context.r11) then registers.r11:=true;
      if (group[1][i].context.R12=group[2][j].context.r12) then registers.r12:=true;
      if (group[1][i].context.R13=group[2][j].context.r13) then registers.r13:=true;
      if (group[1][i].context.R14=group[2][j].context.r14) then registers.r14:=true;
      if (group[1][i].context.R15=group[2][j].context.r15) then registers.r15:=true;
      {$endif}
    end;
  end;

  //do not bother with the registers that are the same in g1 and g2
  //start a compare on the registers
  if registers.rax=false then
  begin
    li:=lvregisters.Items.Add;
    if processhandler.is64Bit then li.caption:='RAX' else li.caption:='EAX';
    li.data:=pointer(tregisterinfo.Create(self, 0));
  end;

  if registers.rbx=false then
  begin
    li:=lvregisters.Items.Add;
    if processhandler.is64Bit then li.caption:='RBX' else li.caption:='EBX';
    li.data:=pointer(tregisterinfo.Create(self, 1));
  end;

  if registers.rcx=false then
  begin
    li:=lvregisters.Items.Add;
    if processhandler.is64Bit then li.caption:='RCX' else li.caption:='ECX';
    li.data:=pointer(tregisterinfo.Create(self, 2));
  end;

  if registers.rdx=false then
  begin
    li:=lvregisters.Items.Add;
    if processhandler.is64Bit then li.caption:='RDX' else li.caption:='EDX';
    li.data:=pointer(tregisterinfo.Create(self, 3));
  end;

  if registers.rsi=false then
  begin
    li:=lvregisters.Items.Add;
    if processhandler.is64Bit then li.caption:='RSI' else li.caption:='ESI';
    li.data:=pointer(tregisterinfo.Create(self, 4));
  end;

  if registers.rdi=false then
  begin
    li:=lvregisters.Items.Add;
    if processhandler.is64Bit then li.caption:='RDI' else li.caption:='EDI';
    li.data:=pointer(tregisterinfo.Create(self, 5));
  end;

  if registers.rbp=false then
  begin
    li:=lvregisters.Items.Add;
    if processhandler.is64Bit then li.caption:='RBP' else li.caption:='EBP';
    li.data:=pointer(tregisterinfo.Create(self, 6));
  end;

  li:=lvregisters.Items.Add;
  if processhandler.is64Bit then li.caption:='RSP (Snapshot)' else li.caption:='RSP (Snapshot)';
  li.data:=pointer(tregisterinfo.Create(self, 7));

  if processhandler.is64bit then
  begin
    if registers.r8=false then
    begin
      li:=lvregisters.Items.Add;
      li.caption:='R8';
      li.data:=pointer(tregisterinfo.Create(self, 8));
    end;

    if registers.r9=false then
    begin
      li:=lvregisters.Items.Add;
      li.caption:='R9';
      li.data:=pointer(tregisterinfo.Create(self, 9));
    end;

    if registers.r10=false then
    begin
      li:=lvregisters.Items.Add;
      li.caption:='R10';
      li.data:=pointer(tregisterinfo.Create(self, 10));
    end;

    if registers.r11=false then
    begin
      li:=lvregisters.Items.Add;
      li.caption:='R11';
      li.data:=pointer(tregisterinfo.Create(self, 11));
    end;

    if registers.r12=false then
    begin
      li:=lvregisters.Items.Add;
      li.caption:='R12';
      li.data:=pointer(tregisterinfo.Create(self, 12));
    end;

    if registers.r13=false then
    begin
      li:=lvregisters.Items.Add;
      li.caption:='R13';
      li.data:=pointer(tregisterinfo.Create(self, 13));
    end;

    if registers.r14=false then
    begin
      li:=lvregisters.Items.Add;
      li.caption:='R14';
      li.data:=pointer(tregisterinfo.Create(self, 14));
    end;

    if registers.r15=false then
    begin
      li:=lvregisters.Items.Add;
      li.caption:='R15';
      li.data:=pointer(tregisterinfo.Create(self, 15));
    end;
  end;

  i:=0;
  while i<lvRegisters.items.count do
  begin
    ri:=tregisterinfo(lvRegisters.Items[i].data);
    s:='';
    if ri.g1same then
      s:='"Group 1" has common value 0x'+inttohex(ri.values[1][0],1);

    if ri.g2same then
    begin
      if s<>'' then
        s:=s+' and ';
      s:=s+'"Group 2" has common value 0x'+inttohex(ri.values[2][0],1);
    end;

    //check if all addresses are readable
    allreadable:=true;
    for j:=1 to 2 do
    begin
      for k:=0 to length(ri.values[j])-1 do
        if readprocessmemory(processhandle, pointer(ri.values[j][k]),@b,1,x)=false then
        begin
          allreadable:=false;
          break;
        end;

      if allreadable=false then break;
    end;

    if allreadable then
    begin
      if s<>'' then
        s:=s+' or ';

      s:=s+rsDblClickLaunchComp;

      ri.validaddress:=true;
    end;

    if s<>'' then
    begin
      lvRegisters.Items[i].SubItems.add(s);
      inc(i);
    end
    else
    begin
      //I can't do anything with this. Not an address, no value commonalities, useless
      lvRegisters.items[i].Delete;
      ri.free;
    end;
  end;
end;

end.

