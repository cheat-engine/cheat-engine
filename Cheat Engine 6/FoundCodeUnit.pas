unit FoundCodeUnit;

{$MODE Delphi}

interface

uses
  windows, LCLIntf, LResources, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,disassembler,CEFuncProc,ExtCtrls, Menus, NewKernelHandler, clipbrd;

type Tcoderecord = class
  public
  address: ptrUint;
  size: integer;
  opcode: string;
  description: string;
  eax,ebx,ecx,edx,esi,edi,ebp,esp,eip: dword;
  context: TContext;
end;

type

  { TFoundCodeDialog }

  TFoundCodeDialog = class(TForm)
    FoundcodeList: TListBox;
    Panel1: TPanel;
    Description: TLabel;
    pmOptions: TPopupMenu;
    ReplacewithcodethatdoesnothingNOP1: TMenuItem;
    Showthisaddressinthedisassembler1: TMenuItem;
    Addtothecodelist1: TMenuItem;
    MoreInfo1: TMenuItem;
    Panel2: TPanel;
    btnOK: TButton;
    Panel3: TPanel;
    btnExtraInfo: TButton;
    btnAddToCodeList: TButton;
    btnOpenDisassembler: TButton;
    btnReplacewithnops: TButton;
    N1: TMenuItem;
    Copyselectiontoclipboard1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FoundcodeListClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnReplacewithnopsClick(Sender: TObject);
    procedure btnOpenDisassemblerClick(Sender: TObject);
    procedure btnAddToCodeListClick(Sender: TObject);
    procedure FoundcodeListDblClick(Sender: TObject);
    procedure btnExtraInfoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FoundcodeListContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure pmOptionsPopup(Sender: TObject);
    procedure Copyselectiontoclipboard1Click(Sender: TObject);
  private
    { Private declarations }
    procedure moreinfo;
  public
    { Public declarations }
    useexceptions: boolean;
    usesdebugregs: boolean;
    procedure AddRecord;

  end;


resourcestring
  strClose='Close';

var
  FoundCodeDialog: TFoundCodeDialog;

implementation

uses CEDebugger,debughelper, debugeventhandler,
     MemoryBrowserFormUnit,
     {$ifdef net}unit2,ceclient,{$else}MainUnit,kerneldebugger,{$endif}
     AdvancedOptionsUnit ,formFoundcodeListExtraUnit,MainUnit2;

procedure TFoundCodedialog.AddRecord;
{
Invoked by the debugger thread
It takes the data from the current thread and stores it in the processlist
}
var currentthread: TDebugThreadHandler;
  opcode: string;
  address,address2: ptrUint;
  desc: string;

  coderecord: TCodeRecord;
  i: integer;
begin
  //the debuggerthread is idle at this point
  currentThread:=debuggerthread.CurrentThread;
  if currentthread<>nil then
  begin
    address:=currentThread.context.{$ifdef cpu64}Rip{$else}eip{$endif};
    if usesdebugregs then //find out the previous opcode
      address:=previousopcode(address);

    //check if address is inside the list
    for i:=0 to foundcodelist.Items.Count-1 do
      if TCodeRecord(foundcodelist.Items.Objects[i]).address=address then exit; //it's already in the list

    //disassemble to get the opcode and size
    address2:=address;
    opcode:=disassemble(address2,desc);

    coderecord:=TCoderecord.create;
    coderecord.address:=address;
    coderecord.size:=address2-address;
    coderecord.opcode:=opcode;
    coderecord.description:=desc;
    coderecord.context:=currentthread.context^;
    FoundcodeList.Items.AddObject(opcode, tobject(coderecord));
  end;
end;

procedure TFoundCodedialog.moreinfo;
var disassembled: array[1..5] of string;
    address: ptrUint;
    itemindex: integer;
    temp,temp2: string;
    max: dword;
    p: dword;
    i: integer;
    coderecord: TCodeRecord;
    firstchar: string;
begin
  itemindex:=foundcodelist.ItemIndex;
  if itemindex<>-1 then
  begin
    FormFoundCodeListExtra:=TFormFoundCodeListExtra.Create(nil);
    if useexceptions then
      FormFoundCodeListExtra.Label18.Visible:=false
    else
      FormFoundCodeListExtra.Label18.Visible:=true;

    coderecord:=TCodeRecord(foundcodelist.items.objects[itemindex]);

    address:=coderecord.address;
    address:=previousopcode(address);
    address:=previousopcode(address);

    disassembled[1]:=disassemble(address,temp);
    disassembled[2]:=disassemble(address,temp);

    if address<>coderecord.address then
    begin
      disassembled[1]:='';
      disassembled[2]:='';
      disassembled[3]:=coderecord.opcode;
      disassembled[4]:='';
      disassembled[5]:='';
    end
    else
    begin
      disassembled[3]:=disassemble(address,temp);
      disassembled[4]:=disassemble(address,temp);
      disassembled[5]:=disassemble(address,temp);
    end;

    //convert disassembled strings to address+opcode only (no bytes)
    //xxxxxxxx - xx xx xx - opcode
    temp:=copy(disassembled[1],pos('-',disassembled[1])+2,length(disassembled[1]));
    temp:=copy(temp,pos('-',temp)+2,length(temp));
    disassembled[1]:=copy(disassembled[1],1,pos('-',disassembled[1]))+' '+temp;

    temp:=copy(disassembled[2],pos('-',disassembled[2])+2,length(disassembled[2]));
    temp:=copy(temp,pos('-',temp)+2,length(temp));
    disassembled[2]:=copy(disassembled[2],1,pos('-',disassembled[2]))+' '+temp;

    temp:=copy(disassembled[3],pos('-',disassembled[3])+2,length(disassembled[3]));
    temp:=copy(temp,pos('-',temp)+2,length(temp));
    disassembled[3]:=copy(disassembled[3],1,pos('-',disassembled[3]))+' '+temp;

    temp:=copy(disassembled[4],pos('-',disassembled[4])+2,length(disassembled[4]));
    temp:=copy(temp,pos('-',temp)+2,length(temp));
    disassembled[4]:=copy(disassembled[4],1,pos('-',disassembled[4]))+' '+temp;

    temp:=copy(disassembled[5],pos('-',disassembled[5])+2,length(disassembled[5]));
    temp:=copy(temp,pos('-',temp)+2,length(temp));
    disassembled[5]:=copy(disassembled[5],1,pos('-',disassembled[5]))+' '+temp;



    with FormFoundCodeListExtra do
    begin
      Label1.Caption:=disassembled[1];
      Label2.Caption:=disassembled[2];
      Label3.Caption:=disassembled[3];
      Label4.Caption:=disassembled[4];
      Label5.Caption:=disassembled[5];


      if processhandler.is64bit then
        firstchar:='R' else firstchar:='E';

      label7.caption:=firstchar+'AX='+IntToHex(coderecord.context.{$ifdef cpu64}Rax{$else}Eax{$endif},8);
      label8.caption:=firstchar+'BX='+IntToHex(coderecord.context.{$ifdef cpu64}Rbx{$else}Ebx{$endif},8);
      label9.caption:=firstchar+'CX='+IntToHex(coderecord.context.{$ifdef cpu64}Rcx{$else}Ecx{$endif},8);
      label11.caption:=firstchar+'DX='+IntToHex(coderecord.context.{$ifdef cpu64}Rdx{$else}Edx{$endif},8);
      label16.caption:=firstchar+'SI='+IntToHex(coderecord.context.{$ifdef cpu64}Rsi{$else}Esi{$endif},8);
      label14.caption:=firstchar+'DI='+IntToHex(coderecord.context.{$ifdef cpu64}Rdi{$else}Edi{$endif},8);
      label13.caption:=firstchar+'SP='+IntToHex(coderecord.context.{$ifdef cpu64}Rsp{$else}Esp{$endif},8);
      label12.caption:=firstchar+'BP='+IntToHex(coderecord.context.{$ifdef cpu64}Rbp{$else}Ebp{$endif},8);
      label15.caption:=firstchar+'IP='+IntToHex(coderecord.context.{$ifdef cpu64}Rip{$else}Eip{$endif},8);

      {$ifdef cpu64}
      //r8
      //r9
      //...
      {$endif}
      label6.Caption:=coderecord.description;
    end;

    //parse the disassembled[3] string to help the user find the pointer
    //first find the [xxx]

    temp:=copy(disassembled[3],pos('[',disassembled[3])+1,(pos(']',disassembled[3])-1)-(pos('[',disassembled[3])));
    if temp<>'' then
    begin
      //parse
      //find the biggest value, registers or exact value
      max:=0;
      if pos('eax',temp)>0 then if coderecord.eax>max then max:=coderecord.eax;
      if pos('ebx',temp)>0 then if coderecord.ebx>max then max:=coderecord.ebx;
      if pos('ecx',temp)>0 then if coderecord.ecx>max then max:=coderecord.ecx;
      if pos('edx',temp)>0 then if coderecord.edx>max then max:=coderecord.edx;
      if pos('edi',temp)>0 then if coderecord.edi>max then max:=coderecord.edi;
      if pos('esi',temp)>0 then if coderecord.esi>max then max:=coderecord.esi;
      if pos('ebp',temp)>0 then if coderecord.ebp>max then max:=coderecord.ebp;
      if pos('esp',temp)>0 then if coderecord.esp>max then max:=coderecord.esp;

      //the offset is always at the end, so read from back to front
      temp2:='';
      for i:=length(temp) downto 1 do
        if temp[i] in ['0'..'9','a'..'f'] then temp2:=temp[i]+temp2 else break;

      if temp2<>'' then //I know this isn't completly correct e.g: [eax*4] but even then the 4 will NEVER be bigger than eax (unless it's to cause a crash)
      begin
        p:=StrToInt('$'+temp2);
        if p>max then max:=p;
      end;

      formfoundcodelistextra.probably:=max;
    end else formfoundcodelistextra.label17.caption:='';


    maX:=formfoundcodelistextra.width;
    if formfoundcodelistextra.label1.width>max then max:=formfoundcodelistextra.label1.width;
    if formfoundcodelistextra.label2.width>max then max:=formfoundcodelistextra.label2.width;
    if formfoundcodelistextra.label3.width>max then max:=formfoundcodelistextra.label3.width;
    if formfoundcodelistextra.label4.width>max then max:=formfoundcodelistextra.label4.width;
    if formfoundcodelistextra.label5.width>max then max:=formfoundcodelistextra.label5.width;
 //   if formfoundcodelistextra.Label17.width>max then max:=formfoundcodelistextra.Label17.width;

    if max<>formfoundcodelistextra.Width then
      formfoundcodelistextra.Width:=max+5;


    formfoundcodelistextra.context:=coderecord.context;
    FormFoundCodeListExtra.Show;
  //  FormFoundCodeListExtra.free;
  end;
end;

procedure TFoundCodeDialog.FoundcodeListClick(Sender: TObject);
var coderecord: TCodeRecord;
begin
  if foundcodelist.SelCount>0 then
  begin
    btnReplacewithnops.enabled:=true;
    btnOpenDisassembler.enabled:=true;
    btnAddToCodeList.enabled:=true;
    btnExtraInfo.Enabled:=true;

    coderecord:=TCodeRecord(foundcodelist.Items.Objects[foundcodelist.itemindex]);
    description.Caption:=coderecord.description;
  end
  else
  begin
    btnReplacewithnops.enabled:=false;
    btnOpenDisassembler.enabled:=false;
    btnAddToCodeList.enabled:=false;
    btnExtraInfo.Enabled:=false;
    if foundcodelist.Items.Count=0 then
      description.caption:='Use the game/application for a while and make the address you''re watching change. The list will be filled with addresses that contain code that change the watched address.'
    else
      description.caption:='Select a item from the list for a small description';
  end;
end;

procedure TFoundCodeDialog.FormCreate(Sender: TObject);
begin
  btnOk.caption:=strStop;
end;

procedure TFoundCodeDialog.btnOKClick(Sender: TObject);
var original: dword;
    i: integer;
begin
  if btnOK.caption=strStop then
  begin
    if kdebugger.isactive then
    begin
      //todo: Make this only disable the related breakpoints
      kdebugger.DisableAllBreakpoints;
      btnOK.caption:=strClose;
    end
    else
    begin
      debuggerthread.CodeFinderStop(self);
      btnOK.caption:=strClose;
    end;
  end else close;


end;

procedure TFoundCodeDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  action:=caFree;
  foundcodedialog:=nil;
end;

procedure TFoundCodeDialog.btnReplacewithnopsClick(Sender: TObject);
var codelength: integer;
    written: dword;
    i,j: integer;
    nops: array of byte;
    a: dword;
    original: dword;

    mbi : _MEMORY_BASIC_INFORMATION;

    coderecord: TCodeRecord;
begin

  with foundcodelist do
  begin
    for j:=0 to foundcodelist.Count-1 do
    begin
      if foundcodelist.Selected[j] then
      begin
        coderecord:=TcodeRecord(foundcodelist.items.objects[j]);
        codelength:=coderecord.size;
        //add it to the codelist
        if advancedoptions.AddToCodeList(coderecord.address,coderecord.size,true, foundcodelist.SelCount>1) then
        begin
          setlength(nops,codelength);
          for i:=0 to codelength-1 do
            nops[i]:=$90;  //$90=nop


          zeromemory(@mbi,sizeof(mbi));

          if debuggerthread<>nil then debuggerthread.Suspend;
          RewriteCode(processhandle,coderecord.address,@nops[0],codelength);
          if debuggerthread<>nil then debuggerthread.Resume;
        end;
      end;
    end;
  end;


end;

procedure TFoundCodeDialog.btnOpenDisassemblerClick(Sender: TObject);
var coderecord: TCodeRecord;
begin
  if foundcodelist.itemindex<>-1 then
  begin
    coderecord:=TcodeRecord(foundcodelist.items.objects[foundcodelist.itemindex]);
    memorybrowser.disassemblerview.SelectedAddress:=coderecord.address;
    memorybrowser.panel1.visible:=true;
    memorybrowser.show;
  end;
end;

procedure TFoundCodeDialog.btnAddToCodeListClick(Sender: TObject);
var i: integer;
    coderecord: TCodeRecord;
begin

  for i:=0 to foundcodelist.count-1 do
  begin
    if foundcodelist.Selected[i] then
    begin
      coderecord:=TcodeRecord(foundcodelist.items.objects[i]);
      advancedoptions.AddToCodeList(coderecord.address,coderecord.size,false, foundcodelist.SelCount>1);
    end;
  end;
  advancedoptions.Show;
end;

procedure TFoundCodeDialog.FoundcodeListDblClick(Sender: TObject);
begin
  MoreInfo;
end;

procedure TFoundCodeDialog.btnExtraInfoClick(Sender: TObject);
begin
  moreinfo;
end;

procedure TFoundCodeDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if btnOK.caption=strStop then btnOK.Click;
  CanClose:=true;
end;

procedure TFoundCodeDialog.FoundcodeListContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
var selected: boolean;
begin
  foundcodelist.ItemIndex:=foundcodelist.ItemAtPos(mousepos,true);

  selected:=foundcodelist.itemindex<>-1;
  ReplacewithcodethatdoesnothingNOP1.Enabled:=selected;
  Showthisaddressinthedisassembler1.enabled:=selected;
  Addtothecodelist1.enabled:=selected;
  MoreInfo1.Enabled:=selected;
end;

procedure TFoundCodeDialog.pmOptionsPopup(Sender: TObject);
begin
  n1.visible:=foundcodelist.ItemIndex<>-1;
  Copyselectiontoclipboard1.visible:=foundcodelist.ItemIndex<>-1;
end;

procedure TFoundCodeDialog.Copyselectiontoclipboard1Click(Sender: TObject);
var
  i: integer;
  s: string;
begin
  s:='';
  for i:=0 to FoundcodeList.Items.count-1 do
    if FoundcodeList.Selected[i] then
      s:=s+FoundcodeList.Items[i]+#13#10;

  clipboard.AsText:=s;
end;

initialization
  {$i FoundCodeUnit.lrs}

end.
