unit FoundCodeUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,disassembler,cefuncproc{$ifdef net},NetAPIs{$endif};

type Tcoderecord = record
  address: dword;
  size: integer;
  opcode: string;
  desciption: string;
  eax,ebx,ecx,edx,esi,edi,ebp,esp,eip: dword;
end;

type
  TFoundCodeDialog = class(TForm)
    btnOK: TButton;
    FoundcodeList: TListBox;
    Description: TLabel;
    btnReplacewithnops: TButton;
    btnOpenDisassembler: TButton;
    btnAddToCodeList: TButton;
    btnExtraInfo: TButton;
    procedure FoundcodeListClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnReplacewithnopsClick(Sender: TObject);
    procedure btnOpenDisassemblerClick(Sender: TObject);
    procedure btnAddToCodeListClick(Sender: TObject);
    procedure FoundcodeListDblClick(Sender: TObject);
    procedure btnExtraInfoClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
    procedure moreinfo;
  public
    { Public declarations }
    coderecords: array of TCodeRecord;
    useexceptions: boolean;

  end;


resourcestring
  strClose='Close';

var
  FoundCodeDialog: TFoundCodeDialog;

implementation

{$R *.dfm}
uses debugger,
     MemoryBrowserFormUnit,
     {$ifdef net}unit2,ceclient,{$else}MainUnit,debugger2,{$endif}
     advancedoptionsunit ,formFoundcodeListExtraUnit,mainunit2;

procedure TFoundCodedialog.moreinfo;
var disassembled: array[1..5] of string;
    address: dword;
    itemindex: integer;
    temp,temp2: string;
    max: integer;
    p: dword;
    i: integer;
begin
  itemindex:=foundcodelist.ItemIndex;
  if itemindex<>-1 then
  begin
    FormFoundCodeListExtra:=TFormFoundCodeListExtra.Create(nil);
    if useexceptions then
    begin
      FormFoundCodeListExtra.Label18.Visible:=false;
      FormFoundCodeListExtra.Height:=254;
    end
    else
    begin
      FormFoundCodeListExtra.Label18.Visible:=true;
      FormFoundCodeListExtra.Height:=289;
    end;

    address:=coderecords[ItemIndex].address;
    address:=previousopcode(address);
    address:=previousopcode(address);

    disassembled[1]:=disassemble(address,temp);
    disassembled[2]:=disassemble(address,temp);

    if address<>coderecords[itemindex].address then
    begin
      disassembled[1]:='';
      disassembled[2]:='';
      disassembled[3]:=coderecords[itemindex].opcode;
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

      label7.caption:='EAX='+IntToHex(coderecords[itemindex].eax,8);
      label8.caption:='EBX='+IntToHex(coderecords[itemindex].ebx,8);
      label9.caption:='ECX='+IntToHex(coderecords[itemindex].ecx,8);
      label11.caption:='EDX='+IntToHex(coderecords[itemindex].edx,8);
      label16.caption:='ESI='+IntToHex(coderecords[itemindex].esi,8);
      label14.caption:='EDI='+IntToHex(coderecords[itemindex].edi,8);
      label13.caption:='ESP='+IntToHex(coderecords[itemindex].esp,8);
      label12.caption:='EBP='+IntToHex(coderecords[itemindex].ebp,8);
      label15.caption:='EIP='+IntToHex(coderecords[itemindex].eip,8);

      label6.Caption:=coderecords[itemindex].desciption;
    end;

    //parse the disassembled[3] string to help the user find the pointer
    //first find the [xxx]

    temp:=copy(disassembled[3],pos('[',disassembled[3])+1,(pos(']',disassembled[3])-1)-(pos('[',disassembled[3])));
    if temp<>'' then
    begin
      //parse
      //find the biggest value, registers or exact value
      max:=0;
      if pos('eax',temp)>0 then if coderecords[itemindex].eax>max then max:=coderecords[itemindex].eax;
      if pos('ebx',temp)>0 then if coderecords[itemindex].ebx>max then max:=coderecords[itemindex].ebx;
      if pos('ecx',temp)>0 then if coderecords[itemindex].ecx>max then max:=coderecords[itemindex].ecx;
      if pos('edx',temp)>0 then if coderecords[itemindex].edx>max then max:=coderecords[itemindex].edx;
      if pos('edi',temp)>0 then if coderecords[itemindex].edi>max then max:=coderecords[itemindex].edi;
      if pos('esi',temp)>0 then if coderecords[itemindex].esi>max then max:=coderecords[itemindex].esi;
      if pos('ebp',temp)>0 then if coderecords[itemindex].ebp>max then max:=coderecords[itemindex].ebp;
      if pos('esp',temp)>0 then if coderecords[itemindex].esp>max then max:=coderecords[itemindex].esp;

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


    FormFoundCodeListExtra.Show;
  //  FormFoundCodeListExtra.free;
  end;
end;

procedure TFoundCodeDialog.FoundcodeListClick(Sender: TObject);
begin
  if foundcodelist.ItemIndex<>-1 then
  begin
    btnReplacewithnops.enabled:=true;
    btnOpenDisassembler.enabled:=true;
    btnAddToCodeList.enabled:=true;
    btnExtraInfo.Enabled:=true;
    description.Caption:=coderecords[foundcodelist.itemindex].desciption;
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

procedure TFoundCodeDialog.btnOKClick(Sender: TObject);
var original: dword;
    i: integer;
begin
  {$ifndef net}
  if btnOK.caption=strStop then
  begin
    if debuggerthread2<>nil then
    begin
      //new debugger
      //set the threads back to normal
      debuggerthread2.Terminate;
      debuggerthread2.WaitFor;
      freeandnil(debuggerthread2);
      btnOK.caption:=strClose;
    end
    else
    begin
      try
        crdebugging.Acquire;
        with debuggerthread do
        begin
          if (debuggerthread=nil) or (not debuggerthread.attached) then
          begin
            btnOK.caption:=strClose;
            exit;
          end;

          if findwriter2 then
          begin
            debuggerthread.Suspend;
            zeromemory(@debuggerthread.DRRegs,sizeof(debuggerthread.DRRegs));
            debuggerthread.DRRegs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
            debuggerthread.DRRegs.Dr7:=reg0set or reg1set or reg2set or reg3set;

            for i:=0 to length(debuggerthread.threadlist)-1 do
            begin
              suspendthread(debuggerthread.threadlist[i][1]);
              SetThreadContext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs);
              resumethread(debuggerthread.threadlist[i][1]);
            end;

            debuggerthread.FindWriter2:=false;
            debuggerthread.Resume;
          end
          else
          begin
            if WaitForSingleObject(semaphore,30000)=WAIT_FAILED then
            begin
              messagedlg('Timeout on stopping the code finder, the debugger has crashed!',mtError,[mbok],0);
              debuggerthread.Terminate;
              debuggerthread.free;
              debuggerthread:=nil;

              closehandle(debugger.Semaphore);
              debugger.Semaphore:=createsemaphore(nil,1,1,nil);
              terminateprocess(processhandle,0);
              close;
              exit;
            end;

            //set the original protection back

            debuggerthread.readonlyset:=false;
            debuggerthread.findreaderset:=false;
            debuggerthread.alsowrites:=false;

            debuggerthread.readonlyremoved:=true;
            debuggerthread.findreaderremoved:=true;

            if debuggerthread.readonlyset then
              VirtualProtectEx(processhandle,pointer(readonly.Address),readonly.size,readonly.originalprotection,original);

            if debuggerthread.findreaderset then
              VirtualProtectEx(processhandle,pointer(findreader.Address),findreader.size,findreader.originalprotection,original);


            //set the read-only flag to false
            releasesemaphore(semaphore,1,nil);
          end;
          btnOK.caption:=strClose;
        end;

      finally
        crdebugging.release;
      end;

    end;


  end
  else close;
  
  {$else}
  //handle network version stop here
  if btnOK.caption=strStop then
  begin
    output[0]:=CS_StopCodefinder;
    sendbuf(1);
    btnreplacewithnops.Enabled:=true;
    btnOpenDisassembler.Enabled:=true;
    btnAddToCodeList.Enabled:=true;
    btnOK.caption:=strClose;
  end else close;

  {$endif}
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
    i: integer;
    nops: array of byte;
    a: dword;
    original: dword;

    mbi : _MEMORY_BASIC_INFORMATION;
  //set the protectionlabel
begin
  with foundcodelist do
  begin
    codelength:=coderecords[itemindex].size;
    //add it to the codelist
    advancedoptions.AddToCodeList(coderecords[itemindex].address,codelength,true);

    setlength(nops,codelength);
    for i:=0 to codelength-1 do
      nops[i]:=$90;  //$90=nop


    zeromemory(@mbi,sizeof(mbi));

    if debuggerthread<>nil then debuggerthread.Suspend;
    RewriteCode(processhandle,coderecords[itemindex].address,@nops[0],codelength);
    if debuggerthread<>nil then debuggerthread.Resume;
  end;
end;

procedure TFoundCodeDialog.btnOpenDisassemblerClick(Sender: TObject);
begin
  memorybrowser.Disassembleraddress:=coderecords[foundcodelist.itemindex].address;
  memorybrowser.panel1.visible:=true;
  memorybrowser.show;

end;

procedure TFoundCodeDialog.btnAddToCodeListClick(Sender: TObject);
begin
  advancedoptions.AddToCodeList(coderecords[foundcodelist.itemindex].address,coderecords[foundcodelist.itemindex].size,false);
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

end.
