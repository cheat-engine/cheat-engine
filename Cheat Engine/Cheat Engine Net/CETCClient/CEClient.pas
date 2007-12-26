unit CEClient;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ScktComp, ExtCtrls,SyncObjs,registry,winsock, IdBaseComponent,
  IdComponent, IdTCPConnection, IdTCPClient;


const
  //Client to server communication
  CS_GetProcessList=0; //no param
  CS_GetWindowList=1; //no param
  CS_OpenProcess=2; //procid: dword
  CS_AddAddress=3; // Address:dword;valtype:byte;bitnr:byte;length:byte
  CS_RefreshList=4; //Start:word;stop:word
  CS_SetConfig=5; //ShowAsSigned:byte BinariesAsDecimal:byte max:word; buffersize:dword;skip_page_no_cache: byte;UseDebugRegs:byte;UseDBKQueryMemoryRegion:byte;UseDBKReadWriteMemory:byte;UseDBKOpenProcess:byte)
  CS_ClearRecordList=6; //no param
  CS_ChangeValueOfAddress=7; //recnr: word; length:byte; newvalue:string
  CS_FreezeAddress=8; //recnr: word
  CS_ReadProcessMemory=9; //address:dword; length:word;
  CS_WriteProcessMemory=10; //address:dword; length:word; bytes:array of byte
  CS_FirstScan=11; //start,stop:dword;Scantype:byte;vartype:byte;scanvaluelength:byte;scanvalue:string;scanoptions:byte
  CS_NextScan=12;
  CS_NewScan=13;
  CS_CancelScan=14;
  CS_DeleteAddress=15; //recnr:word
  CS_SetTimerSpeed=16; //Updateinterval:word; freezeinterval:word
  CS_UnfreezeAddress=17; //recnr:word
  CS_ProcessItemAck=18;
  CS_SetHyperScanState=19; //(state:byte); //0=off 1=on
  CS_EnableSpeedhack=20; //(speed:single;sleeptime:dword)
  CS_DisableSpeedhack=21; //
  CS_EnableDebugger=22; //
  CS_FindWhatWrites=23;  //(address:dword;size:word)
  CS_FindWhatReads=24;   //(address:dword;size:word)
  CS_FindWhatAccesses=25;//(address:dword;size:word)
  CS_StopCodefinder=26;
  CS_VirtualProtectEx=27; //(Address: dword; dwSize:dword; NewProtect: DWORD);
  CS_SuspenProcess=28;
  CS_ResumeProcess=29;
  CS_Password=30; //Password(passwordlength:byte;password:string);

  CS_TerminateServer=252;


  //server to client communication
  SC_TellUpdateSpeed=0; //updatespeed:word
  SC_TellFreezeSpeed=1; //freezespeed:word
  SC_ProcessListItem=2; //processid:dword; stringlength:byte;processname:array of char
  SC_StopProcessList=3; // noparam
  SC_OpenProcessSuccess=4; //'
  SC_OpenProcessFailed=5;
  SC_RecordReceived=6; //Indicated that the record has been received  (not really necesary)
  SC_ValueUpdated=7; ////valuelist(Entry:word; memorysize:word; readmemory: array of bytes)
  SC_ValueListDone=8; //All values have been sent to the client
  SC_DebugRegsPresent=9;
  SC_ValueChanged=10; //0=success 1=Incorrect value 2=unwritable 3=record doesn't exist

  SC_ReadProcessMemoryResult=11;  //(successboolean: byte; actualread: word; bytesread: array of byte)
  SC_WriteProcessMemoryResult=12; //(successboolean: byte; actualwritten: word)

  SC_ScanResultCount=13; //count:int64
  SC_ScanResult=14; //address:dword; valuesize:byte; value:array of bytes
  SC_AddressUnfrozen=15; //recnr: word;
  SC_UpdateProgressbar=16; //max:dword; position:dword
  //SC_ScanFailed=17; //obsolete replaced by a negative scanresult value
  SC_Disconnect=18;

  SC_Hyperscanstatus=19; //status:byte //0=off 1=on
  SC_SpeedhackStatus=20; //status:byte
  SC_DebuggerStatus=21; //status: byte 0=off 1=on

  SC_FoundCode=22; //(Address: dword;eax:dword; ebx:dword; ecx:dword; edx:dword;esi:dword;edi:dword;ebp:dword;esp:dword;eip:dword;)
  SC_VirtualProtectExResult=23; //(status:byte; oldprotecT:dword); //status 0=failed 1=success

const
  Exact_value = 0;
  Increased_value = 1;
  Increased_value_by = 2;
  Decreased_value = 3;
  Decreased_value_by = 4;
  Changed_value = 5;
  Unchanged_value = 6;
  Advanced_Scan = 7;
  String_Scan = 8;
  SmallerThan = 9;
  BiggerThan = 10;

type
  MemoryRecord = record
        Description : string[50];
        Address : dword;
        VarType : byte;
        Bit     : Byte;
        BitLength: byte;
        Frozen : boolean;
        FrozenValue : Int64;
        FrozenDirection: integer;
        ShowAsHex: Boolean;
        Group:  Byte;
  end;

type
  MemoryRecordOld = record
        Description : string[50];
        Address : dword;
        VarType : byte;
        Frozen : boolean;
        FrozenValue : Dword;
  end;


type THandlecommandThread=class(TThreaD)
  private
    currentcommand: integer;

  public
    go: boolean;
    procedure Execute; override;
    procedure HCommand;
end;

type
  TConnectForm = class(TForm)
    Button1: TButton;
    EditPort: TEdit;
    TimeOutTimer: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    editAddress: TComboBox;
    IdTCPClient1: TIdTCPClient;
    Timer1: TTimer;
    editPassword: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimeOutTimerTimer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    imalive: boolean;
    a,b,c,d,e: string;

    procedure ThreadMessage(var message:TMessage); message wm_user+1;
  public
    { Public declarations }
    recbuf: TMemorystream;
    hct: THandlecommandThread;
    procedure waitforCommand(command:byte);
    procedure handleCommand(command: byte);
    procedure ReceiveBuf(var Buf; Count: Integer);
  end;

type rec = record
  Command: Byte;
  {
    0=request process list (no items)
     +PROCESS:process+chr(0)
     +PROCESSLIST:0
     -PROCESSLIST:1
    1=request window list (no items)
    2=open processid; (processid)
    3=add record (address,valtype,bit)
    4=delete address (index)
    5=change freeze timer (freezetimer)
    6=change update timer (updatetimer)
    7=get memoryranges (start,stop,scanreadonly)
    8=next scan(value,fvalue,scantype,valtype,maximum)
    9=getmemoryrangesandscanmemory(start,stop,scanreadonly,value,fvalue,valtype,maximum);
    10=Change value at address... (index,scanvalue,fvalue,valtype)
    11=request update of list(no item)
    12=Freeze address (index)
    13=Unfreeze address (index)
    14=Change type of address X (integer,valtype)
    15=Change record X (address,valtype,bit)
  }
  processid: DWORD; //some of the items aren't being used!
  address: Dword;
  bit:     byte;
  valtype: Byte;
  value:   int64;
  Fvalue:  Double;
  Frozen:  Boolean;
  FreezeTimer: Integer;
  UpdateTimer: Integer;
  start: Dword;
  Stop: Dword;
  Scanreadonly: boolean;
  maximum: Integer;
  scantype: byte;
  scanvalue: string[30];
end;


type tdata=record
  dw1,dw2,dw3: dword;
  st: pchar;
end;

var
  ConnectForm: TConnectForm;
  output: array [0..2049] of byte;
  send: rec; //deprecated

  readevents: integer;

  ReadProcessMemoryEvent: TEvent;
  ReadProcessMemoryEventDone: TEvent;
  WriteProcessMemoryEvent: TEvent;
  WriteProcessMemoryEventDone: TEvent;
  Debuggerstatusevent: TEvent;
  Debuggerstatuseventdone: tevent;
  VirtualProtectExevent: TEvent;
  VirtualProtectExeventdone: tevent;

  ig: boolean;

procedure SendBuf(count: integer);
function ReadProcessMemoryNet(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;

function WriteProcessMemoryNet(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL;

function VirtualProtectExNet(hProcess: THandle; lpAddress: Pointer; dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL;

implementation

uses Unit2, processwindow, Unit1, Unit3, changetimerunit, formsettingsunit,
  MemoryBrowserFormUnit, formScanningUnit, FoundCodeUnit,disassembler,cefuncproc,mainunit2;

{$R *.DFM}


procedure THandlecommandThread.Execute;
begin
  freeonterminate:=true;
  go:=true;
  while not terminated do
  begin
    try
      connectform.IdTCPClient1.ReadBuffer(currentcommand,1);
      case currentcommand of

        SC_ReadProcessMemoryResult:
        begin
          outputdebugstring('readprocessmemory result');
          readprocessmemoryevent.SetEvent;
          readprocessmemoryeventdone.waitfor(infinite);
          readprocessmemoryeventdone.ResetEvent;
          continue;
        end;

        SC_WriteProcessMemoryResult:
        begin
          outputdebugstring('Writeprocessmemory result');
          Writeprocessmemoryevent.SetEvent;
          Writeprocessmemoryeventdone.waitfor(infinite);
          Writeprocessmemoryeventdone.ResetEvent;
          continue;
        end;

        SC_DebuggerStatus:
        begin
          outputdebugstring('Debuggerstatus result');
          Debuggerstatusevent.SetEvent;
          Debuggerstatuseventdone.waitfor(infinite);
          Debuggerstatuseventdone.ResetEvent;
          continue;
        end;

        SC_VirtualProtectExResult:
        begin
          outputdebugstring('VirtualProtectExEx result');
          VirtualProtectExevent.SetEvent;
          VirtualProtectExeventdone.waitfor(infinite);
          VirtualProtectExeventdone.ResetEvent;
          continue;
        end;



        else
        begin
          hcommand;
         // synchronize(hcommand); //all other commands that need to be thread safe
        end;
      end;
    except
      outputdebugstring('crash while reading the buffer');
    end;
  end;
end;

procedure THandlecommandThread.HCommand;
begin
  connectform.handleCommand(currentcommand);
end;

procedure TConnectform.ThreadMessage(var message:TMessage);
var st:pchar;
    data: ^tdata;
    data2b,data3b: array of byte;
    data2: pointer absolute data2b;

    address,addressfound:dword;
    opcode,desc:string;
    i: integer;
begin
  case message.wParam of
    SC_TELLUPDATESPEED:  mainform.StatusBar1.Panels[1].Text:='Update timer speed';
    SC_TellFreezeSpeed:  mainform.StatusBar1.Panels[1].Text:='Freeze timer speed';
    SC_ProcessListItem:
    begin
      mainform.StatusBar1.Panels[1].Text:='Receiving process list';

      st:=pointer(message.LParam);
      if proceswindow<>nil then
      begin
        proceswindow.SetProcessWindowState(false);
        proceswindow.ListBox1.Items.Add(st);
      end;
      freemem(st);
    end;

    SC_StopProcessList:
    begin
      mainform.StatusBar1.Panels[1].Text:='End of list';
      if proceswindow<>nil then
        proceswindow.SetProcessWindowState(true);
    end;

    SC_OpenProcessSuccess:
    begin
      mainform.StatusBar1.Panels[1].Text:='Opened process';
      mainform.processlabel.caption:=mainform.opening;
      mainform.ProcessOpened:=true;
    end;

    SC_OpenProcessFailed:  Messagedlg('Error opening the process',mtError,[mbok],0);
    SC_RecordReceived:  mainform.StatusBar1.Panels[1].Text:='Server received address';
    SC_ValueUpdated:
    begin
      data2:=pointer(message.LParam);
      data3b:=@data2b[4];


      //mainform.memrec[pword(@data2b[0])^].VarType
      mainform.values[pword(@data2b[0])^]:=RawToString(data3b,mainform.memrec[pword(@data2b[0])^].VarType,mainform.memrec[pword(@data2b[0])^].ShowAsHex,pword(@data2b[2])^);
      data3b:=nil;
      freemem(data2);
    end;

    SC_ValueListDone: mainform.ShowValues;
    SC_DebugRegsPresent:
    begin
      with formsettings do
      begin
        rbDebugRegisters.Enabled:=true;
        label6.Enabled:=true;
        label7.Enabled:=true;
      end;
    end;

    SC_ScanResultCount:
    begin
      st:=pointer(message.LParam);
      mainform.FoundLabel.Caption:=st;

      if formscanning<>nil then
      begin
        formscanning.closeme:=true;
        formscanning.ModalResult:=mrOK;
      end;

      try
        i:=strtoint(st);
      except
      //omg something went really wrong.......

      end;

      if i<0 then
      begin
        case i of
        -1: Messagedlg('The type was wrong',mtError,[mbok],0);
        -2: Messagedlg('This option is not supported',mtError,[mbok],0);
        -3: Messagedlg('No readable memory was found',mtError,[mbok],0);
        end;


      end;

      freemem(st);
    end;

    SC_ScanResult:
    begin
      //SC_ScanResult=14; //address:dword; valuesize:byte; value:array of bytes
      data2:=pointer(message.LParam);
      data3b:=@data2b[5];
      mainform.FoundList.Items.Add(inttohex(pdword(@data2b[0])^,8)+'-'+RawToString(data3b,getvartype,mainform.HexadecimalCheckbox.checked,data2b[4]));
      data3b:=nil;
      freemem(data2);
    end;

    sc_addressunfrozen:
    begin
      data:=pointer(message.LParam);
      if data.dw1<mainform.numberofrecords then
        mainform.memrec[data.dw1].frozen:=false;
      mainform.updatescreen;

      freemem(data);
    end;

    SC_UpdateProgressbar:
    begin
      data:=pointeR(message.LParam);

      if data.dw1<>mainform.progressbar1.Max then mainform.progressbar1.Max:=data.dw1;
      mainform.ProgressBar1.Position:=data.dw2;

      freemem(data);
    end;

    {
    //obsolete
    SC_ScanFailed:
    begin
      if formscanning<>nil then
      begin
        formscanning.closeme:=true;
        formscanning.ModalResult:=mrOK;
      end;
      Messagedlg('The scan has failed!',mtError,[mbok],0);
    end;
    }

    SC_Disconnect: messagedlg('The server has been shut down.',mtInformation,[mbok],0);


    SC_Hyperscanstatus:
    begin
      data:=pointeR(message.LParam);
      if data.dw1=0 then mainform.CheckBox8.Checked:=false;
      freemem(data);
    end;

    sc_speedhackstatus:
    begin
      data:=pointeR(message.LParam);
      if data.dw1=0 then
      begin
        with mainform do
        begin
          cbspeedhack.Checked:=false;
          cbspeedhack.Enabled:=true;
          cbSpeedhack.Cursor:=crdefault;
          btnSetSpeedhack.Enabled:=true;

          edit1.visible:=false;
          edit2.visible:=false;
          label51.visible:=false;
          label52.visible:=false;
          btnSetSpeedhack.visible:=false;
        end;
      end
      else
      begin
        with mainform do
        begin
          edit1.visible:=true;
          edit2.visible:=true;
          label51.visible:=true;
          label52.visible:=true;
          btnSetSpeedhack.visible:=true;

          cbspeedhack.Enabled:=true;
          cbspeedhack.Cursor:=crdefault;

          label51.Enabled:=true;
          label52.Enabled:=true;
          edit1.Enabled:=true;
          edit2.Enabled:=true;
          btnSetSpeedhack.Enabled:=true;
        end;

      end;

      freemem(data);
    end;

    sc_foundcode:
    begin
      data2:=pointer(message.LParam);

    {
      pdword(@output[1])^:=addressfound;
      pdword(@output[5])^:=context.Eax;
      pdword(@output[9])^:=context.Ebx;
      pdword(@output[13])^:=context.Ecx;
      pdword(@output[17])^:=context.Edx;
      pdword(@output[21])^:=context.Esi;
      pdword(@output[25])^:=context.Edi;
      pdword(@output[29])^:=context.Ebp;
      pdword(@output[33])^:=context.Esp;
      pdword(@output[37])^:=context.Eip;  }

      if foundcodedialog<>nil then
      with foundcodedialog do
      begin
        addressfound:=pdword(@data2b[0])^;
        address:=addressfound;
        opcode:=disassemble(address,desc);

        setlength(coderecords,length(coderecords)+1);
        coderecords[length(coderecords)-1].address:=addressfound;
        coderecords[length(coderecords)-1].size:=address-addressfound;
        coderecords[length(coderecords)-1].opcode:=opcode;
        coderecords[length(coderecords)-1].desciption:=desc;

        coderecords[length(coderecords)-1].eax:=pdword(@data2b[4])^;
        coderecords[length(coderecords)-1].ebx:=pdword(@data2b[8])^;
        coderecords[length(coderecords)-1].ecx:=pdword(@data2b[12])^;
        coderecords[length(coderecords)-1].edx:=pdword(@data2b[16])^;
        coderecords[length(coderecords)-1].esi:=pdword(@data2b[20])^;
        coderecords[length(coderecords)-1].edi:=pdword(@data2b[24])^;
        coderecords[length(coderecords)-1].ebp:=pdword(@data2b[28])^;
        coderecords[length(coderecords)-1].esp:=pdword(@data2b[32])^;
        coderecords[length(coderecords)-1].eip:=pdword(@data2b[36])^;
        Foundcodelist.Items.Add(opcode);
      end;

      freemem(data2);
    end;

  end;

  data2:=nil; //cleanup
end;



procedure TConnectForm.Button1Click(Sender: TObject);
var found:boolean;
    f:string;
    g,h,i,j,k:string;
    reg:TRegistry;
    passwordbuffer: array of byte;
begin
  //add the ip to the reg (if it isn't in already)
  found:=false;
  f:=editaddress.text;

  if length(editpassword.text)=0 then raise exception.create('Please give a valid password');

  if (f=a) or (f=b) or (f=c) or (f=d) or (f=e) then found:=true;

  if not found then
  begin
    //add to top and move the rest one down
    g:=a;
    h:=b;
    i:=c;
    j:=d;

    a:=f;
    b:=g;
    c:=h;
    d:=i;
    e:=j;
  end
  else
  begin
    //switch
    if b=f then
    begin
      g:=a;
      a:=b;
      b:=g;
    end;

    if c=f then
    begin
      g:=a;
      h:=b;
      a:=f;
      b:=g;
      c:=h;
    end;

    if d=f then
    begin
      g:=a;
      h:=b;
      i:=c;
      a:=f;
      b:=g;
      c:=h;
      d:=i;
    end;

    if e=f then
    begin
      g:=a;
      h:=b;
      i:=c;
      j:=d;
      a:=f;
      b:=g;
      c:=h;
      d:=i;
      e:=j;
    end;

  end;

  try
    reg:=Tregistry.Create;

    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine\Network',false) then
    begin
      reg.writeString('Connection1',a);
      reg.WriteString('Connection2',b);
      reg.WriteString('Connection3',c);
      reg.WriteString('Connection4',d);
      reg.WriteString('Connection5',e);
    end;

    reg.CloseKey;
    reg.free;

  except
    ;
  end;

  editaddress.Clear;
  if a='' then editaddress.Text:='127.0.0.1'
  else
  begin
    editaddress.Items.Add(a);
    editaddress.Text:=a;
    if b<>'' then editaddress.Items.Add(b);
    if c<>'' then editaddress.Items.Add(c);
    if d<>'' then editaddress.Items.Add(d);
    if e<>'' then editaddress.Items.Add(e);
  end;


  idTCPClient1.Host:=EditAddress.Text;
  idTCPClient1.Port:=StrToInt(editPort.text);

  try
    idTCPClient1.Connect;
    if idtcpclient1.Connected then
    begin
      setlength(passwordbuffer,length(editpassword.text)+2);
      passwordbuffer[0]:=30;
      passwordbuffer[1]:=length(editpassword.Text);
      copymemory(@passwordbuffer[2],@editpassword.text[1],passwordbuffer[1]);

      idTCPClient1.WriteBuffer(passwordbuffer[0],length(passwordbuffer));

      //create a thread that will handle the incomming messages
      timeouttimer.enabled:=true;

      mainform:=tmainform.Create(self);
      mainform.StatusBar1.Panels[0].Text:='Connected to '+idtcpclient1.Host;

      hct:=THandlecommandThread.Create(false);
      Mainform.show;
      connectform.visible:=false;
    end;
  except

  end;
end;


procedure TConnectForm.FormCreate(Sender: TObject);
var reg: TRegistry;
begin
  a:='';
  b:='';
  c:='';
  d:='';
  e:='';

  try
    reg:=Tregistry.Create;

    Reg.RootKey := HKEY_CURRENT_USER;
    if Reg.OpenKey('\Software\Cheat Engine\Network',false) then
    begin
      EditAddress.Items.Clear;
      try
        a:=reg.ReadString('Connection1');
        b:=reg.ReadString('Connection2');
        c:=reg.ReadString('Connection3');
        d:=reg.ReadString('Connection4');
        e:=reg.ReadString('Connection5');
      except
        ;
      end;

      if a='' then editaddress.Text:='127.0.0.1'
      else
      begin
        editaddress.Items.Add(a);
        editAddress.text:=a;
        if b<>'' then editaddress.Items.Add(b);
        if c<>'' then editaddress.Items.Add(c);
        if d<>'' then editaddress.Items.Add(d);
        if e<>'' then editaddress.Items.Add(e);
      end;

    end;

    reg.CloseKey;
    reg.free;

  except
    ;
  end;


  readevents:=0;

  readprocessmemoryevent:=tevent.Create(nil,false,false,'');
  readprocessmemoryeventDone:=tevent.Create(nil,false,false,'');
  Writeprocessmemoryevent:=tevent.Create(nil,false,false,'');
  WriteprocessmemoryeventDone:=tevent.Create(nil,false,false,'');
  Debuggerstatusevent:=tevent.Create(nil,false,false,'');
  DebuggerstatuseventDone:=tevent.Create(nil,false,false,'');
  VirtualProtectExEvent:=TEvent.create(nil,false,false,'');
  VirtualProtectExEventDone:=TEvent.create(nil,false,false,'');
end;

procedure SendBuf(count: integer);
begin
  if count=0 then raise exception.Create('SendBuf was called with a size of 0');

  connectform.IdTCPClient1.writebuffer(output[0],count);
end;

function VirtualProtectExNet(hProcess: THandle; lpAddress: Pointer; dwSize, flNewProtect: DWORD; var OldProtect: DWORD): BOOL;
var address: dword;
    output:array [0..11] of byte;

    status:byte;
begin
  address:=dword(lpAddress);

  result:=true;
  if dwsize=0 then exit;

  output[0]:=CS_VirtualProtectEx;
  pdword(@output[1])^:=address;
  pdword(@output[5])^:=dwsize;
  pdword(@output[9])^:=flNewProtect;
  connectform.IdTCPClient1.writebuffer(output[0],13);

  VirtualProtectExEvent.WaitFor(infinite);
  VirtualProtectExEvent.ResetEvent;

  //a result has been received
  connectform.receivebuf(status,1);
  connectform.ReceiveBuf(oldprotect,4);


  VirtualProtectExeventdone.SetEvent;

  result:=status=1; //last time counts
end;


function WriteProcessMemoryNet(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL;
var position: ^byte;
    address: dword;
    lefttowrite: Dword;

    success: byte;
    byteswritten: word;

    totalwritten: dword;
    output:array [0..800] of byte;
begin
  try
  lpNumberOfBytesWritten:=0;

  position:=lpBuffer;
  address:=dword(lpBaseAddress);

  result:=true;

  if nsize=0 then exit;

  lefttowrite:=nSize;


  while (result) and (lefttowrite>0) do
  begin
    output[0]:=CS_WRITEPROCESSMEMORY;
    pdword(@output[1])^:=address;

    if lefttowrite<2000 then pword(@output[5])^:=lefttowrite else pword(@output[5])^:=2000;

    CopyMemory(@output[7],position,pword(@output[5])^);

    connectform.IdTCPClient1.writebuffer(output[0],7+pword(@output[5])^);

    Writeprocessmemoryevent.WaitFor(infinite);
    Writeprocessmemoryevent.ResetEvent;



    //a result has been received
    connectform.receivebuf(success,1);
    connectform.ReceiveBuf(byteswritten,2);

    writeprocessmemoryeventdone.SetEvent;

    result:=success=1; //last time counts

    inc(position,byteswritten);
    inc(address,byteswritten);
    inc(lpNumberOfBytesWritten,byteswritten);
    dec(lefttowrite,byteswritten);

  end;

  except
    result:=false;
  end;

end;


function ReadProcessMemoryNet(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;
  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL;
var position: ^byte;
    address: dword;
    lefttoread: Dword;

    success: byte;
    bytesread: word;

    totalread: dword;
    output: array [0..10] of byte;
begin
  try
  lpNumberOfBytesRead:=0;

  position:=lpBuffer;
  address:=dword(lpBaseAddress);

  result:=true;

  if nsize=0 then exit;

  lefttoread:=nSize;


  while (result) and (lefttoread>0) do
  begin
    output[0]:=9;  //1 byte
    pdword(@output[1])^:=address; //4 bytes

    if lefttoread<2000 then pword(@output[5])^:=lefttoread else pword(@output[5])^:=2000; //2 bytes

    connectform.IdTCPClient1.writebuffer(output[0],7);

    if readprocessmemoryevent.WaitFor(30000)=wrTimeOut then
    begin
      result:=false;
      readprocessmemoryevent.ResetEvent;
      exit;
    end;

    readprocessmemoryevent.ResetEvent;


    //a result has been received
    connectform.receivebuf(success,1);
    connectform.ReceiveBuf(bytesread,2);

    result:=success=1; //last time counts

    connectform.ReceiveBuf(position^,bytesread);

    inc(position,bytesread);
    inc(address,bytesread);
    inc(lpNumberOfBytesRead,bytesread);
    dec(lefttoread,bytesread);

    readprocessmemoryeventdone.SetEvent;
  end;

  except
    result:=false;
  end;
end;

procedure TConnectForm.waitforCommand(command:byte);
var rc: byte;
    FDSet: TFDSet;
    TimeVal: TTimeVal;
    i: integer;
begin
  rc:=command+1; //only if the processor is malfunctioning this returns the same
  while rc<>command do
  begin
    idtcpclient1.ReadBuffer(rc,1);
    if rc<>command then handleCommand(rc);
  end;
end;

procedure TConnectForm.ReceiveBuf(var Buf; Count: Integer);
begin
  idtcpclient1.ReadBuffer(buf,count);
end;

procedure TConnectForm.handleCommand(command: byte);
var
    b: byte;
    d: dword;
    i64: int64;

    st: string;
    st2: pchar;

    data: ^tdata;
    data2: pointer;
    send: array[0..10] of byte;
    tempbuf: array of byte;
begin
  case command of
    SC_TELLUPDATESPEED,
    SC_TELLFREEZESPEED,
    SC_StopProcessList,
    SC_OpenProcessSuccess,
    SC_OpenProcessFailed,
    SC_RecordReceived,
    SC_ValueListDone,
    SC_DebugRegsPresent,
    //SC_ScanFailed,
    SC_Disconnect: postmessage(connectform.Handle,wm_user+1,command,0);


    SC_ProcessListItem:
    begin //process list
      receivebuf(d,4);
      receivebuf(b,1);
      receivebuf(output,b);
      output[b]:=0;

      st:=inttohex(d,8)+'-'+pchar(@output[0]);
      getmem(st2,length(st)+1);
      copymemory(st2,@st[1],length(st));
      st2[length(st)]:=#0;
      postmessage(connectform.Handle,wm_user+1,command,dword(st2));
    end;

    SC_ValueUpdated:
    begin
      //record update
      //valuelist(Entry:word; memorysize:word; readmemory: array of bytes)

      receivebuf(output[0],2);
      receivebuf(output[2],2);

      getmem(data2,4+pword(@output[2])^);
      CopyMemory(data2,@output[0],4);

      setlength(tempbuf,pword(@output[2])^);
      Receivebuf(tempbuf[0],pword(@output[2])^);
      copymemory(pointer(dword(data2)+4),tempbuf,pword(@output[2])^);
      setlength(tempbuf,0);

      postmessage(connectform.Handle,wm_user+1,command,dword(data2));
    end;

    SC_ScanResultCount:
    begin
      //done scanning
      receivebuf(i64,8);
      st:=inttostr(i64);
      getmem(st2,length(st)+1);
      copymemory(st2,@st[1],length(st));
      st2[length(st)]:=#0;
      postmessage(connectform.Handle,wm_user+1,command,dword(st2));
    end;

    SC_ScanResult:
    begin
      //address:dword; valuesize:byte; value:array of bytes
      receivebuf(output[0],4);
      receivebuf(output[4],1);
      receivebuf(output[5],output[4]);
      getmem(data2,5+output[4]);
      copymemory(data2,@output[0],5+output[4]);
      postmessage(connectform.Handle,wm_user+1,command,dword(data2));
    end;

    SC_AddressUnfrozen:
    begin
      receivebuf(output[0],2);

      getmem(data,sizeof(tdata));
      data.dw1:=pword(@output[0])^;
      postmessage(connectform.Handle,wm_user+1,command,dword(data));


    end;

    SC_UpdateProgressbar:
    begin
      receivebuf(output[0],8);
      getmem(data,sizeof(tdata));
      data.dw1:=pdword(@output[0])^;
      data.dw2:=pdword(@output[4])^;
      postmessage(connectform.Handle,wm_user+1,command,dword(data));

    end;


    SC_Hyperscanstatus,SC_Speedhackstatus:
    begin
      receivebuf(output[0],1);
      getmem(data,sizeof(tdata));
      data.dw1:=output[0];
      postmessage(connectform.Handle,wm_user+1,command,dword(data));
    end;

    SC_Foundcode:
    begin
      getmem(data2,40);
      receivebuf(data2^,40);
      postmessage(connectform.Handle,wm_user+1,command,dword(data2));
    end;



   253:
   begin
     outputdebugstring('Handled the data command');
   end;

   255:begin //Are you alive? ()
         send[0]:=255;
         connectform.IdTCPClient1.writebuffer(send[0],1);
         //YES....I....AM!!!!!!
         ImAlive:=true;
       end;
  end;
end;


procedure TConnectForm.TimeOutTimerTimer(Sender: TObject);
begin
{  if not imalive then
  begin
    showmessage('Connection Lost...');
    clientsocket1.Close;
  end;
  imalive:=false;}
end;

procedure TConnectForm.FormShow(Sender: TObject);
begin
  with formsettings do
  begin
    rdWriteExceptions.Checked:=true;
    rbDebugRegisters.Enabled:=false;
    label6.Enabled:=false;
    label7.Enabled:=false;
  end;
end;



procedure TConnectForm.Timer1Timer(Sender: TObject);
var c: byte;
begin

end;

procedure TConnectForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if hct<>nil then hct.Terminate;
  idtcpclient1.Disconnect;
end;

end.
