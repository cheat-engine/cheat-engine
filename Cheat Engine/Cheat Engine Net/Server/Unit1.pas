unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, ScktComp,ScanThread,WinSock, Sockets,
  IdBaseComponent, IdComponent, IdTCPServer, Menus,syncobjs,newkernelhandler;

const
  //Client to server communication
  CS_GetProcessList=0; //no param
  CS_GetWindowList=1; //no param
  CS_OpenProcess=2; //procid: dword
  CS_AddAddress=3; // Address:dword;valtype:byte;bitnr:byte;length:byte
  CS_RefreshList=4; //Start:word;stop:word
  CS_SetConfig=5; //ShowAsSigned:byte BinariesAsDecimal:byte max:word; buffersize:dword;skip_page_no_cache: byte;UseDebugRegs:byte;stealthusermode:byte;stealthkernelmode:byte
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


  //server to client communication
  SC_TellUpdateSpeed=0; //updatespeed:word
  SC_TellFreezeSpeed=1; //freezespeed:word
  SC_ProcessListItem=2; //processid:dword; stringlength:byte;processname:array of char
  SC_StopProcessList=3; // noparam
  SC_OpenProcessSuccess=4; //'
  SC_OpenProcessFailed=5;
  SC_RecordReceived=6; //Indicated that the record has been received  (not really necesary)
  SC_ValueUpdated=7; //recnr:word; length:byte; value:string
  SC_ValueListDone=8; //All values have been sent to the client
  SC_DebugRegsPresent=9;
  SC_ValueChanged=10; //0=success 1=Incorrect value 2=unwritable 3=record doesn't exist

  SC_ReadProcessMemoryResult=11;
  SC_WriteProcessMemoryResult=12;

  SC_ScanResultCount=13; //count:int64
  SC_ScanResult=14; //stringlength:byte; result:string
  SC_AddressUnfrozen=15; //recnr: word;
  SC_UpdateProgressbar=16; //max:word; position:word
  SC_ScanFailed=17;
  SC_Disconnect=18;

  SC_Hyperscanstatus=19; //status:byte //0=off 1=on
  SC_SpeedhackStatus=20; //status:byte
  SC_DebuggerStatus=21; //status: byte 0=off 1=on

  SC_FoundCode=22; //(Address: dword;eax:dword; ebx:dword; ecx:dword; edx:dword;esi:dword;edi:dword;ebp:dword;esp:dword;eip:dword;)
  SC_VirtualProtectExResult=23; //(status:byte; oldprotecT:dword); //status 0=failed 1=success

type TSettings = record
  ShowAsSigned: boolean;
  BinariesAsDecimal: boolean;
  max: word;
  buffersize: dword;
  skip_page_no_cache:boolean;
  usedebugregs: boolean;
end;

type mymemoryrecord = record
  address: dword;
  bit: byte;
  bitlength: byte;
  vartype: byte;
  frozen: boolean;
  Frozenvalue: int64;
  FrozenFvalue: double;
  Frozenstring: string;
  frozenBytes: array of Byte;
end;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Portvalue: TEdit;
    Label1: TLabel;
    StatusBar1: TStatusBar;
    FreezeTimer: TTimer;
    Log: TMemo;
    UpdateTimer: TTimer;
    ListBox: TListBox;
    hexcb: TCheckBox;
    TimeOutTest: TTimer;
    Progressbartimer: TTimer;
    Button2: TButton;
    IdTCPServer1: TIdTCPServer;
    PopupMenu1: TPopupMenu;
    Savetofile1: TMenuItem;
    OpenDialog1: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure ServerSocket1ClientError(Sender: TObject;
      Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
      var ErrorCode: Integer);
    procedure Button1Click(Sender: TObject);
    procedure ServerSocket1ClientDisconnect(Sender: TObject;
      Socket: TCustomWinSocket);
    procedure UpdateTimerTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FreezeTimerTimer(Sender: TObject);
    procedure TimeOutTestTimer(Sender: TObject);
    procedure ProgressbartimerTimer(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure IdTCPServer1Execute(AThread: TIdPeerThread);
    procedure Savetofile1Click(Sender: TObject);
  private
    { Private declarations }
    logon: boolean;
    online: boolean;
    settings: TSettings;
    currenttype: byte;
    keepalivesend:boolean;
    UpdatelistCS: TCriticalSection;

    hypermode: boolean;
    cescanhook:thandle;
    cehookdll: thandle;

    procedure GetWindowList2;
    procedure GetProcessList2;
    function ChangeValue(recordnr: integer; newvalue:string):byte;
    procedure FreezeAddress(i: word);
    procedure SetReadWriteBreakpoint(address: dword; size: dword);
    procedure SetReadBreakpoint(address: dword; size: dword);
    procedure SetWriteBreakpoint(address: dword; size: dword);

  public
    { Public declarations }
    Numberofrecords: Integer;
    memrec: array of mymemoryrecord;
    senddata: boolean;
    lag: Integer;

    progressbar:TProgressBar;
    output: array [0..2048] of byte;

    SThread: TScanThread;
    Progressbar1: TProgressbar;
    closed: boolean;
    hyperscanenabled: boolean;
    speedhackenabled: boolean;
    HyperscanFileMapping: THandle;


    procedure UpdateList; overload;
    procedure UpdateList(start: word;stop:word); overload;
    procedure ReceiveBuf(var Buf; Count: Integer;socket:TCustomWinSocket);
    procedure SendBuf(count: integer);
    procedure Enablehypermode;
    procedure DisableHypermode;
    procedure DisableSpeedhack;

   // debuggerthread: tdebuggerthread;
  end;

var
  Form1: TForm1;


resourcestring strAddressHasToBeReadable='This address can''t be read';
implementation

{$R *.DFM}
uses CEFuncProc,debugger;

procedure TForm1.SendBuf(count: integer);
var i,j: integer;
    counter: integer;
    FDSet: TFDSet;
    TimeVal: TTimeVal;
    x: integer;
    bytenr: integer;
    threadlist: tlist;
begin
  threadlist:=idtcpserver1.Threads.LockList;
  try
    for i:=0 to threadlist.count-1 do
      TIdPeerThread(threadlist[i]).Connection.WriteBuffer(output[0],count);
  finally
    idtcpserver1.Threads.UnlockList;
  end;
end;

procedure TForm1.FreezeAddress(i: word);
var error: dword;

    a: single;
    b: double;
    controle: String;

    j,k,sel: Integer;
    db: Byte;
    dw: Word;
    dd: dword;
    di64: Int64;

    read8: array of byte;
    read9: pbyte;

    freeze: boolean;
    freezegroup: array [1..4] of boolean;
    temps: pchar;

    temp: string;
begin
  //read memory
  case memrec[i].VarType of
    0:
    begin  //byte
      readprocessmemory(processhandle,pointer(memrec[i].Address),addr(db),1,error);
      if error=1 then
      begin
        memrec[i].FrozenValue:=db;
        memrec[i].Frozen:=true;
      end;
    end;

    1:
    begin  //word
      readprocessmemory(processhandle,pointer(memrec[i].Address),addr(dw),2,error);
      if error=2 then
      begin
        memrec[i].FrozenValue:=dw;
        memrec[i].Frozen:=true;
      end;
    end;

    2:
    begin  //dword
      readprocessmemory(processhandle,pointer(memrec[i].Address),addr(dd),4,error);
      if error=4 then
      begin
        memrec[i].FrozenValue:=dd;
        memrec[i].Frozen:=true;
      end;
    end;

    3:  //float
    begin
      readprocessmemory(processhandle,pointer(memrec[i].Address),addr(a),4,error);
      if error=4 then
      begin
        controle:=FloatToStr(a);
        if system.pos('NAT',controle)>0 then error:=0;
        if system.pos('INF',controle)>0 then error:=0;
        if error<>0 then
        begin
          memrec[i].FrozenFvalue:=a;
          memrec[i].Frozen:=true;
        end;
      end;
    end;

    4:
    begin
      readprocessmemory(processhandle,pointer(memrec[i].Address),addr(b),8,error);
      if error=8 then
      begin
        controle:=FloatToStr(b);
        if system.pos('NAT',controle)>0 then error:=0;
        if system.pos('INF',controle)>0 then error:=0;
        if error<>0 then
        begin
          memrec[i].frozenfvalue:=b;
          memrec[i].Frozen:=true;
        end;
      end;
    end;

    5:
    begin  //binary
      k:=1+((memrec[i].Bit+memrec[i].bitlength) div 8);
      setlength(read8,k);
      readprocessmemory(processhandle,pointer(memrec[i].Address),addr(read8[0]),k,error);
      if error=k then
      begin
        //find out the current bit combination (use frozenstrings to store the value)
        //convert what i need to a string of bits
        temp:='';
        j:=memrec[i].Bit;
        read9:=@read8[0];
        for k:=1 to memrec[i].bitlength do
        begin
          temp:=temp+IntToStr(getbit(j,read9^));
          inc(j);
          if j>=8 then
          begin
            j:=0;
            inc(read9);
          end;
        end;

        //the tempstring now contaisn the bits (bit0 is first char...)
        memrec[i].Frozenstring:=temp;
        memrec[i].Frozen:=true;
      end;
    end;

    6:
    begin  //int64
      readprocessmemory(processhandle,pointer(memrec[i].Address),addr(dI64),8,error);
      if error=8 then
      begin
        memrec[i].FrozenValue:=di64;
        memrec[i].Frozen:=true;
      end;
    end;

    7:
    begin  //text
      getmem(temps,memrec[i].bit);
      readprocessmemory(processhandle,pointer(memrec[i].Address),temps,memrec[i].Bit,error);
      if error=memrec[i].bit then
      begin
        memrec[i].FrozenString:=temps;
        memrec[i].Frozen:=true;
      end;
      freemem(temps);
    end;

    8:
    begin  //array of byte
      setlength(memrec[i].FrozenBytes,memrec[i].bit);
      readprocessmemory(processhandle,pointer(memrec[i].Address),memrec[i].FrozenBytes,memrec[i].Bit,error);
      if error=memrec[i].bit then
      memrec[i].Frozen:=true;
    end;
  end;


  if error=0 then
  begin
    //freeze failed
    output[0]:=SC_ADDRESSUNFROZEN;
    pword(@output[1])^:=i;
    sendbuf(3);
  end;

  //and freeze it
end;

function TForm1.ChangeValue(recordnr: integer; newvalue:string):byte;
var newvalue1: Byte;
    oldvalue1: byte;
    newvalue2: word;
    newvalue3: dword;
    newvalue4: Single;
    newvalue5: Double;
    newvalue6: Int64;
    newvalueSt: String;
    newvalue8: TBytes;

    newbytes: array of byte;

    text: pchar;
    addzero: boolean;
    write: dword;
    error,i,j,k,l: Integer;
    bl:integer;

    original: dword;
    thistype: integer;

    nrselected: integer;

begin
  newvaluest:=newvalue;
  if (newvalue='??') or (newvalue='NAN') or (newvalue='INF') then
  begin
    result:=1;
    exit;
  end;

  thistype:=memrec[recordnr].vartype;

  error:=0;

  case thistype of
    0,1,2,5,6: begin
                  val(newvaluest,newvalue6,error);
                  if error=0 then
                  begin
                    newvalue1:=byte(newvalue6);
                    newvalue2:=word(newvalue6);
                    newvalue3:=dword(newvalue6);
                  end;
                end;

    3,4:        begin
                  val(newvaluest,newvalue5,error);
                  newvalue4:=newvalue5;
                end;

    7:          begin
                  addzero:=false;
                  newvalue1:=0;

                  getmem(text,length(newvaluest));
                  StrCopy(text, PChar(newvaluest));

                  VirtualProtectEx(processhandle,  pointer(memrec[recordnr].Address),1,PAGE_EXECUTE_READWRITE,original);

                  writeprocessmemory(processhandle,pointer(memrec[recordnr].Address),text,length(newvaluest),write);
                  if addzero then writeprocessmemory(processhandle,pointer(memrec[recordnr].Address+length(newvaluest)),addr(newvalue1),1,write);

                  VirtualProtectEx(processhandle,  pointer(memrec[recordnr].Address),1,original,write);

                  memrec[recordnr].Frozenstring:=newvaluest;
                  freemem(text);
                  exit;
                end;

    8:          begin
                  if nrselected>1 then
                  begin
                    beep;
                    exit;
                  end;

                  for i:=1 to length(newvaluest) do
                  case newvaluest[i] of
                    '0'..'9' : ;
                    'a'..'f' : ;
                    'A'..'F' : ;
                    ' ','-' : ;
                    else raise exception.create('This is not a valid notation');
                  end;

                  ConvertStringToBytes(newvaluest,true,newvalue8);

                  memrec[recordnr].Bit:=length(newvalue8);
                  setlength(memrec[recordnr].frozenbytes,length(newvalue8));

                  for i:=0 to length(newvalue8)-1 do
                    memrec[recordnr].frozenbytes[i]:=newvalue8[i];

                  VirtualProtectEx(processhandle,  pointer(memrec[recordnr].Address),1,PAGE_EXECUTE_READWRITE,original);
                  writeprocessmemory(processhandle,pointer(memrec[recordnr].Address),@memrec[recordnr].frozenbytes[0],length(newvalue8),write);
                  //set old security back
                  VirtualProtectEx(processhandle,  pointer(memrec[recordnr].Address),1,original,write);
                  setlength(newvalue8,0);
                  exit;
                end;
  end;


  if error>0 then raise Exception.Create('This is not a valid value!');

  i:=recordnr;

  begin
    begin
      VirtualProtectEx(processhandle,  pointer(memrec[i].Address),1,PAGE_EXECUTE_READWRITE,original);

      if memrec[i].VarType=0 then //byte
      begin
        writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(newvalue1),1,write);
        memrec[i].FrozenValue:=newvalue1;
      end;

      if memrec[i].VarType=1 then //word
      begin
        writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(newvalue2),2,write);
        memrec[i].FrozenValue:=newvalue2;
      end;

      if memrec[i].VarType=2 then //dword
      begin
        writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(newvalue3),4,write);
        memrec[i].FrozenValue:=newvalue3;
      end;

      if memrec[i].VarType=3 then //single
      begin
        writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(newvalue4),4,write);
        memrec[i].FrozenFValue:=newvalue4
      end;

      if memrec[i].VarType=4 then //double
      begin
        writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(newvalue5),8,write);
        memrec[i].FrozenFValue:=newvalue5
      end;

      if memrec[i].VarType=5 then //binary
      begin
        if settings.BinariesAsDecimal then
          newvaluest:=inttobin(abs(newvalue6));

        bl:=1+((length(newvaluest)-1) div 8);

        setlength(newbytes,bl);
        ReadProcessMemory(processhandle,pointer(memrec[i].Address),@newbytes[0],bl,write);

        if settings.BinariesAsDecimal then
          newvaluest:=inttobin(abs(newvalue6));


        j:=0;
        k:=memrec[i].bit;
        for l:=length(newvaluest) downto 1 do
        begin
          case newvaluest[l] of
           '0' : setbit(k,newbytes[j],0);
           '1' : setbit(k,newbytes[j],1);
           '*','?': ;
           else raise exception.Create(newvaluest+' is not a valid binary notation!');
          end;
          inc(k);
          if k>=8 then
          begin
            inc(j);
            k:=0;
          end;
        end;

        writeprocessmemory(processhandle,pointer(memrec[i].Address),@newbytes[0],bl,write);
        memrec[i].frozenstring:=newvaluest;
        setlength(newbytes,0);
      end;

      if memrec[i].VarType=6 then //Int64
      begin
        writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(newvalue6),8,write);
        memrec[i].FrozenValue:=newvalue6;
      end;

      //set old security back
      VirtualProtectEx(processhandle,  pointer(memrec[i].Address),1,original,write);
    end;
  end;

  updatelist(recordnr,recordnr);
end;

procedure TForm1.UpdateList(start: word;stop:word);
var i,j,k: Integer;
    read1: byte;
    read2: word;
    read3: dword;
    read4: single;
    read5: double;
    read6: Int64;
    read7: pchar;
    read8: array of byte;
    read9: pbyte;

    count: dword;
    rec: Integer;
    v,temp,temp2: string;

    nrofbytes: integer;

    counter: integer;
begin
  updatelistcs.Enter;
  try
    count:=0;
    counter:=0;

    for rec:=start to stop do
    begin
      if rec>(numberofrecords-1) then
      begin
        output[0]:=8;
        sendbuf(1);
        exit;
      end;

      if counter>1024 then
      begin
        sendbuf(counter);
        counter:=0;
      end;

      output[0]:=SC_ValueUpdated;
      pword(@output[1])^:=rec; //recnr

      if not memrec[rec].frozen then
      begin
        case memrec[rec].vartype of
        0: begin //byte
             readprocessmemory(processhandle,pointer(memrec[rec].address),addr(read1),1,count);
             if count>0 then
             begin
               if settings.ShowAsSigned then
                 v:=IntToStr(ShortInt(read1))
               else
                 v:=IntToStr(read1);
             end else v:='??'
           end;

      1: begin //word
           readprocessmemory(processhandle,pointer(memrec[rec].address),addr(read2),2,count);
           if count=0 then v:='??' else
           begin
             if settings.ShowAsSigned then
               v:=IntToStr(SmallInt(read2))
             else
               v:=IntToStr(read2);
           end;
         end;

      2: begin //dword
           readprocessmemory(processhandle,pointer(memrec[rec].address),addr(read3),4,count);
           if count=0 then v:='??' else
           begin
             if settings.ShowAsSigned then
               v:=IntToStr(Longint(read3))
             else
               v:=IntToStr(read3);
           end;
         end;

      3: begin //float
           readprocessmemory(processhandle,pointer(memrec[rec].address),addr(read4),4,count);
           if count=0 then
             v:='??'
           else
             v:=FloatToStr(read4);
         end;

      4: begin  //double
           readprocessmemory(processhandle,pointer(memrec[rec].address),addr(read5),8,count);
           if count=0 then
             v:='??'
           else
             v:=FloatToStr(read5);
         end;

      5: begin //binary
           //read the bytes
           nrofbytes:=1+((memrec[rec].Bit+memrec[rec].bitlength) div 8);
           setlength(read8,nrofbytes);

           readprocessmemory(processhandle,pointer(memrec[rec].address),addr(read8[0]),nrofbytes,count);
           if count=0 then v:='??' else
           begin
             //convert what i need to a string of bits
             temp:='';
             j:=memrec[rec].Bit;
             read9:=@read8[0];
             for k:=1 to memrec[rec].bitlength do
             begin
               temp:=temp+IntToStr(getbit(j,read9^));
               inc(j);
               if j>=8 then
               begin
                 j:=0;
                 inc(read9);
               end;
             end;

             temp2:='';
             for k:=length(temp) downto 1 do
               temp2:=temp2+temp[k];

             if settings.BinariesAsDecimal then
             begin
               try
                 v:=IntToStr(bintoint(temp2));
               except
                 v:='Too long';
               end;
             end else v:=temp2;

             //and convert them to a decimal value
           end;
         end;

      6: begin //Int64
           readprocessmemory(processhandle,pointer(memrec[rec].address),addr(read6),8,count);
           if count=0 then v:='??' else
           begin
             v:=IntToStr(read6);
           end;
         end;

      7: begin  //text
           getmem(read7,memrec[rec].Bit+1);
           readprocessmemory(processhandle,pointer(memrec[rec].Address),read7,memrec[rec].Bit,count);
           if count<memrec[rec].Bit then v:='??' else
           begin
             read7[memrec[rec].Bit]:=chr(0);
             v:=read7;
           end;
           freemem(read7);
         end;

      8: begin //array of byte
           setlength(read8,memrec[rec].Bit);
           readprocessmemory(processhandle,pointer(memrec[rec].Address),read8,memrec[rec].Bit,count);

           if count<memrec[rec].Bit then v:='??' else
           begin
             temp:='';
             for j:=0 to memrec[rec].Bit-1 do
               temp:=temp+IntToHex(read8[j],2)+' ';

             v:=temp;
           end;

           setlength(read8,0);
         end;

      end;
    end
    else
    begin
      if settings.ShowAsSigned then
      begin
        if memrec[rec].VarType=0 then v:=IntToStr(ShortInt(memrec[rec].frozenvalue)) else
        if memrec[rec].VarType=1 then v:=IntToStr(SmallInt(memrec[rec].frozenvalue)) else
        if memrec[rec].VarType=2 then v:=IntToStr(LongInt(memrec[rec].frozenvalue));
      end else
      begin
        if memrec[rec].VarType=0 then v:=IntToStr(byte(memrec[rec].frozenvalue)) else
        if memrec[rec].VarType=1 then v:=IntToStr(word(memrec[rec].frozenvalue)) else
        if memrec[rec].VarType=2 then v:=IntToStr(dword(memrec[rec].frozenvalue));
      end;

      if memrec[rec].VarType=3 then  //signle
      begin
        read4:=memrec[rec].FrozenFvalue;
        v:=FloatToStr(read4);
      end;

      if memrec[rec].VarType=4 then  //double
      begin
        read5:=memrec[rec].FrozenFvalue;
        v:=FloatToStr(read5);
      end;

      if memrec[rec].Vartype=5 then //binary
      begin
        //turn arround
        temp2:=memrec[rec].frozenstring;

        temp:='';
        for j:=length(temp2) downto 1 do
          temp:=temp+temp2[j];

        if settings.BinariesAsDecimal then
          v:=IntToStr(bintoint(temp))
        else
          v:=temp;
      end;

      if memrec[rec].VarType=6 then  //int64
      begin
        v:=IntToStr((memrec[rec].frozenvalue));
      end;

      if memrec[rec].VarType=7 then //text
      begin
        v:=memrec[rec].Frozenstring;
      end;

      if memrec[rec].VarType=8 then //array of byte
      begin
        temp:='';

        for j:=0 to length(memrec[i].frozenBytes)-1 do
          temp:=temp+IntToHex(memrec[i].frozenBytes[j],2)+' ';
        v:=temp;
      end;

    end;


    output[3]:=length(v);
    copymemory(@output[4],@v[1],output[3]);
    sendbuf(4+output[3]);

  end;


  output[0]:=SC_VALUELISTDONE;
  sendbuf(1);

  finally
    updatelistcs.Leave;
  end;
end;

procedure TForm1.UpdateList;
begin
  UpdateList(0,numberofrecords-1);


end;

procedure TForm1.FormCreate(Sender: TObject);
var pid: dword;
    ownprocesshandle: THandle;
    tokenhandle: thandle;
    tp:TTokenPrivileges;
    prev: TTokenPrivileges;

    ReturnLength: Dword;
begin
  //create the filemapping for hypermode scanning
  HyperscanFileMapping:=CreateFileMapping($FFFFFFFF,nil,PAGE_READWRITE,0,sizeof(tscansettings),'CEHYPERSCANSETTINGS');
  if HyperscanFileMapping=0 then
  begin
    FreeLibrary(CEHOOKDLL);
    exit;
  end;

  HyperscanView:=MapViewOfFile(HyperscanFileMapping,FILE_MAP_ALL_ACCESS,0,0,0);
  if hyperscanview=nil then
  begin
    closehandle(HyperscanFileMapping);
    FreeLibrary(CEHOOKDLL);
  end;
  try
  zeromemory(hyperscanview,sizeof(tscansettings));

  hyperscanview.mainformHandle:=handle;
  hyperscanview.applicantionhandle:=application.Handle;
  hyperscanview.CheatEngineDir:=CheatEngineDir;
  hyperscanview.CEProcessID:=GetCurrentProcessId;
  hyperscanview.CEMainThreadID:=getcurrentthreadid;
  except

  end;
  

  pid:=GetCurrentProcessID;

  ownprocesshandle:=OpenProcess(PROCESS_ALL_ACCESS,true,pid);
  tokenhandle:=0;

  if ownprocesshandle<>0 then
  begin
    if OpenProcessToken(ownprocesshandle,TOKEN_QUERY or TOKEN_ADJUST_PRIVILEGES	,tokenhandle) then
    begin
      if lookupPrivilegeValue(nil, 'SeDebugPrivilege' ,tp.Privileges[0].Luid) then
      begin
        tp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
        tp.PrivilegeCount := 1; // One privilege to set
        AdjustTokenPrivileges(tokenhandle,false,tp,sizeof(tp),@prev,returnlength);
      end;
    end;
  end;

  Set8087CW($133f);  //bye,bye fp exceptions
  cefuncproc.buffersize:=512*1024;
  progressbar1:=TProgressbar.Create(nil);
  progressbar1.Visible:=true;
  progressbar1.Parent:=form1;
  progressbar1.Left:=500;
  progressbar1.SendToBack;

  senddata:=true;
  logon:=false;
  lag:=500;
  GetCEdir;

  Scan_MEM_PRIVATE:=true;
  Scan_MEM_IMAGE:=true;
  Scan_MEM_MAPPED:=false;

  UpdatelistCS:=TCriticalSection.Create;
end;

procedure TForm1.ServerSocket1ClientError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  senddata:=true;
  ErrorCode:=0;
  setlength(memrec,0);
  numberofrecords:=0;
  online:=false;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if button1.caption='Start Server' then
  begin
  try
    closeD:=false;
    idtcpserver1.DefaultPort:=StrToInt(PortValue.Text);
    idtcpserver1.Active:=true;



    online:=true;
    statusBar1.Panels[0].Text:='Online';

    log.Lines.Add('Server started on port:'+PortValue.text);
    Button1.caption:='Stop Server';
  except
    on EConvertError do
      showmessage(PortValue.text+' is not an valid value');
  end;
  end else
  begin
    idtcpserver1.Active:=false;
    closeD:=true;

    online:=false;
    log.Lines.Add('Server Stopped');
    Button1.caption:='Start Server';
    statusBar1.Panels[0].Text:='Offline';
  end;
end;

procedure TForm1.ReceiveBuf(var Buf; Count: Integer;socket:TCustomWinSocket);
//this function keeps on going till the ammount of bytes requiested is read
var bp: ^byte;
    left: integer;
    read: integer;
    timeout: integer;
begin
  left:=count;
  timeout:=0;
  bp:=@buf;

  while left>0 do
  begin
    read:=socket.ReceiveBuf(bp^,left);
    if read>0 then
    begin
      inc(bp,read);
      dec(left,read);

    end
    else
    begin
      if timeout=30*(1000 div 20) then //30 seconds (assuming the inc(timeout) isn't too slow)
        raise exception.Create('Timeout while waiting for data');

      inc(timeout);
      sleep(20);
    end;
  end;
end;


procedure TForm1.ServerSocket1ClientDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  setlength(memrec,0);

  if SThread<>nil then
  begin
    terminatethread(SThread.Handle,0);
    freememory;
    closefiles;
    SThread.Free;
  end;

  senddata:=true;
  log.Lines.Add(socket.RemoteAddress+' disconnected!');
  setlength(memrec,0);
  numberofrecords:=0;
  online:=false;
end;

procedure TForm1.UpdateTimerTimer(Sender: TObject);
begin
  try
    if numberofrecords>0 then UpdateList;
  except
    log.Lines.Add('The update values routine caused an exception');
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
var i: Integer;
begin
  output[0]:=SC_DISCONNECT;
  sendbuf(1);

  shutdown;
  closed:=true;
  Updatelistcs.Free;

end;

procedure TForm1.FreezeTimerTimer(Sender: TObject);
var i,j: Integer;
    write: dword;
    write1: byte;
    write2: word;
    write3: dword;
    write4: single;
    write5: double;
    write6: Int64;
    error: boolean;
    count: integer;
begin
  try
  count:=0;
  for i:=0 to numberofrecords-1 do
    if memrec[i].frozen then
    begin
      error:=false;

      case memrec[i].VarType of
        0:      begin
                  write1:=byte(memrec[i].FrozenValue);
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write1),1,write);
                  error:=write<>1;
                end;

        1:      begin
                  write2:=word(memrec[i].FrozenValue);
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write2),2,write);
                  error:=write<>2;
                end;

        2:      begin
                  write3:=memrec[i].FrozenValue;
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write3),4,write);
                  error:=write<>4;
                end;

        3:      begin
                  write4:=memrec[i].FrozenFValue;
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write4),4,write);
                  error:=write<>4;
                end;

        4:      begin
                  write5:=memrec[i].FrozenFvalue;
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write5),8,write);
                  error:=write<>8;
                end;

        5:      begin
                  ReadProcessMemory(processhandle,pointer(memrec[i].Address),addr(write1),1,write);
                  case memrec[i].Bit of
                    0       :       if memrec[i].FrozenValue=0 then write1:=write1 and 254 {Bit 0 becomes 0} else
                                                                    write1:=write1 or 1; {bit 0 becomes a 1}

                    1       :       if memrec[i].FrozenValue=0 then write1:=write1 and 253 {Bit 0 becomes 0} else
                                                                    write1:=write1 or 2;{bit 0 becomes a 1}

                    2       :       if memrec[i].FrozenValue=0 then write1:=write1 and 251 {Bit 0 becomes 0} else
                                                                    write1:=write1 or 4;{bit 0 becomes a 1}

                    3       :       if memrec[i].FrozenValue=0 then write1:=write1 and 247 {Bit 0 becomes 0} else
                                                                    write1:=write1 or 8;{bit 0 becomes a 1}

                    4       :       if memrec[i].FrozenValue=0 then write1:=write1 and 239 {Bit 0 becomes 0} else
                                                                    write1:=write1 or 16; {bit 0 becomes a 1}

                    5       :       if memrec[i].FrozenValue=0 then write1:=write1 and 223 {Bit 0 becomes 0} else
                                                                    write1:=write1 or 32; {bit 0 becomes a 1}

                    6       :       if memrec[i].FrozenValue=0 then write1:=write1 and 191 {Bit 0 becomes 0} else
                                                                    write1:=write1 or 64; {bit 0 becomes a 1}

                    7       :       if memrec[i].FrozenValue=0 then write1:=write1 and 127 {Bit 0 becomes 0} else
                                                                    write1:=write1 or 128; {bit 0 becomes a 1}

                  end;
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write1),1,write);
                  error:=write<>1;
                end;

        6:      begin  //int64
                  write6:=memrec[i].FrozenValue;
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write6),8,write);
                  error:=write<>8;
                end;


      end;

      if error then
      begin

        memrec[i].frozen:=false;
        output[0]:=sc_addressunfrozen;
        pword(@output[1])^:=i;
        sendbuf(3);
      end;
    end;

  except
    log.Lines.Add('Exception while trying to freeze an address');
  end;
end;

procedure TForm1.TimeOutTestTimer(Sender: TObject);
begin
  try
    output[0]:=255;
    sendbuf(1);
    keepalivesend:=true;
  except
    log.Lines.Add('Failed to send the keepalive signal');
  end;
end;

procedure TForm1.ProgressbartimerTimer(Sender: TObject);
begin
  if sthread<>nil then
  begin
    try
      output[0]:=SC_UpdateProgressbar;
      pdword(@output[1])^:=progressbar1.Max;
      pdword(@output[5])^:=progressbar1.position;
      sendbuf(9);
    except
      log.Lines.Add('Failed to update the progressbar on the client');
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var i,j: integer;
    counter: integer;
    FDSet: TFDSet;
    TimeVal: TTimeVal;
    x: integer;
    bytenr: integer;
    t: string;
    count: integer;
    max: dword;
begin
  t:='blablabla';
  i:=0;
  j:=0;

  while i<10000 do
  begin
    output[0]:=SC_ProcessListItem;
    pdword(@output[1])^:=i;
    output[5]:=9;
    copymemory(@output[6],@t[1],9);
    sendbuf(16);
    inc(i);
  end;

  output[0]:=SC_StopProcessList;
  sendbuf(1);
end;

procedure TForm1.GetProcessList2;
var ai:tbytes;
begin
  GetProcessList(Listbox,ai);
end;

procedure TForm1.GetWindowList2;
begin
  getwindowlist(ListBox);
end;

procedure TForm1.IdTCPServer1Execute(AThread: TIdPeerThread);
var b: byte;
    command: byte;
    ai: TBytes;
    i,j,k: Integer;

    dw: dword;

    cp: ^dword;
    temp: string;
    tempbuf: pointer;
    check: boolean;

    actualread:dword;
    actualwritten: dword;

    recordnr: word;

    output: array[0..2048] of byte;

    templistbox: tlistbox;

    peerip: string;
begin
  try

  try
  peerip:=athread.Connection.Socket.Binding.PeerIP;
  log.lines.add(peerip+' connected to the server');

  if getsystemtype>=3 then
  begin
    output[0]:=SC_DEBUGREGSPRESENT;
    athread.Connection.WriteBuffer(output[0],1);
  end;

  while (not closed) and (athread<>nil) and (not athread.Terminated) and (athread.Connection.Connected) do
  begin
    athread.Connection.ReadBuffer(command,1);

    case command of
      CS_GetProcessList:
      begin
        log.lines.add('Process list');
        try
          getprocesslist(ListBox,ai);
        except
          log.Lines.Add('Getting the process list failed (Server side)');
          output[0]:=SC_StopProcessList; //end of process list
          athread.Connection.WriteBuffer(output[0],1);
        end;

        for i:=0 to listbox.Items.Count-1 do
        begin
          //output=
          //2,procid,lengthofstring,string
          output[0]:=SC_PROCESSLISTITEM;  //processlist item
          cp:=@output[1];


          cp^:=StrToInt('$'+copy(listbox.Items[i],0,pos('-',listbox.Items[i])-1));

          temp:=copy(listbox.Items[i],
                     pos('-',listbox.Items[i])+1,
                     length(listbox.items[i])
                     );


          output[5]:=length(temp);
          copymemory(@output[6],@temp[1],length(temp));
          athread.Connection.WriteBuffer(output[0],6+output[5]);
        end;

        output[0]:=SC_StopProcessList; //end of process list
        athread.Connection.WriteBuffer(output[0],1);
      end;


      CS_GETWINDOWLIST:
      begin
        log.lines.add('window list');

        getwindowlist2;


        for i:=0 to listbox.Items.Count-1 do
        begin
          //output=
          //2,procid,lengthofstring,string
          output[0]:=2;  //processlist item

          cp:=@output[1];
          cp^:=StrToInt('$'+copy(listbox.Items[i],0,pos('-',listbox.Items[i])-1));

          temp:=copy(listbox.Items[i],
                     pos('-',listbox.Items[i])+1,
                     length(listbox.items[i])
                     );
          output[5]:=length(temp);

          copymemory(@output[6],@temp[1],length(temp));
          athread.Connection.WriteBuffer(output[0],6+output[5]);
        end;

        output[0]:=SC_StopProcessList; //end of process list
        athread.Connection.WriteBuffer(output[0],1);
      end;

      CS_OpenProcess: //procid: dword
      begin
        log.lines.add('Open process');
        athread.Connection.ReadBuffer(dw,4);

        DetachIfPossible;
        if processhandle<>0 then closehandle(processhandle);
        processhandle:=0;
        ProcessID:=dw;
        Open_Process;

        if Processhandle>0 then
          output[0]:=SC_OpenProcessSuccess
        else
        begin
          output[0]:=SC_OpenProcessFailed;
          log.lines.add('Openprocess failed:'+IntToStr(le));
        end;

        athread.Connection.WriteBuffer(output[0],1);
      end;

      CS_ADDADDRESS:
      begin
        log.Lines.Add('Address added');
        inc(numberofrecords);
        setlength(memrec,numberofrecords);

        athread.Connection.ReadBuffer(memrec[numberofrecords-1].address,4);
        athread.Connection.ReadBuffer(memrec[numberofrecords-1].vartype,1);
        athread.Connection.ReadBuffer(memrec[numberofrecords-1].bit,1);
        athread.Connection.ReadBuffer(memrec[numberofrecords-1].bitlength,1);

        output[0]:=SC_RecordReceived;
        athread.Connection.WriteBuffer(output[0],1);
      end;

      CS_RefreshList:
      begin
        //update list (start,stop)
        log.Lines.Add('Updating the records of the client');

        athread.Connection.ReadBuffer(output[0],4);
        updatelist(pword(@output[0])^,pword(@output[2])^);
      end;

      CS_SetConfig:
      begin
        //ShowAsSigned:byte BinariesAsDecimal:byte max:word; buffersize:dword;skip_page_no_cache: byte;UseDebugRegs:byte;stealthusermode:byte;stealthkernelmode:byte
        log.Lines.Add('config:');
        athread.Connection.ReadBuffer(output[0],15);

        settings.ShowAsSigned:=output[0]=1;
        settings.BinariesAsDecimal:=output[1]=1;

        settings.max:=pword(@output[2])^;
        settings.buffersize:=pdword(@output[4])^;

        settings.skip_page_no_cache:=output[8]=1;
        settings.usedebugregs:=output[9]=1;

        if output[10]=1 then
        begin
          log.lines.add('Use DBK QueryMemoryRegion');
          UseDBKQueryMemoryRegion;
        end
        else
        begin
          log.lines.add('Use default QueryMemoryRegion');
          DontUseDBKQueryMemoryRegion;
        end;

        if output[11]=1 then
        begin
          log.lines.add('Use DBK Read/WriteProcessMemory');
          UseDBKReadWriteMemory;
        end else
        begin
          log.lines.add('Use default Read/WriteProcessMemory');
          DontUseDBKReadWriteMemory;
        end;

        if output[12]=1 then
        begin
          log.lines.add('Use DBK OpenProcess');
          UseDBKOpenProcess;
        end
        else
        begin
          log.lines.add('Use default OpenProcess');
          UseDBKOpenProcess;
        end;

        if output[13]=1 then
          enablestealth else disablestealth;

        if output[14]=1 then
          protectce;

        if settings.showassigned then log.Lines.Add('Show as signed');
        if settings.BinariesAsDecimal then log.Lines.Add('Handle binaries as decimals');
        log.lines.add('max='+IntToStr(settings.max));
        log.lines.add('buffersize='+IntToStr(settings.buffersize));
        if settings.skip_page_no_cache then log.Lines.Add('Don''t scan no cache mem');
        if settings.usedebugregs then log.Lines.Add('Use debug regs');


        cefuncproc.Skip_PAGE_NOCACHE:=settings.skip_page_no_cache;
        cefuncproc.buffersize:=settings.buffersize;


      end;

      CS_ClearRecordList:
      begin
        log.Lines.Add('Clear Record list');
        numberofrecords:=0;
        setlength(memrec,0);
      end;

      CS_ChangeValueOfAddress:
      begin
        log.Lines.add('Change value of address x');

        athread.Connection.ReadBuffer(output[0],3);
        athread.connection.readbuffer(output[3],output[2]);
        output[3+output[2]]:=0;

        changevalue(pword(@output[0])^,pchar(@output[3]));
      end;

      CS_FREEZEADDRESS:
      begin
        log.Lines.Add('freeze address');
        athread.connection.readbuffer(output[0],2);
        FreezeAddress(pword(@output[0])^);
      end;

      CS_ReadProcessMemory:
      begin
        //log.Lines.add('ReadProcessMemory');

        athread.connection.readbuffer(output[0],6);

        getmem(tempbuf,pword(@output[4])^);
        check:=readprocessmemory(processhandle,pointer(pdword(@output[0])^),tempbuf,pword(@output[4])^,actualread);

        output[0]:=SC_ReadProcessMemoryResult; //readprocess result
        if check then output[1]:=0 else output[1]:=1;
        pword(@output[2])^:=word(actualread);

        copymemory(@output[4],tempbuf,actualread);
        freemem(tempbuf);
        athread.Connection.WriteBuffer(output[0],4+word(actualread));

      end;

      CS_WriteProcessMemory:
      begin
        //log.Lines.add('WriteProcessMemory');

        athread.connection.readbuffer(output[0],4+2);
        athread.connection.readbuffer(output[6],pword(@output[4])^);

        check:=WriteProcessMemory(processhandle,pointer(pdword(@output[0])^),@output[6],pword(@output[4])^,actualwritten);

        output[0]:=12; //writeprocessmemory result
        if check then output[1]:=0 else output[1]:=1;
        pword(@output[2])^:=word(actualwritten);
        athread.Connection.WriteBuffer(output[0],4);

      end;

      CS_FirstScan:
      begin
        log.Lines.add('First Scan');
        athread.connection.readbuffer(output[0],4+4+1+1+1); //start(4),stop(4),scantype(1),vartype(1),stringlength(1)
        athread.connection.readbuffer(output[11],output[10]); //string user typed in
        athread.connection.readbuffer(output[11+output[10]],1); //scanoptions

        SThread:=TScanThread.Create(true);
        //set the scan options
        SThread.firstScan:=True;
        SThread.progressbar:=progressbar1;
        SThread.max:=settings.max;


        SThread.StartAddress:=pdword(@output[0])^;
        SThread.StopAddress:=pdword(@output[4])^;
        SThread.Vartype:=output[8];
        currenttype:=output[8];

        SThread.Scantype:=output[9];
        SThread.ScanOptions:=output[11+output[10]];

        output[11+output[10]]:=0;

        SThread.ScanValue:=pchar(@output[11]);

        //start the scan
        SThread.Resume;
      end;

      CS_NEXTSCAN:
      begin
        log.Lines.add('Next Scan');
        athread.connection.readbuffer(output[0],1+1); //scantype(1),stringlength(1)
        athread.connection.readbuffer(output[2],output[1]); //string user typed in
        athread.connection.readbuffer(output[2+output[1]],1); //scanoptions

        SThread:=TScanThread.Create(true);
        SThread.firstScan:=false;
        SThread.progressbar:=progressbar1;
        SThread.max:=settings.max;

        SThread.Scantype:=output[0];
        SThread.ScanOptions:=output[2+output[1]];
        output[2+output[1]]:=0;
        SThread.Scanvalue:=pchar(@output[2]);
        SThread.Vartype:=currenttype;

        SThread.Resume;
      end;

      CS_NEWSCAN:
      begin
        log.Lines.Add('New Scan');
        closefiles;
        freememory;
      end;

      CS_CANCELSCAN:
      begin
        log.Lines.Add('Client canceled the scan');

        if sthread<>nil then
        begin
          terminatethread(SThread.Handle,0);
          freememory;
          closefiles;


          SThread.Free;
          SThread:=nil;
        end;
      end;

      CS_DELETEADDRESS:
      begin
        log.lines.add('Delete address');
        athread.Connection.ReadBuffer(recordnr,2);


        if numberofrecords-1>=recordnr then
        begin
          for j:=recordnr to numberofrecords-2 do
            memrec[j]:=memrec[j+1];

          dec(numberofrecords);
          setlength(memrec,numberofrecords);
        end;
      end;

      CS_SETTIMERSPEED:
      begin
        log.Lines.Add('Update timer intervals');
        athread.Connection.ReadBuffer(output[0],4);
        updatetimer.Interval:=pword(@output[0])^;
        freezeTimer.Interval:=pword(@output[2])^;
      end;

      CS_UNFREEZEADDRESS:
      begin
        log.lines.Add('Address unfrozen');
        athread.Connection.ReadBuffer(output[0],2);
        memrec[pword(@output[0])^].frozen:=false;
      end;

    {  CS_SetHyperscanState:
      begin

        athread.Connection.ReadBuffer(output[0],1);

        if output[0]=1 then //on
        begin
          log.Lines.Add('enable hyperscan');

          hyperscanenabled:=true;
          try
            if not speedhackenabled then enablehypermode;
          except
            hyperscanenableD:=false;
          end;

        end
        else
        begin
          //off
          log.Lines.Add('disable hyperscan');
          hyperscanenabled:=false;
          try
            If not speedhackenabled then disablehypermode;
          except

          end;

        end;

        output[0]:=SC_Hyperscanstatus;

        if hyperscanenabled and hypermode then
          output[1]:=1 //on
        else
          output[1]:=0;

        athread.Connection.WriteBuffer(output[0],2);
      end;

      CS_EnableSpeedhack:
      begin
        log.Lines.Add('Starting speedhack');
        athread.Connection.ReadBuffer(output[0],8);

        speedhackenabled:=true;
        try
          enableHypermode;
        except
          speedhackenableD:=false;
        end;


         //speed:single;sleeptime:dword)
        dw:=sendmessage(hyperscanwindow,wm_user+6,pdword(@output[4])^,pdword(@output[0])^);
        if dw<>12345 then
        begin
          log.Lines.Add('Failed to start the speedhack');
          speedhackenableD:=false;
        end;

        output[0]:=SC_SpeedhackStatus;
        if speedhackenabled then output[1]:=1 else output[1]:=0;
        athread.Connection.WriteBuffer(output[0],2);

      end;

      cs_disablespeedhack:
      begin
        log.lines.add('Dissable speedhack');
        if speedhackenabled then
          if hyperscanenabled then disablespeedhack else disablehypermode;

        //tell the client the status of the speedhack
        output[0]:=SC_SpeedhackStatus;
        if speedhackenabled then output[1]:=1 else output[1]:=0;
        athread.Connection.WriteBuffer(output[0],2);
      end;       }

      CS_EnableDebugger:
      begin
        output[0]:=SC_Debuggerstatus;
        output[1]:=0;

        try
          log.Lines.Add('Requested to start the debugger if it isn''t working yet');
          if startdebuggerifneeded then output[1]:=1;
          log.lines.add('still allive');
        except
          log.Lines.Add('Bah, an exception occured while enabling the debugger')
        end;

        athread.Connection.WriteBuffer(output[0],2);
      end;

      CS_FindWhatWrites:  //(address: dword,size:word);
      begin
        athread.Connection.ReadBuffer(output[0],6);
        log.lines.add('Find out what writes to '+IntToHex(pdword(@output[0])^,8)+ '(length='+inttostr(pword(@output[4])^)+')');
        SetWriteBreakpoint(pdword(@output[0])^,pword(@output[4])^);
      end;

      CS_FindWhatReads:
      begin
        athread.Connection.ReadBuffer(output[0],6);
        log.lines.add('Find out what reads from '+IntToHex(pdword(@output[0])^,8)+ '(length='+inttostr(pword(@output[4])^)+')');
        SetReadBreakpoint(pdword(@output[0])^,pword(@output[4])^);
      end;

      CS_FindWhatAccesses:
      begin
        athread.Connection.ReadBuffer(output[0],6);
        log.lines.add('Find out what accesses '+IntToHex(pdword(@output[0])^,8)+ '(length='+inttostr(pword(@output[4])^)+')');
        SetReadWriteBreakpoint(pdword(@output[0])^,pword(@output[4])^);
      end;

      CS_StopCodeFinder:
      begin
        log.Lines.Add('Stop the codefinder');

        try
          crdebugging.Acquire;
          with debuggerthread do
          begin
            if (debuggerthread=nil) or (not debuggerthread.attached) then
              continue;

            if findwriter2 then
            begin
              debuggerthread.Suspend;
              zeromemory(@debuggerthread.DRRegs,sizeof(debuggerthread.DRRegs));
              debuggerthread.DRRegs.ContextFlags:=CONTEXT_DEBUG_REGISTERS;
              debuggerthread.DRRegs.Dr7:=reg0set or reg1set or reg2set or reg3set;

              for i:=0 to length(debuggerthread.threadlist)-1 do
              begin
                suspendthread(debuggerthread.threadlist[i][1]);
                if not SetThreadContext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs) then showmessage('I cant seem to remove the breakpoint from one of the threads!');
                resumethread(debuggerthread.threadlist[i][1]);
              end;

              debuggerthread.FindWriter2:=false;
              debuggerthread.Resume;
            end
            else
            begin
              if WaitForSingleObject(semaphore,30000)=WAIT_FAILED then
              begin
                debuggerthread.Terminate;
                debuggerthread.free;
                debuggerthread:=nil;

                closehandle(debugger.Semaphore);
                debugger.Semaphore:=createsemaphore(nil,1,1,nil);
                terminateprocess(processhandle,0);
                close;
                continue;
              end;

              //set the original protection back
              if debuggerthread.readonlyset then
                VirtualProtectEx(processhandle,pointer(readonly.Address),readonly.size,readonly.originalprotection,dw);

              if debuggerthread.findreaderset then
                VirtualProtectEx(processhandle,pointer(findreader.Address),findreader.size,findreader.originalprotection,dw);


              //set the read-only flag to false
              debuggerthread.readonlyset:=false;
              debuggerthread.findreaderset:=false;
              debuggerthread.alsowrites:=false;

              debuggerthread.readonlyremoved:=true;
              debuggerthread.findreaderremoved:=true;

              releasesemaphore(semaphore,1,nil);
            end;
          end;

        finally
          crdebugging.release;
        end;

      end;

      CS_VirtualProtectEx:
      begin
        log.Lines.Add('VirtualProtectEx');
        athread.Connection.ReadBuffer(output[0],12);
        if VirtualProtectEx(processhandle,pointer(pdword(@output[0])^),pdword(@output[4])^,pdword(@output[8])^,dw) then
          output[1]:=1
        else
          output[1]:=0;

        output[0]:=SC_VirtualProtectExResult;
        pdword(@output[2])^:=dw;

        athread.Connection.WriteBuffer(output[0],6);
      end;


      CS_SuspenProcess:
      begin
        debuggerthread.Suspend;
        for i:=0 to length(debuggerthread.threadlist)-1 do
          suspendthread(debuggerthread.threadlist[i][1]);
      end;

      CS_ResumeProcess:
      begin
        debuggerthread.resume;
        for i:=0 to length(debuggerthread.threadlist)-1 do
          resumethread(debuggerthread.threadlist[i][1]);
      end;


      253:
      begin
        log.Lines.Add('data');
        output[0]:=253;
        athread.Connection.WriteBuffer(output[0],1);
      end;


      255:
      begin
        log.lines.add('Keep-alive');
        keepalivesend:=false;
      end;

      else log.Lines.Add('WTF?');
    end;

  end;

  finally
    log.Lines.Add(peerip+' disconnected');
    senddata:=true;
    setlength(memrec,0);
    numberofrecords:=0;
    online:=false;
  end;

  except
    //
  end;
end;

procedure TForm1.Savetofile1Click(Sender: TObject);
begin
  if opendialog1.Execute then
    log.Lines.SaveToFile(opendialog1.filename);
end;

procedure TForm1.disablespeedhack;
begin
{
  speedhackenabled:=false;
  postmessage(hyperscanwindow,wm_user+5,1,0);}
end;


procedure TForm1.disableHypermode;
var i: integer;
begin
{  if not hypermode then exit;

  if speedhackenabled then disablespeedhack;
  hyperscanenabled:=false;
  speedhackenableD:=false;

  if hyperscanwindow<>0 then postmessage(HyperscanWindow,wm_destroy,0,0);

  hyperscanwindow:=0;
  CEScanHook:=0;
  hypermode:=false; }
end;

procedure TForm1.enableHypermode;
var
  CEScanProcAddress:pointer;

  winhandle,possiblewinhandle: thandle;
  winprocess: dword;
  winthreadid: dword;
  i: integer;
begin
(*  if hypermode then exit;

  {$ifdef debug}
  showmessage('hypermode is going to be enabled');
  {$endif}

  //find a window that belongs to the program (preferable the main window, the one with most objects)
  possiblewinhandle:=0;
  hyperscanwindow:=0;

  {$ifdef debug}
  showmessage('Going to find a suitable window');
  {$endif}

  winhandle:=getwindow(getforegroundwindow,GW_HWNDFIRST);
  while winhandle<>0 do
  begin
    winthreadid:=GetWindowThreadProcessId(winhandle,@winprocess);
    if winprocess=processid then
    begin
      possiblewinhandle:=winhandle;
      if GetWindow(possiblewinhandle,GW_CHILD)<>0 then break;  //if we find one that has at least one component then stop searching
    end;
    winhandle:=getwindow(winhandle,GW_HWNDNEXT);
  end;

  if possiblewinhandle=0 then exit;

  {$ifdef debug}
  showmessage('A suitable window has been found. It''s thread is '+IntToStr(possiblewinhandle));
  {$endif}

  {$ifdef debug}
  showmessage('Loading the CEHook.dll file');
  {$endif}

  CEHOOKDLL:=LoadLibrary('CEHook.dll');
  if CEHOOKDLL=0 then exit;

  {$ifdef debug}
  showmessage('Dll has been loaded. DLLHandle='+IntToStr(cehookdll));
  {$endif}

  {$ifdef debug}
  showmessage('Going to find the MyHook function');
  {$endif}

  //still here so the dll is loaded
  CEScanProcAddress:=GetProcAddress(CEHOOKDLL,'MyHook');
  if (CEScanProcAddress=nil) then//something went wrong (dont know why though)
  begin
    {$ifdef debug}
    showmessage('The MyHook api was not found');
    {$endif}

    FreeLibrary(CEHOOKDLL);
    exit;
  end;

  {$ifdef debug}
  showmessage('MyHook function found:'+IntToHex(dword(CEScanProcAddress),8));
  {$endif}

  hyperscanview.startaddress:=processid; //use the startaddress to identify the target process. (so dont do CE)
  hyperscanview.mainformhandle:=handle;

  {$ifdef debug}
  showmessage('Calling SetwindowsHookEx using the threadid from the window I found');
  {$endif}

  hyperscanview.scanning:=false;
  CEScanHook:=setwindowshookex(WH_CALLWNDPROCRET	,CEScanProcAddress,CEHOOKDLL,{GetWindowThreadProcessId(possiblewinhandle,@winprocess)}0); //just to get the dll inside the process
  hyperscanview.StopAddress:=CEScanhook;


  {$ifdef debug}
  showmessage('The result of CEScanHook is '+IntToStr(CEScanHook));
  {$endif}

  hyperscanview.formscanningHandle:=CEScanHook;
  SendMessage(possiblewinhandle,wm_user+666,$33333333,0);


  {$ifdef debug}
  showmessage('Going to send the target window the message to initialize');
  {$endif}

  i:=0;
  while (not hyperscanview.scanning) and (i<500) do
  begin
    sleep(10);
    inc(i);
  end;


  UnhookWindowsHookEx(CEScanHook);
  FreeLibrary(CEHOOKDLL);
  hyperscanview.scanning:=false;

  if i>=500 then
  begin
    hyperscanenabled:=false;
    speedhackenableD:=false;
  end;



  //SendMessage(possiblewinhandle,wm_user+666,$33333333,0);

  hyperscanwindow:=hyperscanview.hyperscanwindow;

  if speedhackenabled then
    sendmessage(hyperscanwindow,wm_user+4,0,0); //enable speedhack

  hypermode:=true;
  *)
end;


procedure Tform1.SetReadWriteBreakpoint(address: dword; size: dword);
var mbi: _Memory_Basic_Information;
    i: integer;
    ct: _context;
    regsinuse: integer;
    olda,olds: dword;
    dr: dword;

procedure Set4bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0rw or debugexact or reg0len4;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1rw or debugexact or reg1len4;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2rw or debugexact or reg2len4;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3rw or debugexact or reg3len4;
       end;
  end;

  inc(address,4);
  dec(size,4);
  inc(regsinuse);
end;

procedure Set2bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0rw or debugexact or reg0len2;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1rw or debugexact or reg1len2;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2rw or debugexact or reg2len2;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3rw or debugexact or reg3len2;
       end;
  end;

  inc(address,2);
  dec(size,2);
  inc(regsinuse);
end;

procedure Set1bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0rw or debugexact;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1rw or debugexact;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2rw or debugexact;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3rw or debugexact;
       end;
  end;

  inc(address);
  dec(size);
  inc(regsinuse);
end;

resourcestring strAccessed='The following opcodes accessed the selected address';
var rd: dword;
    tmp: byte;
begin
  //check if you can read address to address+size
  readprocessmemory(processhandle,pointer(address),@tmp,1,rd);
  if rd<>1 then raise exception.Create(strAddressHasToBeReadable);


  olda:=address;
  olds:=size;
  zeromemory(@ct,sizeof(ct));
  ct.ContextFlags:=CONTEXT_DEBUG_REGISTERS;


  if settings.usedebugregs then
  begin
    regsinuse:=0;
    ct.dr7:=0;
    while (regsinuse<4) and (size>0) do
    begin
      if size>=4 then
      begin
        if (address mod 4)>0 then
        begin
          if (address mod 2)>0 then
          begin
            set1bytebreak; //watch on a byte
            continue;
          end
          else
          begin
            set2bytebreak;
            continue;
          end;
        end
        else
        begin
          set4bytebreak;
          continue;
        end;
      end;

      if size>=2 then
      begin
        if (address mod 2)>0 then
        begin
          set1bytebreak; //watch on a byte
          continue;
        end
        else
        begin
          set2bytebreak;
          continue;
        end;
      end;


      if size=1 then
        set1bytebreak;
    end;

   // ct.dr7:=$D0303;
    debuggerthread.DRRegs:=ct;


    debuggerthread.Suspend;
    for i:=0 to length(debuggerthread.threadlist)-1 do
    begin
      suspendthread(debuggerthread.threadlist[i][1]);
      if not SetThreadContext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs) then showmessage('failed 2');
      resumethread(debuggerthread.threadlist[i][1]);
    end;

    debuggerthread.FindWriter2:=true;
    debuggerthread.Resume;
  end
  else
  begin
    //dont use debug regs
    virtualqueryEx(processhandle,pointer(address),mbi,sizeof(mbi));

    debugger.DebuggerThread.findreader.pagebase:=dword(mbi.BaseAddress);
    debugger.DebuggerThread.findreader.pagesize:=dword(mbi.RegionSize);
    debugger.DebuggerThread.findreader.Address:=address;
    debugger.DebuggerThread.findreader.size:=size;
    DebuggerThread.findreaderset:=true;
    DebuggerThread.alsowrites:=true;

    VirtualProtectEx(processhandle,pointer(address),size,PAGE_NOACCESS,debugger.DebuggerThread.findreader.originalprotection);
  end;
end;


procedure Tform1.SetReadBreakpoint(address: dword; size: dword);
var mbi: _Memory_Basic_Information;
    i: integer;
    tmp:byte;
    rD:dword;
resourcestring strOpcodeRead='The following opcodes read from the selected address';
begin
  readprocessmemory(processhandle,pointer(address),@tmp,1,rd);
  if rd<>1 then raise exception.Create(strAddressHasToBeReadable);

  virtualqueryEx(processhandle,pointer(address),mbi,sizeof(mbi));

  debugger.DebuggerThread.findreader.pagebase:=dword(mbi.BaseAddress);
  debugger.DebuggerThread.findreader.pagesize:=dword(mbi.RegionSize);
  debugger.DebuggerThread.findreader.Address:=address;
  debugger.DebuggerThread.findreader.size:=size;
  DebuggerThread.findreaderset:=true;

  VirtualProtectEx(processhandle,pointer(address),size,PAGE_NOACCESS,debugger.DebuggerThread.findreader.originalprotection);
end;

procedure TForm1.SetWriteBreakpoint(address: dword; size: dword);
var mbi: _Memory_Basic_Information;
    i: integer;
    ct: _context;
    regsinuse: integer;
    olda,olds: dword;
    dr: dword;

procedure Set4bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0w or debugexact or reg0len4;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1w or debugexact or reg1len4;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2w or debugexact or reg2len4;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3w or debugexact or reg3len4;
       end;
  end;

  inc(address,4);
  dec(size,4);
  inc(regsinuse);
end;

procedure Set2bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0w or debugexact or reg0len2;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1w or debugexact or reg1len2;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2w or debugexact or reg2len2;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3w or debugexact or reg3len2;
       end;
  end;

  inc(address,2);
  dec(size,2);
  inc(regsinuse);
end;

procedure Set1bytebreak;
begin
  case regsinuse of
    0: begin
         ct.dr0:=address;
         ct.dr7:=ct.dr7 or reg0set or reg0w or debugexact;
       end;

    1: begin
         ct.dr1:=address;
         ct.dr7:=ct.Dr7 or reg1set or reg1w or debugexact;
       end;

    2: begin
         ct.Dr2:=address;
         ct.dr7:=ct.dr7 or reg2set or reg2w or debugexact;
       end;

    3: begin
         ct.Dr3:=address;
         ct.dr7:=ct.dr7 or reg3set or reg3w or debugexact;
       end;
  end;

  inc(address);
  dec(size);
  inc(regsinuse);
end;

var rd: dword;
    tmp: byte;
resourcestring strOpcodeChanged='The following opcodes changed the selected address';    
begin
  //check if you can read address to address+size
  readprocessmemory(processhandle,pointer(address),@tmp,1,rd);
  if rd<>1 then raise exception.Create(strAddressHasToBeReadable);


  olda:=address;
  olds:=size;
  zeromemory(@ct,sizeof(ct));
  ct.ContextFlags:=CONTEXT_DEBUG_REGISTERS;

  if settings.usedebugregs then
  begin
    regsinuse:=0;
    ct.dr7:=0;
    while (regsinuse<4) and (size>0) do
    begin
      if size>=4 then
      begin
        if (address mod 4)>0 then
        begin
          if (address mod 2)>0 then
          begin
            set1bytebreak; //watch on a byte
            continue;
          end
          else
          begin
            set2bytebreak;
            continue;
          end;
        end
        else
        begin
          set4bytebreak;
          continue;
        end;
      end;

      if size>=2 then
      begin
        if (address mod 2)>0 then
        begin
          set1bytebreak; //watch on a byte
          continue;
        end
        else
        begin
          set2bytebreak;
          continue;
        end;
      end;


      if size=1 then
        set1bytebreak;
    end;

   // ct.dr7:=$D0303;
    debuggerthread.DRRegs:=ct;


    debuggerthread.Suspend;
    for i:=0 to length(debuggerthread.threadlist)-1 do
    begin
      suspendthread(debuggerthread.threadlist[i][1]);
      if not SetThreadContext(debuggerthread.threadlist[i][1],debuggerthread.DRRegs) then showmessage('failed 2');
      resumethread(debuggerthread.threadlist[i][1]);
    end;

    debuggerthread.FindWriter2:=true;
    debuggerthread.Resume;
  end
  else
  begin
    virtualqueryEx(processhandle,pointer(olda),mbi,sizeof(mbi));

    debugger.DebuggerThread.readonly.pagebase:=dword(mbi.BaseAddress);
    debugger.DebuggerThread.readonly.pagesize:=dword(mbi.RegionSize);
    debugger.DebuggerThread.readonly.Address:=olda;
    debugger.DebuggerThread.readonly.size:=olds;
    DebuggerThread.readonlyset:=true;

    VirtualProtectEx(processhandle,pointer(olda),olds,PAGE_EXECUTE_READ,debugger.DebuggerThread.readonly.originalprotection);
  end;


end;



end.
