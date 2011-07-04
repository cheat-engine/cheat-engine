unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, CheckLst,tlhelp32, ExtCtrls;

type
  grouptype = array [1..4] of boolean;

type
  MemoryRecord = record
        Description : string[50];
        Address : dword;
        VarType : byte;
        Bit     : Byte;
        Frozen : boolean;
        FrozenValue : Int64;
        Group:  Byte;
  end;

type
  TForm1 = class(TForm)
    Timer1: TTimer;
    Timer2: TTimer;
    procedure Timer1Timer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
    function Open_Process(pr_name: string): boolean;
  public
    { Public declarations }
    Processhandle: Dword;
    memrec: array of MemoryRecord;

    numberofrecords: Integer;
    Frozenrecords: array of boolean;
    FrozenFvalue: array of double;
    FrozenSValue: array of String[255];

    Processname: String[50];
    TrainerTitle: String[50];
    Usercomments: String[200];

    groups:  grouptype;
    groupnames: array[1..4] of string[50];

    grouplist: array[1..4] of byte;

    UsePicture: Boolean;
    imagestream: TMemorystream;

    style: byte;
    MemrecToListbox: array of Integer; //the index sas wich record it is, the byte at wich index of the listbox
    function checkitem(x: Integer):boolean;
    procedure UncheckItem(x: Integer);
  end;

var
  Form1: TForm1;

implementation

uses style3unit, Style1Unit, style2unit;

{$R *.DFM}

function TForm1.Open_Process(pr_name: String):boolean;
Var SNAPHandle: THandle;
    ProcessEntry: ProcessEntry32;
    Check: Boolean;
    FullProcessName: String;
    I: Integer;
    procname: string;
begin
  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  If SnapHandle>0 then
  begin
    ProcessEntry.dwSize:=SizeOf(ProcessEntry);
    Check:=Process32First(SnapHandle,ProcessEntry);
    while check=true do
    begin
      FullProcessName:='';
      FullProcessName:=processentry.szExeFile;
      i:=Length(FullProcessName);
      while (i>0) and (FullProcessname[i-1]<>'\') do dec(i);
      procname:=copy(FullProcessName,i,length(FullProcessname)-i+1);
      if uppercase(procname)=uppercase(processname) then
      begin
        Processhandle:=OpenProcess(PROCESS_ALL_ACCESS,false,processentry.th32ProcessID);
        result:=true;
        exit;
      end;
      check:=Process32Next(SnapHandle,ProcessEntry);
    end;
    result:=false;
  end else result:=false;
end;

Procedure TForm1.UncheckItem(x: integer);
var index: array of Integer;
    number: Integer;
    i: Integer;
    recnr: Integer;

begin
  number:=0;
  for i:=0 to numberofrecords-1 do
  begin
    if MemrecToListbox[i]=x then
    begin
      inc(number);
      setlength(index,number);
      index[number-1]:=i;
    end;
  end;

  for i:=0 to number-1 do
  begin
    //freeze the records in index
    recnr:=index[i];
    memrec[recnr].Frozen:=false;
  end;

end;

function TForm1.checkitem(x: Integer):boolean;
var index: array of Integer;
    number: Integer;
    i: Integer;
    recnr: Integer;
    //------
    error: Dword;
    controle: string;
    db:  byte;
    dw:  word;
    dd:  dword;
    a: single;
    b: double;
    dI64: int64;


begin
  //figure out wich records are selected to be frozen.
  if processhandle=0 then
  begin
    //find the process and open it. If it fails give a message and exit the procedure
    if not open_process(processname) then
    begin
      showmessage('You must load the game before you can check this!');
      result:=false;
      exit;
    end;
  end;

  number:=0;
  for i:=0 to numberofrecords-1 do
  begin
    if MemrecToListbox[i]=x then
    begin
      inc(number);
      setlength(index,number);
      index[number-1]:=i;
    end;
  end;

  result:=false;
  for i:=0 to number-1 do
  begin
    //freeze the records in index
    recnr:=index[i];
    if not frozenrecords[recnr] then
    begin //find the current value
      case memrec[index[i]].VarType of
        0  :  begin  //byte
                readprocessmemory(processhandle,pointer(memrec[recnr].Address),addr(db),1,error);
                if error=1 then
                begin
                  memrec[recnr].FrozenValue:=db;
                  memrec[recnr].Frozen:=true;
                  result:=true;
                end;
              end;

        1  :  begin  //word
                readprocessmemory(processhandle,pointer(memrec[recnr].Address),addr(dw),2,error);
                if error=2 then
                begin
                  memrec[recnr].FrozenValue:=dw;
                  memrec[recnr].Frozen:=true;
                  result:=true;
                end;
              end;

        2  :  begin  //dword
                readprocessmemory(processhandle,pointer(memrec[recnr].Address),addr(dd),4,error);
                if error=4 then
                begin
                  memrec[recnr].FrozenValue:=dd;
                  memrec[recnr].Frozen:=true;
                  result:=true;
                end;
              end;

        3  :  begin  //single
                readprocessmemory(processhandle,pointer(memrec[recnr].Address),addr(a),4,error);
                if error=4 then
                begin
                  controle:=FloatToStr(a);
                  if system.pos('NAT',controle)>0 then exit;
                  if system.pos('INF',controle)>0 then exit;
                  frozenfvalue[recnr]:=a;
                  memrec[recnr].Frozen:=true;
                  result:=true;
                end;
              end;

        4  :  begin
                readprocessmemory(processhandle,pointer(memrec[recnr].Address),addr(b),8,error);
                if error=8 then
                begin
                  controle:=FloatToStr(a);
                  if system.pos('NAT',controle)>0 then exit;
                  if system.pos('INF',controle)>0 then exit;
                  frozenfvalue[recnr]:=b;
                  memrec[recnr].Frozen:=true;
                  result:=true;
                end;
              end;

        5  :  begin  //bit
                readprocessmemory(processhandle,pointer(memrec[recnr].Address),addr(db),1,error);
                if error=1 then
                begin
                  //find out if there's a 1 or 0 at that bit
                  case memrec[recnr].Bit of
                    0  :   if (db and 1)=0 then memrec[recnr].FrozenValue:=0 else memrec[recnr].FrozenValue:=1;
                    1  :   if (db and 2)=0 then memrec[recnr].FrozenValue:=0 else memrec[recnr].FrozenValue:=1;
                    2  :   if (db and 4)=0 then memrec[recnr].FrozenValue:=0 else memrec[recnr].FrozenValue:=1;
                    3  :   if (db and 8)=0 then memrec[recnr].FrozenValue:=0 else memrec[recnr].FrozenValue:=1;
                    4  :   if (db and 16)=0 then memrec[recnr].FrozenValue:=0 else memrec[recnr].FrozenValue:=1;
                    5  :   if (db and 32)=0 then memrec[recnr].FrozenValue:=0 else memrec[recnr].FrozenValue:=1;
                    6  :   if (db and 64)=0 then memrec[recnr].FrozenValue:=0 else memrec[recnr].FrozenValue:=1;
                    7  :   if (db and 128)=0 then memrec[recnr].FrozenValue:=0 else memrec[recnr].FrozenValue:=1;
                  end;

                  memrec[recnr].Frozen:=true;
                  result:=true;
                end;
              end;

        6  :  begin  //int64
                readprocessmemory(processhandle,pointer(memrec[recnr].Address),addr(dI64),8,error);
                if error=8 then
                begin
                  memrec[recnr].FrozenValue:=di64;
                  memrec[recnr].Frozen:=true;
                  result:=true;
                end;
              end;
         end;
    end else
    begin
      memrec[recnr].Frozen:=true; //use the value stored.
      result:=true;
    end;
  end;

  if result=false then
  begin
    showmessage('I can''t freeze this address. Is the right game (or version) running?');
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
var i: Integer;
    write: dword;
    write1: byte;
    write2: word;
    write3: dword;
    write4: single;
    write5: double;
    write6: Int64;
begin
  for i:=0 to numberofrecords-1 do
    if memrec[i].frozen then
    begin
      case memrec[i].VarType of
        0:      begin
                  write1:=byte(memrec[i].FrozenValue);
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write1),1,write);
                end;

        1:      begin
                  write2:=word(memrec[i].FrozenValue);
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write2),2,write);
                end;

        2:      begin
                  write3:=memrec[i].FrozenValue;
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write3),4,write);
                end;

        3:      begin
                  write4:=FrozenFValue[i];
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write4),4,write);
                end;

        4:      begin
                  write5:=FrozenFValue[i];
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write5),8,write);
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
                end;

        6:      begin  //int64
                  write6:=memrec[i].FrozenValue;
                  writeprocessmemory(processhandle,pointer(memrec[i].Address),addr(write6),8,write);
                end;


      end;

    end;

end;

procedure TForm1.FormCreate(Sender: TObject);
var settings: TFileStream;
    startsettings: int64;
    records: dword;
    i: Integer;
{
var
    TRpicture: TFilestream;
    TrainerDAT: TFileStream;
    Trainer: TFileStream;

    StartOfSettings: Int64;
    i: integer;

    groups: Grouptype;      //array[1..4] of boolean;

    results: word;
}
begin
  processhandle:=0;
  settings:=TFileStream.Create(application.ExeName,fmOpenRead or fmShareDenyNone);
  settings.Seek(-8,sofromend);
  settings.ReadBuffer(startsettings,sizeof(startsettings));
  settings.Seek(startsettings,sofrombeginning);

  settings.ReadBuffer(processname,sizeof(processname));
  settings.ReadBuffer(trainertitle,sizeof(trainertitle));
  settings.ReadBuffer(usercomments,sizeof(usercomments));

  settings.ReadBuffer(records,sizeof(records));
  numberofrecords:=records;
  setlength(memrec,numberofrecords);
  setlength(frozenfvalue,numberofrecords);
  setlength(frozenSvalue,numberofrecords);
  setlength(MemrecToListbox,numberofrecords);
  setlength(frozenrecords,numberofrecords);

  settings.ReadBuffer(style,sizeof(style));
  settings.ReadBuffer(usepicture,sizeof(usepicture));
  settings.ReadBuffer(groups,sizeof(groups));
  settings.ReadBuffer(groupnames,sizeof(groupnames));
  settings.ReadBuffer(pointer(memrec)^,records*sizeof(memoryrecord));
  settings.ReadBuffer(pointer(frozenfvalue)^,records*sizeof(double));
  settings.ReadBuffer(pointer(frozensvalue)^,records*255);  
  if usepicture then
  begin
    imagestream:=TMemorystream.Create;
    imagestream.CopyFrom(settings,settings.Size-settings.Position);
  end;

  for i:=0 to numberofrecords-1 do
  begin
    frozenrecords[i]:=memrec[i].Frozen;
    memrec[i].Frozen:=false;
  end;

  settings.free;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  application.Title:=Trainertitle;
  case style of
    1: style1.show;
    2: style2.show;
    3: style3.show;
    else application.Terminate;
  end;
  timer2.Enabled:=false;
end;

end.
