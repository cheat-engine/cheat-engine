unit ScanThread;

interface

uses
  Classes,windows,ScktComp,stdctrls,ComCtrls,sysutils;

//scanoptions
const
  SO_FASTSCAN=$1;
  SO_HEXADECIMAL=$2;
  SO_READONLY=$4;
  SO_FINDONLYONE=$8;
  SO_ISBINARY=$10;
  SO_UNICODE=$20;

type
  TScanThread = class(TThread)
  private
    { Private declarations }
    scanresults: int64;
    stringlist: TStringlist;
    lasterror: string;
    Procedure SendResults;
    procedure ScanFailed;
  public
    Firstscan: boolean;
    StartAddress:Dword;
    StopAddress:Dword;
    Scantype: byte;
    Vartype: byte;
    ScanValue: string;
    max: word;
    ScanOptions: byte;


    progressbar: TProgressbar;
  protected
    procedure Execute; override;
  end;

implementation

uses unit1,cefuncproc;
{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TScanThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{ TScanThread }

Procedure TScanThread.ScanFailed;
begin
  with form1 do
  begin
    log.Lines.Add('The scan failed. Reason:'+lasterror);
    output[0]:=SC_ScanFailed;
    sendbuf(1);
  end;
end;

Procedure TScanThread.SendResults;
var i,j: integer;
begin
  j:=0;
  for i:=0 to stringlist.count-1 do
  with form1 do
  begin
    if j>1024 then
    begin
      sendbuf(j);
      j:=0;
    end;

    output[j]:=SC_ScanResult;
    inc(j);

    output[j]:=length(stringlist[i]);
    inc(j);

    copymemory(@output[j],@stringlist[i][1],output[j-1]);
    inc(j,length(stringlist[i]));

  end;

  form1.sendbuf(j);

  form1.output[0]:=SC_ScanResultCount;
  pint64(@form1.output[1])^:=scanresults;
  form1.SendBuf(9);

  form1.Log.Lines.Add('done scanning');

  Form1.SThread:=nil;
end;

procedure TScanThread.Execute;
var count: int64;
begin
  { Place thread code here }
  FreeOnTerminate:=true;
  progressbar.Max:=100;
  progressbar.Position:=0;
  progressbar.Step:=1;

  stringlist:=TStringlist.Create;
  
  //check the parameters to see how to scan
    try
      if firstscan then
      begin
        if scantype=Advanced_Scan then
        begin
          //get memory ranges
          scanresults:=GetMemoryRanges2(startaddress,stopaddress,(scanoptions and SO_READONLY)=SO_READONLY,Progressbar,vartype,(scanoptions and SO_FASTSCAN)=SO_FASTSCAN);
        end
        else
        begin
          //get memory ranges and scan
          scanresults:=GetMemoryRangesAndScanValue2(startaddress,stopaddress,(scanoptions and SO_READONLY)=SO_READONLY,(scanoptions and SO_FINDONLYONE)=SO_FINDONLYONE,scantype,vartype,stringlist,scanvalue,'0',rounded,max,(scanoptions and SO_HEXADECIMAL)=SO_HEXADECIMAL,progressbar,(scanoptions and SO_FASTSCAN)=SO_FASTSCAN,(scanoptions and SO_UNICODE)=SO_UNICODE);
        end;
      end
      else
      begin
        //nextscan
        scanresults:=NextScan2(stringlist,scanvalue,'0',scantype,vartype,rounded,max,(scanoptions and SO_HEXADECIMAL)=SO_HEXADECIMAL,progressbar,(scanoptions and SO_FASTSCAN)=SO_FASTSCAN,(scanoptions and SO_UNICODE)=SO_UNICODE,false);
      end;

      Synchronize(SendResults);
    except
      on E: Exception do
      begin
        LastError:=e.Message;
        Synchronize(ScanFailed);
      end;
    end;

  //when done scanning send the results to the client
  stringlist.free;
end;

end.
