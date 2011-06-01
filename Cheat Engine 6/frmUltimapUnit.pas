unit frmUltimapUnit;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  DBK32functions, NewKernelHandler, cefuncproc, AvgLvlTree, math;

type TUltimap_DataHandlerThread=class(TThread)
  public
    branchtree: TAvgLvlTree;
    procedure execute; override;
end;

type
  TBranchData=record
    toAddress: PtrUInt;
    //other stuff to come
  end;

  PBranchdata=^TBranchData;

type

  { TfrmUltimap }

  TfrmUltimap = class(TForm)
    Button1: TButton;
    Button2: TButton;
    edtWorkerCount: TEdit;
    edtFilename: TEdit;
    edtBufSize: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    cbLogToFile: TRadioButton;
    cbParseData: TRadioButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { private declarations }
    branchtree: TAvgLvlTree;
    workers: Array of TUltimap_DataHandlerThread;

    function branchcompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
  public
    { public declarations }
  end; 

var
  frmUltimap: TfrmUltimap;

implementation

{$R *.lfm}

{TUltimap_DataHandlerThread}

procedure TUltimap_DataHandlerThread.execute;
var UltimapDataEvent: TUltimapDataEvent;
  buf: PByteArray;
  i: integer;
  z: qword;
begin
  while not terminated do
  begin
    if ultimap_waitForData(1000, @UltimapDataEvent) then
    begin
      try

        OutputDebugString('UltimapDataEvent.BlockID='+inttostr(UltimapDataEvent.BlockID));
        OutputDebugString('UltimapDataEvent.Address='+inttohex(UltimapDataEvent.Address,8));
        OutputDebugString('UltimapDataEvent.Size='+inttostr(UltimapDataEvent.Size));

        buf:=pointer(UltimapDataEvent.Address);
        try
          OutputDebugString(format('1=%x 2=%x 3=%x 4=%x\n',[buf[0], buf[1], buf[2], buf[3]]));

          z:=0;
          for i:=0 to UltimapDataEvent.Size-1 do
            z:=z+buf[i];

          if z=123 then
            outputdebugstring('z=123')
          else
            OutputDebugString('z is NOT 123');



        except
          OutputDebugString('The buffer was unreadable:'+inttostr(i)+'/'+inttostr(UltimapDataEvent.Size));
        end;


      finally
        ultimap_continue(@UltimapDataEvent);
      end;

    end else OutputDebugString('No event');
  end;
end;

{ TfrmUltimap }

function TfrmUltimap.branchcompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  //used to sort the binary tree
  result:=CompareValue(pbranchdata(Data1).toAddress, pbranchdata(Data2).toAddress);
end;

procedure TfrmUltimap.Button1Click(Sender: TObject);
var
  cr3: qword;
  bufsize: dword;
  filename: widestring;
  workercount: integer;
  i: integer;
begin
  GetCR3(processhandle, cr3);
  bufsize:=strtoint(edtBufSize.text);
  filename:=edtFilename.text;
  workercount:=strtoint(edtWorkerCount.text);

  if bufsize<2000 then
    raise exception.create('This function needs at least 200 bytes for the header of the buffer');

  if workercount>64 then
    raise exception.create('The maximum number of workers is 64');


  setlength(workers, workercount);

  branchtree:=TAvgLvlTree.CreateObjectCompare(branchcompare);


  if ultimap(cr3, (1 shl 6) or (1 shl 7) or (1 shl 9) or (1 shl 8), bufsize, false, pwidechar(filename), workercount) then
  begin
    for i:=0 to workercount-1 do
    begin
      workers[i]:=TUltimap_DataHandlerThread.Create(true);
      workers[i].branchtree:=branchtree;
      workers[i].Start;
    end;
  end;
end;

procedure TfrmUltimap.Button2Click(Sender: TObject);
var i: integer;
begin
  for i:=0 to length(workers)-1 do
    workers[i].Terminate;

  for i:=0 to length(workers)-1 do
    workers[i].WaitFor;


  ultimap_disable();
end;

end.

