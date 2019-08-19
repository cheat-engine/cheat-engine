unit frmBusyUnit;

{$mode objfpc}{$H+}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, memscan;

type

  { TfrmBusy }

  TfrmBusy = class(TForm)
    Label1: TLabel;
    Timer1: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    oktoclose: boolean;
    fReason: TPostScanState;
    procedure setReason(r: TPostScanState);
  public
    { public declarations }
    WaitForHandle: THandle;
    memscan: TMemScan;
    property reason: TPostScanState read fReason write setReason;
  end;

resourcestring
  rsBusy='The previous scan is still being processed. Please wait';
  rsJustFinished = '(Just starting to process)';
  rsOptimizingScanResults = '(Optimizing result list for improved speed)';
  rsTerminatingThreads = '(Freeing scanner thread memory)';
  rsSavingFirstScanResults = '(Copying results for first scan scanner option)';
  rsShouldBeFinished = '(There''s no reason to wait)';
  rsSavingFirstScanResults2 = '(Still copying the results for the first scan '
    +'scanner option)';

implementation

{$R *.lfm}

{ TfrmBusy }

procedure TfrmBusy.setReason(r: TPostScanState);
var reasonstr: string;
begin
  if r<>fReason then
  begin
    freason:=r;

    reasonstr:=#13#10;

    case r of
      psJustFinished: reasonstr:=reasonstr+rsJustFinished;
      psOptimizingScanResults: reasonstr:=reasonstr+rsOptimizingScanResults;
      psTerminatingThreads: reasonstr:=reasonstr+rsTerminatingThreads;
      psSavingFirstScanResults: reasonstr:=reasonstr+rsSavingFirstScanResults;
      psShouldBeFinished: reasonstr:=reasonstr+rsShouldBeFinished;
      psSavingFirstScanResults2: reasonstr:=reasonstr+rsSavingFirstScanResults2
      else
        reasonstr:='';
    end;



    label1.caption:=rsBusy+reasonstr;
  end;
end;

procedure TfrmBusy.FormShow(Sender: TObject);
begin
  timer1.enabled:=true;
end;

procedure TfrmBusy.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  canclose:=oktoclose;
end;

procedure TfrmBusy.Timer1Timer(Sender: TObject);
var r:dword;
begin
  if (WaitForHandle<>0) then
  begin
    r:=WaitForSingleObject(WaitForHandle, 50);
    if r<>WAIT_TIMEOUT then
    begin
      oktoclose:=true;

      if fsModal in FFormState then
      begin
        if r=WAIT_OBJECT_0 then
          modalresult:=mrok
        else
          modalresult:=mrcancel;
      end
      else
        close;

    end;
  end;

  if (memscan<>nil) and (reason<>memscan.postscanstate) then
  begin
    reason:=memscan.postScanState;
  end;
end;

end.

