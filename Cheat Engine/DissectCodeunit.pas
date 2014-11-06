unit DissectCodeunit;

{$MODE Delphi}

interface

uses
  jwawindows, windows, LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,DissectCodeThread,CEFuncProc,
  symbolhandler, LResources, frmReferencedStringsUnit, newkernelhandler, MemFuncs, commonTypeDefs;



type TOnDoneDissect=(odDoNothing, odOpenReferedStringList, odOpenReferedFunctionsList);

type

  { TfrmDissectCode }

  TfrmDissectCode = class(TForm)
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Panel1: TPanel;
    lbModuleList: TListBox;
    Panel2: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Panel3: TPanel;
    Label6: TLabel;
    Label7: TLabel;
    btnStart: TButton;
    cbIncludesystemModules: TCheckBox;
    Label4: TLabel;
    lblStringRef: TLabel;
    Label5: TLabel;
    lblConditionalJumps: TLabel;
    Label9: TLabel;
    lblUnConditionalJumps: TLabel;
    Label11: TLabel;
    lblCalls: TLabel;
    Label1: TLabel;
    lblMaxOffset: TLabel;
    procedure btnStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure cbIncludesystemModulesClick(Sender: TObject);
  private
    { Private declarations }
    starttime: dword;
    procedure cleanModuleList;
    procedure fillModuleList(withSystemModules: boolean);
  public
    { Public declarations }
    ondone: TOnDoneDissect;

  end;

var
  frmDissectCode: TfrmDissectCode;

implementation

uses frmReferencedFunctionsUnit;

resourcestring
  rsStop = 'Stop';
  rsStart = 'Start';
  rsPleaseSelectSomethingToScan = 'Please select something to scan';
  rsDone = 'done';


procedure TfrmDissectCode.btnStartClick(Sender: TObject);
var start,stop:PtrUInt;
    tempregions: tmemoryregions;
    i,j: integer;
    temp: tmemoryregion;
    h,m,s,ms: word;
    n: integer;
    flipped: boolean;
begin
  if btnStart.caption=rsStop then
  begin
    timer1.Enabled:=false;
    if dissectcode<>nil then
    begin
      dissectcode.cancelscan;
      dissectcode.clear;
    end;
    Timer1Timer(timer1);

    btnStart.Caption:=rsStart;
    //showmessage('dissected till address '+inttohex(dissectcode.currentaddress,8));
    exit;
  end;



  if lbModuleList.SelCount=0 then raise exception.Create(rsPleaseSelectSomethingToScan);

  if dissectcode=nil then
    dissectcode:=TDissectCodeThread.create(false);

  dissectcode.clear;


  setlength(dissectcode.memoryregion,0);

  for i:=0 to lbModuleList.items.count-1 do
  begin
    if lbModuleList.Selected[i] then
    begin
      getexecutablememoryregionsfromregion(tmoduledata(lbModuleList.Items.Objects[i]).moduleaddress,tmoduledata(lbModuleList.Items.Objects[i]).moduleaddress+tmoduledata(lbModuleList.Items.Objects[i]).modulesize,tempregions);
      setlength(dissectcode.memoryregion,length(dissectcode.memoryregion)+length(tempregions));

      for j:=0 to length(tempregions)-1 do
        dissectcode.memoryregion[length(dissectcode.memoryregion)-length(tempregions)+j]:=tempregions[j];
    end;
  end;


  //sort the regions so they are from big to small (bubblesort)
  n:=length(dissectcode.memoryregion);
  for i:=0 to n-1 do
  begin
    flipped:=false;
    for j:=0 to n-2-i do
    begin
      if dissectcode.memoryregion[j+1].BaseAddress<dissectcode.memoryregion[j].BaseAddress then//swap
      begin
        temp:=dissectcode.memoryregion[j+1];
        dissectcode.memoryregion[j+1]:=dissectcode.memoryregion[j];
        dissectcode.memoryregion[j]:=temp;
        flipped:=true;
      end;
    end;

    if not flipped then break;
  end;

  btnStart.Caption:=rsStop;
  timer1.Enabled:=true;

  starttime:=gettickcount;

  dissectcode.dowork;

end;

procedure TfrmDissectCode.FormCreate(Sender: TObject);
begin
  btnstart.caption:=rsStart;
end;

procedure TfrmDissectCode.Timer1Timer(Sender: TObject);
var h,m,s,ms: word;
    currenttime: int64;

    x: double;
begin
  currenttime:=gettickcount;
  currenttime:=currenttime-starttime;
  //currenttime holds the number of milliseconds that have passed (usually devidable by 1000 because of the timer)

  //dissectcode.bytesread holds the number of bytes read in currenttime
  x:=dissectcode.totalread/currenttime;
  if x=0 then beep;
  //x now holds the number of bytes it scans in 1 ms
  //bytes left / x = milliseconds left

  x:=(dissectcode.totalmemory-dissectcode.totalread)/x;
  currenttime:=trunc(x);

  ms:=currenttime mod 1000;
  currenttime:=currenttime div 1000;
  s:=currenttime mod 60;
  currenttime:=currenttime div 60;
  m:=currenttime mod 60;
  currenttime:=currenttime div 60;
  h:=currenttime;
  label7.caption:=format('%.2d:%.2d:%.2d',[h,m,s]);


  lblStringRef.caption:=inttostr(dissectcode.nrofstring);
  lblConditionalJumps.caption:=inttostr(dissectcode.nrofconditionaljumps);
  lblUnConditionalJumps.caption:=inttostr(dissectcode.nrofunconditionaljumps);
  lblCalls.caption:=inttostr(dissectcode.nrofcalls);
  lblMaxOffset.caption:=inttostr(dissectcode.maxoffset);


  progressbar1.position:=dissectcode.percentagedone;

  progressbar1.Hint:=inttohex(dissectcode.currentaddress,8);
  if dissectcode.done then
  begin
    timer1.Enabled:=false;
    btnStart.Caption:=rsStart;
    ProgressBar1.Position:=0;
    label7.Caption:=rsDone;




    if ondone=odOpenReferedStringList then
    begin
      close;
      if frmReferencedStrings=nil then
        frmReferencedStrings:=tfrmReferencedStrings.Create(self);

      frmReferencedStrings.Show;
    end;

    if ondone=odOpenReferedFunctionsList then
    begin
      close;
      if frmReferencedFunctions=nil then
        frmReferencedFunctions:=tfrmReferencedFunctions.create(self);

      frmReferencedFunctions.show;
    end;

    ondone:=odDoNothing;
  end;
end;

procedure TfrmDissectCode.FormClose(Sender: TObject;
  var Action: TCloseAction);
var i: integer;
begin
  cleanModuleList;

end;

procedure TfrmDissectCode.cleanModuleList;
begin
  cefuncproc.cleanModuleList(lbModulelist.items);
end;

procedure TfrmDissectCode.fillModuleList(withSystemModules: boolean);
begin
  cefuncproc.GetModuleList(lbModuleList.Items, withSystemModules);
end;

procedure TfrmDissectCode.FormShow(Sender: TObject);
begin
  fillModuleList(cbIncludesystemModules.checked);
  if lbModuleList.Count>0 then
  begin
    lbModuleList.ItemIndex:=0;
    lbModuleList.Selected[0]:=true;
  end;
end;

procedure TfrmDissectCode.cbIncludesystemModulesClick(Sender: TObject);
begin
  fillmodulelist(cbIncludesystemModules.checked);
end;

initialization
  {$i DissectCodeunit.lrs}

end.
