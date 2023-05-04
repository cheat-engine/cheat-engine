unit DissectCodeunit;

{$MODE Delphi}

interface

uses
  {$ifdef darwin}
  macport,
  {$endif}
  {$ifdef windows}
  jwawindows, windows,
  {$endif}
  LCLIntf, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, DissectCodeThread, CEFuncProc,
  symbolhandler, LResources, Menus, frmReferencedStringsUnit, newkernelhandler,
  MemFuncs, commonTypeDefs, ProcessHandlerUnit, betterControls;



type TOnDoneDissect=(odDoNothing, odOpenReferedStringList, odOpenReferedFunctionsList);

type

  { TfrmDissectCode }

  TfrmDissectCode = class(TForm)
    edtCustomRangeStart: TEdit;
    edtCustomRangeStop: TEdit;
    Label10: TLabel;
    Label8: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    OpenDialog1: TOpenDialog;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
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
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
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

uses frmReferencedFunctionsUnit, PEInfounit;

resourcestring
  rsStop = 'Stop';
  rsStart = 'Start';
  rsPleaseSelectSomethingToScan = 'Please select something to scan or enter a custom range';
  rsDone = 'done';
  rsDissectDataLoaded = 'Dissect data loaded';


procedure TfrmDissectCode.btnStartClick(Sender: TObject);
var start,stop:PtrUInt;
    tempregions: tmemoryregions;
    i,j: integer;
    temp: tmemoryregion;
    h,m,s,ms: word;
    n: integer;
    flipped: boolean;

    customRangeStart: ptruint;
    customRangeStop: ptruint;
    hasCustomRange: boolean=false;
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

  if (trim(edtCustomRangeStart.text)<>'') and (trim(edtCustomRangeStop.text)<>'') then
  begin
    customRangeStart:=symhandler.getAddressFromName(edtCustomRangeStart.text);
    customRangeStop:=symhandler.getAddressFromName(edtCustomRangeStop.text);

    hasCustomRange:=true;
  end;




  if (lbModuleList.SelCount=0) and (hasCustomRange=false) then raise exception.Create(rsPleaseSelectSomethingToScan);

  if dissectcode=nil then
    dissectcode:=TDissectCodeThread.create(false);

  dissectcode.clear;


  setlength(dissectcode.memoryregion,0);

  if hasCustomRange then
  begin
    getexecutablememoryregionsfromregion(customRangeStart, customRangeStop, tempregions);
    setlength(dissectcode.memoryregion,length(dissectcode.memoryregion)+length(tempregions));

    for i:=0 to length(tempregions)-1 do
    begin
      if tempregions[i].BaseAddress<customrangestart then
      begin
        dec(tempregions[i].MemorySize, customrangestart-tempregions[i].BaseAddress);
        tempregions[i].BaseAddress:=customrangestart;
      end;

      if (tempregions[i].BaseAddress+tempregions[i].MemorySize)>customrangestop then
        tempregions[i].MemorySize:=customrangestop-tempregions[i].BaseAddress;


      dissectcode.memoryregion[length(dissectcode.memoryregion)-length(tempregions)+i]:=tempregions[i];
    end;
  end;

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

  if LoadFormPosition(self) then
    autosize:=false;
end;

procedure TfrmDissectCode.FormDestroy(Sender: TObject);
begin
  SaveFormPosition(self);
end;

procedure TfrmDissectCode.MenuItem2Click(Sender: TObject);
begin
  if opendialog1.execute then
  begin
    if dissectcode=nil then
      dissectcode:=TDissectCodeThread.create(false);

    dissectcode.loadFromFile(opendialog1.filename);

    lblStringRef.caption:=inttostr(dissectcode.nrofstring);
    lblConditionalJumps.caption:=inttostr(dissectcode.nrofconditionaljumps);
    lblUnConditionalJumps.caption:=inttostr(dissectcode.nrofunconditionaljumps);
    lblCalls.caption:=inttostr(dissectcode.nrofcalls);
    lblMaxOffset.caption:=inttostr(dissectcode.maxoffset);

    showmessage(rsDissectDataLoaded);
  end;
end;

procedure TfrmDissectCode.MenuItem3Click(Sender: TObject);
begin
  if savedialog1.execute then
  begin
    if dissectcode=nil then
      dissectcode:=TDissectCodeThread.create(false);

    dissectcode.saveTofile(savedialog1.filename);
  end;
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
  autosize:=false;
  cleanModuleList;

end;

procedure TfrmDissectCode.cleanModuleList;
begin
  cefuncproc.cleanModuleList(lbModulelist.items);
end;

procedure TfrmDissectCode.fillModuleList(withSystemModules: boolean);
var i: integer;
    md: tmoduledata;

    buf: array [0..4096] of byte;
    x: ptruint;

    base: ptruint;
    codebase: ptruint;
    codesize: integer;
begin
  cefuncproc.GetModuleList(lbModuleList.Items, withSystemModules);

  //adjust the moduleinfo to code section only
  for i:=0 to lbModuleList.Count-1 do
  begin
    md:=tmoduledata(lbModuleList.Items.Objects[i]);
    base:=md.moduleaddress;

    if ReadProcessMemory(processhandle, pointer(base), @buf[0],4096,x) then
    begin
      codebase:=peinfo_getcodebase(@buf[0],4096);
      codesize:=peinfo_getcodesize(@buf[0],4096);

      if (codebase>0) and (codesize>0) then
      begin
        md.moduleaddress:=base+codebase;
        md.modulesize:=codesize;
      end;
    end;
  end;

end;

procedure TfrmDissectCode.FormShow(Sender: TObject);
begin
  fillModuleList(cbIncludesystemModules.checked);

  if (lbModuleList.Count>0) and (trim(edtCustomRangeStart.text)='') and (trim(edtCustomRangeStop.text)='') then //select the first one
  begin
    lbModuleList.ItemIndex:=0;
    lbModuleList.Selected[0]:=true;
  end;

  if autosize then
  begin
    autosize:=false;
    if panel1.clientwidth<label3.Width then
      width:=width+(label3.Width-panel1.clientwidth);

    if panel3.height<(edtCustomRangeStop.Top+edtCustomRangeStop.height+3) then
      height:=height+(edtCustomRangeStop.Top+edtCustomRangeStop.height+3)-panel3.height;
  end;

end;

procedure TfrmDissectCode.cbIncludesystemModulesClick(Sender: TObject);
begin
  fillmodulelist(cbIncludesystemModules.checked);
end;

initialization
  {$i DissectCodeunit.lrs}

end.
