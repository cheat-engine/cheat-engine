unit DissectCodeunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,dissectCodeThread,tlhelp32,cefuncproc,
  symbolhandler, frmReferencedStringsUnit;

type tmoduledata =class
  public
    moduleaddress: dword;
    modulesize: dword;
end;

type TOnDoneDissect=(odDoNothing, odOpenReferedStringList);

type
  TfrmDissectCode = class(TForm)
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Panel1: TPanel;
    ListBox1: TListBox;
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
    procedure btnStartClick(Sender: TObject);
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
    dissectcode: tdissectcodethread;
  end;

var
  frmDissectCode: TfrmDissectCode;

implementation

{$R *.dfm}

procedure TfrmDissectCode.btnStartClick(Sender: TObject);
var start,stop:dword;
    tempregions: tmemoryregions;
    i,j: integer;
    temp: tmemoryregion;
    h,m,s,ms: word;
    n: integer;
    flipped: boolean;
begin
  if btnStart.caption='Stop' then
  begin
    timer1.Enabled:=false;
    dissectcode.terminate;
    dissectcode.WaitFor;
    dissectcode.done:=true;
    Timer1Timer(timer1);

    btnStart.Caption:='Start';
    //showmessage('dissected till address '+inttohex(dissectcode.currentaddress,8));
    exit;
  end;

  if dissectcode<>nil then
  begin
    dissectcode.Terminate;
    dissectcode.WaitFor;
    dissectcode.Free;
  end;

  dissectcode:=TDissectCodeThread.create(true);

 
  begin
    setlength(dissectcode.memoryregion,0);
    if listbox1.SelCount=0 then raise exception.Create('Please select something to scan');
    for i:=0 to listbox1.items.count-1 do
    begin
      if listbox1.Selected[i] then
      begin
        getexecutablememoryregionsfromregion(tmoduledata(listbox1.Items.Objects[i]).moduleaddress,tmoduledata(listbox1.Items.Objects[i]).moduleaddress+tmoduledata(listbox1.Items.Objects[i]).modulesize,tempregions);
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

  end;

  btnStart.Caption:='Stop';
  timer1.Enabled:=true;

  starttime:=gettickcount;

  dissectcode.Resume;
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


  progressbar1.position:=dissectcode.percentagedone;

  progressbar1.Hint:=inttohex(dissectcode.currentaddress,8);
  if dissectcode.done then
  begin
    close;

    if ondone=odOpenReferedStringList then
    begin
      if frmReferencedStrings=nil then
        frmReferencedStrings:=tfrmReferencedStrings.Create(self);

      frmReferencedStrings.Show;
    end;

    ondone:=odDoNothing;
  end;
end;

procedure TfrmDissectCode.FormClose(Sender: TObject;
  var Action: TCloseAction);
var i: integer;
begin
  action:=cafree;
  cleanModuleList;

end;

procedure TfrmDissectCode.cleanModuleList;
var i: integer;
begin
  for i:=0 to listbox1.Count-1 do
    tmoduledata(listbox1.Items.Objects[i]).Free;

  listbox1.items.Clear;
end;

procedure TfrmDissectCode.fillModuleList(withSystemModules: boolean);
var ths: thandle;
    me32: MODULEENTRY32;
    x: pchar;
    moduledata: tmoduledata;
begin
  cleanModuleList;
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,processid);
  if ths<>0 then
  begin
    try
      zeromemory(@me32,sizeof(me32));
      me32.dwSize:=sizeof(me32);
      if module32first(ths,me32) then
      repeat
        x:=@me32.szModule[0];

        if (withSystemModules) or (not symhandler.inSystemModule(dword(me32.modBaseAddr))) then
        begin
          moduledata:=tmoduledata.Create;
          moduledata.moduleaddress:=dword(me32.modBaseAddr);
          moduledata.modulesize:=me32.modBaseSize;
          
          listbox1.Items.AddObject(x,moduledata);
        end;
      until module32next(ths,me32)=false;

    finally
      closehandle(ths);
    end;
  end;
end;

procedure TfrmDissectCode.FormShow(Sender: TObject);
begin
  fillModuleList(false);
  if listbox1.Count>0 then
  begin
    listbox1.ItemIndex:=0;
    listbox1.Selected[0]:=true;
  end;
end;

procedure TfrmDissectCode.cbIncludesystemModulesClick(Sender: TObject);
begin
  fillmodulelist(cbIncludesystemModules.checked);
end;

end.
