unit DissectCodeunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls,dissectCodeThread,tlhelp32,cefuncproc;

type tmoduledata =class
  public
    moduleaddress: dword;
    modulesize: dword;
end;

type
  TfrmDissectCode = class(TForm)
    Button1: TButton;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Panel1: TPanel;
    ListBox1: TListBox;
    Panel2: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label1: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    edtAccuracy: TEdit;
    Label8: TLabel;
    GroupBox1: TGroupBox;
    Label4: TLabel;
    Label5: TLabel;
    edtFrom: TEdit;
    edtTo: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ListBox1Click(Sender: TObject);
    procedure edtFromChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    starttime: int64;
  public
    { Public declarations }
    dissectcode: tdissectcodethread;
    constructor create(aowner: tcomponent; dc: tdissectcodethread);
  end;

var
  frmDissectCode: TfrmDissectCode;

implementation

{$R *.dfm}

constructor TfrmDissectCode.create(aowner: tcomponent; dc: tdissectcodethread);
begin
  dissectcode:=dc;
  inherited create(aowner);
end;

procedure TfrmDissectCode.Button1Click(Sender: TObject);
var start,stop:dword;
    tempregions: tmemoryregions;
    i,j: integer;
    temp: tmemoryregion;
    h,m,s,ms: word;
    n: integer;
    flipped: boolean;
begin
  if button1.caption='Stop' then
  begin
    timer1.Enabled:=false;
    dissectcode.terminate;

    button1.Caption:='Start';
    showmessage('dissected till address '+inttohex(dissectcode.currentaddress,8));
    exit;
  end;

  try
    dissectcode.accuracy:=strtoint(edtAccuracy.text);
  except
    raise exception.Create('Please fill in a valid value for accuracy');
  end;

  if edtfrom.Text<>'' then
  begin
    //user input
    try
      start:=strtoint('$'+edtfrom.Text);
      stop:=strtoint('$'+edtto.Text);
    except
      raise exception.Create('Please fill in a valid ange to scan');
    end;

    getexecutablememoryregionsfromregion(start,stop,dissectcode.memoryregion);
  end
  else
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

  button1.Caption:='Stop';
  timer1.Enabled:=true;

  decodetime(now,h,m,s,ms);
  starttime:=ms+s*1000+m*60*1000+h*60*60*1000;

  dissectcode.Resume;
end;

procedure TfrmDissectCode.Timer1Timer(Sender: TObject);
var h,m,s,ms: word;
    currenttime: int64;

    x: double;
begin
  decodetime(now,h,m,s,ms);
  currenttime:=ms+s*1000+m*60*1000+h*60*60*1000;
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

  progressbar1.position:=dissectcode.percentagedone;

  progressbar1.Hint:=inttohex(dissectcode.currentaddress,8);
  if dissectcode.done then close;
end;

procedure TfrmDissectCode.FormClose(Sender: TObject;
  var Action: TCloseAction);
var i: integer;
begin
  action:=cafree;
  for i:=0 to listbox1.Count-1 do
    tmoduledata(listbox1.Items.Objects[i]).Free;

end;

procedure TfrmDissectCode.ListBox1Click(Sender: TObject);
begin
  if listbox1.SelCount>0 then
  begin
    edtfrom.OnChange:=nil;
    edtto.OnChange:=nil;

    edtfrom.text:='';
    edtto.text:='';

    edtfrom.OnChange:=edtFromChange;
    edtto.OnChange:=edtFromChange;
  end;
end;

procedure TfrmDissectCode.edtFromChange(Sender: TObject);
var i: integer;
begin
  for i:=0 to listbox1.Items.Count-1 do
    listbox1.Selected[i]:=false;
end;

procedure TfrmDissectCode.FormShow(Sender: TObject);
var ths: thandle;
    me32: MODULEENTRY32;
    x: pchar;
    moduledata: tmoduledata;
begin
  ths:=CreateToolhelp32Snapshot(TH32CS_SNAPMODULE,processid);
  if ths<>0 then
  begin
    try
      zeromemory(@me32,sizeof(me32));
      me32.dwSize:=sizeof(me32);
      if module32first(ths,me32) then
      repeat
        x:=@me32.szModule[0];

        moduledata:=tmoduledata.Create;
        moduledata.moduleaddress:=dword(me32.modBaseAddr);
        moduledata.modulesize:=me32.modBaseSize;

        listbox1.Items.AddObject(x,moduledata);
      until module32next(ths,me32)=false;

    finally
      closehandle(ths);
    end;
  end;
end;

end.
