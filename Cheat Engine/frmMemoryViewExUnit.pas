unit frmMemoryViewExUnit;

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Menus, memdisplay, newkernelhandler, cefuncproc,
  syncobjs, math, savedscanhandler, foundlisthelper, CustomTypeHandler,
  symbolhandler, inputboxtopunit, commonTypeDefs;


type TMVCompareMethod=(cmOr, cmXor, cmAnd);

type
  TMemoryDataSource=class(TThread)
  private
    cs: TCriticalSection;
    address: ptruint;
    buf: pbytearray;
    bufsize: integer;
    faddresslistonly: boolean;
    fcompareagainstsavedscan: boolean;
    fvartype: TVariableType;
    fvarsize: integer;

    comparemethod: TMVCompareMethod;

    temppagebuf: pbytearray;

    addresslist: TFoundList;
    previousvaluelist: TSavedScanHandler;
    ct: TCustomtype;



  public
    procedure lock;
    procedure unlock;
    procedure setRegion(address: ptruint; buf: pointer; size: integer);
    procedure execute; override;
    procedure fetchmem;
    procedure setaddresslist(state: boolean; listname: string);
    procedure setcompare(state: boolean; compareMethod: TMVCompareMethod; listname: string);
    constructor create(suspended: boolean);
  end;

  { TfrmMemoryViewEx }

  TfrmMemoryViewEx = class(TForm)
    cbAddresslist: TComboBox;
    cbAddresslistOnly: TCheckBox;
    cbColor: TComboBox;
    cbCompare: TCheckBox;
    cbSavedList: TComboBox;
    edtPitch: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    lblAddress: TLabel;
    MenuItem1: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    pmMemview: TPopupMenu;
    rbAnd: TRadioButton;
    rbOr: TRadioButton;
    rbXor: TRadioButton;
    tbPitch: TTrackBar;
    Timer1: TTimer;
    procedure cbAddresslistChange(Sender: TObject);
    procedure cbAddresslistOnlyChange(Sender: TObject);
    procedure cbAddresslistDropDown(Sender: TObject);
    procedure cbCompareChange(Sender: TObject);
    procedure cbSavedListChange(Sender: TObject);
    procedure cbColorChange(Sender: TObject);

    procedure edtPitchChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure Panel1DblClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure tbPitchChange(Sender: TObject);
  private
    { private declarations }
    buf: pbytearray;
    bufsize: integer;
    datasource: TMemoryDataSource;
    history: TStringList;

    function getCompareMethod: TMVCompareMethod;
    function ondata(newAddress: ptruint; PreferedMinimumSize: integer; var newbase: pointer; var newsize: integer): boolean;
  public
    { public declarations }
    md: TMemDisplay;
  end;

var
  frmMemoryViewEx: TfrmMemoryViewEx;

implementation

uses MemoryBrowserFormUnit, MainUnit, ProcessHandlerUnit;

{$R *.lfm}

resourcestring
  rsGotoAddress = 'Goto Address';
  rsFillInTheAddressYouWantToGoTo = 'Fill in the address you want to go to';
  rsAddressZoom = 'Address : %s zoom : %s';


{ TMemoryDataSource }

constructor TMemoryDataSource.create(suspended: boolean);
begin
  cs:=tcriticalsection.create;

  getmem(temppagebuf, 4096);  //so it doesn't need to be allocated/freed each fetchmem call

  inherited create(suspended);
end;

procedure TMemoryDataSource.setcompare(state: boolean; compareMethod: TMVCompareMethod; listname: string);
begin
  if state then
  begin

    cs.Enter;
    try
      if previousvaluelist<>nil then
        freeandnil(previousvaluelist);

      previousvaluelist:=TSavedScanHandler.create(mainform.memscan.GetScanFolder, listname);
      previousvaluelist.AllowNotFound:=true;
      previousvaluelist.AllowRandomAccess:=true;

      self.compareMethod:=comparemethod;

    finally
      cs.Leave;
    end;

  end;

  fcompareagainstsavedscan:=state;
end;

procedure TMemoryDataSource.setaddresslist(state: boolean; listname: string);
begin
  if state then
  begin
    //Open a "PreviousValue object for the current memscan results.

    cs.Enter;

    try
      if addresslist<>nil then
        freeandnil(addresslist);

      addresslist:=TFoundList.create(nil, mainform.memscan, listname);
      addresslist.Initialize;
      fvartype:=mainform.memscan.VarType;
      ct:=mainform.memscan.CustomType;
      fvarsize:=mainform.memscan.Getbinarysize div 8;

    finally
      cs.leave;
    end;
  end;


  faddresslistonly:=state;

  fetchmem; //update now
end;

procedure TMemoryDataSource.fetchmem;
var x: ptrUint;
  a,a2: ptruint;
  s: integer;

  s2: integer;

  p: PByteArray;
  i: qword;
  j: integer;


  toread: integer;
begin
  lock;

  if buf<>nil then  //not yet initialized
  begin

    a:=address;
    i:=qword(-1);
    if faddresslistonly then
      i:=addresslist.FindClosestAddress(address-fvarsize+1); //all following accesses will be sequential


    toread:=bufsize;


    //while a<address+bufsize do

    while toread>0 do
    begin
      s:=minX((address+bufsize)-a, 4096-(a mod 4096)); //the number of bytes left in this page or for this buffer

      x:=0;
      if faddresslistonly then
      begin
        //check if this page has any addresses.
        zeromemory(@buf[a-address], s);

        if int64(i)<>-1 then
        begin
          a2:=addresslist.GetAddress(i);
          //get the first addresses that belong to this page (or has bytes in it)
          while (i<addresslist.count-1) and (a2<a-fvarsize+1) do
          begin
            inc(i);
            a2:=addresslist.GetAddress(i)
          end;

          while (i<addresslist.count-1) and (a2<a+s) do
          begin
            //render to the buffer
            s2:=fvarsize;
            if integer(a2-a)<0 then //cut off if it falls before the region
            begin
              dec(s2, integer(a2-a));
              inc(a2, integer(a2-a));
            end;

            if (a2-a+s2)>s then //cut off if it falls after the region
            begin
              s2:=s-(a2-a);
            end;

            if s2>0 then
            begin
              x:=0;
              ReadProcessMemory(processhandle, pointer(a2), @buf[a2-address], s2, x);

              if fcompareagainstsavedscan and (previousvaluelist<>nil) then
              begin
                //get the saved scan
                p:=previousvaluelist.getpointertoaddress(a2, fvartype, ct);
                if p<>nil then
                begin
                  case comparemethod of
                    cmor: for j:=0 to x-1 do buf[a2-address+j]:=buf[a2-address+j] or p[j];
                    cmxor: for j:=0 to x-1 do buf[a2-address+j]:=buf[a2-address+j] xor p[j];
                    cmand: for j:=0 to x-1 do buf[a2-address+j]:=buf[a2-address+j] and p[j];

                  end;


                end;

              end;

              inc(i);
              a2:=addresslist.GetAddress(i);
            end;
          end;
        end;

      end
      else
      begin
        ReadProcessMemory(processhandle, pointer(a), @buf[a-address], s, x);
        if x<s then //zero the unread bytes
          zeromemory(@buf[a-address], s-x);
      end;

      dec(toread,s);

      a:=a+s; //next page
    end;

  end;
  unlock;
end;

procedure TMemoryDataSource.execute;
begin
  while not terminated do
  begin
    sleep(100);

    fetchmem;
  end;
end;

procedure TMemoryDataSource.lock;
begin
  cs.enter
end;

procedure TMemoryDataSource.unlock;
begin
  cs.leave;
end;

procedure TMemoryDataSource.setRegion(address: ptruint; buf: pointer; size: integer);
begin
  lock;
  self.address:=address;
  self.buf:=buf;
  bufsize:=size;

  fetchmem;
  unlock;


end;

{ TfrmMemoryViewEx }
function TfrmMemoryViewEx.ondata(newAddress: ptruint; PreferedMinimumSize: integer; var newbase: pointer; var newsize: integer): boolean;
var x: dword;
begin

  //todo: Pre-buffer when going up. (allocate 4096 bytes in front, and give a pointer to 4096 bytes after. Only when the newaddress becomes smaller than the base realloc

//  label1.caption:=inttohex(newaddress,8);

  datasource.lock;
  if bufsize<PreferedMinimumSize then
  begin
    try
      ReAllocMem(buf, PreferedMinimumSize+4096);
    except
      beep;
    end;

    if buf=nil then
      bufsize:=0
    else
      bufsize:=PreferedMinimumSize+4096;
  end;

  datasource.setRegion(newaddress, buf, bufsize);
  datasource.unlock;


  newbase:=buf;
  newsize:=bufsize;
  result:=newsize>=PreferedMinimumSize; //allow the move if allocated enough memory
end;

procedure TfrmMemoryViewEx.FormCreate(Sender: TObject);
begin
  //create a datasource thread
  history:=tstringlist.create;

  datasource:=TMemoryDataSource.create(true); //possible to add multiple readers in the future

  md:=TMemDisplay.Create(self);
  md.onData:=ondata;

  getmem(buf,4096);
  bufsize:=4096;

  datasource.setRegion(MemoryBrowser.hexview.Address and ptruint(not $FFF), buf, bufsize);
  md.setPointer(MemoryBrowser.hexview.Address and ptruint(not $FFF), buf, bufsize);
  md.Align:=alClient;
  md.parent:=panel1;
  md.PopupMenu:=pmMemview;

  md.OnDblClick:=Panel1DblClick;



  datasource.Start;
end;

procedure TfrmMemoryViewEx.edtPitchChange(Sender: TObject);
var newpitch: integer;
begin
  try
    newpitch:=strtoint(edtpitch.Caption);
    md.setPixelsPerLine(newpitch);
    edtPitch.Font.Color:=clDefault;
  except
    edtPitch.Font.Color:=clred;
  end;
end;

procedure TfrmMemoryViewEx.cbAddresslistOnlyChange(Sender: TObject);
begin
  cbAddresslist.enabled:=cbAddresslistOnly.checked;
  cbCompare.Enabled:=cbAddresslistOnly.checked;
  cbSavedList.enabled:=cbAddresslistOnly.checked;

  if datasource<>nil then
    datasource.setaddresslist(cbAddresslistOnly.checked, 'TMP');
end;

procedure TfrmMemoryViewEx.cbAddresslistChange(Sender: TObject);
begin
  if cbAddresslist.ItemIndex=0 then
    datasource.setaddresslist(cbAddresslistOnly.checked, 'TMP')
  else
    datasource.setaddresslist(cbAddresslistOnly.checked, cbAddresslist.text);
end;

procedure TfrmMemoryViewEx.cbAddresslistDropDown(Sender: TObject);
begin
  TComboBox(sender).Items.Clear;
  TComboBox(sender).DropDownCount:=mainform.memscan.getsavedresults(TComboBox(sender).Items)+1;
  TComboBox(sender).Items.Insert(0,'Current scanlist');
end;

function TfrmMemoryViewEx.getCompareMethod: TMVCompareMethod;
//returns the compare method currently selected
begin
  result:=cmOr;
  if rbxor.checked then
    result:=cmxOr
  else
  if rbAnd.checked then
    result:=cmAnd;

end;

procedure TfrmMemoryViewEx.cbCompareChange(Sender: TObject);
begin
  cbSavedList.enabled:=cbCompare.checked;
  rbOr.enabled:=cbCompare.checked;
  rbAnd.enabled:=cbCompare.checked;
  rbXor.enabled:=cbCompare.checked;

  datasource.setcompare(cbCompare.checked, getCompareMethod,  'TMP');
end;

procedure TfrmMemoryViewEx.cbSavedListChange(Sender: TObject);
begin
  if cbSavedList.ItemIndex=0 then
    datasource.setcompare(cbCompare.checked, getCompareMethod, 'TMP')
  else
    datasource.setcompare(cbCompare.checked, getCompareMethod, cbSavedList.text);
end;

procedure TfrmMemoryViewEx.cbColorChange(Sender: TObject);
var i: integer;
begin
  case cbcolor.itemindex of
    0: md.setFormat($1900);
    1: md.setFormat($1907);
    2: md.setFormat($1908);
  end;


end;

procedure TfrmMemoryViewEx.FormDestroy(Sender: TObject);
begin
  if datasource<>nil then
  begin
    datasource.Terminate;
    datasource.WaitFor;
    freeandnil(datasource);
  end;
end;

procedure TfrmMemoryViewEx.MenuItem1Click(Sender: TObject);
var newaddress: string;
    canceled: boolean;
begin
  newaddress:=inputboxtop(rsGotoAddress, rsFillInTheAddressYouWantToGoTo, IntTohex(md.getTopLeftAddress, 8), true, canceled, History);

  if not canceled then
  begin
    md.MoveTo(0,0);
    md.setPointer(symhandler.getAddressFromName(newaddress));
  end;
end;

procedure TfrmMemoryViewEx.Panel1DblClick(Sender: TObject);
var c: tpoint;
    address: ptruint;
begin
  c:=md.ScreenToClient(mouse.cursorpos);

  address:=md.getAddressFromScreenPosition(c.x, c.y);

  MemoryBrowser.hexview.Address:=address;
  MemoryBrowser.show;
end;

procedure TfrmMemoryViewEx.Timer1Timer(Sender: TObject);
begin
  lbladdress.caption:=Format(rsAddressZoom, [inttohex(md.getTopLeftAddress, 8), floattostr(md.zoom)]);
end;

procedure TfrmMemoryViewEx.tbPitchChange(Sender: TObject);
begin
  edtPitch.caption:=inttostr(tbPitch.position);//inttostr(trunc(2**tbPitch.position));
end;

end.

