unit MemoryTrainerUnit;

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,settingsunit,tlhelp32,shellapi,math,extratrainercomponents,
  userdefinedformunit, XPMan,newkernelhandler, symbolhandler,frmautoinjectunit,
  cefuncproc,autoassembler,hotkeyhandler;



type TBytes= array of integer;

type TcodeEntry = record
  address: dword;
  modulename: string;
  moduleoffset: dword;
  originalopcode: array of byte;
end;

type TCEPointer=record
  Address: Dword;  //only used when last pointer in list
  interpretableaddress: string;
  offset: integer;
end;

type TAddressEntry = record
  address: dword;
  interpretableaddress: string;
  ispointer: boolean;
  pointers: array of TCEPointer;
  bit: byte;
  memtyp: integer;
  frozen: boolean;
  frozendirection: byte;
  setvalue: boolean;
  userinput: boolean;
  value: string;

  valuei: int64;
  valuef: double;
  valuea: array of byte;
  valuelength: integer;

  autoassemblescript: string;
  allocs: TCEAllocArray;
end;

type Ttrainerdata = record
  description: string;
  hotkeytext: string;
  hotkey: TKeyCombo;
  active: boolean;
  codeentrys: array of TCodeEntry;
  addressentrys: array of TAddressEntry;
end;


type
  TfrmMemoryTrainer = class(TForm)
    Panel2: TPanel;
    Button1: TButton;
    ScrollBox1: TScrollBox;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Image1: TImage;
    btnLaunch: TButton;
    Timer1: TTimer;
    OpenDialog1: TOpenDialog;
    Freezer: TTimer;
    Button2: TButton;
    XPManifest1: TXPManifest;
    Button3: TButton;
    Timer2: TTimer;
    Timer3: TTimer;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnLaunchClick(Sender: TObject);
    procedure FreezerTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure CheatClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }

    reinitializedesired: boolean;

    procedure redefinecodeentries;
    procedure reinterpretaddresses;
    
    procedure hotkeyhandler(var Message: TWMHotKey); message WM_HOTKEY2;
    procedure checkforprocess;
    procedure executecheat(sender: tobject);
  public
    { Public declarations }
    filename,process: string;
    autolaunch:boolean;

    aboutboxtext: string;
    viewdefault: boolean;
    trainerdata: array of TTrainerdata;
    clist: tcheatlist;
  end;



type TSetColorThread = class(TThread)
  private
    recordnr: integer;
    procedure setcolor;
  public

    Constructor MyCreate(recnr: integer);
    procedure Execute; override;
  end;



var
  frmMemoryTrainer: TfrmMemoryTrainer;

implementation

{$R *.DFM}

function StrToFloat(const S: string): Extended;
begin
  //gets rid of international confusion

  DecimalSeparator:='.';
  try
    result:=sysutils.StrToFloat(s);
  except
    DecimalSeparator:=',';
    result:=sysutils.StrToFloat(s);
  end;
end;

function ReadProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer;  nSize: DWORD; var lpNumberOfBytesRead: DWORD): BOOL; stdcall;
var c: _context;
    y: ^byte;
begin
  if protect then
  begin
    c.ContextFlags:=CONTEXT_FULL or CONTEXT_FLOATING_POINT or CONTEXT_DEBUG_REGISTERS;
    getthreadcontext(getcurrentthread,c);
    if (c.dr6<>0) or (c.dr7<>0) then exit;

    y:=@newkernelhandler.readprocessMemory;
    if y^=$cc then exit;

    if dword(@newkernelhandler.ReadProcessMemory)>dword(WindowsKernel) then
      newkernelhandler.ReadProcessMemory(hProcess,lpBaseAddress,lpBuffer,nSize, lpNumberOfBytesRead);
  end
  else
    newkernelhandler.ReadProcessMemory(hProcess,lpBaseAddress,lpBuffer,nSize, lpNumberOfBytesRead);
end;

function WriteProcessMemory(hProcess: THandle; const lpBaseAddress: Pointer; lpBuffer: Pointer; nSize: DWORD; var lpNumberOfBytesWritten: DWORD): BOOL; stdcall;
var c: _context;
    y: ^byte;
begin
  if protect then
  begin
    c.ContextFlags:=CONTEXT_FULL or CONTEXT_FLOATING_POINT or CONTEXT_DEBUG_REGISTERS;
    getthreadcontext(getcurrentthread,c);
    if (c.dr6<>0) or (c.dr7<>0) then exit;

    y:=@newkernelhandler.writeprocessMemory;
    if y^=$cc then exit;


    if dword(@newkernelhandler.writeprocessMemory)>dword(WindowsKernel) then
      newkernelhandler.writeprocessMemory(hProcess,lpBaseAddress,lpBuffer,nSize, lpNumberOfBytesWritten);
  end
  else
    newkernelhandler.writeprocessMemory(hProcess,lpBaseAddress,lpBuffer,nSize, lpNumberOfBytesWritten);
end;

procedure setbit(bitnr: integer; var bt: Byte;state:integer);
{
 pre: bitnr=bit between 0 and 7
         bt=pointer to the byte
 post: bt has the bit set specified in state
 result: bt has a bit set or unset
}
begin
  if state=1 then
    bt:=bt or trunc(power(2,bitnr))  //set that bit to 1
  else
    bt:=bt and ($ff xor trunc(power(2,bitnr))); //set the bit to 0
end;

function getbit(bitnr: integer; bt: Byte):integer;
begin
  if (trunc(power(2,bitnr)) and bt)>0 then result:=1 else result:=0;
end;



constructor TSetColorThread.MyCreate(recnr: integer);
begin
  recordnr:=recnr;
  inherited create(false);
end;

procedure TSetColorThread.setcolor;
var i: integer;
begin
  if userdefinedform<>nil then
  begin
    for i:=0 to length(userdefinedform.cheat)-1 do
      if userdefinedform.cheat[i].cheatnr=recordnr then
        userdefinedform.cheat[i].activated:=frmMemoryTrainer.trainerdata[recordnr].active;

    for i:=0 to length(userdefinedform.cheatlist)-1 do
      userdefinedform.cheatlist[i].Items[recordnr].activated:=frmMemoryTrainer.trainerdata[recordnr].active;

  end;
  frmMemoryTrainer.clist.Items[recordnr].activated:=frmMemoryTrainer.trainerdata[recordnr].active;
end;

procedure TSetColorThread.Execute;
begin
  FreeOnTerminate:=true;
  sleep(500);
  synchronize(setcolor);
end;

//copy/paste from the ce source
procedure ConvertStringToBytes(scanvalue:string; hex:boolean;var bytes: TBytes);
var i,j,k: integer;
    helpstr:string;
begin
  while scanvalue[length(scanvalue)]=' ' do
    scanvalue:=copy(scanvalue,1,length(scanvalue)-1);

  if (pos('-',scanvalue)>0) or (pos(' ',scanvalue)>0) then
  begin
    //syntax is xx-xx-xx or xx xx xx
    j:=1;
    k:=0;
    scanvalue:=scanvalue+' ';

    for i:=1 to length(scanvalue) do
    begin
      if (scanvalue[i]=' ') or (scanvalue[i]='-') then
      begin
        helpstr:=copy(scanvalue,j,i-j);
        j:=i+1;
        setlength(bytes,k+1);
        try
          if hex then bytes[k]:=strtoint64('$'+helpstr)
                 else bytes[k]:=strtoint64(helpstr);
        except
          bytes[k]:=-1;
          //if it is not a '-' or ' ' or a valid value then I assume it is a wildcard.(
        end;
        inc(k);
      end;
    end;
  end else
  begin
    //syntax is xxxxxx
    k:=0;
    j:=1;
    for i:=1 to length(scanvalue) do
    begin
      if (i mod 2)=0 then
      begin
        helpstr:=copy(scanvalue,j,i-j+1);
        j:=i+1;
        setlength(bytes,k+1);
        try
          bytes[k]:=strtoint64('$'+helpstr);
        except
          bytes[k]:=-1;
        end;
        inc(k);
      end;
    end;
  end;
end;

procedure TFrmMemoryTrainer.reinterpretaddresses;
var i,j: integer;
begin
  reinitializedesired:=false;
  for i:=0 to length(trainerdata)-1 do
  begin
    for j:=0 to length(trainerdata[i].addressentrys)-1 do
    begin
      if trainerdata[i].addressentrys[j].interpretableaddress<>'' then
      begin
        try
          trainerdata[i].addressentrys[j].address:=symhandler.getAddressFromName(trainerdata[i].addressentrys[j].interpretableaddress);
        except
          reinitializedesired:=true;
        end;
      end;
    end;
  end;
  {

  //update reinterpetable addresses
  for i:=0 to numberofrecords-1 do
  begin
    if memrec[i].interpretableaddress<>'' then
    begin
      try
        memrec[i].address:=symhandler.getAddressFromName(memrec[i].interpretableaddress,false); //don't wait for symbols here
      except

      end;
    end;

    if memrec[i].IsPointer and (memrec[i].pointers[length(memrec[i].pointers)-1].interpretableaddress<>'') then
      memrec[i].pointers[length(memrec[i].pointers)-1].Address:=symhandler.getAddressFromName(memrec[i].pointers[length(memrec[i].pointers)-1].interpretableaddress,false);
  end; }
end;

procedure TFrmMemoryTrainer.redefinecodeentries;
var i,j: integer;
begin
  symhandler.loadmodulelist;

  for i:=0 to length(trainerdata)-1 do
  begin
    for j:=0 to length(trainerdata[i].codeentrys)-1 do
    begin
      if trainerdata[i].codeentrys[j].modulename<>'' then //make sure a modulename was filled in otherwise keep the old address
        trainerdata[i].codeentrys[j].address:=symhandler.getaddressfromname(trainerdata[i].codeentrys[j].modulename)+trainerdata[i].codeentrys[j].moduleoffset
    end;
  end;
end;

procedure TFrmMemoryTrainer.checkforprocess;
Var SNAPHandle: THandle;
    ProcessEntry: ProcessEntry32;
    Check: Boolean;
    FullProcessName,ProcessName: String;
    I: Integer;
begin
  SNAPHandle:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  If SnapHandle>0 then
  begin
    ProcessEntry.dwSize:=SizeOf(ProcessEntry);
    Check:=Process32First(SnapHandle,ProcessEntry);
    while check=true do
    begin
      ProcessName:='';
      FullProcessName:='';
      FullProcessName:=processentry.szExeFile;
      i:=Length(FullProcessName);
      while (i>0) and (FullProcessname[i-1]<>'\') do dec(i);
      processname:=copy(FullProcessName,i,length(FullProcessname)-i+1);

      if uppercase(processname)=uppercase(process) then
      begin
        ProcessID:=ProcessEntry.th32ProcessID;
        btnLaunch.Enabled:=false;

        if processhandle=0 then
          processhandle:=openprocess(process_all_access,false,processid);

        symhandler.showmodules:=true;
        symhandler.showsymbols:=true;
        symhandler.reinitialize;

        reinterpretaddresses;        
        exit;
      end;

      check:=Process32Next(SnapHandle,ProcessEntry);
    end;
  end;

  if processhandle<>0 then closehandle(processhandle);
  processhandle:=0;
  btnLaunch.Enabled:=true;
end;

procedure TFrmMemoryTrainer.hotkeyhandler(var Message: TWMHotKey);
var i,j,k,l,m,err: integer;
    v: int64;
    v2: double;
    bytes: dword;
    nops: array of byte;
    original: dword;

    newvalue1: byte;
    newvalue2: word;
    newvalue3: dword;
    newvalue4: single;
    newvalue5: double;
    newvalue6: int64;

    dontputback: boolean;

    bl: integer;
    newbytes: array of byte;
    newinput: string;
    realaddress,realaddress2,count:dword;

    sl: tstringlist;

    aa: tCEAllocArray;
begin
  checkforprocess;
  //handle the hotkeys
  i:=message.HotKey;

  if i=$bfff then  //pop-up
  begin
    application.Restore;
    if userdefinedform<>nil then
    begin
      //show userdefined window
      userdefinedform.Show;
      userdefinedform.Position:=poScreenCenter;
    end
    else
    begin
      //show default window
      frmmemorytrainer.Show;
      frmmemorytrainer.Position:=poScreenCenter;
    end;
    application.BringToFront;
    
  end;


  if (i>=0) and (i<length(trainerdata)) then
  begin
    if processhandle=0 then exit;

    if trainerdata[i].active then //is currenly active
    begin
      //deactivate  (if needed)
      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        //find this codeentry in other trainerdatas
        dontputback:=false;

        for k:=0 to length(trainerdata)-1 do
        begin
          if (k<>i) and (trainerdata[k].active) then
          begin
            for l:=0 to length(trainerdata[k].codeentrys)-1 do
            begin
              //it has also code entrys
              //check if it has a code entry that matches mine
              if trainerdata[i].codeentrys[j].address=trainerdata[k].codeentrys[l].address then
              begin
                //CRAAAAAAAAAAAAAAAP! Cant people just use their brains ?
                dontputback:=true;
              end;
            end;
          end;
        end;

        if not dontputback then
        begin
          original:=0;
          VirtualProtectEx(processhandle,  pointer(trainerdata[i].codeentrys[j].address),length(nops),PAGE_EXECUTE_READWRITE,original);
          writeprocessmemory(processhandle,pointer(trainerdata[i].codeentrys[j].address),@trainerdata[i].codeentrys[j].originalopcode[0],length(trainerdata[i].codeentrys[j].originalopcode),bytes);

          //set old security back
          VirtualProtectEx(processhandle,pointer(trainerdata[i].codeentrys[j].address),length(nops),original,original);
        end;
      end;


      {$ifdef standalonetrainerwithassembler}
      //see if it contains a auto assemble entry
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        if trainerdata[i].addressentrys[j].memtyp=255 then
        begin
          //auto assemble script
          //dissable it
          sl:=tstringlist.Create;
          sl.Text:=trainerdata[i].addressentrys[j].autoassemblescript;

          try
            try
              if autoassemble(sl,false,false,false,false,trainerdata[i].addressentrys[j].allocs)=false then
              begin
                symhandler.reinitialize;   //second chance, else death sentence
                autoassemble(sl,false,false,false,false,trainerdata[i].addressentrys[j].allocs);
              end;

              reinterpretaddresses;
            finally
              sl.free;
            end;
          except
            //failed
          end;

        end;
      end;
      {$endif}

      trainerdata[i].active:=false;  //also disables the freezer
    end
    else //not currently active
    begin
      //activate (if needed)
      redefinecodeentries;

      for j:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        setlength(nops,length(trainerdata[i].codeentrys[j].originalopcode));
        readprocessmemory(processhandle,pointer(trainerdata[i].codeentrys[j].address),@nops[0],length(trainerdata[i].codeentrys[j].originalopcode),bytes);
        if not comparemem(@trainerdata[i].codeentrys[j].originalopcode[0],@nops[0],length(trainerdata[i].codeentrys[j].originalopcode)) then
        begin
          //it not as expected
          //check if it got nopped
          for k:=0 to length(nops)-1 do
          begin
            if nops[k]<>$90 then
            begin
              messagedlg('Incorrect version, or the process got messed up!',mterror,[mbok],0);
              application.Terminate;
              exit;
            end;
          end;
        end;

        for k:=0 to length(nops)-1 do
          nops[k]:=$90;

        original:=0;
        VirtualProtectEx(processhandle,  pointer(trainerdata[i].codeentrys[j].address),length(nops),PAGE_EXECUTE_READWRITE,original);
        writeprocessmemory(processhandle,pointer(trainerdata[i].codeentrys[j].address),@nops[0],length(nops),bytes);

        //set old security back
        VirtualProtectEx(processhandle,pointer(trainerdata[i].codeentrys[j].address),length(nops),original,original);
        trainerdata[i].active:=true;
      end;


      //enable auto assemble scripts
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        if trainerdata[i].addressentrys[j].memtyp=255 then
        begin
          //auto assemble script
          //dissable it
          sl:=tstringlist.Create;
          sl.Text:=trainerdata[i].addressentrys[j].autoassemblescript;

          try
            try
              setlength(trainerdata[i].addressentrys[j].allocs,1);
              if autoassemble(sl,false,true,false,false,trainerdata[i].addressentrys[j].allocs) = false then
              begin
                //failure to inject script, try again but not reload the symbols
                symhandler.reinitialize;
                autoassemble(sl,false,true,false,false,trainerdata[i].addressentrys[j].allocs);
              end;

              reinterpretaddresses;
            finally
              sl.free;
            end;
          except
            //failed
          end;

        end;
      end;


      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        //set value
        if trainerdata[i].addressentrys[j].ispointer then
        begin
          realaddress2:=trainerdata[i].addressentrys[j].pointers[length(trainerdata[i].addressentrys[j].pointers)-1].Address;
          for k:=length(trainerdata[i].addressentrys[j].pointers)-1 downto 0 do
          begin
            readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
            if count=4 then
              realaddress2:=realaddress+trainerdata[i].addressentrys[j].pointers[k].offset
            else
            begin
              realaddress2:=0;
              break;
            end;
          end;
          if realaddress2=0 then continue;
          realaddress:=realaddress2;
        end else realaddress:=trainerdata[i].addressentrys[j].address;

        if trainerdata[i].addressentrys[j].setvalue then
        begin
          if trainerdata[i].addressentrys[j].userinput then
          begin
            //find a new value

            newinput:=clist.Items[i].Editvalue;  //default
            if userdefinedform<>nil then
              if length(userdefinedform.cheatlist)>0 then
                newinput:=userdefinedform.cheatlist[0].Items[i].Editvalue
              else
              begin
                //no cheatlist!!!!
                //try to find a tcheatlist
                for k:=0 to length(userdefinedform.cheat)-1 do
                  if userdefinedform.cheat[k].cheatnr=i then
                  begin
                    newinput:=userdefinedform.cheat[k].Editvalue;
                    break;
                  end;
              end;

            val(newinput,v,err);
            if err=0 then trainerdata[i].addressentrys[j].valuei:=v;

            val(newinput,v2,err);
            if err=0 then trainerdata[i].addressentrys[j].valuef:=v2;

          end;

          VirtualProtectEx(processhandle,  pointer(realaddress),1,PAGE_EXECUTE_READWRITE,original);

          case trainerdata[i].addressentrys[j].memtyp of
           0  : begin   //byte
                  newvalue1:=byte(trainerdata[i].addressentrys[j].valuei);
                  WriteProcessMemory(processhandle,pointer(realaddress),@newvalue1,1,bytes);
                end;

           1  : begin //word
                  newvalue2:=word(trainerdata[i].addressentrys[j].valuei);
                  WriteProcessMemory(processhandle,pointer(realaddress),@newvalue2,2,bytes);
                end;

           2  : begin  //dword
                  newvalue3:=dword(trainerdata[i].addressentrys[j].valuei);
                  WriteProcessMemory(processhandle,pointer(realaddress),@newvalue3,4,bytes);
                end;

           3  : begin //single
                  newvalue4:=trainerdata[i].addressentrys[j].valuef;
                  WriteProcessMemory(processhandle,pointer(realaddress),@newvalue4,4,bytes);
                end;

           4  : begin //single
                  newvalue5:=trainerdata[i].addressentrys[j].valuef;
                  WriteProcessMemory(processhandle,pointer(realaddress),@newvalue5,8,bytes);
                end;

           5  : begin  //binary
                  bl:=1+((length(trainerdata[i].addressentrys[j].value)-1) div 8);
                  setlength(newbytes,bl);
                  ReadProcessMemory(processhandle,pointer(realaddress),@newbytes[0],bl,bytes);

                  l:=trainerdata[i].addressentrys[j].bit;
                  k:=0;
                  for m:=length(trainerdata[i].addressentrys[j].value) downto 1 do
                  begin
                    case trainerdata[i].addressentrys[j].value[i] of
                      '0' : setbit(l,newbytes[k],0);
                      '1' : setbit(l,newbytes[k],1);
                      '*','?','x': ;
                    end;

                    inc(l);
                    if l>=8 then
                    begin
                      inc(k);
                      l:=0;
                    end;
                  end;

                  writeprocessmemory(processhandle,pointer(realaddress),@newbytes[0],bl,bytes);
                  setlength(newbytes,0);
                end;

           6  : begin  //int64
                  newvalue6:=dword(trainerdata[i].addressentrys[j].valuei);
                  WriteProcessMemory(processhandle,pointer(realaddress),@newvalue6,8,bytes);
                end;

          7,8:  begin  //array of bytes , or array of char
                  writeprocessmemory(processhandle,pointer(realaddress),@trainerdata[i].addressentrys[j].valuea[0],trainerdata[i].addressentrys[j].valuelength,bytes);
                end;
          end;

          VirtualProtectEx(processhandle,  pointer(realaddress),1,original,original);

        end;

        //freeze
        if trainerdata[i].addressentrys[j].frozen then
        begin
        //get the current value of the address and set that as the freeze address
          if not trainerdata[i].addressentrys[j].setvalue then
          case trainerdata[i].addressentrys[j].memtyp of
           0  : begin   //byte
                  readprocessmemory(processhandle,pointer(realaddress),@newvalue1,1,bytes);
                  trainerdata[i].addressentrys[j].valuei:=newvalue1;
                end;

           1  : begin //word
                  readprocessmemory(processhandle,pointer(realaddress),@newvalue2,2,bytes);
                  trainerdata[i].addressentrys[j].valuei:=newvalue2;
                end;

           2  : begin  //dword
                  readprocessmemory(processhandle,pointer(realaddress),@newvalue3,4,bytes);
                  trainerdata[i].addressentrys[j].valuei:=newvalue3;
                end;

           3  : begin //single
                  ReadProcessMemory(processhandle,pointer(realaddress),@newvalue4,4,bytes);
                  trainerdata[i].addressentrys[j].valuef:=newvalue4;
                end;

           4  : begin //single
                  ReadProcessMemory(processhandle,pointer(realaddress),@newvalue5,8,bytes);
                  trainerdata[i].addressentrys[j].valuef:=newvalue5;
                end;

           5  : begin  //binary
                  bl:=1+((length(trainerdata[i].addressentrys[j].value)-1) div 8);
                  setlength(newbytes,bl);
                  ReadProcessMemory(processhandle,pointer(realaddress),@newbytes[0],bl,bytes);

                  l:=trainerdata[i].addressentrys[j].bit;
                  k:=0;
                  for m:=length(trainerdata[i].addressentrys[j].value) downto 1 do
                  begin
                    trainerdata[i].addressentrys[j].value[m]:=IntToStr(getbit(l,newbytes[k]))[1];
                    inc(l);
                    if l>=8 then
                    begin
                      inc(k);
                      l:=0;
                    end;
                  end;
                  setlength(newbytes,0);
                end;

           6  : begin  //int64
                  ReadProcessMemory(processhandle,pointer(realaddress),@newvalue6,8,bytes);
                  newvalue6:=dword(trainerdata[i].addressentrys[j].valuei);
                end;

          7,8:  begin  //array of bytes , or array of char
                  readprocessmemory(processhandle,pointer(realaddress),@trainerdata[i].addressentrys[j].valuea[0],trainerdata[i].addressentrys[j].valuelength,bytes);
                end;
          end;


          trainerdata[i].active:=true;
        end;



      end;

    end;


    //indicate that this hotkey got pressed
    beep;
    if userdefinedform<>nil then
    begin
      for j:=0 to length(userdefinedform.cheat)-1 do
        if userdefinedform.cheat[j].cheatnr=i then
          userdefinedform.cheat[j].activated:=true;

      for j:=0 to length(userdefinedform.cheatlist)-1 do
        userdefinedform.cheatlist[j].Items[i].activated:=true;
    end;
    frmMemoryTrainer.clist.Items[i].activated:=true;
    //and activate the thread that sleeps 500 ms and keeps the collor to activated when active , and black when not

    TSetColorThread.mycreate(i);
  end;

end;

procedure TfrmMemoryTrainer.FormClose(Sender: TObject;
  var Action: TCloseAction);
var i: integer;
begin
  try
    {
    for i:=0 to length(trainerdata)-1 do
      unregisterhotkey(self.Handle,i);

    unregisterhotkey(self.Handle,$bfff);}
  finally
    application.Terminate;
  end;
end;

procedure TfrmMemoryTrainer.FormCreate(Sender: TObject);
var temp: dword;
    x: pchar;
    i,j,k,l,err: integer;

    bp1,bp2: ^byte;
    imagestream: Tmemorystream;
    buf: pchar;
    temp2: integer;

    temptb: TBytes;

    tempab: array of byte;
    tempb: boolean;
    tempc: tcolor;
    tempi: integer;
    tempbc: tbevelcut;
    tempbk: tbevelkind;
    tempcursor: tcursor;
    temps: string;

    popup: boolean;

    keymod:word;

    pid: dword;
    ownprocesshandle: THandle;
    tokenhandle: thandle;
    tp:TTokenPrivileges;
    prev: TTokenPrivileges;

    hotkey: TKeyCombo;

    ReturnLength: Dword;
begin
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

  viewdefault:=true;

  clist:=tcheatlist.create(nil);
  with clist do
  begin
    Left:=0;
    Top:=15;
    Width:=scrollbox1.Width-3;
    Height:=50;
    AutoSize:=true;
    Anchors:=[akleft,aktop,akright];
    parent:=scrollbox1;
  end;

  trainerfile.ReadBuffer(temp,4);  //version (can be discarded)

  //trainerdatasize
  trainerfile.ReadBuffer(temp,4);
  setlength(trainerdata,temp);

  for i:=0 to length(trainerdata)-1 do
  begin
    trainerdata[i].active:=false;

    //description
    trainerfile.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainerfile.ReadBuffer(pointer(x)^,temp);
    x[temp]:=#0;
    trainerdata[i].description:=x;
    freemem(x);

    //hotkeytext
    trainerfile.ReadBuffer(temp,4);
    getmem(x,temp+1);
    trainerfile.ReadBuffer(pointer(x)^,temp);
    x[temp]:=#0;
    trainerdata[i].hotkeytext:=x;
    freemem(x);

    trainerfile.ReadBuffer(trainerdata[i].hotkey,sizeof(trainerdata[i].hotkey));

    //nr of code entries
    trainerfile.ReadBuffer(temp,4);
    setlength(trainerdata[i].codeentrys,temp);

    for j:=0 to length(trainerdata[i].codeentrys)-1 do
    begin
      trainerfile.ReadBuffer(temp,4);

      if protect then temp:=temp xor $11221122;
      trainerdata[i].codeentrys[j].address:=temp;


      //modulename
      trainerfile.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainerfile.ReadBuffer(pointer(x)^,temp);
      x[temp]:=#0;
      trainerdata[i].codeentrys[j].modulename:=x;
      freemem(x);

      //moduleoffset
      trainerfile.ReadBuffer(temp,4);
      if protect then temp:=temp xor $22112211;

      trainerdata[i].codeentrys[j].moduleoffset:=temp;





      trainerfile.ReadBuffer(temp,4);
      setlength(trainerdata[i].codeentrys[j].originalopcode,temp);

      trainerfile.ReadBuffer(pointer(trainerdata[i].codeentrys[j].originalopcode)^,temp);
    end;

    //address entrys
    trainerfile.ReadBuffer(temp,4);
    setlength(trainerdata[i].addressentrys,temp);

    for j:=0 to length(trainerdata[i].addressentrys)-1 do
    begin
      trainerfile.readBuffer(temp,sizeof(trainerdata[i].addressentrys[j].address));
      if protect then temp:=temp xor $dead1337;

      trainerdata[i].addressentrys[j].address:=temp;

      //interpretableaddress
      trainerfile.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainerfile.ReadBuffer(pointer(x)^,temp);
      x[temp]:=#0;

      if protect then //decrypt temps
        for k:=0 to temp-1 do
          x[k]:=chr(ord(x[k]) xor (k+2));

      trainerdata[i].addressentrys[j].interpretableaddress:=x;          


      trainerfile.readBuffer(trainerdata[i].addressentrys[j].ispointer,sizeof(trainerdata[i].addressentrys[j].ispointer));

      trainerfile.ReadBuffer(tempi,4);
      setlength(trainerdata[i].addressentrys[j].pointers,tempi);
      for k:=0 to tempi-1 do
      begin
        trainerfile.readBuffer(trainerdata[i].addressentrys[j].pointers[k].address,sizeof(trainerdata[i].addressentrys[j].pointers[k].address));

        //interpretableaddress for pointer
        trainerfile.ReadBuffer(temp,4);
        getmem(x,temp+1);
        trainerfile.ReadBuffer(pointer(x)^,temp);
        x[temp]:=#0;

        if protect then //decrypt temps
          for l:=0 to temp-1 do
            x[l]:=chr(ord(x[l]) xor (l+3));
        trainerdata[i].addressentrys[j].pointers[k].interpretableaddress:=x;

        trainerfile.readBuffer(trainerdata[i].addressentrys[j].pointers[k].offset,sizeof(trainerdata[i].addressentrys[j].pointers[k].offset));
      end;

      trainerfile.ReadBuffer(trainerdata[i].addressentrys[j].bit,sizeof(trainerdata[i].addressentrys[j].bit));
      trainerfile.readBuffer(trainerdata[i].addressentrys[j].memtyp,sizeof(trainerdata[i].addressentrys[j].memtyp));
      trainerfile.readbuffer(trainerdata[i].addressentrys[j].frozen,sizeof(trainerdata[i].addressentrys[j].frozen));
      trainerfile.readbuffer(trainerdata[i].addressentrys[j].frozendirection,sizeof(trainerdata[i].addressentrys[j].frozendirection));
      trainerfile.readbuffer(trainerdata[i].addressentrys[j].setvalue,sizeof(trainerdata[i].addressentrys[j].setvalue));
      trainerfile.readbuffer(trainerdata[i].addressentrys[j].userinput,sizeof(trainerdata[i].addressentrys[j].userinput));

      trainerfile.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainerfile.ReadBuffer(pointer(x)^,temp);
      x[temp]:=#0;
      trainerdata[i].addressentrys[j].value:=x;
      freemem(x);

      trainerdata[i].addressentrys[j].valuelength:=length(trainerdata[i].addressentrys[j].value);

      //autoassemblerscript
      trainerfile.ReadBuffer(temp,4);
      getmem(x,temp+1);
      trainerfile.ReadBuffer(pointer(x)^,temp);
      x[temp]:=#0;

      if protect then //decrypt temps
        for k:=0 to temp-1 do
          x[k]:=chr(ord(x[k]) xor (k+1));

      trainerdata[i].addressentrys[j].autoassemblescript:=x;
      freemem(x);
    end;
  end;

  //title
  //trainerfile.ReadBuffer(temp,4);

  temp:=0;
  trainerfile.ReadBuffer(temp,4);
  getmem(x,temp+1);
  trainerfile.ReadBuffer(pointer(x)^,temp);
  x[temp]:=#0;

  if protect then
  begin
    for i:=0 to temp-1 do
      x[i]:=chr(ord(x[i]) xor 56);
  end;

  caption:=x;
  application.Title:=caption;
  freemem(x);

  //launch filename
  trainerfile.ReadBuffer(temp,4);
  getmem(x,temp+1);
  trainerfile.ReadBuffer(x^,temp);
  x[temp]:=#0;
  filename:=x;

  //autolaunch
  trainerfile.ReadBuffer(autolaunch,sizeof(autolaunch));

  //popup on keypress
  trainerfile.ReadBuffer(popup,sizeof(popup));

  //processname
  trainerfile.ReadBuffer(temp,4);
  getmem(x,temp+1);
  trainerfile.ReadBuffer(x^,temp);
  x[temp]:=#0;
  process:=x;
  freemem(x);

  //hotkeytext (discarded)
  trainerfile.ReadBuffer(temp,4);
  getmem(x,temp);
  trainerfile.ReadBuffer(x^,temp);
  freemem(x);

  //hotkey+shiftstate
  trainerfile.ReadBuffer(hotkey,sizeof(hotkey));

  if popup then //register this hotkey
  begin
    registerhotkey2(self.Handle,$bfff,hotkey)
  end;


  //aboutboxtext
  trainerfile.ReadBuffer(temp,4);
  getmem(x,temp+1);
  trainerfile.ReadBuffer(x^,temp);
  x[temp]:=#0;

  if protect then
  begin
    for i:=0 to temp-1 do
      x[i]:=chr(ord(x[i]) xor 166);
  end;

  aboutboxtext:=x;
  freemem(x);

  //freeze timer interval
  trainerfile.ReadBuffer(tempi,sizeof(tempi));
  freezer.Interval:=tempi;

  trainerfile.ReadBuffer(temp,4);
  if temp=$666 then
  begin
    //default uid
    //image
    trainerfile.ReadBuffer(temp,4); //size of image
    if temp>0 then
      image1.Picture.Bitmap.LoadFromStream(trainerfile);

    trainerfile.ReadBuffer(temp2,sizeof(width));
    width:=temp2;

    trainerfile.ReadBuffer(temp2,sizeof(height));
    height:=temp2;

    trainerfile.ReadBuffer(temp2,sizeof(panel1.width));
    panel1.width:=temp2;

    trainerfile.ReadBuffer(temp2,sizeof(panel1.height));
    panel1.height:=temp2;


  end
  else
  begin
    //user defined ui
    viewdefault:=false;



    userdefinedform:=tuserdefinedform.create(self);
    userdefinedform.Caption:=caption;

    //windowwidth
    trainerfile.readbuffer(temp,4);
    userdefinedform.Width:=temp;

    //windowheight
    trainerfile.readbuffer(temp,4);
    userdefinedform.height:=temp;
    userdefinedform.Position:=poscreencenter;
    userdefinedform.BorderStyle:=bsSingle;    

    while true do
    begin
      trainerfile.ReadBuffer(temp,4);
      case temp of
      0:   with tbutton2.create(nil) do
           begin
             trainerfile.ReadBuffer(temp,sizeof(integer));
             left:=temp;
             trainerfile.ReadBuffer(temp,sizeof(integer));
             top:=temp;
             trainerfile.ReadBuffer(temp,sizeof(integer));
             width:=temp;
             trainerfile.ReadBuffer(temp,sizeof(integer));
             height:=temp;

             //caption
             trainerfile.ReadBuffer(temp,4);
             getmem(x,temp+1);
             trainerfile.ReadBuffer(x^,temp);
             x[temp]:=#0;
             caption:=x;
             freemem(x);

             //wordwrap
             trainerfile.ReadBuffer(tempb,sizeof(tempb));
             wordwrap:=tempb;


             //onclick
             trainerfile.ReadBuffer(temp,sizeof(tag));
             case temp of
               1: onclick:=button1.OnClick;
               2: onclick:=btnlaunch.onClick;
               3: onclick:=button2.onClick;
               4: onclick:=button3.OnClick;
               else
               begin
                 if temp>=5 then //it's an execute cheat
                   onclick:=executecheat;
               end;
             end;

             trainerfile.ReadBuffer(temp,4);
             getmem(x,temp+1);
             trainerfile.ReadBuffer(x^,temp);
             x[temp]:=#0;
             command:=x;
             freemem(x);

             parent:=userdefinedform;
           end;

      1:   begin //tcheatlist
             setlength(userdefinedform.cheatlist,length(userdefinedform.cheatlist)+1);
             userdefinedform.cheatlist[length(userdefinedform.cheatlist)-1]:=tcheatlist.create(nil);

             with userdefinedform.cheatlist[length(userdefinedform.cheatlist)-1] do
             begin
               trainerfile.ReadBuffer(temp,sizeof(integer));
               left:=temp;
               trainerfile.ReadBuffer(temp,sizeof(integer));
               top:=temp;
               trainerfile.ReadBuffer(temp,sizeof(integer));
               width:=temp;
               trainerfile.ReadBuffer(temp,sizeof(integer));
               height:=temp;

               trainerfile.ReadBuffer(tempc,sizeof(tcolor));
               activationcolor:=tempc;
               trainerfile.ReadBuffer(tempc,sizeof(tcolor));
               color:=tempc;
               trainerfile.ReadBuffer(tempc,sizeof(tcolor));
               textcolor:=tempc;

               trainerfile.ReadBuffer(tempi,sizeof(integer));
               hotkeyleft:=tempi;
               trainerfile.ReadBuffer(tempi,sizeof(integer));
               descriptionleft:=tempi;
               trainerfile.ReadBuffer(tempi,sizeof(integer));
               editleft:=tempi;
               trainerfile.ReadBuffer(tempi,sizeof(integer));
               editwidth:=tempi;

               trainerfile.ReadBuffer(tempbc,sizeof(tbevelcut));
               bevelinner:=tempbc;
               trainerfile.ReadBuffer(tempbc,sizeof(tbevelcut));
               bevelouter:=tempbc;
               trainerfile.ReadBuffer(tempi,sizeof(integer));
               bevelwidth:=tempi;
               trainerfile.ReadBuffer(tempbk,sizeof(tbevelkind));
               bevelkind:=tempbk;

               trainerfile.ReadBuffer(tempb,sizeof(boolean));
               hascheckbox:=tempb;
               trainerfile.ReadBuffer(tempb,sizeof(boolean));
               showhotkeys:=tempb;

               parent:=userdefinedform;
             end;
           end;

      2:   begin //tcheat
             setlength(userdefinedform.cheat,length(userdefinedform.cheat)+1);
             userdefinedform.cheat[length(userdefinedform.cheat)-1]:=tcheat.create(nil);

             with userdefinedform.cheat[length(userdefinedform.cheat)-1] do
             begin
               trainerfile.ReadBuffer(temp,sizeof(integer));
               left:=temp;
               trainerfile.ReadBuffer(temp,sizeof(integer));
               top:=temp;
               trainerfile.ReadBuffer(temp,sizeof(integer));
               width:=temp;
               trainerfile.ReadBuffer(temp,sizeof(integer));
               height:=temp;

               trainerfile.ReadBuffer(cheatnr,sizeof(integer));
               trainerfile.ReadBuffer(tempc,sizeof(tcolor));
               activationcolor:=tempc;
               trainerfile.ReadBuffer(tempc,sizeof(tcolor));
               color:=tempc;
               trainerfile.ReadBuffer(tempc,sizeof(tcolor));
               textcolor:=tempc;

               trainerfile.ReadBuffer(tempi,sizeof(integer));
               hotkeyleft:=tempi;
               trainerfile.ReadBuffer(tempi,sizeof(integer));
               descriptionleft:=tempi;
               trainerfile.ReadBuffer(tempi,sizeof(integer));
               editleft:=tempi;
               trainerfile.ReadBuffer(tempi,sizeof(integer));
               editwidth:=tempi;

               trainerfile.ReadBuffer(tempb,sizeof(boolean));
               hascheckbox:=tempb;
               trainerfile.ReadBuffer(tempb,sizeof(boolean));
               showhotkey:=tempb;

               parent:=userdefinedform;
             end;
           end;

      3:   with timage2.create(nil) do //timage
           begin
             trainerfile.ReadBuffer(temp,sizeof(integer));
             left:=temp;
             trainerfile.ReadBuffer(temp,sizeof(integer));
             top:=temp;
             trainerfile.ReadBuffer(temp,sizeof(integer));
             width:=temp;
             trainerfile.ReadBuffer(temp,sizeof(integer));
             height:=temp;

             trainerfile.ReadBuffer(tempcursor,sizeof(tcursor));
             cursor:=tempcursor;

             trainerfile.ReadBuffer(tempb,sizeof(tempb));
             stretch:=tempb;
             trainerfile.ReadBuffer(tempb,sizeof(tempb));
             transparent:=tempb;

             trainerfile.ReadBuffer(temp,sizeof(integer));
             case temp of
               1: onclick:=button1.OnClick;
               2: onclick:=btnlaunch.onClick;
               3: onclick:=button2.onClick;
               4: onclick:=button3.OnClick;
               else
               begin
                 if temp>=5 then //it's an execute cheat
                   onclick:=executecheat;
               end;
             end;

             trainerfile.ReadBuffer(temp,4);
             if temp>0 then
               picture.Bitmap.LoadFromStream(trainerfile);

             trainerfile.ReadBuffer(temp,4);
             getmem(x,temp+1);
             trainerfile.ReadBuffer(x^,temp);
             x[temp]:=#0;
             command:=x;
             freemem(x);


             parent:=userdefinedform;
           end;

      4:   with tlabel2.Create(nil) do
           begin
             trainerfile.ReadBuffer(temp,sizeof(integer));
             left:=temp;
             trainerfile.ReadBuffer(temp,sizeof(integer));
             top:=temp;
             trainerfile.ReadBuffer(temp,sizeof(integer));
             width:=temp;
             trainerfile.ReadBuffer(temp,sizeof(integer));
             height:=temp;

             //caption
             trainerfile.ReadBuffer(temp,4);
             getmem(x,temp+1);
             trainerfile.ReadBuffer(x^,temp);
             x[temp]:=#0;
             caption:=x;
             freemem(x);

             //wordwrap
             trainerfile.ReadBuffer(tempb,sizeof(tempb));
             wordwrap:=tempb;

             trainerfile.ReadBuffer(tempc,sizeof(tcolor));
             font.Color:=tempc;

             //command
             trainerfile.ReadBuffer(temp,4);
             getmem(x,temp+1);
             trainerfile.ReadBuffer(x^,temp);
             x[temp]:=#0;
             command:=x;
             freemem(x);

             //cursor
             trainerfile.ReadBuffer(tempcursor,sizeof(tcursor));
             cursor:=tempcursor;

             //tag
             trainerfile.ReadBuffer(temp,sizeof(integer));
             case temp of
               1: onclick:=button1.OnClick;
               2: onclick:=btnlaunch.onClick;
               3: onclick:=button2.onClick;
               4: onclick:=button3.OnClick;
               else
               begin
                 if temp>=5 then //it's an execute cheat
                   onclick:=executecheat;
               end;
             end;

             trainerfile.ReadBuffer(tempb,sizeof(tempb));
             if tempb then
               Font.Style:=[fsUnderline]
             else
               Font.Style:=[];

             parent:=userdefinedform;


           end;

        $ffffffff: break;
        else raise exception.Create('The trainer has been corrupted');
      end;
    end;

    userdefinedform.show;
  end;

  //fill in the list
  for i:=0 to length(trainerdata)-1 do
  begin
    tempb:=false;
    for j:=0 to length(trainerdata[i].addressentrys)-1 do
      if trainerdata[i].addressentrys[j].userinput then
      begin
        tempb:=true;
        temps:=trainerdata[i].addressentrys[j].value;
      end;


    if userdefinedform<>nil then
    begin
      for j:=0 to length(userdefinedform.cheatlist)-1 do
      begin
        with userdefinedform.cheatlist[j] do
        begin
          addcheat(trainerdata[i].hotkeytext,trainerdata[i].description,temps,tempb);
        end;
      end;

      for j:=0 to length(userdefinedform.cheat)-1 do
      begin
        with userdefinedform.cheat[j] do
        begin
          Description:=trainerdata[j].description;
          Hotkey:=trainerdata[j].hotkeytext;
          HasEditBox:=tempb;
          Editvalue:=temps;
        end;
      end;
    end;

    clist.addcheat(trainerdata[i].hotkeytext,trainerdata[i].description,temps,tempb);

    //if not registerhotkey(self.Handle,i,trainerdata[i].hotshift,trainerdata[i].hotkey) then
    if not RegisterHotKey2(self.handle,i,trainerdata[i].hotkey) then
    begin
      //couldn't assign hotkey
      //disable that hotkey
      if userdefinedform<>nil then
      for j:=0 to length(userdefinedform.cheatlist)-1 do
      begin
        with userdefinedform.cheatlist[j] do
        begin
          userdefinedform.cheatlist[j].Items[i].Description:='';
          userdefinedform.cheatlist[j].Items[i].Hotkey:='';
          userdefinedform.cheatlist[j].Items[i].HasEditBox:=false;
        end;
      end else
      begin
        clist.Items[i].Description:='';
        clist.Items[i].Hotkey:='';
        clist.Items[i].HasEditBox:=false;
      end;

      if userdefinedform<>nil then
      for j:=0 to length(userdefinedform.cheat)-1 do
      begin
        with userdefinedform.cheat[j] do
        begin
          if cheatnr=i then
          begin
            Description:='';
            Hotkey:='';
            HasEditBox:=false;
          end;
        end;
      end;
    end;

    for j:=0 to length(trainerdata[i].addressentrys)-1 do
    begin
      if trainerdata[i].addressentrys[j].setvalue then
      case trainerdata[i].addressentrys[j].memtyp of
        0,1,2,6 : begin
                      try
                        trainerdata[i].addressentrys[j].valuei:=StrToInt64(trainerdata[i].addressentrys[j].value);
                      except
                        raise exception.Create('The value of '+trainerdata[i].description+' is invalid');
                      end;
                    end;

        3,4:        begin
                      try
                        trainerdata[i].addressentrys[j].valuef:=StrToFloat(trainerdata[i].addressentrys[j].value);
                      except
                        raise exception.Create('The value of '+trainerdata[i].description+' is invalid');
                      end;
                    end;

        5:          trainerdata[i].addressentrys[j].valuelength:=length(trainerdata[i].addressentrys[j].value);

        7:          begin
                      setlength(trainerdata[i].addressentrys[j].valuea,length(trainerdata[i].addressentrys[j].value));
                      for k:=0 to length(trainerdata[i].addressentrys[j].value)-1 do
                        trainerdata[i].addressentrys[j].valuea[k]:=ord(trainerdata[i].addressentrys[j].value[k+1]);

                      trainerdata[i].addressentrys[j].valuelength:=length(trainerdata[i].addressentrys[j].valuea);
                    end;

        8:          begin
                      ConvertStringToBytes(trainerdata[i].addressentrys[j].value,true,temptb);
                      setlength(trainerdata[i].addressentrys[j].valuea,length(temptb));
                      for k:=0 to length(temptb)-1 do
                        trainerdata[i].addressentrys[j].valuea[k]:=byte(temptb[k]);

                      setlength(temptb,0);
                      trainerdata[i].addressentrys[j].valuelength:=length(trainerdata[i].addressentrys[j].valuea);
                    end;

      end;

    end;      

  end;

  

  if userdefinedform<>nil then
  begin
    for i:=0 to length(userdefinedform.cheat)-1 do
      userdefinedform.cheat[i].onmousedown:=cheatclick;

    for i:=0 to length(userdefinedform.cheatlist)-1 do
      for j:=0 to userdefinedform.cheatlist[i].Count-1 do
        userdefinedform.cheatlist[i].Items[j].OnMouseDown:=cheatclick;
  end;

  //find out if the process is open
  CheckForProcess;

  if processhandle<>0 then
  begin //check and see if the process got changed. (check the code and see if they are nopped)
    for i:=0 to length(trainerdata)-1 do
      for J:=0 to length(trainerdata[i].codeentrys)-1 do
      begin
        //check for nops
        //if yes then set it to active
        setlength(tempab,length(trainerdata[i].codeentrys[j].originalopcode));
        readprocessmemory(processhandle,pointer(trainerdata[i].codeentrys[j].address),@tempab[0],length(tempab),temp);
        if not comparemem(@trainerdata[i].codeentrys[j].originalopcode[0],@tempab[0],length(tempab)) then
        begin
          //it is not the same
          //check if tempab consists out of nops
          for k:=0 to length(tempab)-1 do
            if tempab[k]<>$90 then raise exception.Create('The version of '+process+' that is currently running, is not compatible with this trainer!');

          //if we get here then it got nopped so set it to activated
          trainerdata[i].active:=true;

          if userdefinedform<>nil then
          begin
            for k:=0 to length(userdefinedform.cheatlist)-1 do
              userdefinedform.cheatlist[k].Items[i].activated:=true;

            for k:=0 to length(userdefinedform.cheat)-1 do
               if (userdefinedform.cheat[k].cheatnr=i) then
                 userdefinedform.cheat[k].activated:=true;
          end
          else clist.Items[i].activated:=true;
        end;
      end;
  end else if autolaunch then btnlaunch.Click;
end;

procedure TfrmMemoryTrainer.Timer1Timer(Sender: TObject);
begin
  CheckForProcess;
  timer1.Enabled:=false;
  timer1.interval:=15000;
  timer1.enabled:=true;
end;

procedure TfrmMemoryTrainer.btnLaunchClick(Sender: TObject);
begin
  if fileexists(filename) then
  begin
    shellexecute(0,'open',pchar(filename),'','',sw_show);
    btnlaunch.Enabled:=false;
    timer1.Enabled:=false;
    timer1.interval:=10000;
    timer1.enabled:=true;
  end
  else
  if fileexists(extractfilename(filename)) then
  begin
    shellexecute(0,'open',pchar(extractfilename(filename)),'','',sw_show);
    btnlaunch.Enabled:=false;
    timer1.Enabled:=false;
    timer1.interval:=10000;
    timer1.enabled:=true;
  end
  else
  begin
    opendialog1.FileName:=extractfilename(filename);
    if opendialog1.Execute then
    begin
      btnlaunch.Enabled:=false;
      timer1.Enabled:=false;
      timer1.interval:=10000;
      timer1.enabled:=true;
      filename:=opendialog1.filename;
      shellexecute(0,'open',pchar(filename),'','',sw_show);
    end;
  end;
end;

procedure TfrmMemoryTrainer.FreezerTimer(Sender: TObject);
var i,j,k,l,m: integer;
    newvalue1: byte;
    newvalue2: word;
    newvalue3: dword;
    newvalue4: single;
    newvalue5: double;
    newvalue6: int64;

    oldvalue1: byte;
    oldvalue2: word;
    oldvalue3: dword;
    oldvalue4: single;
    oldvalue5: double;
    oldvalue6: int64;


    bl: integer;
    newbytes: array of byte;
    bytes: dword;
    realaddress,realaddress2,count:dword;
    original:dword;
begin
  for i:=0 to length(trainerdata)-1 do
  begin
    if trainerdata[i].active then
    begin
      for j:=0 to length(trainerdata[i].addressentrys)-1 do
      begin
        if trainerdata[i].addressentrys[j].frozen then
        begin
          if trainerdata[i].addressentrys[j].IsPointer then
          begin
            //find the real address
            realaddress2:=trainerdata[i].addressentrys[j].pointers[length(trainerdata[i].addressentrys[j].pointers)-1].Address;
            for k:=length(trainerdata[i].addressentrys[j].pointers)-1 downto 0 do
            begin
              readprocessmemory(processhandle,pointer(realaddress2),@realaddress,4,count);
              if count=4 then
                realaddress2:=realaddress+trainerdata[i].addressentrys[j].pointers[k].offset
              else
              begin
                realaddress2:=0;
                break;
              end;
            end;
            if realaddress2=0 then continue;
            realaddress:=realaddress2;
          end else realaddress:=trainerdata[i].addressentrys[j].Address;


          VirtualProtectEx(processhandle,  pointer(realaddress),1,PAGE_EXECUTE_READWRITE,original);
          //set value
          case trainerdata[i].addressentrys[j].memtyp of
           0  : begin   //byte
                  if trainerdata[i].addressentrys[j].frozendirection=0 then
                  begin
                    newvalue1:=byte(trainerdata[i].addressentrys[j].valuei);
                    WriteProcessMemory(processhandle,pointer(realaddress),@newvalue1,1,bytes);
                  end
                  else
                  begin
                    readprocessmemory(processhandle,pointer(realaddress),@newvalue1,1,bytes);
                    if trainerdata[i].addressentrys[j].Frozendirection=1 then
                    begin
                      //allow decreased values
                      if newvalue1<byte(trainerdata[i].addressentrys[j].valuei) then trainerdata[i].addressentrys[j].valuei:=newvalue1
                      else
                      begin
                        newvalue1:=byte(trainerdata[i].addressentrys[j].valuei);
                        WriteProcessMemory(processhandle,pointer(realaddress),@newvalue1,1,bytes);
                      end;
                    end
                    else
                    begin
                      //allow increased values
                      if newvalue1>byte(trainerdata[i].addressentrys[j].valuei) then trainerdata[i].addressentrys[j].valuei:=newvalue1
                      else
                      begin
                        newvalue1:=byte(trainerdata[i].addressentrys[j].valuei);
                        WriteProcessMemory(processhandle,pointer(realaddress),@newvalue1,1,bytes);
                      end;
                    end;
                  end;
                end;

           1  : begin //word
                  newvalue2:=word(trainerdata[i].addressentrys[j].valuei);
                  if trainerdata[i].addressentrys[j].frozendirection=0 then WriteProcessMemory(processhandle,pointer(realaddress),@newvalue2,2,bytes)
                  else
                  begin
                    ReadProcessMemory(processhandle,pointer(realaddress),@oldvalue2,2,bytes);
                    if trainerdata[i].addressentrys[j].frozendirection=1 then
                    begin
                      //allow decrease
                      if oldvalue2<newvalue2 then
                        trainerdata[i].addressentrys[j].valuei:=oldvalue2
                      else
                        WriteProcessMemory(processhandle,pointer(realaddress),@newvalue2,2,bytes);

                    end
                    else
                    begin
                      //allow inc
                      if oldvalue2>newvalue2 then
                        trainerdata[i].addressentrys[j].valuei:=oldvalue2
                      else
                        WriteProcessMemory(processhandle,pointer(realaddress),@newvalue2,2,bytes);
                    end;


                  end;
                end;

           2  : begin  //dword
                  newvalue3:=dword(trainerdata[i].addressentrys[j].valuei);
                  if trainerdata[i].addressentrys[j].frozendirection=0 then WriteProcessMemory(processhandle,pointer(realaddress),@newvalue3,4,bytes)
                  else
                  begin
                    ReadProcessMemory(processhandle,pointer(realaddress),@oldvalue3,4,bytes);
                    if trainerdata[i].addressentrys[j].frozendirection=1 then
                    begin
                      //allow decrease
                      if oldvalue3<newvalue3 then
                        trainerdata[i].addressentrys[j].valuei:=oldvalue3
                      else
                        WriteProcessMemory(processhandle,pointer(realaddress),@newvalue3,4,bytes)
                    end
                    else
                    begin
                      if oldvalue3>newvalue3 then
                        trainerdata[i].addressentrys[j].valuei:=oldvalue3
                      else
                        WriteProcessMemory(processhandle,pointer(realaddress),@newvalue3,4,bytes)

                    end;

                  end;
                end;

           3  : begin //single
                  newvalue4:=trainerdata[i].addressentrys[j].valuef;
                  if trainerdata[i].addressentrys[j].frozendirection=0 then WriteProcessMemory(processhandle,pointer(realaddress),@newvalue4,4,bytes)
                  else
                  begin
                    ReadProcessMemory(processhandle,pointer(realaddress),@oldvalue4,4,bytes);
                    if (not isnan(oldvalue4)) and (not isinfinite(oldvalue4)) then
                    begin
                      if trainerdata[i].addressentrys[j].frozendirection=1 then
                      begin
                        //allow decrease
                        if oldvalue4<newvalue4 then
                          trainerdata[i].addressentrys[j].valuef:=oldvalue4
                        else
                          WriteProcessMemory(processhandle,pointer(realaddress),@newvalue4,4,bytes)
                      end
                      else
                      begin
                        if oldvalue4>newvalue4 then
                          trainerdata[i].addressentrys[j].valuef:=oldvalue4
                        else
                          WriteProcessMemory(processhandle,pointer(realaddress),@newvalue4,4,bytes);

                      end;
                    end else WriteProcessMemory(processhandle,pointer(realaddress),@newvalue4,4,bytes);
                  end;
                end;

           4  : begin //double
                  newvalue5:=double(trainerdata[i].addressentrys[j].valuef);
                  if trainerdata[i].addressentrys[j].frozendirection=0 then WriteProcessMemory(processhandle,pointer(realaddress),@newvalue5,8,bytes)
                  else
                  begin
                    ReadProcessMemory(processhandle,pointer(realaddress),@oldvalue5,8,bytes);
                    if (not isnan(oldvalue5)) and (not isinfinite(oldvalue5)) then
                    begin
                      if trainerdata[i].addressentrys[j].frozendirection=1 then
                      begin
                        //allow decrease
                        if oldvalue5<newvalue5 then
                          trainerdata[i].addressentrys[j].valuef:=oldvalue5
                        else
                          WriteProcessMemory(processhandle,pointer(realaddress),@newvalue5,8,bytes)
                      end
                      else
                      begin
                        if oldvalue5>newvalue5 then
                          trainerdata[i].addressentrys[j].valuef:=oldvalue5
                        else
                          WriteProcessMemory(processhandle,pointer(realaddress),@newvalue5,8,bytes)

                      end;
                    end else WriteProcessMemory(processhandle,pointer(realaddress),@newvalue5,8,bytes)
                  end;

                end;

           5  : begin  //binary
                  bl:=1+((length(trainerdata[i].addressentrys[j].value)-1) div 8);
                  setlength(newbytes,bl);
                  ReadProcessMemory(processhandle,pointer(realaddress),@newbytes[0],bl,bytes);


                  l:=trainerdata[i].addressentrys[j].bit;
                  k:=0;
                  for m:=length(trainerdata[i].addressentrys[j].value) downto 1 do
                  begin
                    case trainerdata[i].addressentrys[j].value[m] of
                      '0' : setbit(l,newbytes[k],0);
                      '1' : setbit(l,newbytes[k],1);
                      '*','?','x': ;
                    end;

                    inc(l);
                    if l>=8 then
                    begin
                      inc(k);
                      l:=0;
                    end;
                  end;

                  writeprocessmemory(processhandle,pointer(realaddress),@newbytes[0],bl,bytes);
                  setlength(newbytes,0);
                end;

           6  : begin  //int64
                  newvalue6:=dword(trainerdata[i].addressentrys[j].valuei);
                  if trainerdata[i].addressentrys[j].frozendirection=0 then WriteProcessMemory(processhandle,pointer(realaddress),@newvalue6,8,bytes)
                  else
                  begin
                    ReadProcessMemory(processhandle,pointer(realaddress),@oldvalue6,8,bytes);
                    if trainerdata[i].addressentrys[j].frozendirection=1 then
                    begin
                      //allow decrease
                      if oldvalue6<newvalue6 then
                        trainerdata[i].addressentrys[j].valuei:=oldvalue6
                      else
                        WriteProcessMemory(processhandle,pointer(realaddress),@newvalue6,8,bytes)
                    end
                    else
                    begin
                      if oldvalue6>newvalue6 then
                        trainerdata[i].addressentrys[j].valuei:=oldvalue6
                      else
                        WriteProcessMemory(processhandle,pointer(realaddress),@newvalue6,8,bytes)

                    end;

                  end;
                end;

          7,8:  begin  //array of bytes , or array of char
                  writeprocessmemory(processhandle,pointer(realaddress),@trainerdata[i].addressentrys[j].valuea[0],trainerdata[i].addressentrys[j].valuelength,bytes);
                end;
           end;
          VirtualProtectEx(processhandle,  pointer(realaddress),1,original,original);

        end;
      end;
    end;
  end;
end;

procedure TfrmMemoryTrainer.Button1Click(Sender: TObject);
begin
  close;
end;

procedure TfrmMemoryTrainer.Button2Click(Sender: TObject);
begin
  showmessage(aboutboxtext);
end;

procedure TfrmMemoryTrainer.Button3Click(Sender: TObject);
begin
  if (sender is Tlabel2) then shellexecute(0,'open',pchar(tlabel2(sender).command),'','',sw_show);
  if (sender is TImage2) then shellexecute(0,'open',pchar(timage2(sender).command),'','',sw_show);
  if (sender is TButton2) then shellexecute(0,'open',pchar(tbutton2(sender).command),'','',sw_show);
end;

procedure TfrmMemoryTrainer.Timer2Timer(Sender: TObject);
begin
  if reinitializedesired and (processhandle<>0) then
  begin
    //last time not all addresses got loaded successfull
    reinterpretaddresses;

    if reinitializedesired and symhandler.isloaded then symhandler.reinitialize;
  end;
end;

procedure TfrmMemoryTrainer.Timer3Timer(Sender: TObject);
begin
  //every 30 seconds reinterpret anyhow. (not really needed since dlls usually dont change address, but lets do it anyhow) 
  reinterpretaddresses;
end;

procedure TfrmMemoryTrainer.CheatClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var t: tcheat;
    m: twmhotkey;
begin
  //sender is of type tcheat
  t:=tcheat(sender);

  //fake a hotkeypress
  m.hotkey:=t.cheatnr;

  hotkeyhandler(m);
end;

procedure TfrmMemoryTrainer.executecheat(sender: tobject);
var cheatnr: integer;
    m: twmhotkey;
begin
  if (sender is tbutton) then
    cheatnr:=tbutton(sender).Tag;

  if (sender is tlabel) then
    cheatnr:=tlabel(sender).tag;

  if (sender is timage) then
    cheatnr:=tlabel(sender).tag;

  cheatnr:=cheatnr-5;
  m.HotKey:=cheatnr;
  hotkeyhandler(m);
end; //onclick event

end.
