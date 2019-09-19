unit speedhack2;

{$MODE Delphi}

interface

uses Classes,LCLIntf, SysUtils, NewKernelHandler,CEFuncProc, symbolhandler,
     autoassembler, dialogs,Clipbrd, commonTypeDefs, controls;

type TSpeedhack=class
  private
    fProcessId: dword;
    initaddress: ptrUint;
    lastSpeed: single;
  public
    procedure setSpeed(speed: single);
    function getSpeed: single;
    property processid: dword read fProcessId;
    constructor create;
    destructor destroy; override;
  end;

var speedhack: TSpeedhack;

implementation

uses frmAutoInjectUnit, networkInterface, networkInterfaceApi, ProcessHandlerUnit, Globals;

resourcestring
  rsFailureEnablingSpeedhackDLLInjectionFailed = 'Failure enabling speedhack. (DLL injection failed)';
  rsFailureConfiguringSpeedhackPart = 'Failure configuring speedhack part';
  rsFailureSettingSpeed = 'Failure setting speed';

constructor TSpeedhack.create;
var i: integer;
    script: tstringlist;
    AllocArray: TCEAllocArray;
    exceptionlist: TCEExceptionListArray;
    x: ptrUint;
//      y: dword;
    a,b: ptrUint;
    c: TCEConnection;
    e: boolean;

    fname: string;
    err: boolean;

    path: string;

    QPCAddress: ptruint;
begin
  initaddress:=0;

  if processhandler.isNetwork then
  begin
    c:=getconnection;
    c.loadExtension(processhandle); //just to be sure

    symhandler.reinitialize;
    symhandler.waitforsymbolsloaded(true);
  end
  else
  begin
    try
      if processhandler.is64bit then
        fname:='speedhack-x86_64.dll'
      else
        fname:='speedhack-i386.dll';

      symhandler.waitforsymbolsloaded(true, 'kernel32.dll'); //speed it up (else it'll wait inside the symbol lookup of injectdll)

      symhandler.waitForExports;
      symhandler.getAddressFromName('speedhackversion_GetTickCount',false,e);
      if e then
      begin
        injectdll(CheatEngineDir+fname);
        symhandler.reinitialize;
        symhandler.waitforsymbolsloaded(true)
      end;


    except
      on e: exception do
      begin
        raise exception.Create(rsFailureEnablingSpeedhackDLLInjectionFailed+': '+e.message);
      end;
    end;
  end;

       
  script:=tstringlist.Create;
  try
    if processhandler.isNetwork then
    begin
      //linux


      //gettimeofday

      if symhandler.getAddressFromName('vdso.gettimeofday', true, err)>0 then //prefered
        fname:='vdso.gettimeofday'
      else
      if symhandler.getAddressFromName('libc.gettimeofday', true, err)>0 then //secondary
        fname:='libc.gettimeofday'
      else
      if symhandler.getAddressFromName('gettimeofday', true, err)>0 then //really nothing else ?
        fname:='gettimeofday'
      else
        fname:=''; //give up


      if fname<>'' then //hook gettimeofday
      begin
        //check if it already has a a speedhack running
        a:=symhandler.getAddressFromName('real_gettimeofday');
        b:=0;

        readprocessmemory(processhandle,pointer(a),@b,processhandler.pointersize,x);
        if b=0 then //not yet hooked
        begin
          generateAPIHookScript(script, fname, 'new_gettimeofday', 'real_gettimeofday');

          try
            //Clipboard.AsText:=script.text;
            autoassemble(script,false);
          except
          end;
        end;
      end;

      script.clear;
      //clock_gettime
      if symhandler.getAddressFromName('vdso.clock_gettime', true,err)>0 then //prefered
        fname:='vdso.clock_gettime'
      else
      if symhandler.getAddressFromName('librt.clock_gettime', true, err)>0 then //secondary
        fname:='librt.clock_gettime'
      else
      if symhandler.getAddressFromName('libc.clock_gettime', true, err)>0 then //seen this on android
        fname:='libc.clock_gettime'
      else
      if symhandler.getAddressFromName('clock_gettime', true, err)>0 then //really nothing else ?
        fname:='clock_gettime'
      else
        fname:=''; //give up


      if fname<>'' then //hook gettimeofday
      begin
        //check if it already has a a speedhack running
        a:=symhandler.getAddressFromName('real_clock_gettime');
        b:=0;

        readprocessmemory(processhandle,pointer(a),@b,processhandler.pointersize,x);
        if b=0 then //not yet hooked
        begin
          generateAPIHookScript(script, fname, 'new_clock_gettime', 'real_clock_gettime');

          try
            //Clipboard.AsText:=script.text;
            autoassemble(script,false);
          except
          end;
        end;
      end;

    end
    else
    begin
      //windows
      if processhandler.is64bit then
        script.Add('alloc(init,512, GetTickCount)')
      else
        script.Add('alloc(init,512)');
      //check if it already has a a speedhack script running

      a:=symhandler.getAddressFromName('realgettickcount', true) ;
      b:=0;
      readprocessmemory(processhandle,pointer(a),@b,processhandler.pointersize,x);
      if b<>0 then //already configured
        generateAPIHookScript(script, 'GetTickCount', 'speedhackversion_GetTickCount')
      else
        generateAPIHookScript(script, 'GetTickCount', 'speedhackversion_GetTickCount', 'realgettickcount');

      //if ssCtrl in GetKeyShiftState then //debug code
      //  Clipboard.AsText:=script.text;

      try
        setlength(AllocArray,0);

        autoassemble(script,false,true,false,false,AllocArray, exceptionlist);
        //clipboard.AsText:=script.text;

        //fill in the address for the init region
        for i:=0 to length(AllocArray)-1 do
          if AllocArray[i].varname='init' then
          begin
            initaddress:=AllocArray[i].address;
            break;
          end;


      except
        on e:exception do
        begin
          clipboard.AsText:=script.text;
          raise exception.Create(rsFailureConfiguringSpeedhackPart+' 1: '+e.message);
        end;
      end;

      //timegettime
      if symhandler.getAddressFromName('timeGetTime',true,err)>0 then //might not be loaded
      begin
        script.Clear;
        script.Add('timeGetTime:');
        script.Add('jmp speedhackversion_GetTickCount');
        try
          autoassemble(script,false);
        except //don't mind
        end;
      end;


      //qpc
      qpcaddress:=symhandler.getAddressFromName('ntdll.RtlQueryPerformanceCounter',true, err);
      if err then
        qpcaddress:=symhandler.getAddressFromName('kernel32.RtlQueryPerformanceCounter',true);


      script.clear;
      a:=symhandler.getAddressFromName('realQueryPerformanceCounter') ;
      b:=0;
      readprocessmemory(processhandle,pointer(a),@b,processhandler.pointersize,x);

      if b<>0 then //already configured
        generateAPIHookScript(script, inttohex(qpcaddress,8), 'speedhackversion_QueryPerformanceCounter')
      else
        generateAPIHookScript(script, inttohex(qpcaddress,8), 'speedhackversion_QueryPerformanceCounter', 'realQueryPerformanceCounter');

      try
        autoassemble(script,false);
      except //do mind
        raise exception.Create(rsFailureConfiguringSpeedhackPart+' 2');
      end;

      //gettickcount64
      if symhandler.getAddressFromName('GetTickCount64',true,err)>0 then
      begin
        script.clear;
        a:=symhandler.getAddressFromName('realGetTickCount64') ;
        b:=0;
        readprocessmemory(processhandle,pointer(a),@b,processhandler.pointersize,x);
        if b<>0 then //already configured
          generateAPIHookScript(script, 'GetTickCount64', 'speedhackversion_GetTickCount64')
        else
          generateAPIHookScript(script, 'GetTickCount64', 'speedhackversion_GetTickCount64', 'realGetTickCount64');

        try
          autoassemble(script,false);
        except //do mind
          raise exception.Create(rsFailureConfiguringSpeedhackPart+' 3');
        end;
      end;


    end;

  finally
    script.free;
  end;

  setspeed(1);
  fprocessid:=processhandlerunit.processid;
end;

destructor TSpeedhack.destroy;
var script: tstringlist;
    i: integer;
    x: dword;
begin
  if fprocessid=processhandlerunit.ProcessID then
  begin

    try
      setSpeed(1);
    except
    end;
  end;

  //do not undo the speedhack script (not all games handle a counter that goes back)
 
end;

function TSpeedhack.getSpeed: single;
begin
  if self=nil then
    result:=1
  else
    result:=lastspeed;
end;

procedure TSpeedhack.setSpeed(speed: single);
var x: single;
    script: Tstringlist;

begin
  if processhandler.isNetwork then
  begin
    getConnection.speedhack_setSpeed(processhandle, speed);
    lastspeed:=speed;
  end
  else
  begin
    x:=speed;
    script:=tstringlist.Create;
    try
      script.add('CreateThread('+inttohex(initaddress,8)+')');
      script.add('label(newspeed)');
      script.add(inttohex(initaddress,8)+':');
      if processhandler.is64Bit then
      begin
        script.add('sub rsp,#40');
        script.add('movss xmm0,[newspeed]');
      end
      else
        script.add('push [newspeed]');

      script.add('call InitializeSpeedhack');

      if processhandler.is64Bit then
      begin
        script.add('add rsp,#40');
        script.add('ret');
      end
      else
      begin
        script.add('ret 4');
      end;

      script.add('newspeed:');
      script.add('dd '+inttohex(pdword(@x)^,8));

      try

  //      showmessage(script.Text);
       // Clipboard.AsText:=script.text;
        autoassemble(script,false);
      except
        raise exception.Create(rsFailureSettingSpeed);
      end;

      lastspeed:=speed;
    finally
      script.free;
    end;

  end;

end;

{
alloc(bla,2048)
alloc(newspeed,4);

bla:
sub rsp,28

movss xmm0,[newspeed]
call speedhack_initializeSpeed

add rsp,28
ret

newspeed:
dd (float)-1.0

createthread(bla)
}

end.
