unit speedhack2;

{$MODE Delphi}

interface

uses Classes,LCLIntf, SysUtils, NewKernelHandler,CEFuncProc, symbolhandler,
     autoassembler, dialogs,Clipbrd;

type TSpeedhack=class
  private
    processid: dword;
    initaddress: ptrUint;
  public
    procedure setSpeed(speed: single);
    constructor create;
    destructor destroy; override;
  end;

var speedhack: TSpeedhack;

implementation

uses frmAutoInjectUnit;

resourcestring
  rsFailureEnablingSpeedhackDLLInjectionFailed = 'Failure enabling speedhack. (DLL injection failed)';
  rsFailureConfiguringSpeedhackPart = 'Failure configuring speedhack part';
  rsFailureSettingSpeed = 'Failure setting speed';

constructor TSpeedhack.create;
var i: integer;
    script: tstringlist;
    AllocArray: TCEAllocArray;
    x,y: dword;
    a,b: ptrUint;
begin
  initaddress:=0;


  try
    if processhandler.is64bit then
      injectdll(CheatEngineDir+'speedhack-x86_64.dll','')
    else
      injectdll(CheatEngineDir+'speedhack-i386.dll','');
    symhandler.reinitialize;
    symhandler.waitforsymbolsloaded;
  except
    on e: exception do
    begin
      raise exception.Create(rsFailureEnablingSpeedhackDLLInjectionFailed+': '+e.message);
    end;
  end;

       
  script:=tstringlist.Create;
  try
    if processhandler.is64bit then
      script.Add('alloc(init,512, GetTickCount)')
    else
      script.Add('alloc(init,512)');
    //check if it already has a a speedhack script running

    a:=symhandler.getAddressFromName('realgettickcount') ;
    b:=0;
    readprocessmemory(processhandle,pointer(a),@b,processhandler.pointersize,x);
    if b<>0 then //already configured
      generateAPIHookScript(script, 'GetTickCount', 'speedhackversion_GetTickCount')
    else
      generateAPIHookScript(script, 'GetTickCount', 'speedhackversion_GetTickCount', 'realgettickcount');



    try
      setlength(AllocArray,0);

      autoassemble(script,false,true,false,false,AllocArray);

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
    script.Clear;
    script.Add('timeGetTime:');
    script.Add('jmp speedhackversion_GetTickCount');
    try
      autoassemble(script,false);
    except //don't mind
    end;


    script.clear;
    a:=symhandler.getAddressFromName('realQueryPerformanceCounter') ;
    b:=0;
    readprocessmemory(processhandle,pointer(a),@b,processhandler.pointersize,x);
    if b<>0 then //already configured
      generateAPIHookScript(script, 'QueryPerformanceCounter', 'speedhackversion_QueryPerformanceCounter')
    else
      generateAPIHookScript(script, 'QueryPerformanceCounter', 'speedhackversion_QueryPerformanceCounter', 'realQueryPerformanceCounter');

    try
      autoassemble(script,false);
    except //do mind
      raise exception.Create(rsFailureConfiguringSpeedhackPart+' 2');
    end;

  finally
    script.free;
  end;

  setspeed(1);
  processid:=cefuncproc.processid;
end;

destructor TSpeedhack.destroy;
var script: tstringlist;
    i: integer;
    x: dword;
begin
  if processid=cefuncproc.ProcessID then
  begin

    try
      setSpeed(1);
    except
    end;
  end;

  //do not undo the speedhack script (not all games handle a counter that goes back)
 
end;

procedure TSpeedhack.setSpeed(speed: single);
var x: single;
    script: Tstringlist;

begin
  x:=speed;
  script:=tstringlist.Create;
  try
    script.add('CreateThread('+inttohex(initaddress,8)+')');
    script.add('label(newspeed)');
    script.add(inttohex(initaddress,8)+':');
    if processhandler.is64Bit then
    begin
      script.add('sub rsp,#32');
      script.add('movss xmm0,[newspeed]');
      script.add('push rcx');
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
  finally
    script.free;
  end;

end;


end.
