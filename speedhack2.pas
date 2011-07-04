unit speedhack2;

interface

uses Classes,windows, SysUtils, newkernelhandler,cefuncproc, symbolhandler,
     autoassembler, dialogs;

type TSpeedhack=class
  private
    processid: dword;
    initaddress: dword;
  public
    procedure setSpeed(speed: single);
    constructor create;
    destructor destroy; override;
  end;

var speedhack: TSpeedhack;

implementation

uses frmAutoInjectUnit;

constructor TSpeedhack.create;
var i: integer;
    script: tstringlist;
    AllocArray: TCEAllocArray;
    x,y: dword;
begin
  try
    injectdll(CheatEngineDir+'speedhack.dll','');
    symhandler.reinitialize;
    symhandler.waitforsymbolsloaded;
  except
    raise exception.Create('Failure enabling speedhack. (DLL injection failed)');
  end;

       
  script:=tstringlist.Create;
  try
    script.Add('alloc(init,512)');
    //check if it already has a a speedhack script running

    x:=symhandler.getAddressFromName('realgettickcount') ;
    y:=0;
    readprocessmemory(processhandle,pointer(x),@y,4,x);
    if y<>0 then //already configured
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
      raise exception.Create('Failure configuring speedhack part 1');
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
    x:=symhandler.getAddressFromName('realQueryPerformanceCounter') ;
    y:=0;
    readprocessmemory(processhandle,pointer(x),@y,4,x);
    if y<>0 then //already configured
      generateAPIHookScript(script, 'QueryPerformanceCounter', 'speedhackversion_QueryPerformanceCounter')
    else
      generateAPIHookScript(script, 'QueryPerformanceCounter', 'speedhackversion_QueryPerformanceCounter', 'realQueryPerformanceCounter');

    try
      autoassemble(script,false);
    except //do mind
      raise exception.Create('Failure configuring speedhack part 2');
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
    script.add(inttohex(initaddress,8)+':');
    script.add('push '+inttohex(pdword(@x)^,8) );
    script.add('call InitializeSpeedhack');
    script.add('ret');
    try

//      showmessage(script.Text);
      autoassemble(script,false);
    except
      raise exception.Create('Failure setting speed');
    end;
  finally
    script.free;
  end;

end;


end.
