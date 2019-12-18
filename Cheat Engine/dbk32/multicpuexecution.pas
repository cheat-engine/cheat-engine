unit multicpuexecution;
{
This unit provides some routines that make it easier to pinpoint which cpu will execute a certain piece of code
}

{$MODE Delphi}

interface

uses
  {$ifdef darwin}macport,{$endif}
  {$ifdef windows}windows,{$endif}classes, sysutils, LCLIntf;

type TCpuSpecificFunction=function(parameters: pointer): BOOL; stdcall;
function foreachcpu(functionpointer: TCpuSpecificFunction; parameters: pointer) :boolean;
function forspecificcpu(cpunr: integer; functionpointer: TCpuSpecificFunction; parameters: pointer) :boolean;


implementation


type Tforeachcpu=class(tthread)
  private

  public
    fp: TCpuSpecificFunction;
    parameter: pointer;
    r: boolean;
    procedure execute; override;
  end;

procedure Tforeachcpu.execute;
begin
  r:=fp(parameter);
end;

function forspecificcpu(cpunr: integer; functionpointer: TCpuSpecificFunction; parameters: pointer) :boolean;
var PA,SA:DWORD_PTR;
begin
  {$ifdef windows}
  result:=true;
  GetProcessAffinityMask(getcurrentprocess,PA,SA);

  if ((1 shl cpunr) and SA) = 0 then
  begin
    //cpu doesn't exist, do nothing
    OutputDebugString(pchar(format('forspecificcpu:Cpu number %d does not exist',[cpunr])));
    exit;
  end;



  SetProcessAffinityMask(GetCurrentProcess,(1 shl cpunr));
  sleep(0);
  with Tforeachcpu.Create(true) do
  begin
    fp:=functionpointer;
    parameter:=parameters;
    start;
    waitfor;
    if result then result:=r; //one false and it stays false
    free;
  end;
  SetProcessAffinityMask(GetCurrentProcess,PA);
  {$else}
  result:=false;
  {$endif}
end;

function foreachcpu(functionpointer: TCpuSpecificFunction; parameters: pointer) :boolean;
var
  cpunr,PA,SA:DWORD_PTR;
  r: bool;
begin
  {$ifdef windows}
  result:=true;
  GetProcessAffinityMask(getcurrentprocess,PA,SA);

  for cpunr:=0 to 31 do
    if ((1 shl cpunr) and SA)>0 then //cpu found
    begin
      r:=forspecificcpu(cpunr,functionpointer, parameters);
      if result then result:=r;
    end;

  SetProcessAffinityMask(GetCurrentProcess,PA);

  {$else}
  result:=false;
  {$endif}
end;

end.
