unit tracerIgnore;

{$mode delphi}

interface

uses
  Classes, SysUtils, symbolhandler, symbolhandlerstructs, cefuncproc, globals;

type
  TTracerIgnore=class
  private
    ignoredtracerregions: array of record
      start: ptruint;
      stop: ptruint;
    end;
  public
    function InIgnoredModuleRange(address: ptruint): boolean;
    procedure loadIgnoredModules;
  end;

var IgnoredModuleListHandler: TTracerIgnore;

implementation


procedure TTracerIgnore.loadIgnoredModules;
var
  s: string;
  i,j: integer;
  donottrace: tstringlist;
  mi: tmoduleinfo;
begin
  //parse the donottrace.txt file and find the range
  setlength(ignoredtracerregions, 0);

  s:=cheatenginedir+'donottrace.txt';
  if FileExists(s) then //if the list exists
  begin
    donottrace:=tstringlist.create;

    try
      donottrace.LoadFromFile(s, true);

      i:=0;
      while i<donottrace.Count do
      begin
        j:=pos('#', donottrace[i]);
        if j>0 then
          donottrace[i]:=trim(copy(donottrace[i], 1, j-1));

        donottrace[i]:=lowercase(donottrace[i]);

        if donottrace[i]='' then
          donottrace.Delete(i)
        else
        begin
          if symhandler.getmodulebyname(donottrace[i], mi) then
          begin
            //found it
            setlength(ignoredtracerregions, length(ignoredtracerregions)+1);
            ignoredtracerregions[length(ignoredtracerregions)-1].start:=mi.baseaddress;
            ignoredtracerregions[length(ignoredtracerregions)-1].stop:=mi.baseaddress+mi.basesize;
          end;
          inc(i);
        end;
      end;
    except
      //don't care if file can't be loaded anyhow
    end;
  end;

end;

function TTracerIgnore.InIgnoredModuleRange(address: ptruint): boolean;
var i: integer;
begin
  result:=false;
  for i:=0 to length(ignoredtracerregions)-1 do
    if InRangeX(address, ignoredtracerregions[i].start, ignoredtracerregions[i].stop) then
      exit(true);
end;



initialization
  IgnoredModuleListHandler:=TTracerIgnore.create;

end.

