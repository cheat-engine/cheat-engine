unit settingsunit;

interface

uses forms,classes, SysUtils,dialogs;


var trainerfile: TFileStream;
protect:boolean;

function openself: boolean;

implementation

function openself: boolean;
var temp: integer;
begin
  //use c:\bla.exe till the final build (just to simplify debugging)
  try
    {$ifdef trainerbeta}
      trainerfile:=TFileStream.create('c:\xxx.exe',fmOpenRead or fmShareDenyNone);
    {$else}
      trainerfile:=TFileStream.create(application.exename,fmOpenRead or fmShareDenyNone);
    {$endif}
    //find out of this is a patcher or a memorybrowser
    trainerfile.Position:=80;

    trainerfile.Readbuffer(temp,4);
    trainerfile.position:=temp;

    trainerfile.ReadBuffer(temp,4);

    protect:=temp=$22322;

    if temp=$111111 then
      result:=true
    else
      result:=false;
      
  except
    result:=false;
    messagedlg('There was an error while trying to read the trainer. Or the file got changed(like a exe-compresser), or you''re using an old unpatched version of win 95 or earlier',mterror,[mbok],0);
    application.Terminate;
  end;

end;

end.
