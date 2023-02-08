program cepack;

//command line tool to compress not often used files in CE (32-bit tutorial, trainermaker files, driver files, dbvm, etc...)
//compress:   -c infile outfile
//decompress: -x infile outfile


//use the packfiles.bat file to autocompress the files before building the setup


{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes,
  sysutils,
  libcepack;



begin
  writeln('CE File Packer v 1.0');
  try
    if ParamCount>=3 then
    begin
      if paramstr(1)='-c' then
        cepackfile(paramstr(2), paramstr(3))
      else
      if paramstr(1)='-x' then
        ceunpackfile(paramstr(2), paramstr(3),false)
      else
      begin
        writeln('Parameter 1 is has to be -c (compress) or -x (extract)');
        exit;
      end;

    end;
    WriteLn('done');

  except
    on e:exception do
      writeln(e.message);
  end;

end.

