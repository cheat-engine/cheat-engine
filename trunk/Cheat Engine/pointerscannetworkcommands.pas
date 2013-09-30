unit PointerscanNetworkCommands;

{$mode delphi}

interface

uses
  Classes, SysUtils;


//--------Network commands--------
const
  //pointerscan commands
  //client->server commands
  CMD_GETSCANPARAMETERS=0;
  CMD_UPDATEWORKERSTATUS=1;
  //server->client commands
    CMDUPDATEREPLY_EVERYTHINGOK=0;
    CMDUPDATEREPLY_HEREARESOMEPATHSTOEVALUATE=1;
    CMDUPDATEREPLY_PLEASESENDMESOMEPATHS=2;
    CMDUPDATEREPLY_GOKILLYOURSELF=3;


  //downloader commands
  DCMD_GETSCANDATASIZE=0; //(): integer;
  DCMD_GETSCANDATA=1; //(offset: qword; chunksize: dword)->stream

  //rescan commands
  RCMD_GETPARAMS=0; //()=>baseStart: qword; baseEnd: qword; startoffsetlength: dword; startoffset: array of dword;  endoffsetlength: dword; endoffset: array of dword; address: qword; forvalue: byte(bool); overwrite: bool; mustbeinrange: boolean; valuescandword: dword; valuscansingle: single; valuescansingleMax: single; valuescandouble: double; valuescandoubleMax: double ;originalptrfilenamelength: dword; originaptrfilename: string[originalptrfilenamelength]; filenamelength: dword; filename: string[filenamelength]; modulebasecount: integer modulebase: qword[modulebasecount])
  RCMD_SETID=1; //(workerid)->ok: byte
  RCMD_GETMEMORYREGIONS=2; //() => count:dword, count*(baseaddress: qword; size: qword)
  RCMD_GETPAGES=3; //(Base:QWORD, count: byte) => count*(readable:BOOL, stream:bytearray)  (in the order of base, base+4096, base+8192, ...)
  RCMD_STATUS=4; //(PointersEvaluated: qword; TotalPointersToEvaluate: qword)

implementation

end.

