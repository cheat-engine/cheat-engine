unit PointerscanNetworkCommands;

{$mode delphi}

interface

uses
  Classes, SysUtils;


//--------Network commands--------
{
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
      }

const
  PSCONNECT_INIT=$ce;

  PSCMD_HELLO=0;
  PSCMD_YOUREINTHEQUEUE=1;
  PSCMD_UPDATESTATUS=2;
  PSCMD_AMITRUSTED=3;
  PSCMD_SENDPATHS=4;
  PSCMD_CANUPLOADRESULTS=5;
  PSCMD_UPLOADRESULTS=6;
  PSCMD_PREPAREFORMYTERMINATION=7;
  PSCMD_GOODBYE=8;

  //update reply commands:
  PSUPDATEREPLYCMD_DONEWSCAN=0;
  PSUPDATEREPLYCMD_GIVEMEYOURPATHS=1;
  PSUPDATEREPLYCMD_HEREARESOMEPATHS=2;
  PSUPDATEREPLYCMD_CURRENTSCANHASENDED=3;
  PSUPDATEREPLYCMD_EVERYTHINGOK=4;

implementation

end.

