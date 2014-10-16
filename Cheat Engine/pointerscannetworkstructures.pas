unit PointerscanNetworkStructures;

{
unit containing some structures used to pass information between functions and child/parents
}

{$mode delphi}

interface

uses
  Classes, SysUtils;

type
  TPublicParentData=record
    connected: boolean;
    ip: string;
    port: word;
    lastupdatesent: qword;
    waitingforreconnect: boolean;
  end;

  TPublicConnectionEntry=record //information publicly available about children
    parentconnectedto: boolean; //true if the parent connected to the child
    ip: string;
    port: word;
    isidle: boolean;
    disconnected: boolean;
    pathsevaluated: qword;
    trustedconnection: boolean;
    threadcount: integer;
    pathquesize: integer;
    totalpathqueuesize: integer;
    resultsfound: qword;

    uploadingscandata:boolean;
    uploadscandataprogress: integer;
    uploadscandataspeed: integer;
    downloadingResuls: boolean;
  end;

  TConnectionEntryArray=array of TPublicConnectionEntry;


type
  TPSHelloMsg=record
    publicname: string;
    currentscanid: uint32;
    scannerid: uint32;
  end;

type
  TPSUpdateStatusMsg=record
    currentscanid: uint32;
    isidle: byte;
    totalthreadcount: integer;
    pathsevaluated: qword;
    localpathqueuecount: uint32;
    totalpathQueueCount: uint32;
    queuesize: uint32;
  end;

implementation

end.

