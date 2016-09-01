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
    name: string;
    ip: string;
    port: word;
    lastupdatesent: qword;
    waitingforreconnect: boolean;
  end;

  TPublicConnectionEntry=record //information publicly available about children
    parentconnectedto: boolean; //true if the parent connected to the child
    ip: string;
    childid: integer;
    port: word;
    queued: boolean;
    queuesize: integer;
    queuepos: integer;

    isidle: boolean;
    disconnected: boolean;
    lasterror: string;
    pathsevaluated: qword;
    trustedconnection: boolean;
    potentialthreadcount: integer;
    actualthreadcount: integer;
    pathquesize: integer;
    totalpathqueuesize: integer;
    resultsfound: qword;

    uploadingscandata:boolean;
    ScanDataSent: qword;
    ScanDataTotalSize: qword;
    ScanDataStartTime: qword;
    downloadingResuls: boolean;

    lastUpdateReceived: qword;
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
    potentialthreadcount: integer;
    actualthreadcount: integer;
    pathsevaluated: qword;
    localpathqueuecount: uint32;
    totalpathQueueCount: uint32;
    queuesize: uint32;
  end;

implementation

end.

