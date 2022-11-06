unit PointerscanStructures;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport, Classes, SysUtils, Sockets, CELazySocket, commonTypeDefs;
  {$endif}
  {$ifdef windows}
  windows, Classes, SysUtils, winsock, CELazySocket, commonTypeDefs;
  {$endif}

const
  MAXQUEUESIZE=64;
  pointerscanfileversion=2;

type



  TPathQueueElement=record
    tempresults: array of dword;
    valuelist: array of qword;
    valuetofind: qword;
    startlevel: integer;
  end;
  PPathQueueElement=^TPathQueueElement;


  TMainPathQueue=array [0..MAXQUEUESIZE-1] of TPathQueueElement;

  PMainPathQueue=^TMainPathQueue;
  TDynPathQueue=array of TPathQueueElement;

  TPointerscanControllerParent=record
    socket: TSocketstream;
    scanid: dword;
    name: string;

    iConnectedTo: boolean;
    connectdata: record
      ip: string;
      port: word;
      password: string;
    end;
    ip: string;
    port: word;

    trustsme: boolean;
    knowsIAmTerminating: boolean;
    connecttime: qword;
  end;

  PPointerscanControllerParent=^TPointerscanControllerParent;

  TPointerscancontrollerchild=record   //todo: maybe turn this into a class with methods to call child commands ?
    socket: TSocketstream;
    MissingSince: qword; //holds the time when the connection was lost. If this is set to 0 the child info will be deleted
    Error: string;

    iConnectedTo: boolean;
    connectdata: record
      ip: string;
      port: word;
      password: string;
    end;
    ip: string;
    port: word;
    childid: integer;
    trusted: boolean;
    takePathsAndDisconnect: boolean;
    terminating: boolean;

    idle: boolean;
    //pathspersecond: qword;

    totalPathsEvaluated: qword;
    pathqueuesize: integer;
    totalpathqueuesize: integer;
    potentialthreadcount: integer;
    actualthreadcount: integer;

    resultsfound: qword;

    queued: boolean;
    queuepos: dword;
    queuesize: dword;

    trustlevel: integer;
    nontrustedlastpaths: TDynPathQueue;
    nontrustedlastpathstime: qword;


    scandatauploader: TThread;
    ScanDataSent: qword;
    ScanDataTotalSize: qword;
    ScanDataStartTime: qword;
    hasReceivedScandata: boolean;

    LastUpdateReceived: qword;

    scanresultDownloader: TThread; //not nil if the results it has sent me are still being processed
    resultstream: TFilestream; //if initializer this holds an open filestream to the .ptr associated with this child
  end;

  PPointerscancontrollerchild=^TPointerscancontrollerchild;

  TQueueWriterMethod=procedure(sender: TObject; PathQueueElement: TPathQueueElement) of object;


implementation

end.

