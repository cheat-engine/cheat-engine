unit frmSortPointerlistUnit;

{$mode delphi}

interface

uses
  {$ifdef darwin} macport, {$endif}
  {$IFDEF windows} windows, {$ENDIF} Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, PointerscanresultReader, maps, ComCtrls, math;

type

  { TfrmSortPointerlist }

  TPointerListSortThread=class(tthread)
  private
    procedure sortdone;
    procedure sorterror;
  public
    initialtime: dword;
    position, maxposition: qword;
    pointerscanresults: TPointerscanresultReader;
    callback: TNotifyEvent;
    callbackOnError: TNotifyEvent;
    column: integer;
    error: string;
    tempname: string;

    tempfilelist: tstrings;
    destructor destroy; override;
    procedure execute; override;
  end;



  TfrmSortPointerlist = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    lblTimeLeft: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    sorter: TPointerListSortThread;
    procedure done(sender: TObject);
    procedure error(sender: TObject);
  public
    { public declarations }
    function dowork(column: integer; ptrfile: string; var tempname: string; tempfilelist: tstrings): boolean;
  end;

implementation

{$R *.lfm}

uses pointerscannerfrm;

resourcestring
  rstimeLeft='Estimated time left';

procedure TPointerListSortThread.sortdone;
begin
  if assigned(callback) then
    callback(self);
end;

procedure TPointerListSortThread.sorterror;
begin
  if assigned(callbackOnError) then
  callbackOnError(self);
end;

destructor TPointerListSortThread.destroy;
begin
  if pointerscanresults<>nil then
    freeandnil(pointerscanresults);
end;

procedure TPointerListSortThread.execute;
var i: qword;
  j: integer;
  p: PPointerscanResult;

  v: qword;
  files: TMap;

  keytype: TMapIdType;

  f: Tfilestream;
  entrysize: integer;

  mi: TMapIterator;

  temp: dword;
  tempstring: string;
  mb: qword;
begin
  try
    entrysize:=pointerscanresults.entrySize;
    if column=0 then
      keytype:=ituPtrSize
    else
      keytype:=its4; //max offset size (signed 32-bit, max relative address)

    files:=tmap.Create(keytype, sizeof(TFilestream));
    tempname:=Pointerscanresults.filename+'.sorted';

    position:=0;
    maxposition:=Pointerscanresults.count;
    initialtime:=gettickcount;

    try
      i:=0;

      while i<Pointerscanresults.count do
      begin
        if terminated then exit;

        position:=i;

        p:=pointerscanresults.getPointer(i);

        if column=0 then //modules are limited to 32-bit length, so seperate them by the modulenr
        begin
          if p.modulenr>=0 then
          begin
            mb:=pointerscanresults.getModuleBase(p.modulenr);
            if mb>0 then
              v:=mb+p.moduleoffset
            else
              v:=qword(qword(p.modulenr+1) shl 32) or dword(p.moduleoffset)
          end
          else
            v:=p.moduleoffset;



        end
        else
        begin
          if column>p.offsetcount then
            v:=p.offsetcount-column //-1 //p.offsets[0]
          else
            v:=p.offsets[p.offsetcount-column];
        end;

        if files.GetData(v, f)=false then  //create this pointerfile.
        begin
          f:=tfilestream.Create(tempname+'.'+inttohex(v,1), fmcreate);
          files.Add(v, f);
        end;

        p:=pointerscanresults.LastRawPointer;
        f.Write(p^, entrysize);
        inc(i);
      end;

    finally
      mi:=TMapIterator.Create(files);

      i:=0;
      if files.count>0 then
      begin
        mi.first;

        while not mi.EOM do
        begin
          f:=nil;
          mi.GetData(f);
          if f<>nil then
          begin
            tempfilelist.Add(f.filename);
            inc(i);
            f.free;
          end;

          mi.Next;


        end;
      end;

      mi.free;
      files.free;
    end;


    if pointerscanresults<>nil then
      freeandnil(pointerscanresults);

    synchronize(sortdone);

  except
    on e:exception do
    begin
      error:=e.Message;
      synchronize(sorterror);
    end;
  end;
end;


procedure TfrmSortPointerlist.Button1Click(Sender: TObject);
begin
  modalresult:=mrcancel;
end;

procedure TfrmSortPointerlist.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin

end;

procedure TfrmSortPointerlist.FormDestroy(Sender: TObject);
begin
  if sorter<>nil then
  begin
    sorter.Terminate;
    sorter.WaitFor;
    freeandnil(sorter);
  end;
end;

procedure TfrmSortPointerlist.Timer1Timer(Sender: TObject);
var
  difftime: dword;
  timeperpos: double;
  totaltime: qword;
  timeleft: string;
  seconds, minutes, hours: qword;
begin
  if sorter<>nil then
  begin
    difftime:=gettickcount-sorter.initialtime;

    if sorter.position>10 then
    begin
      timeperpos:=difftime/sorter.position;
      totaltime:=ceil(timeperpos*(sorter.maxposition-sorter.position)); //time that it will take for the rest

      progressbar1.position:=ceil(sorter.position/sorter.maxposition*100);

      seconds:=(totaltime div 1000);
      minutes:=(seconds div 60);
      hours:=(minutes div 60);

      seconds:=seconds mod 60;
      minutes:=minutes mod 60;

      timeleft:=format('%.2d:%.2d:%.2d', [hours, minutes, seconds]);

      lblTimeLeft.caption:=format('%s : %s', [rsTimeLeft, timeleft]);
    end;

  end;
end;

procedure TfrmSortPointerlist.done(sender: TObject);
begin
  modalresult:=mrok;
end;

procedure TfrmSortPointerlist.error(sender: TObject);
begin
  messagedlg(TPointerListSortThread(sender).error, mtError, [mbok] , 0);
  modalresult:=mrcancel;
end;


function TfrmSortPointerlist.dowork(column: integer; ptrfile: string; var tempname: string; tempfilelist: tstrings): boolean;
begin
  sorter:=TPointerListSortThread.Create(true);
  sorter.callback:=done;
  sorter.callbackOnError:=error;
  sorter.column:=column;
  sorter.pointerscanresults:=TPointerscanresultReader.create(ptrfile);
  sorter.tempfilelist:=tempfilelist;


  sorter.Start;
  result:=showmodal=mrok;

  tempname:=sorter.tempname;
end;

end.

