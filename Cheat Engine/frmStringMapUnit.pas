unit frmStringMapUnit;

{$mode delphi}

{
This unit will create a map that holds the addresses of all the strings in the game
}

interface

uses
  windows, Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics,
  Dialogs, math, ComCtrls, ExtCtrls, StdCtrls, maps, Menus, cefuncproc,
  memfuncs, newkernelhandler, AvgLvlTree, bigmemallochandler, symbolhandler,
  oldRegExpr, commonTypeDefs;

type

  { TfrmStringMap }

  Pstringdata=^TStringdata;
  TStringData=record
    address: ptruint;
    stringsize: integer;
    unicode: boolean;
    previous: PStringData;
    next: PStringdata;
  end;


  TStringScan=class(tthread)
  private
    f: tfilestream;
    regex: TREGExprEngine;
    muststartwithregex: boolean;
    progressbar: TProgressBar;
    stringtree: TAvgLvlTree;
    bma: TBigMemoryAllocHandler;
    procedure AddString(address: ptruint; size: integer; unicode: boolean);

    procedure docleanup;
  public
    procedure execute; override;
    constructor create(suspended: boolean; progressbar: TProgressbar; stringtree: TAvgLvlTree; bma: TBigMemoryAllocHandler; regex: TREGExprEngine; muststartwithregex: boolean; filename: string);
  end;

  TfrmStringMap = class(TForm)
    btnFree: TButton;
    btnScan: TButton;
    btnShowList: TButton;
    cbRegExp: TCheckBox;
    cbCaseSensitive: TCheckBox;
    cbMustBeStart: TCheckBox;
    cbSaveToDisk: TCheckBox;
    edtRegExp: TEdit;
    FindDialog1: TFindDialog;
    smImageList: TImageList;
    lblStringCount: TLabel;
    ListView1: TListView;
    miFind: TMenuItem;
    miNext: TMenuItem;
    Panel1: TPanel;
    Panel2: TPanel;
    pmStringList: TPopupMenu;
    ProgressBar1: TProgressBar;
    SaveDialog1: TSaveDialog;
    procedure btnScanClick(Sender: TObject);
    procedure btnFreeClick(Sender: TObject);
    procedure btnShowListClick(Sender: TObject);
    procedure cbRegExpChange(Sender: TObject);
    procedure FindDialog1Find(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListView1DblClick(Sender: TObject);
    procedure miFindClick(Sender: TObject);
    procedure miNextClick(Sender: TObject);
    procedure Panel1Resize(Sender: TObject);
  private
    { private declarations }

    bma: TBigMemoryAllocHandler;
    isfillinglist: boolean;
    regex: TRegExprEngine;
  public
    { public declarations }
    scanner: TStringScan;
    stringtree: TAvgLvlTree;


    procedure dosearch;
    function treecompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
    procedure cleanup;
    function isString(address: ptruint): boolean;
    function getString(address: ptruint): PStringData;
    function findNearestString(address: ptruint): PStringData;
  end; 

var
  frmStringMap: TfrmStringMap;

implementation

{ TfrmStringMap }

uses MemoryBrowserFormUnit, ProcessHandlerUnit;

resourcestring
  rsStop = 'Stop';
  rsGenerateStringMap = 'Generate string map';
  rsStringcount = 'Stringcount: %s';
  rsBtnShowList = '<<Show list';
  rsNoReadableMemoryFound = 'No readable memory found';
  rsError = 'Error=';
  rsUnhandledTStringScanCrash = 'Unhandled TStringScan crash';
  rsGenerateRegExprEngineFailed = 'GenerateRegExprEngine failed';

procedure TStringscan.docleanup;
begin
  if frmstringmap<>nil then //how the fuck did this thread get started if it is nil ?
  begin
    progressbar.Position:=0;

    with frmstringmap do
    begin
      btnScan.Caption:=rsGenerateStringMap;
      lblStringCount.caption:=Format(rsStringcount, [inttostr(stringtree.Count)]);
      btnFree.enabled:=true;
    end;
  end;




end;

procedure TStringScan.AddString(address: ptruint; size: integer; unicode: boolean);
var e: Pstringdata;
  n: TAvgLvlTreeNode;
  prev, next: TAvgLvlTreeNode;

  s: pchar;
  ws: pwidechar;
  x: dword;
begin
  if size>1024 then exit; //tl;dr
  if symhandler.inSystemModule(address) then exit; //don't add it

  //replace this with saving the results to disk
  e:=bma.alloc(sizeof(TStringData));
  e.address:=address;
  e.stringsize:=size;
  e.unicode:=unicode;

  n:=stringtree.Add(e);
  prev:=stringtree.FindPrecessor(n);
  next:=stringtree.FindSuccessor(n);

  if prev=nil then
    e.previous:=nil
  else
  begin
    e.previous:=prev.Data;
    pstringdata(prev.data).next:=e;
  end;

  if next=nil then
    e.next:=nil
  else
  begin
    e.next:=next.Data;
    pstringdata(next.data).previous:=e;
  end;
end;

procedure TStringScan.execute;
var buf: PByteArray;
  maxbuf: uint64;
  address: ptruint;

  total: ptruint;
  totalhandled: ptruint;

  mr: TMemoryRegions;
  i,j: integer;

  s: integer;
  currentbufsize: ptruint;
  currentpos: ptruint; //position in the current memory region
  x: ptruint;

  unicode: boolean;
  start: integer; //index where the first valid character is

  str: string;
  index, len: integer;

  savetofile: boolean;
begin
  totalhandled:=0;


  try
    //get memory regions
    buf:=nil;
    try
      setlength(mr,0);
      total:=getallmemoryregions(mr);
      if total>0 then
      begin
        maxbuf:=0; //find the max size
        for i:=0 to length(mr)-1 do
{$if FPC_FULLVERSION<30200}
          maxbuf:=max(int64(mr[i].MemorySize), maxbuf);
{$else}
          maxbuf:=max(mr[i].MemorySize, maxbuf);
{$endif}

        if maxbuf=0 then
          raise exception.create(rsNoReadableMemoryFound);

{$if FPC_FULLVERSION<30200}
        maxbuf:=min(maxbuf, 512*1024);
{$else}
        maxbuf:=min(maxbuf, qword(512*1024));
{$endif}

        getmem(buf, maxbuf);

      end
      else
        raise exception.create(rsNoReadableMemoryFound);


      for i:=0 to length(mr)-1 do
      begin
        if terminated then break;

        currentpos:=0;
        while (not terminated) and (currentpos<mr[i].MemorySize) do
        begin
          unicode:=false;
          s:=mr[i].MemorySize;
          currentbufsize:=4096; //min(s-currentpos, maxbuf);
          if ReadProcessMemory(processhandle, pointer(mr[i].BaseAddress+currentpos) , buf, currentbufsize,x) then
          begin
            //find and add the strings
            currentbufsize:=x;

            start:=-1;
            for j:=0 to currentbufsize-2 do
            begin



              if (buf[j] in [$20..$7f]) or (unicode and (buf[j]=0) and ((j<>0) and (buf[j-1]<>0))) then
              begin

                if start=-1 then
                begin
                  start:=j;
                  if buf[j+1]=0 then
                    unicode:=true;
                end;
              end
              else
              begin
                if start<>-1 then
                begin
                  //still here, so the previous character was 0 or the current char is invalid
                  if ((not unicode) and (j-start>4)) or (unicode and (j-start>9)) then
                  begin
                    //found something that resembles a string

                    if regex<>nil then
                    begin
                      buf[j]:=0;

                      if unicode then
                      begin
                        buf[j-1]:=0;
                        str:=PWideChar(@buf[start])
                      end
                      else
                        str:=PChar(@buf[start]);

                      index:=0;
                      len:=0;
                      if RegExprPos(regex, pchar(str) , index,len) then
                      begin
                        if (not muststartwithregex) or (muststartwithregex and (index=0)) then
                        begin
                          AddString(mr[i].BaseAddress+currentpos+start,j-start, unicode);

                          if f<>nil then
                          begin
                            str:=inttohex(mr[i].BaseAddress+currentpos+start,8)+' - '+str+#13#10;
                            f.WriteBuffer(str[1], length(str));
                          end;

                        end
                      end
                    end
                    else
                    begin
                      AddString(mr[i].BaseAddress+currentpos+start,j-start, unicode);
                      if f<>nil then
                      begin

                        if unicode then
                        begin
                          buf[j]:=0;
                          buf[j-1]:=0;

                          str:=PWideChar(@buf[start])
                        end
                        else
                        begin
                          buf[j]:=0;
                          str:=PChar(@buf[start]);
                        end;

                        str:=inttohex(mr[i].BaseAddress+currentpos+start,8)+' - '+str+#13#10;
                        f.WriteBuffer(str[1], length(str));
                      end;
                    end;

                  end;
                end;

                start:=-1;
                unicode:=false;
              end;
            end;

          end
          else
          begin
            currentbufsize:=4096;
          end;

          inc(currentpos, currentbufsize);
          inc(totalhandled, currentbufsize);
          progressbar.Position:=trunc((totalhandled / total) * 100);
        end;
      end;
    finally
      if buf<>nil then
        FreeMemAndNil(buf);

      if f<>nil then
        freeandnil(f);

      synchronize(docleanup);
    end;
  except
    on e: exception do
      MessageBox(0, pchar(ansitoutf8(rsError+e.message)),pchar(rsUnhandledTStringScanCrash), MB_OK or MB_ICONERROR);
  end;
end;

constructor TStringScan.create(suspended: boolean; progressbar: TProgressbar; stringtree: TAvgLvlTree; bma: TBigMemoryAllocHandler; regex: TRegExprEngine; muststartwithregex: boolean; filename: string);
begin
  self.stringtree:=stringtree;
  self.progressbar:=progressbar;
  self.bma:=bma;
  self.regex:=regex;
  self.muststartwithregex:=muststartwithregex;
  if filename<>'' then
    f:=tfilestream.create(filename, fmCreate);

  progressbar.Position:=0;
  progressbar.max:=100;



  inherited create(suspended);
end;

function TfrmStringMap.treecompare(Tree: TAvgLvlTree; Data1, Data2: Pointer): integer;
begin
  //used to sort the binary tree
  result:=CompareValue(pstringdata(Data1).address, pstringdata(Data2).address);
end;

//------------------------------------------------------------

procedure TfrmStringMap.cleanup;
var n:  TAvgLvlTreeNode;
  i: integer;
  t,p: Pstringdata;
begin
  isfillinglist:=false;

  listview1.BeginUpdate;
  listview1.Clear;
  listview1.EndUpdate;

  if scanner<>nil then
  begin
    scanner.Terminate;
    scanner.WaitFor;
    scanner.free;
    scanner:=nil;
  end;

  if stringtree<>nil then
  begin
    stringtree.clear;
    stringtree.free;
    stringtree:=nil;
  end;

  if bma<>nil then
  begin
    bma.free;
    bma:=nil;
  end;

  if regex<>nil then
  begin
    regex.free;
    regex:=nil;
  end;

  lblStringCount.caption:=Format(rsStringcount, ['0']);
end;



procedure TfrmStringMap.btnScanClick(Sender: TObject);
var mapIdType: TMapIdType;
    regflags: tregexprflags;
    filename: string;
begin

  isfillinglist:=false;

  if btnScan.caption=rsStop then
  begin
    Cleanup;
    caption:=rsGenerateStringMap;
  end
  else
  begin
    Cleanup;

    if ProcessHandler.is64Bit then
      mapIdType:=itu8 //unsigned 8 bytes
    else
      mapIdType:=itu4;

    stringtree:=TAvgLvlTree.CreateObjectCompare(treecompare);

   { treememorymanager:=TAvgLvlTreeNodeMemManager.Create;
    treememorymanager.MinimumFreeNode:=102400;
    treememorymanager.MaximumFreeNodeRatio:=16;


    stringtree.NodeMemManager:=treememorymanager;}



    bma:=TBigMemoryAllocHandler.create;

    if cbRegExp.checked then
    begin
      if cbCaseSensitive.checked then
        regflags:=[]
      else
        regflags:=[ref_caseinsensitive];

      regex:=GenerateRegExprEngine(pchar(edtRegExp.Text), regflags);

      if regex=nil then
        raise exception.create(rsGenerateRegExprEngineFailed);
    end
    else
      regex:=nil;

    btnScan.caption:=rsStop;

    if cbSaveToDisk.checked and SaveDialog1.execute then
      filename:=savedialog1.filename
    else
      filename:='';

    scanner:=TStringScan.create(false, progressbar1, stringtree, bma, regex, cbMustBeStart.checked, filename);
  end;

end;

procedure TfrmStringMap.btnFreeClick(Sender: TObject);
begin
  cleanup;
end;

procedure TfrmStringMap.btnShowListClick(Sender: TObject);
var n: TAvgLvlTreeNode;
  p: Pstringdata;

  buf: pansichar;
  wbuf: pwidechar absolute buf;
  x: ptruint;
  li: tlistitem;

  i: integer;

  s: string;
begin
  if not isfillinglist then
  begin
    listview1.clear;

    if stringtree<>nil then
    begin
      if stringtree.Count>0 then
      begin
        n:=stringtree.FindLowest;
        if n<>nil then
        begin
          isfillinglist:=true;
          btnShowList.caption:=rsStop;

          getmem(buf,512);

          try
            p:=n.Data;
            i:=0;

            while p<>nil do
            begin
              inc(i);

              if ReadProcessMemory(processhandle, pointer(p.address), buf, min(509, p.stringsize), x) then
              begin
                li:=listview1.items.add;

                li.caption:=inttohex(p.address,8);
                li.data:=pointer(p.address);

                buf[min(510, p.stringsize)]:=#0;
                buf[min(510, p.stringsize)+1]:=#0;


                if p.unicode then
                  s:=wbuf
                else
                  s:=ansitoutf8(buf);

                li.SubItems.Add(s);

              end;

              if i mod 25 = 0 then application.ProcessMessages;
              if not isfillinglist then break;

              p:=p.next;
            end;

          finally
            FreeMemAndNil(buf);
            isfillinglist:=false;
            btnShowList.caption:=rsBtnShowList;
          end;
        end;
      end;
    end;
  end
  else
  begin
    isfillinglist:=false;
    btnShowList.caption:=rsBtnShowList;
  end;
end;

procedure TfrmStringMap.cbRegExpChange(Sender: TObject);
begin
  cbCaseSensitive.enabled:=cbRegExp.checked;
  cbMustBeStart.enabled:=cbRegExp.checked;
  edtRegExp.enabled:=cbRegExp.checked;
end;



procedure TfrmStringMap.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  isfillinglist:=false;
end;

procedure TfrmStringMap.FormDestroy(Sender: TObject);
begin
  cleanup;
end;

procedure TfrmStringMap.FormResize(Sender: TObject);
begin
  listview1.Column[listview1.ColumnCount-1].Width:=listview1.ClientWidth-listview1.Column[0].Width-3;
end;

procedure TfrmStringMap.ListView1DblClick(Sender: TObject);
begin
  if listview1.Selected<>nil then
    memorybrowser.hexview.address:=ptruint(listview1.Selected.Data);
end;

procedure TfrmStringMap.dosearch;
var
  i: integer;
  lookfor, s: string;
  casesensitive: boolean;
begin
  lookfor:=finddialog1.FindText;
  casesensitive:=frMatchCase in finddialog1.Options;
  if not casesensitive then
    lookfor:=LowerCase(lookfor);

  for i:=listview1.ItemIndex+1 to listview1.Items.Count-1 do
  begin
    s:=listview1.Items[i].SubItems[0];
    if not casesensitive then s:=lowercase(s);
    if pos(lookfor,s)>0 then
    begin
      listview1.Items[i].MakeVisible(false);
      listview1.Selected:=listview1.Items[i];
      listview1.ItemIndex:=i;
      break;
    end;
  end;
end;

procedure TfrmStringMap.FindDialog1Find(Sender: TObject);
begin
  if finddialog1.FindText<>'' then
  begin
    dosearch;
    miNext.Enabled:=true;
  end;

  finddialog1.CloseDialog;
end;


procedure TfrmStringMap.miFindClick(Sender: TObject);
begin
  FindDialog1.Execute;
end;

procedure TfrmStringMap.miNextClick(Sender: TObject);
begin
  if finddialog1.FindText<>'' then
    dosearch;
end;



procedure TfrmStringMap.Panel1Resize(Sender: TObject);
begin
  btnShowList.Top:=(panel1.clientheight) - (btnShowList.height)-3;
end;


function TfrmStringMap.findNearestString(address: ptruint): PStringData;
var
  k: TStringData;
  p: PStringData;
  n: TAvgLvlTreeNode;
begin
  result:=nil;
  if stringtree<>nil then
  begin
    k.address:=address;
    n:=stringtree.FindNearest(@k);

    if n<>nil then
    begin
      p:=PStringData(n.Data);
      while (p<>nil) and (p.address>k.address) do
        p:=p.previous;

      if (p<>nil) and (p.address+p.stringsize<address) then
        p:=p.next;

      result:=p;
    end;

  end;

end;

function TfrmStringMap.getString(address: ptruint): PStringData;
var p: PStringData;
begin
  result:=nil;
  p:=findNearestString(address);

  //it is a fact that p.address <= address
  if p<>nil then
  begin
    if p.address+p.stringsize>address then //if the address falls inside the range of the string
      result:=p;
  end;

end;

function TfrmStringMap.isString(address: ptruint): boolean;
begin
  result:=getString(address)<>nil;
end;


initialization
  {$I frmStringMapUnit.lrs}

end.
