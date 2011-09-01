unit d3dhookUnit;
{
This unit will inject the d3d hook dll into the target process and hook the
apropriate functions.
A shared object will be used for communicating states and data
}

{$mode delphi}

interface

uses
  windows, Classes, SysUtils, sharedMemory, forms, graphics, cefuncproc,
  newkernelhandler, controls, Clipbrd;

type
  TResourceInfo=packed record
    valid: integer;
    updatedpos: integer;
    updatedresource: integer;
    width: integer;
    height: integer;
    x: integer;
    y: integer;
    resourcesize: integer;
    resourceoffset: integer; //offset into the shared memory region containing the bitmat info
  end;

  type TResourceInfoArray=array [0..0] of TResourceInfo;

  TD3DHookShared=packed record
    cheatenginedir: array [0..199] of char;
    dxgi_present: UINT64;
    d3d9_present: UINT64;

    dxgi_newpresent: UINT64;
    d3d9_newpresent: UINT64;

    dxgi_originalpresent: UINT64;
    d3d9_originalpresent: UINT64;



    OverLayHasUpdate: integer; //When set to not 0 the renderer will check what needs to be updated
    overlaycount: integer;
    resources: TResourceInfoArray;
    //followed by the resource data


  end;
  PD3DHookShared=^TD3DHookShared;


type TD3DHook=class
  private
    sharename: string;
    shared: PD3DHookShared; //local address of the D3DHookShared structure

    fmhandle: THandle;

    images: array of TPicture;

    isupdating: integer;

    maxsize: integer;
    fprocessid: dword;
    procedure waitforready;
    procedure UpdateResourceData;
  public
    procedure beginupdate;
    procedure endupdate;

    function createOverlayFromPicture(p: TPicture; x,y: integer): integer;
    procedure SetOverlayVisibility(overlayid: integer; state: boolean);
    procedure updateOverlayImage(overlayid: integer);
    procedure updateOverlayPosition(overlayid,x,y: integer);

    constructor create(size: integer);
    destructor destroy; override;
    property processid: dword read fprocessid;
  end;

var D3DHook: TD3DHook;

function safed3dhook(size: integer=16*1024*1024): TD3DHook;

implementation

uses frmautoinjectunit, autoassembler;

procedure TD3DHook.beginupdate;
var i: integer;
begin
  waitforready;
  inc(isupdating);
end;

procedure TD3DHook.endupdate;
begin
  if isupdating>0 then
  begin
    dec(isupdating);
    if isupdating=0 then
    begin
      updateResourceData;
      shared.OverLayHasUpdate:=1;
    end;
  end;
end;

procedure TD3DHook.updateOverlayImage(overlayid: integer);
begin
  beginupdate;
  shared.resources[overlayid-1].updatedresource:=1;
  endupdate;
end;

procedure TD3DHook.updateOverlayPosition(overlayid,x,y: integer);
begin
  beginupdate;
  shared.resources[overlayid-1].x:=x;
  shared.resources[overlayid-1].y:=y;
  shared.resources[overlayid-1].updatedpos:=1;
  endupdate;
end;

procedure TD3DHook.SetOverlayVisibility(overlayid: integer; state: boolean);
begin
  images[overlayid-1]:=nil;
  if state then
    shared.resources[overlayid-1].valid:=1
  else
    shared.resources[overlayid-1].valid:=0;

  if isupdating=0 then
    shared.OverLayHasUpdate:=1;
end;

procedure TD3DHook.UpdateResourceData;
//Fill in the resources and the offsets pointing to them
var
  i,j: integer;
  s: TMemoryStream;
  start: PByteArray;
begin
  //now update all the entries
  s:=tmemorystream.Create;
  start:=@shared.resources[shared.overlaycount];

  try
    for i:=0 to shared.overlaycount-1 do
    begin
      if (shared.resources[i].valid<>0) then
      begin
        s.Clear;
        images[i].SaveToStream(s);

        shared.resources[i].resourceoffset:=PtrUInt(start)-ptruint(shared);

        if shared.resources[i].resourceoffset+s.Size>maxsize then
        begin
          //out of memory, set this and all following overlays to unusable (stays valid so if a previous overlays is destroyed one might become available again)
          for j:=i to shared.overlaycount-1 do
            shared.resources[j].resourceoffset:=0;  //mark as invalid

          exit;
        end;

        //copy the resource to the target process
        CopyMemory(start, s.Memory, s.Size);


        shared.resources[i].resourcesize:=s.Size;
        inc(start, s.size);
      end;
    end;

  finally
    s.free;
  end;
end;

procedure TD3DHook.waitforready;
var i: integer;
//wait till the overlayhasupdate variable is set to 0. Timeout of 2 second  (0.5 fps games suck)
begin
  if isupdating=0 then
  begin
    i:=0;
    while (i<200) and (shared.OverLayHasUpdate<>0) do    //max 2 second
    begin
      sleep(10); //wait till the last update has been handled
      inc(i);
    end;
    shared.OverLayHasUpdate:=0; //screw it
  end;
end;

function TD3DHook.createOverlayFromPicture(p: TPicture; x,y: integer): integer;
var
  i: integer;
begin
  setlength(images, length(images)+1);
  result:=length(images);

  images[result-1]:=p;

  beginupdate;


  shared.overlaycount:=result;


  shared.resources[result-1].height:=p.Height;
  shared.resources[result-1].width:=p.width;
  shared.resources[result-1].x:=x;
  shared.resources[result-1].y:=y;

  shared.resources[result-1].updatedpos:=1;
  shared.resources[result-1].updatedresource:=1;
  shared.resources[result-1].resourceoffset:=0; //this will be updated when endupdate is called which calls updateresourcedata when ready
  shared.resources[result-1].resourcesize:=0;
  shared.resources[result-1].valid:=1;



  endupdate;

end;

destructor TD3DHook.Destroy;
begin
  UnmapViewOfFile(shared);
  closehandle(fmhandle);
  inherited destroy;
end;

constructor TD3DHook.create(size: integer);
var h: thandle;
    s: TStringList;
begin
  sharename:='CED3D_'+inttostr(processhandler.ProcessID);

  fprocessid:=processid;
  maxsize:=size;

  createSharedMemory(sharename, sizeof(TD3DHookShared)+maxsize);

  fmhandle:=OpenFileMapping(FILE_MAP_EXECUTE or FILE_MAP_READ or FILE_MAP_WRITE, false, pchar(sharename));
  if fmhandle=0 then
    raise exception.create('D3DHook: Failure to open the shared memory object');

  shared:=MapViewOfFile(fmhandle,FILE_MAP_EXECUTE or FILE_MAP_READ or FILE_MAP_WRITE, 0,0,0 );

  if shared=nil then
    raise exception.create('D3DHook: Failure to map the shared memory object');

  ZeroMemory(shared, sizeof(TD3DHookShared));

  shared.cheatenginedir:=CheatEngineDir;

  shared.OverLayHasUpdate:=0;

  h:=CreateEventA(nil, true, false, pchar(sharename+'_READY') );

  if (h<>0) then
  begin
    //now inject the dll
    injectdll(cheatenginedir+'d3dhook.dll');

    //wait till the injection is done
    WaitForSingleObject(h, INFINITE);
    closehandle(h);

    //and hook the functions
    //(script: tstrings; address: string; addresstogoto: string; addresstostoreneworiginalfunction: string=''; nameextension:string='0');
    s:=tstringlist.create;
    try
      if shared.d3d9_present<>0 then
        generateAPIHookScript(s, inttohex(shared.d3d9_present,8), inttohex(shared.d3d9_newpresent,8),  inttohex(shared.d3d9_originalpresent,8), '0');

      if shared.dxgi_present<>0 then
        generateAPIHookScript(s, inttohex(shared.dxgi_present,8), inttohex(shared.dxgi_newpresent,8),  inttohex(shared.dxgi_originalpresent,8), '1');

      //if there is a script execute it.
      if (s.count>0) and (autoassemble(s,false)=false) then
      begin
        //on error write the script to the clipboard
        clipboard.AsText:=s.text; //debug
      end;
    finally
      s.free;
    end;


  end;


end;

function safed3dhook(size: integer=16*1024*1024): TD3DHook;
//Calls the d3dhook constructor but captures exceptions
begin
  if d3dhook=nil then
  begin
    try
      d3dhook:=TD3DHook.Create(size);
    except
      d3dhook:=nil;
    end;
  end
  else
  begin
    if d3dhook.processid<>processid then //different process
    begin
      d3dhook.Free;
      try
        d3dhook:=TD3DHook.Create(size);
      except
        d3dhook:=nil;
      end;
    end;

  end;

  result:=d3dhook;
end;

end.

