unit IconStuff;

{$mode delphi}

interface

uses
  {$ifdef darwin}
  macport, LCLIntf, math,
  {$endif}
  {$ifdef windows}
  windows, CommCtrl,
  {$endif}
  Classes, SysUtils,controls, dialogs, Graphics, forms, ExtCtrls, StdCtrls;

resourcestring
  rsFailureOpen = 'Failure opening ';
  rsUnhandledExt = 'Unhandled file extension';
  rsIconPicker = 'Icon picker';
  rsNoIconFound = 'No icon found in this file';


type TICONDIRENTRY=packed record
  bWidth: byte;
  bHeight: byte;
  bColorCount: byte;
  bReserved: byte;
  wPlanes: word;
  wBitCount: word;
  dwBytesInRes: DWORD;
  dwImageOffset: DWORD;
end;

type TICONDIR=packed record
  idReserved: word; //0
  idType: word; //1=icon
  idCount: word; //nrofentries
  icondirentry: array [0..255] of TICONDIRENTRY;
end;

type TGRPICONDIRENTRY=packed record
  bWidth: byte;
  bHeight: byte;
  bColorCount: byte;
  bReserved: byte;
  wPlanes: word;
  wBitCount: word;
  dwBytesInRes: DWORD;
  id: DWORD;
end;

type TGRPICONDIR=packed record
  idReserved: word; //0
  idType: word; //1=icon
  idCount: word; //nrofentries
  icondirentry: array [0..255] of TGRPICONDIRENTRY;
end;


type PICONDIR=^TICONDIR;
type PGRPICONDIR=^TGRPICONDIR;


function pickIcon: TIcon; //small memleak, I can live with it


implementation

type TIconPicker=class(Tcustomform)
  public
    usedIcon: integer;
    procedure choose(sender: tobject);
end;


procedure TIconPicker.choose(sender: TObject);
begin
  usedicon:=(sender as TImage).tag;
  ModalResult:=mrok;
end;


procedure dealWithIconStream(s: tmemorystream; iconlist: tlist);
var
  icondir: PICONDIR;

  newiconstream: Tmemorystream;
  newicondir: PICONDIR;

  icon: TIcon;
  i: integer;
begin
  icondir:=s.Memory;

  for i:=0 to icondir.idCount-1 do
  begin
    //more than 1 icon

    newiconstream:=tmemorystream.Create;
    newiconstream.Size:=12+sizeof(TICONDIRENTRY);
    newiconstream.Position:=12+sizeof(TICONDIRENTRY);

    newicondir:=newiconstream.memory;
    newicondir.idCount:=1;
    newicondir.idType:=icondir.idType;
    newicondir.idReserved:=icondir.idReserved;
    newicondir.icondirentry[0]:=icondir.icondirentry[i];
    newicondir.icondirentry[0].dwImageOffset:=newiconstream.size;

    s.Position:=icondir.icondirentry[i].dwImageOffset;
    newiconstream.CopyFrom(s, icondir.icondirentry[i].dwBytesInRes);


    newiconstream.position:=0;

    icon:=ticon.create;
    icon.LoadFromStream(newiconstream);

    newiconstream.free;

    iconlist.add(icon);

  end;
end;


function enumfunc(hModule:HANDLE; lpszType:pchar; lpszName:pchar; lParam: PtrInt):WINBOOL;stdcall;
var
  allicons: tmemorystream;
  icon: TIcon;
begin
  icon:=ticon.create;


  if ptruint(lpszName)>$10000 then
    Icon.LoadFromResourceName(hModule,lpszName)
  else
    Icon.LoadFromResourceID(hModule,ptruint(lpszName));

  allicons:=Tmemorystream.Create;
  icon.SaveToStream(allicons);
  icon.free;

  dealWithIconStream(allicons, tlist(lParam));
  allicons.free;


  result:=true;
end;

//type
//  ENUMRESNAMEPROC2 = function (_para1:HANDLE; _para2:LPCTSTR; _para3:LPTSTR; _para4:LONG_PTR):WINBOOL; stdcall;
//function EnumResourceNames(hModule:HINST; lpType:LPCSTR; lpEnumFunc:ENUMRESNAMEPROC2; lParam:LONG_PTR):WINBOOL; external 'kernel32' name 'EnumResourceNamesA';

function pickIcon: TIcon;
var opendialog: Topendialog;
  iconlist: Tlist;
  filename: string;
  ext: string;
  i: integer;
  modulehandle: THandle;
  m: tmemorystream;

  iconpicker: TIconPicker;
  maxheight: integer;
  nextpos: integer;

  p: timage;
begin
  result:=nil;
  iconlist:=tlist.Create;
  opendialog:=TOpenDialog.Create(application);



  if opendialog.execute then
  begin
    filename:=OpenDialog.filename;
    ext:=lowercase(ExtractFileExt(filename));
    {$ifdef windows}
    if (ext='.exe') or (ext='.dll') then
    begin
      modulehandle:=loadlibraryex(pchar(filename), 0, LOAD_LIBRARY_AS_DATAFILE);
      if modulehandle<>0 then
      begin
        EnumResourceNames(modulehandle, RT_GROUP_ICON, enumfunc, PtrInt(iconlist));
        FreeLibrary(modulehandle);
      end
      else
        raise exception.create(rsFailureOpen+filename);
    end
    else
    {$endif}
    if ext='.ico' then
    begin
      m:=tmemorystream.create;
      m.LoadFromFile(filename);
      dealWithIconStream(m, iconlist);
      m.free;
    end
    else
      raise exception.create(rsUnhandledExt);

    if iconlist.count>=1 then
    begin
      maxheight:=10;
      //if iconlist.count>1 then
      begin
        iconpicker:=TIconPicker.CreateNew(nil);
        iconpicker.AutoScroll:=false;

        nextpos:=5;
        for i:=0 to iconlist.count-1 do
        begin
          p:=TImage.Create(iconpicker);
          p.Picture.Icon:=TIcon(iconlist[i]);

          p.width:=p.Picture.Icon.Width;
          p.height:=p.Picture.Icon.Height;

          maxheight:=max(p.height, maxheight);
          p.top:=5;
          p.left:=nextpos;

          nextpos:=p.left+p.width+5;

          p.tag:=i;
          p.Parent:=iconpicker;
          p.OnDblClick:=iconpicker.choose;
        end;






        if iconpicker.clientwidth>nextpos then
          iconpicker.clientwidth:=nextpos+6;

        if nextpos>iconpicker.clientwidth then
          iconpicker.clientwidth:=maxheight*2;

        if nextpos>iconpicker.clientwidth then //still bigger, then scroll
          iconpicker.HorzScrollBar.Range:=nextpos+5;

        iconpicker.HorzScrollBar.Tracking:=true;
        iconpicker.Caption:=rsIconPicker;
        iconpicker.BorderStyle:=bsToolWindow;

        if nextpos>iconpicker.clientwidth then
          iconpicker.clientheight:=maxheight+10+iconpicker.HorzScrollBar.Size
        else
          iconpicker.clientheight:=maxheight+10;

        iconpicker.Position:=poScreenCenter;
        iconpicker.PopupMode:=pmAuto;

        if iconlist.count>1 then
          iconpicker.showmodal;

        result:=ticon(iconlist[iconpicker.usedicon]);
        //small memleak here

        for i:=0 to iconlist.count-1 do
        begin
          if i<>iconpicker.usedicon then
            ticon(iconlist[i]).free;
        end;

        iconpicker.free;
        iconlist.free;

      end;
    end
    else
      raise exception.Create(rsNoIconFound);
  end;
end;

end.

