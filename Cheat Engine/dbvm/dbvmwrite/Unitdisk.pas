unit Unitdisk;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    cbDeviceList: TComboBox;
    Label1: TLabel;
    GroupBox1: TGroupBox;
    lblMediaType: TLabel;
    lblCylinders: TLabel;
    lblTracks: TLabel;
    lblSectors: TLabel;
    lblSectorSize: TLabel;
    lblTotalSize: TLabel;
    GroupBox2: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure cbDeviceListDropDown(Sender: TObject);
    procedure cbDeviceListSelect(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure CheckBox2Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    vmm: TFilestream;
    procedure reloadDriveList;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

const
  //DeviceType
  File_Device_BEEP = $00000001;
  File_Device_CD_ROM = $00000002;
  File_Device_CD_ROM_File_System = $00000003;
  File_Device_CONTROLLER = $00000004;
  File_Device_DataLINK = $00000005;
  File_Device_DFS = $00000006;
  File_Device_Disk = $00000007;
  File_Device_Disk_File_System = $00000008;
  File_Device_File_System = $00000009;
  File_Device_INPORT_PORT = $0000000A;
  File_Device_KEYBOARD = $0000000B;
  File_Device_MAILSLOT = $0000000C;
  File_Device_MIDI_IN = $0000000D;
  File_Device_MIDI_OUT = $0000000E;
  File_Device_MOUSE = $0000000F;
  File_Device_MULTI_UNC_PROVIDER = $00000010;
  File_Device_NAMED_PIPE = $00000011;
  File_Device_NETWORK = $00000012;
  File_Device_NETWORK_BROWSER = $00000013;
  File_Device_NETWORK_File_System = $00000014;
  File_Device_NULL = $00000015;
  File_Device_PARALLEL_PORT = $00000016;
  File_Device_PHYSICAL_NETCARD = $00000017;
  File_Device_PRINTER = $00000018;
  File_Device_SCANNER = $00000019;
  File_Device_SERIAL_MOUSE_PORT = $0000001A;
  File_Device_SERIAL_PORT = $0000001B;
  File_Device_SCREEN = $0000001C;
  File_Device_SOUND = $0000001D;
  File_Device_STREAMS = $0000001E;
  File_Device_TAPE = $0000001F;
  File_Device_TAPE_File_System = $00000020;
  File_Device_TRANSPORT = $00000021;
  File_Device_UNKNOWN = $00000022;
  File_Device_VIDEO = $00000023;
  File_Device_VIRTUAL_Disk = $00000024;
  File_Device_WAVE_IN = $00000025;
  File_Device_WAVE_OUT = $00000026;
  File_Device_8042_PORT = $00000027;
  File_Device_NETWORK_REDIRECTOR = $00000028;
  File_Device_BATTERY = $00000029;
  File_Device_BUS_EXTENDER = $0000002A;
  File_Device_MODEM = $0000002B;
  File_Device_VDM = $0000002C;
  File_Device_Mass_Storage = $0000002D;

  //Method
  Method_Buffered = 0;
  Method_IN_Direct = 1;
  Method_OUT_Direct = 2;
  Method_Neither = 3;

  //Access
  File_Any_Access = 0;
  File_Read_Access = ($0001); // file & pipe
  File_Write_Access = ($0002); // file & pipe
  //FSCTL Access
  File_Read_Data = File_Read_Access;
  File_Write_Data = File_Write_Access;

  //Storage
  IOCTL_Storage_Base = File_Device_MASS_Storage; // $002D
  IOCTL_Disk_Base = File_Device_Disk; // $0007

  //Partition
  Partition_ENTRY_UNUSED = $00; // Entry unused
  Partition_FAT_12 = $01; // 12-bit FAT entries
  Partition_XENIX_1 = $02; // Xenix
  Partition_XENIX_2 = $03; // Xenix
  Partition_FAT_16 = $04; // 16-bit FAT entries
  Partition_EXTENDED = $05; // Extended Partition entry
  Partition_HUGE = $06; // Huge Partition MS-DOS V4
  Partition_IFS = $07; // IFS Partition
  Partition_FAT32 = $0B; // FAT32
  Partition_FAT32_XINT13 = $0C; // FAT32 using extended int13 services
  Partition_XINT13 = $0E; // Win95 Partition using extended int13 services
  Partition_XINT13_EXTENDED = $0F; // Same as type 5 but uses extended int13 services
  Partition_PREP = $41; // PowerPC Reference Platform (PReP) Boot Partition
  Partition_UNIX = $63; // Unix

  VALID_NTFT = $C0; // NTFT uses high order bits

var
  //IOCTL/FSCTL/SMART Code
  IOCTL_Storage_Check_Verify: integer;
  IOCTL_Storage_Media_REMOVAL: integer;
  IOCTL_Storage_Eject_Media: integer;
  IOCTL_Storage_Load_Media: integer;
  IOCTL_Storage_RESERVE: integer;
  IOCTL_Storage_RELEASE: integer;
  IOCTL_Storage_Find_New_Devices: integer;
  IOCTL_Storage_Get_Media_Types: integer;

  IOCTL_Disk_Get_Drive_Geometry: integer;
  IOCTL_Disk_Get_Partition_Info: integer;
  IOCTL_Disk_Set_Partition_Info: integer;
  IOCTL_Disk_Get_Drive_Layout: integer;
  IOCTL_Disk_Set_Drive_Layout: integer;
  IOCTL_Disk_Verify: integer;
  IOCTL_Disk_Format_Tracks: integer;
  IOCTL_Disk_Reassign_Blocks: integer;
  IOCTL_Disk_Performance: integer;
  IOCTL_Disk_is_Writable: integer;
  IOCTL_Disk_Logging: integer;
  IOCTL_Disk_Format_Tracks_EX: integer;
  IOCTL_Disk_Histogram_Structure: integer;
  IOCTL_Disk_Histogram_Data: integer;
  IOCTL_Disk_Histogram_Reset: integer;
  IOCTL_Disk_Request_Structure: integer;
  IOCTL_Disk_Request_Data: integer;

  FSCTL_Lock_Volume: integer;
  FSCTL_Unlock_Volume: integer;
  FSCTL_Dismount_Volume: integer;
  FSCTL_Mount_DBLS_Volume: integer;
  FSCTL_Get_Compression: integer;
  FSCTL_Set_Compression: integer;
  FSCTL_Read_Compression: integer;
  FSCTL_Write_Compression: integer;

  SMART_Get_Version: integer;
  SMART_SEND_Drive_Command: integer;
  SMART_RCV_Drive_Data: integer;
  

implementation

{$R *.dfm}


function Ctl_Code(DeviceType, FuncNo, Method, Access: integer): integer;
begin
  Result := (DeviceType shl 16) or (Access shl 14) or (FuncNo shl 2) or (Method)
end;

procedure Init;
begin
  IOCTL_Storage_Check_Verify := Ctl_Code(IOCTL_Storage_Base, $0200, Method_Buffered, File_Read_Access);
  IOCTL_Storage_Media_REMOVAL := Ctl_Code(IOCTL_Storage_Base, $0201, Method_Buffered, File_Read_Access);
  IOCTL_Storage_Eject_Media := Ctl_Code(IOCTL_Storage_Base, $0202, Method_Buffered, File_Read_Access);
  IOCTL_Storage_Load_Media := Ctl_Code(IOCTL_Storage_Base, $0203, Method_Buffered, File_Read_Access);
  IOCTL_Storage_RESERVE := Ctl_Code(IOCTL_Storage_Base, $0204, Method_Buffered, File_Read_Access);
  IOCTL_Storage_RELEASE := Ctl_Code(IOCTL_Storage_Base, $0205, Method_Buffered, File_Read_Access);
  IOCTL_Storage_Find_New_Devices := Ctl_Code(IOCTL_Storage_Base, $0206, Method_Buffered, File_Read_Access);
  IOCTL_Storage_Get_Media_Types := Ctl_Code(IOCTL_Storage_Base, $0300, Method_Buffered, File_Any_Access);

  IOCTL_Disk_Get_Drive_Geometry := Ctl_Code(IOCTL_Disk_Base, $0000, Method_Buffered, File_Any_Access);
  IOCTL_Disk_Get_Partition_Info := Ctl_Code(IOCTL_Disk_Base, $0001, Method_Buffered, File_Read_Access);
  IOCTL_Disk_Set_Partition_Info := Ctl_Code(IOCTL_Disk_Base, $0002, Method_Buffered, File_Read_Access or File_Write_Access);
  IOCTL_Disk_Get_Drive_Layout := Ctl_Code(IOCTL_Disk_Base, $0003, Method_Buffered, File_Read_Access);
  IOCTL_Disk_Set_Drive_Layout := Ctl_Code(IOCTL_Disk_Base, $0004, Method_Buffered, File_Read_Access or File_Write_Access);
  IOCTL_Disk_Verify := Ctl_Code(IOCTL_Disk_Base, $0005, Method_Buffered, File_Any_Access);
  IOCTL_Disk_Format_Tracks := Ctl_Code(IOCTL_Disk_Base, $0006, Method_Buffered, File_Read_Access or File_Write_Access);
  IOCTL_Disk_Reassign_Blocks := Ctl_Code(IOCTL_Disk_Base, $0007, Method_Buffered, File_Read_Access or File_Write_Access);
  IOCTL_Disk_Performance := Ctl_Code(IOCTL_Disk_Base, $0008, Method_Buffered, File_Any_Access);
  IOCTL_Disk_is_Writable := Ctl_Code(IOCTL_Disk_Base, $0009, Method_Buffered, File_Any_Access);
  IOCTL_Disk_Logging := Ctl_Code(IOCTL_Disk_Base, $000A, Method_Buffered, File_Any_Access);
  IOCTL_Disk_Format_Tracks_EX := Ctl_Code(IOCTL_Disk_Base, $000B, Method_Buffered, File_Read_Access or File_Write_Access);
  IOCTL_Disk_Histogram_Structure := Ctl_Code(IOCTL_Disk_Base, $000C, Method_Buffered, File_Any_Access);
  IOCTL_Disk_Histogram_Data := Ctl_Code(IOCTL_Disk_Base, $000D, Method_Buffered, File_Any_Access);
  IOCTL_Disk_Histogram_Reset := Ctl_Code(IOCTL_Disk_Base, $000E, Method_Buffered, File_Any_Access);
  IOCTL_Disk_Request_Structure := Ctl_Code(IOCTL_Disk_Base, $000F, Method_Buffered, File_Any_Access);
  IOCTL_Disk_Request_Data := Ctl_Code(IOCTL_Disk_Base, $0010, Method_Buffered, File_Any_Access);

  FSCTL_Lock_Volume := Ctl_Code(File_Device_File_System, 6, Method_Buffered, File_Any_Access);
  FSCTL_Unlock_Volume := Ctl_Code(File_Device_File_System, 7, Method_Buffered, File_Any_Access);
  FSCTL_Dismount_Volume := Ctl_Code(File_Device_File_System, 8, Method_Buffered, File_Any_Access);
  FSCTL_Mount_DBLS_Volume := Ctl_Code(File_Device_File_System, 13, Method_Buffered, File_Any_Access);
  FSCTL_Get_Compression := Ctl_Code(File_Device_File_System, 15, Method_Buffered, File_Any_Access);
  FSCTL_Set_Compression := Ctl_Code(File_Device_File_System, 16, Method_Buffered, File_Read_Data or File_Write_Data);
  FSCTL_Read_Compression := Ctl_Code(File_Device_File_System, 17, Method_Neither, File_Read_Data);
  FSCTL_Write_Compression := Ctl_Code(File_Device_File_System, 18, Method_Neither, File_Write_Data);

  SMART_Get_Version := CTL_CODE(IOCTL_Disk_Base, $0020, Method_Buffered, File_Read_Access);
  SMART_SEND_Drive_Command := CTL_CODE(IOCTL_Disk_Base, $0021, Method_Buffered, File_Read_Access or File_Write_Access);
  SMART_RCV_Drive_Data := CTL_CODE(IOCTL_Disk_Base, $0022, Method_Buffered, File_Read_Access or File_Write_Access);
end;

procedure TForm1.reloadDriveList;
var target,target2: pchar;
    p: pchar;
    i,c,c2: integer;
    buffersize: dword;
    oldselect: string;
    osi: integer;
begin
  oldselect:=cbDeviceList.Text;
  cbDeviceList.Clear;
  buffersize:=4096;
  getmem(target,buffersize);
  while (buffersize<1024*1024) do
  begin
    c:=QueryDosDevice(nil,target,buffersize);
    if c=0 then
    begin
      i:=getlasterror;
      if i=122 then
      begin
        freemem(target);
        buffersize:=buffersize*2;
        getmem(target,buffersize);
      end;
    end else break; //enough
  end;

  if buffersize>=1024*1024 then exit;

  i:=0;
  p:=target;
  getmem(target2,buffersize);
  while i<c do
  begin
     if ((p[1]=':') and (p[2]=#0)) or ((p[0]='P') and (p[1]='h')) then
      cbDeviceList.Items.Add(p);

    inc(i,StrLen(p)+1);
    p:=@target[i];
  end;
  freemem(target);
  freemem(target2);

  osi:=cbDeviceList.Items.IndexOf(oldselect);
  cbDeviceList.ItemIndex:=osi;
end;

procedure TForm1.Button1Click(Sender: TObject);
var x: TFilestream;
    bla: array of byte;
    temp: pchar;
begin
  getmem(temp,6);
  x:=TFileStream.Create(pchar('\\.\'+cbDeviceList.text),fmOpenReadWrite or fmShareDenyNone );
  try
    setlength(bla,vmm.size+512-(vmm.size mod 512));
    ZeroMemory(@bla[0],length(bla));

    x.Seek(0,soFromBeginning);
    x.Read(bla[0],512);

    //check if CETC is already writen. If not, query if he's sure
    CopyMemory(temp,@bla[3],5);
    temp[5]:=#0;
    if temp<>'CETC2' then
    begin
      //ask if sure
      if MessageDlg('This disk doesn''t currently contain a version of DBVM. Are you sure you want to put DBVM on this disk?', mtConfirmation, [mbyes,mbno],0)<>mryes then
        exit;
    end;

    //if so, write:
    ZeroMemory(@bla[0],512);
    vmm.Seek(0,soFromBeginning);
    vmm.ReadBuffer(bla[0],vmm.size);

    //setup config values
    if checkbox1.Checked then
      bla[$11]:=0
    else
      bla[$11]:=1;

    if checkbox2.checked then
      bla[$10]:=1
    else
      bla[$10]:=0;

    bla[$f]:=strtoint('$'+edit1.text);

    x.Write(bla[0],length(bla));
  finally
    x.free;
    freemem(temp);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  reloaddrivelist;
  try
    vmm:=TFilestream.Create('vmdisk.img', fmopenread or fmShareDenyNone);
  except
    if opendialog1.Execute then
    begin
      vmm:=TFilestream.Create(opendialog1.filename, fmopenread or fmShareDenyNone);
    end;
  end;
  
end;

procedure TForm1.cbDeviceListDropDown(Sender: TObject);
begin
  reloadDriveList;
end;

type DISK_GEOMETRY=record
   Cylinders: LARGE_INTEGER;
   MediaType: integer;
   TracksPerCylinder: DWORD;
   SectorsPerTrack: DWORD;
   BytesPerSector: DWORD;
end;


procedure TForm1.cbDeviceListSelect(Sender: TObject);
var x: TFilestream;
    diskgeo: DISK_GEOMETRY;
    actualreturned: dword;
    totalsize: int64;
    totalsizestr: string;
begin
  //get diskinfo
  groupbox1.Caption:=cbDeviceList.Text;
  try
    x:=TFileStream.Create(pchar('\\.\'+cbDeviceList.text),fmOpenRead or fmShareDenyNone );
    try

      if DeviceIoControl(x.Handle, IOCTL_DISK_GET_DRIVE_GEOMETRY, nil,0, @diskgeo, sizeof(DISK_GEOMETRY), actualreturned, nil) then
      begin
        lblMediaType.caption:='MediaType:'+inttostr(diskgeo.MediaType);
        if (diskgeo.MediaType>=12) then
        begin
          lblMediaType.font.Color:=clred;
          button1.Enabled:=false;
        end
        else
        begin
          button1.Enabled:=true;
          lblMediaType.Font.color:=clWindowText;
        end;

        lblCylinders.caption:='Cylinders:'+inttostr(diskgeo.Cylinders.QuadPart);
        lblTracks.caption:='Tracks/Cylinder:'+inttostr(diskgeo.TracksPerCylinder);
        lblSectors.caption:='Sectors/Track:'+inttostr(diskgeo.SectorsPerTrack);
        lblSectorSize.caption:='Sector Size:'+inttostr(diskgeo.BytesPerSector);
        if (diskgeo.BytesPerSector<>512) then
          lblSectorSize.Font.Color:=ClRed
        else
          lblSectorSize.Font.Color:=clWindowText;

        totalsize:=diskgeo.BytesPerSector*diskgeo.SectorsPerTrack*diskgeo.TracksPerCylinder*diskgeo.Cylinders.QuadPart;
        totalsizestr:=inttostr(totalsize)+' bytes';
        //div 1000 because of harddisk manifacturers that are retarded
        if (totalsize>1500) then
        begin
          totalsize:=totalsize div 1000;
          totalsizestr:=inttostr(totalsize)+' KB';
        end;

        if (totalsize>1500) then
        begin
          totalsize:=totalsize div 1000;
          totalsizestr:=inttostr(totalsize)+' MB';
        end;

        if (totalsize>1500) then
        begin
          totalsize:=totalsize div 1000;
          totalsizestr:=inttostr(totalsize)+' GB';
        end;

        if (totalsize>1500) then
        begin
          totalsize:=totalsize div 1000;
          totalsizestr:=inttostr(totalsize)+' TB';
        end;

        lblTotalSize.Caption:=totalsizestr;
      end
      else
      begin
        lblMediaType.caption:='MediaType:...';
        lblMediaType.Font.Color:=clWindowText;
        lblCylinders.caption:='Cylinders:...';
        lblTracks.caption:='Tracks/Cylinder:...';
        lblSectors.caption:='Sectors/Track:...';
        lblSectorSize.caption:='Sector Size:...';
        lblTotalSize.Caption:='...';
        lblSectorSize.Font.Color:=clWindowText;
        button1.Enabled:=false;
      end;

    finally
      x.free;
    end;
  except

  end;

end;

procedure TForm1.FormClick(Sender: TObject);
begin
  vmm.free;
end;

procedure TForm1.CheckBox2Click(Sender: TObject);
begin
  edit1.enabled:=checkbox2.checked;
end;

procedure TForm1.Button2Click(Sender: TObject);
var x,y: TFilestream;
    bla: array of byte;
    temp: pchar;
begin
  if savedialog1.execute then
  begin
    getmem(temp,6);
    x:=TFileStream.Create(pchar('\\.\'+cbDeviceList.text),fmOpenRead or fmShareDenyNone );
    try
      setlength(bla,vmm.size+512-(vmm.size mod 512));
      ZeroMemory(@bla[0],length(bla));

      x.Seek(0,soFromBeginning);
      x.Read(bla[0],512);

      //check if CETC is already writen. If not, query if he's sure
      CopyMemory(temp,@bla[3],5);
      temp[5]:=#0;
      (*if temp<>'CETC2' then
      begin
        //ask if sure
        if MessageDlg('This disk doesn''t contain dbvm. You sure you want to read it? ?', mtConfirmation, [mbyes,mbno],0)<>mryes then
          exit;
      end;
      *)

      x.Seek(0,soFromBeginning);
      setlength(bla,1024*1024);
      x.Read(bla[0],1024*1024);

      y:=TFileStream.Create(savedialog1.filename,fmcreate);
      try
        y.Write(bla[0],1024*1024);
      finally
        y.free;
      end;

    finally
      x.free;
      freemem(temp);
    end;

  end;
end;

initialization
  init;

end.
