unit FileMapping;

{$MODE Delphi}

{
Wrapper class for filemapping
version 0.1: Only supports opening of exisiting files and no appending
version 0.2: copy on write, you can now write to it without actually messing up the file

}

interface

uses windows, LCLIntf,classes,SysUtils;

type
  EFileMapError=class(Exception);
  TFileMapping=class
  private
    FileHandle: THandle;
    FileMapping: THandle;
    FFileContent: pointer;
    FFileSize: Qword;
    FFilename: string;
  public
    property fileContent: pointer read FFileContent;
    property filesize: qword read FFileSize;
    property filename: string read FFilename;
    constructor create(filename: string);
    destructor destroy; override;
end;

implementation

//function GetFileSize(hFile:HANDLE; lpFileSizeHigh:LPDWORD):DWORD; external 'kernel32' name 'GetFileSize';

resourcestring
  rsDoesNotExist = '%s does not exist';
  rsMappingFailed = 'Mapping failed';
  rsFailedCreatingAProperView = 'Failed creating a proper view';

destructor TFileMapping.destroy;
begin
  //clean up the mapping
  if FFileContent<>nil then
    UnmapViewOfFile(FFileContent);

  if FileMapping<>0 then
    CloseHandle(FileMapping);

  if FileHandle<>0 then
    CloseHandle(FileHandle);

  inherited destroy;
end;

constructor TFileMapping.create(filename: string);
begin
  inherited create;

  try
    //open file
    FileHandle := CreateFile(PChar(filename), GENERIC_READ or GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if FileHandle = INVALID_HANDLE_VALUE then
      FileHandle := CreateFile(PChar(filename), GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);

    if FileHandle = INVALID_HANDLE_VALUE then
      raise EFileMapError.create(Format(rsDoesNotExist, [filename]));


    FFileSize:=0;
    FFileSize:=GetFileSize(FileHandle,pointer(ptruint(@fileSize)+4));

    //still here, so create filemapping
    FileMapping := CreateFileMapping(FileHandle, nil, PAGE_WRITECOPY	, 0, 0, nil);
    if FileMapping = 0 then raise EFileMapError.create(rsMappingFailed);

    //map it completely
    FFileContent:= MapViewOfFile(FileMapping, FILE_MAP_COPY , 0, 0, 0);
    if FFileContent=nil then raise EFileMapError.Create(rsFailedCreatingAProperView);

    ffilename:=filename;
  except
    on e: exception do
    begin
      if (FileMapping<>0) and (filemapping<>INVALID_HANDLE_VALUE) then
        CloseHandle(FileMapping);

      if (FileHandle<>0) and (FileHandle<>INVALID_HANDLE_VALUE) then
        CloseHandle(FileHandle);

      raise EFileMapError.create(e.message);
    end;
  end;

end;

end.


