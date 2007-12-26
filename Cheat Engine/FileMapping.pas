unit FileMapping;
{
Wrapper class for filemapping
version 0.1: Only supports opening of exisiting files and no appending
version 0.2: copy on write, you can now write to it without actually messing up the file

}

interface

uses windows,classes,SysUtils;

type TFileMapping=class
  private
    FileHandle: THandle;
    FileMapping: THandle;
    FFileContent: pointer;
    FFileSize: Dword;
  public
    property fileContent: pointer read FFileContent;
    property filesize: dword read FFileSize;
    constructor create(filename: string);
    destructor destroy; override;
end;

implementation

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
    if FileHandle = INVALID_HANDLE_VALUE then raise exception.create(filename+' does not exist');

    FFileSize:=GetFileSize(FileHandle,nil);

    //still here, so create filemapping
    FileMapping := CreateFileMapping(FileHandle, nil, PAGE_WRITECOPY	, 0, 0, nil);
    if FileMapping = 0 then raise exception.create('Mapping failed');

    //map it completly
    FFileContent:= MapViewOfFile(FileMapping, FILE_MAP_COPY , 0, 0, 0);
    if FFileContent=nil then raise exception.Create('Failed creating a proper view');

  except
    on e: exception do
    begin
      if (FileMapping<>0) and (filemapping<>INVALID_HANDLE_VALUE) then
        CloseHandle(FileMapping);

      if (FileHandle<>0) and (FileHandle<>INVALID_HANDLE_VALUE) then
        CloseHandle(FileHandle);

      raise e;
    end;
  end;

end;

end.


