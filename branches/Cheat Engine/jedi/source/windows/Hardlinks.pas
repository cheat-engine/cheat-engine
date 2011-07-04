{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit.  Do not edit.                                       }
{**************************************************************************************************}


{**************************************************************************************************}
{                                                                                                  }
{ Project JEDI Code Library (JCL)                                                                  }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Oliver Schneider (Assarbad att gmx dott info).     }
{ Portions created by Oliver Schneider are Copyright (C) 1995 - 2004 Oliver Schneider.             }
{ All rights reserved.                                                                             }
{                                                                                                  }
{ Obtained through:                                                                                }
{   Joint Endeavour of Delphi Innovators (Project JEDI)                                            }
{                                                                                                  }
{ You may retrieve the latest version of the original file at the Original Developer's homepage,   }
{ located at [http://assarbad.net]. Note that the original file can be used with an arbitrary OSI- }
{ approved license as long as you follow the additional terms given in the original file.          }
{ Additionally a C/C++ (MS VC++) version is available under the same terms.                        }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Oliver Schneider (assarbad)                                                                    }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{  Windows NT 4.0 compatible implementation of the CreateHardLink() API introduced in Windows      }
{  2000.                                                                                           }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit Hardlinks;

{$ALIGN ON}
{$MINENUMSIZE 4}

interface

(*
  All possible combinations of the above DEFINEs have been tested and work fine.

   # | A  B  C
  ---|---------
   1 | 0  0  0                 A = STDCALL
   2 | 0  0  X                 B = RTDL
   3 | X  0  0                 C = PREFERAPI
   4 | X  0  X
   5 | X  X  0
   6 | X  X  X
*)
uses
  Windows;


{$EXTERNALSYM CreateHardLinkW}
{$EXTERNALSYM CreateHardLinkA}

// Well, we did not decide yet ;) - bind to either address, depending on whether
// the API could be found.
type
  TFNCreateHardLinkW = function(szLinkName, szLinkTarget: PWideChar; lpSecurityAttributes: PSecurityAttributes): BOOL;  stdcall; 
  TFNCreateHardLinkA = function(szLinkName, szLinkTarget: PAnsiChar; lpSecurityAttributes: PSecurityAttributes): BOOL;  stdcall; 
var
  CreateHardLinkW: TFNCreateHardLinkW = nil;
  CreateHardLinkA: TFNCreateHardLinkA = nil;

var
  hNtDll: THandle = 0; // For runtime dynamic linking
  bRtdlFunctionsLoaded: Boolean = False; // To show wether the RTDL functions had been loaded

implementation

const
  szNtDll           = 'NTDLL.DLL'; // Import native APIs from this DLL
  szCreateHardLinkA = 'CreateHardLinkA';
  szCreateHardLinkW = 'CreateHardLinkW';

(******************************************************************************

 Note, I only include function prototypes and constants here which are needed!
 For other prototypes or constants check out the related books of
 - Gary Nebbett
 - Sven B. Schreiber
 - Rajeev Nagar

 Note, one my homepage I have also some Native APIs listed in Delphi translated
 form. Not all of them might be translated correctly with respect to the fact
 whether or not they are pointer and whether or not the alignment of variables
 or types is always correct. This might be reviewed by me somewhen in future.

 ******************************************************************************)

// =================================================================
// Type definitions
// =================================================================
type
  NTSTATUS = Longint;
  PPWideChar = ^PWideChar;

type
  LARGE_INTEGER = TLargeInteger;
  PLARGE_INTEGER = ^LARGE_INTEGER;

type
  UNICODE_STRING = record
    Length: WORD;
    MaximumLength: WORD;
    Buffer: PWideChar;
  end;
  PUNICODE_STRING = ^UNICODE_STRING;

type
  ANSI_STRING = record
    Length: WORD;
    MaximumLength: WORD;
    Buffer: PAnsiChar;
  end;
  PANSI_STRING = ^ANSI_STRING;

type
  OBJECT_ATTRIBUTES = record
    Length: ULONG;
    RootDirectory: THandle;
    ObjectName: PUNICODE_STRING;
    Attributes: ULONG;
    SecurityDescriptor: Pointer;       // Points to type SECURITY_DESCRIPTOR
    SecurityQualityOfService: Pointer; // Points to type SECURITY_QUALITY_OF_SERVICE
  end;
  POBJECT_ATTRIBUTES = ^OBJECT_ATTRIBUTES;

type
  IO_STATUS_BLOCK = record
    case integer of
      0:
       (Status: NTSTATUS);
      1:
       (Pointer: Pointer;
        Information: ULONG); // 'Information' does not belong to the union!
  end;
  PIO_STATUS_BLOCK = ^IO_STATUS_BLOCK;

type
  _FILE_LINK_RENAME_INFORMATION = record // File Information Classes 10 and 11
    ReplaceIfExists: BOOL;
    RootDirectory: THandle;
    FileNameLength: ULONG;
    FileName: array[0..0] of WideChar;
  end;
  FILE_LINK_INFORMATION = _FILE_LINK_RENAME_INFORMATION;
  PFILE_LINK_INFORMATION = ^FILE_LINK_INFORMATION;
  FILE_RENAME_INFORMATION = _FILE_LINK_RENAME_INFORMATION;
  PFILE_RENAME_INFORMATION = ^FILE_RENAME_INFORMATION;

// =================================================================
// Constants
// =================================================================
const
  FileLinkInformation          = 11;
  FILE_SYNCHRONOUS_IO_NONALERT = $00000020; // All operations on the file are
                                            // performed synchronously. Waits
                                            // in the system to synchronize I/O
                                            // queuing and completion are not
                                            // subject to alerts. This flag
                                            // also causes the I/O system to
                                            // maintain the file position context.
                                            // If this flag is set, the
                                            // DesiredAccess SYNCHRONIZE flag also
                                            // must be set.
  FILE_OPEN_FOR_BACKUP_INTENT  = $00004000; // The file is being opened for backup
                                            // intent, hence, the system should
                                            // check for certain access rights
                                            // and grant the caller the appropriate
                                            // accesses to the file before checking
                                            // the input DesiredAccess against the
                                            // file's security descriptor.
  FILE_OPEN_REPARSE_POINT      = $00200000;
  DELETE                       = $00010000;
  SYNCHRONIZE                  = $00100000;
  STATUS_SUCCESS               = NTSTATUS(0);
  OBJ_CASE_INSENSITIVE         = $00000040;
  SYMBOLIC_LINK_QUERY          = $00000001;

  // Should be defined, but isn't
  HEAP_ZERO_MEMORY             = $00000008;

  // Related constant(s) for RtlDetermineDosPathNameType_U()
  INVALID_PATH                 = 0;
  UNC_PATH                     = 1;
  ABSOLUTE_DRIVE_PATH          = 2;
  RELATIVE_DRIVE_PATH          = 3;
  ABSOLUTE_PATH                = 4;
  RELATIVE_PATH                = 5;
  DEVICE_PATH                  = 6;
  UNC_DOT_PATH                 = 7;

// =================================================================
// Function prototypes
// =================================================================


type
  TRtlCreateUnicodeStringFromAsciiz = function(var destination: UNICODE_STRING;
    source: PChar): Boolean; stdcall;

  TZwClose = function(Handle: THandle): NTSTATUS; stdcall;

  TZwSetInformationFile = function(FileHandle: THandle;
    var IoStatusBlock: IO_STATUS_BLOCK; FileInformation: Pointer;
    FileInformationLength: ULONG; FileInformationClass: DWORD): NTSTATUS; stdcall;

  TRtlPrefixUnicodeString = function(const usPrefix: UNICODE_STRING;
    const usContainingString: UNICODE_STRING; ignore_case: Boolean): Boolean; stdcall;

  TZwOpenSymbolicLinkObject = function(var LinkHandle: THandle;
    DesiredAccess: DWORD; const ObjectAttributes: OBJECT_ATTRIBUTES): NTSTATUS; stdcall;

  TZwQuerySymbolicLinkObject = function(LinkHandle: THandle;
    var LinkTarget: UNICODE_STRING; ReturnedLength: PULONG): NTSTATUS; stdcall;

  TZwOpenFile = function(var FileHandle: THandle; DesiredAccess: DWORD;
    const ObjectAttributes: OBJECT_ATTRIBUTES; var IoStatusBlock: IO_STATUS_BLOCK;
    ShareAccess: ULONG; OpenOptions: ULONG): NTSTATUS; stdcall;

  TRtlAllocateHeap = function(HeapHandle: Pointer; Flags, Size: ULONG): Pointer; stdcall;

  TRtlFreeHeap = function(HeapHandle: Pointer; Flags: ULONG;
    MemoryPointer: Pointer): Boolean; stdcall;

  TRtlDosPathNameToNtPathName_U = function(DosName: PWideChar;
    var NtName: UNICODE_STRING; DosFilePath: PPWideChar;
    NtFilePath: PUNICODE_STRING): Boolean; stdcall;

  TRtlInitUnicodeString = function(var DestinationString: UNICODE_STRING;
    const SourceString: PWideChar): NTSTATUS; stdcall;

  TRtlDetermineDosPathNameType_U = function(wcsPathNameType: PWideChar): DWORD; stdcall;

  TRtlNtStatusToDosError = function(status: NTSTATUS): ULONG; stdcall;

// Declare all the _global_ function pointers for RTDL
var
  RtlCreateUnicodeStringFromAsciiz: TRtlCreateUnicodeStringFromAsciiz = nil;
  ZwClose: TZwClose = nil;
  ZwSetInformationFile: TZwSetInformationFile = nil;
  RtlPrefixUnicodeString: TRtlPrefixUnicodeString = nil;
  ZwOpenSymbolicLinkObject: TZwOpenSymbolicLinkObject = nil;
  ZwQuerySymbolicLinkObject: TZwQuerySymbolicLinkObject = nil;
  ZwOpenFile: TZwOpenFile = nil;
  RtlAllocateHeap: TRtlAllocateHeap = nil;
  RtlFreeHeap: TRtlFreeHeap = nil;
  RtlDosPathNameToNtPathName_U: TRtlDosPathNameToNtPathName_U = nil;
  RtlInitUnicodeString: TRtlInitUnicodeString = nil;
  RtlDetermineDosPathNameType_U: TRtlDetermineDosPathNameType_U = nil;
  RtlNtStatusToDosError: TRtlNtStatusToDosError = nil;


function NtpGetProcessHeap: Pointer; assembler;
asm
  // The structure offsets are now hardcoded to be able to remove otherwise
  // obsolete structure definitions.
//MOV    EAX, FS:[0]._TEB.Peb
  MOV    EAX, FS:[$30]    // FS points to TEB/TIB which has a pointer to the PEB
//MOV    EAX, [EAX]._PEB.ProcessHeap
  MOV    EAX, [EAX+$18] // Get the process heap's handle
(*
An alternative way to achieve exactly the same (at least in usermode) as above:
  MOV    EAX, FS:$18
  MOV    EAX, [EAX+$30]
  MOV    EAX, [EAX+$18]
*)
end;

(******************************************************************************

 Syntax:
 -------
  C-Prototype! (if STDCALL enabled)

  BOOL WINAPI CreateHardLink(
    LPCTSTR lpFileName,
    LPCTSTR lpExistingFileName,
    LPSECURITY_ATTRIBUTES lpSecurityAttributes // Reserved; Must be NULL!

 Compatibility:
 --------------
  The function can only work on file systems that support hardlinks through the
  underlying FS driver layer. Currently this only includes NTFS on the NT
  platform (as far as I know).
  The function works fine on Windows NT4/2000/XP and is considered to work on
  future Operating System versions derived from NT (including Windows 2003).

 Remarks:
 --------
  This function tries to resemble the original CreateHardLinkW() call from
  Windows 2000/XP/2003 Kernel32.DLL as close as possible. This is why many
  functions used are NT Native API, whereas one could use Delphi or Win32 API
  functions (e.g. memory management). BUT I included much more SEH code and
  omitted extra code to free buffers and close handles. This all is done during
  the FINALLY block (so there are no memory leaks anyway ;).

  Note, that neither Microsoft's code nor mine ignore the Security Descriptor
  from the SECURITY_ATTRIBUTES structure. In both cases the security descriptor
  is passed on to ZwOpenFile()!

  The limit of 1023 hardlinks to one file is probably related to the system or
  NTFS respectively. At least I saw no special hint, why there would be such a
  limit - the original CreateHardLink() does not check the number of links!
  Thus I consider the limit being the same for the original and my rewrite.

  For the ANSI version of this function see below ...

 Remarks from the  Platform SDK:
 -------------------------------
  Any directory entry for a file, whether created with CreateFile or
  CreateHardLink, is a hard link to the associated file. Additional hard links,
  created with the CreateHardLink function, allow you to have multiple directory
  entries for a file, that is, multiple hard links to the same file. These may
  be different names in the same directory, or they may be the same (or
  different) names in different directories. However, all hard links to a file
  must be on the same volume.
  Because hard links are just directory entries for a file, whenever an
  application modifies a file through any hard link, all applications using any
  other hard link to the file see the changes. Also, all of the directory
  entries are updated if the file changes. For example, if the file's size
  changes, all of the hard links to the file will show the new size.
  The security descriptor belongs to the file to which the hard link points.
  The link itself, being merely a directory entry, has no security descriptor.
  Thus, if you change the security descriptor of any hard link, you're actually
  changing the underlying file's security descriptor. All hard links that point
  to the file will thus allow the newly specified access. There is no way to
  give a file different security descriptors on a per-hard-link basis.
  This function does not modify the security descriptor of the file to be linked
  to, even if security descriptor information is passed in the
  lpSecurityAttributes parameter.
  Use DeleteFile to delete hard links. You can delete them in any order
  regardless of the order in which they were created.
  Flags, attributes, access, and sharing as specified in CreateFile operate on
  a per-file basis. That is, if you open a file with no sharing allowed, another
  application cannot share the file by creating a new hard link to the file.

  CreateHardLink does not work over the network redirector.

  Note that when you create a hard link on NTFS, the file attribute information
  in the directory entry is refreshed only when the file is opened or when
  GetFileInformationByHandle is called with the handle of the file of interest.

 ******************************************************************************)
function
  MyCreateHardLinkW // ... otherwise this one
  (szLinkName, szLinkTarget: PWideChar; lpSecurityAttributes: PSecurityAttributes): BOOL;
const
// Mask for any DOS style drive path in object manager notation
  wcsC_NtName: PWideChar = '\??\C:';
// Prefix of a mapped path's symbolic link
  wcsLanMan: PWideChar = '\Device\LanmanRedirector\';
// Size required to hold a number of wide characters to compare drive notation
  cbC_NtName = $10; // 16 bytes
// Access mask to use for opening - just two bits
  dwDesiredAccessHL = DELETE or SYNCHRONIZE;
// OpenOptions for opening of the link target
// The flag FILE_OPEN_REPARSE_POINT has been found by comparison. Probably it carries
// some information wether the file is on the same volume?!
  dwOpenOptionsHL = FILE_SYNCHRONOUS_IO_NONALERT or FILE_OPEN_FOR_BACKUP_INTENT or FILE_OPEN_REPARSE_POINT;
// ShareAccess flags
  dwShareAccessHL = FILE_SHARE_READ or FILE_SHARE_WRITE or FILE_SHARE_DELETE;
var
  usNtName_LinkName, usNtName_LinkTarget: UNICODE_STRING;
  usCheckDrive, usSymLinkDrive, usLanMan: UNICODE_STRING;
  wcsNtName_LinkTarget, wcsFilePart_LinkTarget: PWideChar;
  oaMisc: OBJECT_ATTRIBUTES;
  IOStats: IO_STATUS_BLOCK;
  hHeap: Pointer;
  NeededSize: DWORD;
  Status: NTSTATUS;
  hLinkTarget, hDrive: THandle;
  lpFileLinkInfo: PFILE_LINK_INFORMATION;
begin
  Result := False;
  if not bRtdlFunctionsLoaded then
    Exit;
  // Get process' heap
  hHeap := NtpGetProcessHeap;
  {-------------------------------------------------------------
  Preliminary parameter checks which do Exit with error code set
  --------------------------------------------------------------}
  // If any is not assigned ...
  if (szLinkName = nil) or (szLinkTarget = nil) then
  begin
    SetLastError(ERROR_INVALID_PARAMETER);
    Exit;
  end;
  // Determine DOS path type for both link name and target
  if (RtlDetermineDosPathNameType_U(szLinkName) = UNC_PATH) or
    (RtlDetermineDosPathNameType_U(szLinkTarget) = UNC_PATH) then
  begin
    SetLastError(ERROR_INVALID_NAME);
    Exit;
  end;
  // Convert the link target into a UNICODE_STRING
  if not RtlDosPathNameToNtPathName_U(szLinkTarget, usNtName_LinkTarget, nil, nil) then
  begin
    SetLastError(ERROR_PATH_NOT_FOUND);
    Exit;
  end;
  {------------------------
  Actual main functionality
  -------------------------}
  // Initialise the length members
  RtlInitUnicodeString(usNtName_LinkTarget, usNtName_LinkTarget.Buffer);
  // Get needed buffer size (in TCHARs)
  NeededSize := GetFullPathNameW(szLinkTarget, 0, nil, PWideChar(nil^));
  if NeededSize <> 0 then
  begin
    // Calculate needed size (in TCHARs)
    NeededSize := NeededSize + 1; // times SizeOf(WideChar)
    // Freed in FINALLY
    wcsNtName_LinkTarget := RtlAllocateHeap(hHeap, HEAP_ZERO_MEMORY, NeededSize * SizeOf(WideChar));
    // If successfully allocated buffer ...
    if wcsNtName_LinkTarget <> nil then
      try
        {----------------------------------------------------
        Preparation of the checking for mapped network drives
        -----------------------------------------------------}
        // Get the full unicode path name
        if GetFullPathNameW(szLinkTarget, NeededSize, wcsNtName_LinkTarget, wcsFilePart_LinkTarget) <> 0 then
        begin
          // Allocate memory to check the drive object
          usCheckDrive.Buffer := RtlAllocateHeap(hHeap, HEAP_ZERO_MEMORY, cbC_NtName);
          // On success ...
          if usCheckDrive.Buffer <> nil then
            try
              // Copy to buffer and set length members
              lstrcpynW(usCheckDrive.Buffer, wcsC_NtName, lstrlenW(wcsC_NtName) + 1);
              RtlInitUnicodeString(usCheckDrive, usCheckDrive.Buffer);
              // Replace drive letter by the drive letter we want
              usCheckDrive.Buffer[4] := wcsNtName_LinkTarget[0];
              // Init OBJECT_ATTRIBUTES
              oaMisc.Length := SizeOf(oaMisc);
              oaMisc.RootDirectory := 0;
              oaMisc.ObjectName := @usCheckDrive;
              oaMisc.Attributes := OBJ_CASE_INSENSITIVE;
              oaMisc.SecurityDescriptor := nil;
              oaMisc.SecurityQualityOfService := nil;
              {--------------------------------------------
              Checking for (illegal!) mapped network drives
              ---------------------------------------------}
              // Open symbolic link object
              if ZwOpenSymbolicLinkObject(hDrive, SYMBOLIC_LINK_QUERY, oaMisc) = STATUS_SUCCESS then
                try
                  usSymLinkDrive.Buffer := RtlAllocateHeap(hHeap, HEAP_ZERO_MEMORY, MAX_PATH * SizeOf(WideChar));
                  if usSymLinkDrive.Buffer <> nil then
                    try
                      // Query the path the symbolic link points to ...
                      ZwQuerySymbolicLinkObject(hDrive, usSymLinkDrive, nil);
                      // Initialise the length members
                      RtlInitUnicodeString(usLanMan, wcsLanMan);
                      // The path must not be a mapped drive ... check this!
                      if not RtlPrefixUnicodeString(usLanMan, usSymLinkDrive, True) then
                      begin
                        // Initialise OBJECT_ATTRIBUTES
                        oaMisc.Length := SizeOf(oaMisc);
                        oaMisc.RootDirectory := 0;
                        oaMisc.ObjectName := @usNtName_LinkTarget;
                        oaMisc.Attributes := OBJ_CASE_INSENSITIVE;
                        // Set security descriptor in OBJECT_ATTRIBUTES if they were given
                        if lpSecurityAttributes <> nil then
                          oaMisc.SecurityDescriptor := lpSecurityAttributes^.lpSecurityDescriptor
                        else
                          oaMisc.SecurityDescriptor := nil;
                        oaMisc.SecurityQualityOfService := nil;
                        {----------------------
                        Opening the target file
                        -----------------------}
                        Status := ZwOpenFile(hLinkTarget, dwDesiredAccessHL, oaMisc,
                          IOStats, dwShareAccessHL, dwOpenOptionsHL);
                        if Status = STATUS_SUCCESS then
                          try
                            // Wow ... target opened ... let's try to
                            if RtlDosPathNameToNtPathName_U(szLinkName, usNtName_LinkName, nil, nil) then
                              try
                                // Initialise the length members
                                RtlInitUnicodeString(usNtName_LinkName, usNtName_LinkName.Buffer);
                                // Now almost everything is done to create a link!
                                NeededSize := usNtName_LinkName.Length +
                                  SizeOf(FILE_LINK_INFORMATION) + SizeOf(WideChar);
                                lpFileLinkInfo := RtlAllocateHeap(hHeap, HEAP_ZERO_MEMORY, NeededSize);
                                if lpFileLinkInfo <> nil then
                                  try
                                    lpFileLinkInfo^.ReplaceIfExists := False;
                                    lpFileLinkInfo^.RootDirectory := 0;
                                    lpFileLinkInfo^.FileNameLength := usNtName_LinkName.Length;
                                    lstrcpynW(lpFileLinkInfo.FileName, usNtName_LinkName.Buffer,
                                      usNtName_LinkName.Length);
                                    {----------------------------------------------------
                                    Final creation of the link - "center" of the function
                                    -----------------------------------------------------}
                                    // Hard-link the file as intended
                                    Status := ZwSetInformationFile(hLinkTarget, IOStats,
                                      lpFileLinkInfo, NeededSize, FileLinkInformation);
                                    // On success return TRUE
                                    Result := Status >= 0;
                                  finally
                                    // Free the buffer
                                    RtlFreeHeap(hHeap, 0, lpFileLinkInfo);
                                    // Set last error code
                                    SetLastError(RtlNtStatusToDosError(Status));
                                  end
                                else // if lpFileLinkInfo <> nil then
                                  SetLastError(ERROR_NOT_ENOUGH_MEMORY);
                              finally
                                RtlFreeHeap(hHeap, 0, usNtName_LinkName.Buffer);
                              end
                            else // if RtlDosPathNameToNtPathName_U(szLinkName, usNtName_LinkName...
                              SetLastError(ERROR_INVALID_NAME);
                          finally
                            ZwClose(hLinkTarget);
                          end
                        else // if Status = STATUS_SUCCESS then
                          SetLastError(RtlNtStatusToDosError(Status));
                      end
                      else // if not RtlPrefixUnicodeString(usLanMan, usSymLinkDrive, True) then
                        SetLastError(ERROR_INVALID_NAME);
                    finally
                      RtlFreeHeap(hHeap, 0, usSymLinkDrive.Buffer);
                    end
                  else // if usSymLinkDrive.Buffer <> nil then
                    SetLastError(ERROR_NOT_ENOUGH_MEMORY);
                finally
                  ZwClose(hDrive);
                end;
            finally
              RtlFreeHeap(hHeap, 0, usCheckDrive.Buffer);
            end
          else // if usCheckDrive.Buffer <> nil then
            SetLastError(ERROR_NOT_ENOUGH_MEMORY);
        end
        else // if GetFullPathNameW(szLinkTarget, NeededSize, wcsNtName_LinkTarget...
          SetLastError(ERROR_INVALID_NAME);
      finally
        RtlFreeHeap(hHeap, 0, wcsNtName_LinkTarget);
      end
    else // if wcsNtName_LinkTarget <> nil then
      SetLastError(ERROR_NOT_ENOUGH_MEMORY);
  end
  else // if NeededSize <> 0 then
    SetLastError(ERROR_INVALID_NAME);
  // Finally free the buffer
  RtlFreeHeap(hHeap, 0, usNtName_LinkTarget.Buffer);
end;

(******************************************************************************
 Hint:
 -----
  For all closer information see the CreateHardLinkW function above.

 Specific to the ANSI-version:
 -----------------------------
  The ANSI-Version can be used as if it was used on Windows 2000. This holds
  for all supported systems for now.

 ******************************************************************************)

function
  MyCreateHardLinkA // ... otherwise this one
  (szLinkName, szLinkTarget: PAnsiChar; lpSecurityAttributes: PSecurityAttributes): BOOL;
var
  usLinkName: UNICODE_STRING;
  usLinkTarget: UNICODE_STRING;
  hHeap: Pointer;
begin
  Result := False;
  if not bRtdlFunctionsLoaded then
    Exit;
  // Get the process' heap
  hHeap := NtpGetProcessHeap;
  // Create and allocate a UNICODE_STRING from the zero-terminated parameters
  if RtlCreateUnicodeStringFromAsciiz(usLinkName, szLinkName) then
  try
    if RtlCreateUnicodeStringFromAsciiz(usLinkTarget, szLinkTarget) then
    try
      // Call the Unicode version
      Result := CreateHardLinkW(usLinkName.Buffer, usLinkTarget.Buffer, lpSecurityAttributes);
    finally
      // free the allocated buffer
      RtlFreeHeap(hHeap, 0, usLinkTarget.Buffer);
    end;
  finally
    // free the allocate buffer
    RtlFreeHeap(hHeap, 0, usLinkName.Buffer);
  end;
end;

const
// Names of the functions to import
  szRtlCreateUnicodeStringFromAsciiz = 'RtlCreateUnicodeStringFromAsciiz';
  szZwClose                          = 'ZwClose';
  szZwSetInformationFile             = 'ZwSetInformationFile';
  szRtlPrefixUnicodeString           = 'RtlPrefixUnicodeString';
  szZwOpenSymbolicLinkObject         = 'ZwOpenSymbolicLinkObject';
  szZwQuerySymbolicLinkObject        = 'ZwQuerySymbolicLinkObject';
  szZwOpenFile                       = 'ZwOpenFile';
  szRtlAllocateHeap                  = 'RtlAllocateHeap';
  szRtlFreeHeap                      = 'RtlFreeHeap';
  szRtlDosPathNameToNtPathName_U     = 'RtlDosPathNameToNtPathName_U';
  szRtlInitUnicodeString             = 'RtlInitUnicodeString';
  szRtlDetermineDosPathNameType_U    = 'RtlDetermineDosPathNameType_U';
  szRtlNtStatusToDosError            = 'RtlNtStatusToDosError';

var
  hKernel32: THandle = 0;

initialization
  // GetModuleHandle because this DLL is loaded into any Win32 subsystem process anyway
  // implicitly. And Delphi cannot create applications for other subsystems without
  // major changes in SysInit und System units.
  hKernel32 := GetModuleHandle(kernel32);
  // If we prefer the real Windows APIs try to get their addresses
  @CreateHardLinkA := GetProcAddress(hKernel32, szCreateHardLinkA);
  @CreateHardLinkW := GetProcAddress(hKernel32, szCreateHardLinkW);
  // If they could not be retrieved resort to our home-grown version
  if not (Assigned(@CreateHardLinkA) and Assigned(@CreateHardLinkW)) then
  begin

  // GetModuleHandle because this DLL is loaded into any Win32 subsystem process anyway
  // implicitly. And Delphi cannot create applications for other subsystems without
  // major changes in SysInit und System units.
  hNtDll := GetModuleHandle(szNtDll);
  if hNtDll <> 0 then
  begin
    // Get all the function addresses
    @RtlCreateUnicodeStringFromAsciiz := GetProcAddress(hNtDll, szRtlCreateUnicodeStringFromAsciiz);
    @ZwClose := GetProcAddress(hNtDll, szZwClose);
    @ZwSetInformationFile := GetProcAddress(hNtDll, szZwSetInformationFile);
    @RtlPrefixUnicodeString := GetProcAddress(hNtDll, szRtlPrefixUnicodeString);
    @ZwOpenSymbolicLinkObject := GetProcAddress(hNtDll, szZwOpenSymbolicLinkObject);
    @ZwQuerySymbolicLinkObject := GetProcAddress(hNtDll, szZwQuerySymbolicLinkObject);
    @ZwOpenFile := GetProcAddress(hNtDll, szZwOpenFile);
    @RtlAllocateHeap := GetProcAddress(hNtDll, szRtlAllocateHeap);
    @RtlFreeHeap := GetProcAddress(hNtDll, szRtlFreeHeap);
    @RtlDosPathNameToNtPathName_U := GetProcAddress(hNtDll, szRtlDosPathNameToNtPathName_U);
    @RtlInitUnicodeString := GetProcAddress(hNtDll, szRtlInitUnicodeString);
    @RtlDetermineDosPathNameType_U := GetProcAddress(hNtDll, szRtlDetermineDosPathNameType_U);
    @RtlNtStatusToDosError := GetProcAddress(hNtDll, szRtlNtStatusToDosError);
    // Check whether we could retrieve all of them
    bRtdlFunctionsLoaded := // Update the "loaded" status
      Assigned(@RtlCreateUnicodeStringFromAsciiz) and
      Assigned(@ZwClose) and
      Assigned(@ZwSetInformationFile) and
      Assigned(@RtlPrefixUnicodeString) and
      Assigned(@ZwOpenSymbolicLinkObject) and
      Assigned(@ZwQuerySymbolicLinkObject) and
      Assigned(@ZwOpenFile) and
      Assigned(@RtlAllocateHeap) and
      Assigned(@RtlFreeHeap) and
      Assigned(@RtlDosPathNameToNtPathName_U) and
      Assigned(@RtlInitUnicodeString) and
      Assigned(@RtlDetermineDosPathNameType_U) and
      Assigned(@RtlNtStatusToDosError);
  end;

    @CreateHardLinkA := @MyCreateHardLinkA;
    @CreateHardLinkW := @MyCreateHardLinkW;
  end; // if not (Assigned(@CreateHardLinkA) and Assigned(@CreateHardLinkW)) then ...


end.

