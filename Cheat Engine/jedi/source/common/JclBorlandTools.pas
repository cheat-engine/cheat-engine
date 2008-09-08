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
{ The Original Code is DelphiInstall.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Petr Vones. Portions created by Petr Vones are     }
{ Copyright (C) of Petr Vones. All Rights Reserved.                                                }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Andreas Hausladen (ahuser)                                                                     }
{   Florent Ouchet (outchy)                                                                        }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair) - crossplatform & BCB support                                      }
{   Uwe Schuster (uschuster)                                                                       }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Routines for getting information about installed versions of Delphi/C++Builder and performing    }
{ basic installation tasks.                                                                        }
{                                                                                                  }
{ Important notes for C#Builder 1 and Delphi 8:                                                    }
{ These products were not shipped with their native compilers, but the toolkit to build design     }
{ packages is available in codecentral (http://codecentral.borland.com):                           }
{  - "IDE Integration pack for C#Builder 1.0" http://codecentral.borland.com/Item.aspx?ID=21334    }
{  - "IDE Integration pack for Delphi 8" http://codecentral.borland.com/Item.aspx?ID=21333         }
{ It's recommended to extract zip files using the standard pattern of Delphi directories:          }
{  - Binary files go to \bin (DCC32.EXE, RLINK32.DLL and lnkdfm7*.dll)                             }
{  - Compiler files go to \lib (designide.dcp, rtl.dcp, SysInit.dcu, vcl.dcp, vclactnband.dcp,     }
{    vcljpg.dcp and vclx.dcp)                                                                      }
{  - ToolsAPI files go to \source\ToolsAPI (PaletteAPI.pas, PropInspAPI.pas and ToolsAPI.pas)      }
{ Don't mix C#Builder 1 files with Delphi 8 and vice-versa otherwise the compilation will fail     }
{ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                   }
{ !!!!!!!!      The DCPPath for these releases have to $(BDS)\lib      !!!!!!!!!                   }
{ !!!!!!!!    or the directory where compiler files were extracted     !!!!!!!!!                   }
{ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!                   }
{ The default BPL output directory for these products is set to $(BDSPROJECTSDIR)\bpl, it may not  }
{ exist since the product installers don't create it                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-07-20 22:13:14 +0200 (dim., 20 juil. 2008)                         $ }
{ Revision:      $Rev:: 2397                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit JclBorlandTools;

{$I jcl.inc}
{$I crossplatform.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows,
  MSHelpServices_TLB,
  {$ENDIF MSWINDOWS}
  Classes, SysUtils, IniFiles, Contnrs,
  JclBase, JclSysUtils;

// Various definitions
type
  EJclBorRADException = class(EJclError);

  TJclBorRADToolKind = (brDelphi, brCppBuilder, brBorlandDevStudio);
  {$IFDEF KYLIX}
  TJclBorRADToolEdition = (deOPEN, dePRO, deSVR);
  {$ELSE}
  TJclBorRADToolEdition = (deSTD, dePRO, deCSS, deARC);
  {$ENDIF KYLIX}
  TJclBorRADToolPath = string;

const
  SupportedDelphiVersions = [5, 6, 7, 8, 9, 10, 11];
  SupportedBCBVersions    = [5, 6, 10, 11];
  SupportedBDSVersions    = [1, 2, 3, 4, 5];

  // Object Repository
  BorRADToolRepositoryPagesSection    = 'Repository Pages';

  BorRADToolRepositoryDialogsPage     = 'Dialogs';
  BorRADToolRepositoryFormsPage       = 'Forms';
  BorRADToolRepositoryProjectsPage    = 'Projects';
  BorRADToolRepositoryDataModulesPage = 'Data Modules';

  BorRADToolRepositoryObjectType      = 'Type';
  BorRADToolRepositoryFormTemplate    = 'FormTemplate';
  BorRADToolRepositoryProjectTemplate = 'ProjectTemplate';
  BorRADToolRepositoryObjectName      = 'Name';
  BorRADToolRepositoryObjectPage      = 'Page';
  BorRADToolRepositoryObjectIcon      = 'Icon';
  BorRADToolRepositoryObjectDescr     = 'Description';
  BorRADToolRepositoryObjectAuthor    = 'Author';
  BorRADToolRepositoryObjectAncestor  = 'Ancestor';
  BorRADToolRepositoryObjectDesigner  = 'Designer'; // Delphi 6+ only
  BorRADToolRepositoryDesignerDfm     = 'dfm';
  BorRADToolRepositoryDesignerXfm     = 'xfm';
  BorRADToolRepositoryObjectNewForm   = 'DefaultNewForm';
  BorRADToolRepositoryObjectMainForm  = 'DefaultMainForm';

  SourceExtensionDelphiPackage = '.dpk';
  SourceExtensionBCBPackage    = '.bpk';
  SourceExtensionDelphiProject = '.dpr';
  SourceExtensionBCBProject    = '.bpr';
  SourceExtensionBDSProject    = '.bdsproj';
  SourceExtensionDProject      = '.dproj';
  BinaryExtensionPackage       = '.bpl';
  BinaryExtensionLibrary       = '.dll';
  BinaryExtensionExecutable    = '.exe';
  CompilerExtensionDCP         = '.dcp';
  CompilerExtensionBPI         = '.bpi';
  CompilerExtensionLIB         = '.lib';
  CompilerExtensionTDS         = '.tds';
  CompilerExtensionMAP         = '.map';
  CompilerExtensionDRC         = '.drc';
  CompilerExtensionDEF         = '.def';
  SourceExtensionCPP           = '.cpp';
  SourceExtensionH             = '.h';
  SourceExtensionPAS           = '.pas';
  SourceExtensionDFM           = '.dfm';
  SourceExtensionXFM           = '.xfm';
  SourceDescriptionPAS         = 'Pascal source file';
  SourceDescriptionCPP         = 'C++ source file';

  DesignerVCL = 'VCL';
  DesignerCLX = 'CLX';

  ProjectTypePackage = 'package';
  ProjectTypeLibrary = 'library';
  ProjectTypeProgram = 'program';

  Personality32Bit        = '32 bit';
  Personality64Bit        = '64 bit';
  PersonalityDelphi       = 'Delphi';
  PersonalityDelphiDotNet = 'Delphi.net';
  PersonalityBCB          = 'C++Builder';
  PersonalityCSB          = 'C#Builder';
  PersonalityVB           = 'Visual Basic';
  PersonalityDesign       = 'Design';
  PersonalityUnknown      = 'Unknown personality';
  PersonalityBDS          = 'Borland Developer Studio';

  DOFDirectoriesSection = 'Directories';
  DOFUnitOutputDirKey   = 'UnitOutputDir';
  DOFSearchPathName     = 'SearchPath';
  DOFConditionals       = 'Conditionals';
  DOFLinkerSection      = 'Linker';
  DOFPackagesKey        = 'Packages';
  DOFCompilerSection    = 'Compiler';
  DOFPackageNoLinkKey   = 'PackageNoLink';
  // injection of new compiler options to workaround L1496 internal error of Delphi 5 and C++Builder 5
  // adding -B switch to the compiler command line forces units to be built
  DOFAdditionalSection  = 'Additional';
  DOFOptionsKey         = 'Options';

  {$IFDEF KYLIX}
  BorRADToolEditionIDs: array [TJclBorRADToolEdition] of PChar =
    ('OPEN', 'PRO', 'SVR');
  {$ELSE ~KYLIX}
  BorRADToolEditionIDs: array [TJclBorRADToolEdition] of PChar =
    ('STD', 'PRO', 'CSS', 'ARC'); // 'ARC' is an assumption
  {$ENDIF ~KYLIX}

// Installed versions information classes
type
  TJclBorPersonality = (bpDelphi32, bpDelphi64, bpBCBuilder32, bpBCBuilder64,
    bpDelphiNet32, bpDelphiNet64, bpCSBuilder32, bpCSBuilder64,
    bpVisualBasic32, bpVisualBasic64, bpDesign, bpUnknown);
  //  bpDelphi64, bpBCBuilder64);
  
  TJclBorPersonalities = set of TJclBorPersonality;

  TJclBorDesigner = (bdVCL, bdCLX);
  
  TJclBorDesigners = set of TJClBorDesigner;

  TJclBorPlatform = (bp32bit, bp64bit);

const
  JclBorPersonalityDescription: array [TJclBorPersonality] of string =
   (
    Personality32Bit + ' ' + PersonalityDelphi,
    Personality64Bit + ' ' + PersonalityDelphi,
    Personality32Bit + ' ' + PersonalityBCB,
    Personality64Bit + ' ' + PersonalityBCB,
    Personality32Bit + ' ' + PersonalityDelphiDotNet,
    Personality64Bit + ' ' + PersonalityDelphiDotNet,
    Personality32Bit + ' ' + PersonalityCSB,
    Personality64Bit + ' ' + PersonalityCSB,
    Personality32Bit + ' ' + PersonalityVB,
    Personality64Bit + ' ' + PersonalityVB,
    PersonalityDesign,
    PersonalityUnknown
   );

  JclBorDesignerDescription: array [TJclBorDesigner] of string =
    (DesignerVCL, DesignerCLX);
  JclBorDesignerFormExtension: array [TJclBorDesigner] of string =
    (SourceExtensionDFM, SourceExtensionXFM);

type
  TJclBorRADToolInstallation = class;

  TJclBorRADToolInstallationObject = class(TInterfacedObject)
  private
    FInstallation: TJclBorRADToolInstallation;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
  public
    property Installation: TJclBorRADToolInstallation read FInstallation;
  end;

  {$IFDEF MSWINDOWS}
  TJclBorlandOpenHelp = class(TJclBorRADToolInstallationObject)
  private
    function GetContentFileName: string;
    function GetIndexFileName: string;
    function GetLinkFileName: string;
    function GetGidFileName: string;
    function GetProjectFileName: string;
    function ReadFileName(const FormatName: string): string;
  public
    function AddHelpFile(const HelpFileName, IndexName: string): Boolean;
    function RemoveHelpFile(const HelpFileName, IndexName: string): Boolean;
    property ContentFileName: string read GetContentFileName;
    property GidFileName: string read GetGidFileName;
    property IndexFileName: string read GetIndexFileName;
    property LinkFileName: string read GetLinkFileName;
    property ProjectFileName: string read GetProjectFileName;
  end;

  TJclHelp2Object = (hoRegisterSession, hoRegister, hoPlugin);
  TJclHelp2Objects = set of TJclHelp2Object;

  TJclHelp2Manager = class(TJclBorRADToolInstallationObject)
  private
    FHxRegisterSession: IHxRegisterSession;
    FHxRegister: IHxRegister;
    FHxPlugin: IHxPlugIn;
    FIdeNameSpace: WideString;
    function RequireObject(HelpObjects: TJclHelp2Objects): Boolean;
    function GetHxPlugin: IHxPlugin;
    function GetHxRegister: IHxRegister;
    function GetHxRegisterSession: IHxRegisterSession;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation); overload;
  public
    constructor Create; overload;
    destructor Destroy; override;
    function CreateTransaction: Boolean;
    function CommitTransaction: Boolean;
    function RegisterNameSpace(const Name, Collection, Description: WideString): Boolean;
    function UnregisterNameSpace(const Name: WideString): Boolean;
    function RegisterHelpFile(const NameSpace, Identifier: WideString;
      const LangId: Integer; const HxSFile, HxIFile: WideString): Boolean;
    function UnregisterHelpFile(const NameSpace, Identifier: WideString;
      const LangId: Integer): Boolean;
    function PlugNameSpaceIn(const SourceNameSpace,
      TargetNameSpace: WideString): Boolean;
    function UnPlugNameSpace(const SourceNameSpace,
      TargetNameSpace: WideString): Boolean;
    function PlugNameSpaceInBorlandHelp(const NameSpace: WideString): Boolean;
    function UnPlugNameSpaceFromBorlandHelp(const NameSpace: WideString): Boolean;
    property HxRegisterSession: IHxRegisterSession read GetHxRegisterSession;
    property HxRegister: IHxRegister read GetHxRegister;
    property HxPlugin: IHxPlugin read GetHxPlugin;
    property IdeNamespace: WideString read FIdeNameSpace;
  end;
  {$ENDIF MSWINDOWS}

  TJclBorRADToolIdeTool = class(TJclBorRADToolInstallationObject)
  private
    FKey: string;
    function GetCount: Integer;
    function GetParameters(Index: Integer): string;
    function GetPath(Index: Integer): string;
    function GetTitle(Index: Integer): string;
    function GetWorkingDir(Index: Integer): string;
    procedure SetCount(const Value: Integer);
    procedure SetParameters(Index: Integer; const Value: string);
    procedure SetPath(Index: Integer; const Value: string);
    procedure SetTitle(Index: Integer; const Value: string);
    procedure SetWorkingDir(Index: Integer; const Value: string);
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
    procedure CheckIndex(Index: Integer);
  public
    property Count: Integer read GetCount write SetCount;
    function IndexOfPath(const Value: string): Integer;
    function IndexOfTitle(const Value: string): Integer;
    procedure RemoveIndex(const Index: Integer);
    property Key: string read FKey;
    property Title[Index: Integer]: string read GetTitle write SetTitle;
    property Path[Index: Integer]: string read GetPath write SetPath;
    property Parameters[Index: Integer]: string read GetParameters write SetParameters;
    property WorkingDir[Index: Integer]: string read GetWorkingDir write SetWorkingDir;
  end;

  TJclBorRADToolIdePackages = class(TJclBorRADToolInstallationObject)
  private
    FDisabledPackages: TStringList;
    FKnownPackages: TStringList;
    FKnownIDEPackages: TStringList;
    FExperts: TStringList;
    function GetCount: Integer;
    function GetIDECount: Integer;
    function GetExpertCount: Integer;
    function GetPackageDescriptions(Index: Integer): string;
    function GetIDEPackageDescriptions(Index: Integer): string;
    function GetExpertDescriptions(Index: Integer): string;
    function GetPackageDisabled(Index: Integer): Boolean;
    function GetPackageFileNames(Index: Integer): string;
    function GetIDEPackageFileNames(Index: Integer): string;
    function GetExpertFileNames(Index: Integer): string;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
    function PackageEntryToFileName(const Entry: string): string;
    procedure ReadPackages;
    procedure RemoveDisabled(const FileName: string);
  public
    destructor Destroy; override;
    function AddPackage(const FileName, Description: string): Boolean;
    function AddIDEPackage(const FileName, Description: string): Boolean;
    function AddExpert(const FileName, Description: string): Boolean;
    function RemovePackage(const FileName: string): Boolean;
    function RemoveIDEPackage(const FileName: string): Boolean;
    function RemoveExpert(const FileName: string): Boolean;
    property Count: Integer read GetCount;
    property IDECount: Integer read GetIDECount;
    property ExpertCount: Integer read GetExpertCount;
    property PackageDescriptions[Index: Integer]: string read GetPackageDescriptions;
    property IDEPackageDescriptions[Index: Integer]: string read GetIDEPackageDescriptions;
    property ExpertDescriptions[Index: Integer]: string read GetExpertDescriptions;
    property PackageFileNames[Index: Integer]: string read GetPackageFileNames;
    property IDEPackageFileNames[Index: Integer]: string read GetIDEPackageFileNames;
    property ExpertFileNames[Index: Integer]: string read GetExpertFileNames;
    property PackageDisabled[Index: Integer]: Boolean read GetPackageDisabled;
  end;

{$HPPEMIT 'namespace Jclborlandtools'}
{$HPPEMIT '{'}
{$HPPEMIT '  // For some reason, the generator puts this interface after its first'}
{$HPPEMIT '  // usage, resulting in an unusable header file. We fix this by forward'}
{$HPPEMIT '  // declaring the interface.'}
{$HPPEMIT '  __interface IJclCommandLineTool;'}
(*$HPPEMIT '}'*)

  IJclCommandLineTool = interface
    ['{A0034B09-A074-D811-847D-0030849E4592}']
    function GetExeName: string;
    function GetOptions: TStrings;
    function GetOutput: string;
    function GetOutputCallback: TTextHandler;
    procedure AddPathOption(const Option, Path: string);
    function Execute(const CommandLine: string): Boolean;
    procedure SetOutputCallback(const CallbackMethod: TTextHandler);
    property ExeName: string read GetExeName;
    property Options: TStrings read GetOptions;
    property OutputCallback: TTextHandler write SetOutputCallback;
    property Output: string read GetOutput;
  end;

  EJclCommandLineToolError = class(EJclError);

  TJclCommandLineTool = class(TInterfacedObject, IJclCommandLineTool)
  private
    FExeName: string;
    FOptions: TStringList;
    FOutput: string;
    FOutputCallback: TTextHandler;
  protected
    function GetExeName: string;
    function GetOutput: string;
    function GetOptions: TStrings;
    function GetOutputCallback: TTextHandler;
    procedure SetOutputCallback(const CallbackMethod: TTextHandler);
    constructor Create(const AExeName: string);
    procedure AddPathOption(const Option, Path: string);
    function Execute(const CommandLine: string): Boolean;
    property ExeName: string read GetExeName;
    property Output: string read GetOutput;
  public
    destructor Destroy; override;
  end;

  TJclBorlandCommandLineTool = class(TJclBorRADToolInstallationObject, IJclCommandLineTool)
  private
    FOptions: TStringList;
    FOutputCallback: TTextHandler;
    FOutput: string;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation); virtual;
    procedure CheckOutputValid;
    function GetExeName: string; virtual;
    function GetFileName: string;
    function GetOptions: TStrings;
    function GetOutputCallback: TTextHandler;
    procedure SetOutputCallback(const CallbackMethod: TTextHandler);
    function GetOutput: string;
  public
    destructor Destroy; override;
    procedure AddPathOption(const Option, Path: string);
    function Execute(const CommandLine: string): Boolean; virtual;
    property FileName: string read GetFileName;
    property Output: string read GetOutput;
    property OutputCallback: TTextHandler read FOutputCallback write SetOutputCallback;
    property Options: TStrings read GetOptions;
  end;

  TJclBCC32 = class(TJclBorlandCommandLineTool)
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation); override;
    function GetExeName: string; override;
  public
    {$IFDEF KEEP_DEPRECATED}
    function SupportsLibSuffix: Boolean;
    {$ENDIF KEEP_DEPRECATED}
  end;

  TJclDCC32 = class(TJclBorlandCommandLineTool)
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation); override;
    function GetExeName: string; override;
    procedure AddProjectOptions(const ProjectFileName, DCPPath: string);
    function Compile(const ProjectFileName: string): Boolean;
  public
    function Execute(const CommandLine: string): Boolean; override;
    function MakePackage(const PackageName, BPLPath, DCPPath: string; ExtraOptions: string = ''): Boolean;
    function MakeProject(const ProjectName, OutputDir, DcpSearchPath: string; ExtraOptions: string = ''): Boolean;
    procedure SetDefaultOptions; virtual;
    {$IFDEF KEEP_DEPRECATED}
    function SupportsLibSuffix: Boolean;
    {$ENDIF KEEP_DEPRECATED}
  end;
  {$IFDEF KEEP_DEPRECATED}
  TJclDCC = TJclDCC32;
  {$ENDIF KEEP_DEPRECATED}

  TJclBpr2Mak = class(TJclBorlandCommandLineTool)
  protected
    function GetExeName: string; override;
  end;

  TJclBorlandMake = class(TJclBorlandCommandLineTool)
  protected
    function GetExeName: string; override;
  end;

  TJclBorRADToolPalette = class(TJclBorRADToolInstallationObject)
  private
    FKey: string;
    FTabNames: TStringList;
    function GetComponentsOnTab(Index: Integer): string;
    function GetHiddenComponentsOnTab(Index: Integer): string;
    function GetTabNameCount: Integer;
    function GetTabNames(Index: Integer): string;
    procedure ReadTabNames;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
  public
    destructor Destroy; override;
    procedure ComponentsOnTabToStrings(Index: Integer; Strings: TStrings; IncludeUnitName: Boolean = False;
      IncludeHiddenComponents: Boolean = True);
    function DeleteTabName(const TabName: string): Boolean;
    function TabNameExists(const TabName: string): Boolean;
    property ComponentsOnTab[Index: Integer]: string read GetComponentsOnTab;
    property HiddenComponentsOnTab[Index: Integer]: string read GetHiddenComponentsOnTab;
    property Key: string read FKey;
    property TabNames[Index: Integer]: string read GetTabNames;
    property TabNameCount: Integer read GetTabNameCount;
  end;

  TJclBorRADToolRepository = class(TJclBorRADToolInstallationObject)
  private
    FIniFile: TIniFile;
    FFileName: string;
    FPages: TStringList;
    function GetIniFile: TIniFile;
    function GetPages: TStrings;
  protected
    constructor Create(AInstallation: TJclBorRADToolInstallation);
  public
    destructor Destroy; override;
    procedure AddObject(const FileName, ObjectType, PageName, ObjectName, IconFileName, Description,
      Author, Designer: string; const Ancestor: string = '');
    procedure CloseIniFile;
    function FindPage(const Name: string; OptionalIndex: Integer): string;
    procedure RemoveObjects(const PartialPath, FileName, ObjectType: string);
    property FileName: string read FFileName;
    property IniFile: TIniFile read GetIniFile;
    property Pages: TStrings read GetPages;
  end;

  TCommandLineTool = (clAsm, clBcc32, clDcc32, clDccIL, clMake, clProj2Mak);
  TCommandLineTools = set of TCommandLineTool;

  TJclBorRADToolInstallationClass = class of TJclBorRADToolInstallation;

  TJclBorRADToolInstallation = class(TObject)
  private
    FConfigData: TCustomIniFile;
    FConfigDataLocation: string;
    FRootKey: Cardinal;
    FGlobals: TStringList;
    FRootDir: string;
    FBinFolderName: string;
    FBCC32: TJclBCC32;
    FDCC32: TJclDCC32;
    FBpr2Mak: TJclBpr2Mak;
    FMake: IJclCommandLineTool;
    FEditionStr: string;
    FEdition: TJclBorRADToolEdition;
    FEnvironmentVariables: TStringList;
    FIdePackages: TJclBorRADToolIdePackages;
    FIdeTools: TJclBorRADToolIdeTool;
    FInstalledUpdatePack: Integer;
    {$IFDEF MSWINDOWS}
    FOpenHelp: TJclBorlandOpenHelp;
    {$ENDIF MSWINDOWS}
    FPalette: TJclBorRADToolPalette;
    FRepository: TJclBorRADToolRepository;
    FVersionNumber: Integer;    // Delphi 2005: 3   -  Delphi 7: 7 - Delphi 2007: 11
    FVersionNumberStr: string;
    FIDEVersionNumber: Integer; // Delphi 2005: 3   -  Delphi 7: 7 - Delphi 2007: 11
    FIDEVersionNumberStr: string;
    FMapCreate: Boolean;
    {$IFDEF MSWINDOWS}
    FJdbgCreate: Boolean;
    FJdbgInsert: Boolean;
    FMapDelete: Boolean;
    {$ENDIF MSWINDOWS}
    FCommandLineTools: TCommandLineTools;
    FPersonalities: TJclBorPersonalities;
    FOutputCallback: TTextHandler;
    function GetSupportsLibSuffix: Boolean;
    function GetBCC32: TJclBCC32;
    function GetDCC32: TJclDCC32;
    function GetBpr2Mak: TJclBpr2Mak;
    function GetMake: IJclCommandLineTool;
    function GetDescription: string;
    function GetEditionAsText: string;
    function GetIdeExeFileName: string;
    function GetGlobals: TStrings;
    function GetIdeExeBuildNumber: string;
    function GetIdePackages: TJclBorRADToolIdePackages;
    function GetIsTurboExplorer: Boolean;
    function GetLatestUpdatePack: Integer;
    function GetPalette: TJclBorRADToolPalette;
    function GetRepository: TJclBorRADToolRepository;
    function GetUpdateNeeded: Boolean;
  protected
    function ProcessMapFile(const BinaryFileName: string): Boolean;

    // compilation functions
    function CompileDelphiPackage(const PackageName, BPLPath, DCPPath: string): Boolean; overload; virtual;
    function CompileDelphiPackage(const PackageName, BPLPath, DCPPath, ExtraOptions: string): Boolean;
      overload; virtual;
    function CompileDelphiProject(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;
    function CompileBCBPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function CompileBCBProject(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;

    // installation (=compilation+registration) / uninstallation(=unregistration+deletion) functions
    function InstallDelphiPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallDelphiPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallBCBPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallBCBPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallDelphiIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallDelphiIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallBCBIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallBCBIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallDelphiExpert(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;
    function UninstallDelphiExpert(const ProjectName, OutputDir: string): Boolean; virtual;
    function InstallBCBExpert(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;
    function UninstallBCBExpert(const ProjectName, OutputDir: string): Boolean; virtual;

    procedure ReadInformation;
    //function AddMissingPathItems(var Path: string; const NewPath: string): Boolean;
    function RemoveFromPath(var Path: string; const ItemsToRemove: string): Boolean;
    function GetDCPOutputPath: string; virtual;
    function GetBPLOutputPath: string; virtual;
    function GetEnvironmentVariables: TStrings; virtual;
    function GetVclIncludeDir: string; virtual;
    function GetName: string; virtual;
    procedure OutputString(const AText: string);
    function OutputFileDelete(const FileName: string): Boolean;
    procedure SetOutputCallback(const Value: TTextHandler); virtual;

    function GetDebugDCUPath: TJclBorRADToolPath; virtual;
    procedure SetDebugDCUPath(const Value: TJclBorRADToolPath); virtual;
    function GetLibrarySearchPath: TJclBorRADToolPath; virtual;
    procedure SetLibrarySearchPath(const Value: TJclBorRADToolPath); virtual;
    function GetLibraryBrowsingPath: TJclBorRADToolPath; virtual;
    procedure SetLibraryBrowsingPath(const Value: TJclBorRADToolPath); virtual;

    function GetValid: Boolean; virtual;
  public
    constructor Create(const AConfigDataLocation: string; ARootKey: Cardinal = 0); virtual;
    
    destructor Destroy; override;
    class procedure ExtractPaths(const Path: TJclBorRADToolPath; List: TStrings);
    class function GetLatestUpdatePackForVersion(Version: Integer): Integer; virtual;
    class function PackageSourceFileExtension: string; virtual;
    class function ProjectSourceFileExtension: string; virtual;
    class function RadToolKind: TJclBorRadToolKind; virtual;
    {class} function RadToolName: string; virtual;
    function AnyInstanceRunning: Boolean;
    function AddToDebugDCUPath(const Path: string): Boolean;
    function AddToLibrarySearchPath(const Path: string): Boolean;
    function AddToLibraryBrowsingPath(const Path: string): Boolean;
    {$IFDEF KYLIX}
    function ConfigFileName(const Extension: string): string; virtual;
    {$ENDIF KYLIX}
    function FindFolderInPath(Folder: string; List: TStrings): Integer;
    // package functions
      // install = package compile + registration
      // uninstall = unregistration + deletion
    function CompilePackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function InstallIDEPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;
    function UninstallIDEPackage(const PackageName, BPLPath, DCPPath: string): Boolean; virtual;

    // project functions
    function CompileProject(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;
    // expert functions
      // install = project compile + registration
      // uninstall = unregistration + deletion
    function InstallExpert(const ProjectName, OutputDir, DcpSearchPath: string): Boolean; virtual;
    function UninstallExpert(const ProjectName, OutputDir: string): Boolean; virtual;

    // registration/unregistration functions
    function RegisterPackage(const BinaryFileName, Description: string): Boolean; overload; virtual;
    function RegisterPackage(const PackageName, BPLPath, Description: string): Boolean; overload; virtual;
    function UnregisterPackage(const BinaryFileName: string): Boolean; overload; virtual;
    function UnregisterPackage(const PackageName, BPLPath: string): Boolean; overload; virtual;
    function RegisterIDEPackage(const BinaryFileName, Description: string): Boolean; overload; virtual;
    function RegisterIDEPackage(const PackageName, BPLPath, Description: string): Boolean; overload; virtual;
    function UnregisterIDEPackage(const BinaryFileName: string): Boolean; overload; virtual;
    function UnregisterIDEPackage(const PackageName, BPLPath: string): Boolean; overload; virtual;
    function RegisterExpert(const BinaryFileName, Description: string): Boolean; overload; virtual;
    function RegisterExpert(const ProjectName, OutputDir, Description: string): Boolean; overload; virtual;
    function UnregisterExpert(const BinaryFileName: string): Boolean; overload; virtual;
    function UnregisterExpert(const ProjectName, OutputDir: string): Boolean; overload; virtual;

    {$IFDEF KEEP_DEPRECATED}
    function IsBDSPersonality: Boolean;
    {$ENDIF KEEP_DEPRECATED}
    function GetDefaultProjectsDir: string; virtual;
    function GetCommonProjectsDir: string; virtual;
    function RemoveFromDebugDCUPath(const Path: string): Boolean;
    function RemoveFromLibrarySearchPath(const Path: string): Boolean;
    function RemoveFromLibraryBrowsingPath(const Path: string): Boolean;
    function SubstitutePath(const Path: string): string;
    {$IFDEF KEEP_DEPRECATED}
    function SupportsBCB: Boolean;
    {$ENDIF KEEP_DEPRECATED}
    function SupportsVisualCLX: Boolean;
    function SupportsVCL: Boolean;
    function LibFolderName: string;
    function ObjFolderName: string;
    // Command line tools
    property CommandLineTools: TCommandLineTools read FCommandLineTools;
    property BCC32: TJclBCC32 read GetBCC32;
    property DCC32: TJclDCC32 read GetDCC32;
    property Bpr2Mak: TJclBpr2Mak read GetBpr2Mak;
    property Make: IJclCommandLineTool read GetMake;
    // Paths
    property BinFolderName: string read FBinFolderName;
    property BPLOutputPath: string read GetBPLOutputPath;
    property DebugDCUPath: TJclBorRADToolPath read GetDebugDCUPath write SetDebugDCUPath;
    property DCPOutputPath: string read GetDCPOutputPath;
    property DefaultProjectsDir: string read GetDefaultProjectsDir;
    property CommonProjectsDir: string read GetCommonProjectsDir;
    //
    property Description: string read GetDescription;
    property Edition: TJclBorRADToolEdition read FEdition;
    property EditionAsText: string read GetEditionAsText;
    property EnvironmentVariables: TStrings read GetEnvironmentVariables;
    property IdePackages: TJclBorRADToolIdePackages read GetIdePackages;
    property IdeTools: TJclBorRADToolIdeTool read FIdeTools;
    property IdeExeBuildNumber: string read GetIdeExeBuildNumber;
    property IdeExeFileName: string read GetIdeExeFileName;
    property InstalledUpdatePack: Integer read FInstalledUpdatePack;
    property LatestUpdatePack: Integer read GetLatestUpdatePack;
    property LibrarySearchPath: TJclBorRADToolPath read GetLibrarySearchPath write SetLibrarySearchPath;
    property LibraryBrowsingPath: TJclBorRADToolPath read GetLibraryBrowsingPath write SetLibraryBrowsingPath;
    {$IFDEF MSWINDOWS}
    property OpenHelp: TJclBorlandOpenHelp read FOpenHelp;
    {$ENDIF MSWINDOWS}
    property MapCreate: Boolean read FMapCreate write FMapCreate;
    {$IFDEF MSWINDOWS}
    property JdbgCreate: Boolean read FJdbgCreate write FJdbgCreate;
    property JdbgInsert: Boolean read FJdbgInsert write FJdbgInsert;
    property MapDelete: Boolean read FMapDelete write FMapDelete;
    {$ENDIF MSWINDOWS}
    property ConfigData: TCustomIniFile read FConfigData;
    property ConfigDataLocation: string read FConfigDataLocation;
    property Globals: TStrings read GetGlobals;
    property Name: string read GetName;
    property Palette: TJclBorRADToolPalette read GetPalette;
    property Repository: TJclBorRADToolRepository read GetRepository;
    property RootDir: string read FRootDir;
    property UpdateNeeded: Boolean read GetUpdateNeeded;
    property Valid: Boolean read GetValid;
    property VclIncludeDir: string read GetVclIncludeDir;
    property IDEVersionNumber: Integer read FIDEVersionNumber;
    property IDEVersionNumberStr: string read FIDEVersionNumberStr;
    property VersionNumber: Integer read FVersionNumber;
    property VersionNumberStr: string read FVersionNumberStr;
    property Personalities: TJclBorPersonalities read FPersonalities;
    {$IFDEF KEEP_DEPRECATED}
    property DCC: TJclDCC32 read GetDCC32;
    {$ENDIF KEEP_DEPRECATED}
    property SupportsLibSuffix: Boolean read GetSupportsLibSuffix;
    property OutputCallback: TTextHandler read FOutputCallback write SetOutputCallback;
    property IsTurboExplorer: Boolean read GetIsTurboExplorer;
    property RootKey: Cardinal read FRootKey;
  end;

  TJclBCBInstallation = class(TJclBorRADToolInstallation)
  protected
    function GetEnvironmentVariables: TStrings; override;
  public
    constructor Create(const AConfigDataLocation: string; ARootKey: Cardinal = 0); override;
    destructor Destroy; override;
    class function PackageSourceFileExtension: string; override;
    class function ProjectSourceFileExtension: string; override;
    class function RadToolKind: TJclBorRadToolKind; override;
    {class }function RadToolName: string; override;
    class function GetLatestUpdatePackForVersion(Version: Integer): Integer; override;
    {$IFDEF KYLIX}
    function ConfigFileName(const Extension: string): string; override;
    {$ENDIF KYLIX}
  end;

  TJclDelphiInstallation = class(TJclBorRADToolInstallation)
  protected
    function GetEnvironmentVariables: TStrings; override;
  public
    constructor Create(const AConfigDataLocation: string; ARootKey: Cardinal = 0); override;
    destructor Destroy; override;
    class function PackageSourceFileExtension: string; override;
    class function ProjectSourceFileExtension: string; override;
    class function RadToolKind: TJclBorRadToolKind; override;
    class function GetLatestUpdatePackForVersion(Version: Integer): Integer; override;
    function InstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean; reintroduce;
    {class }function RadToolName: string; override;
    {$IFDEF KYLIX}
    function ConfigFileName(const Extension: string): string; override;
    {$ENDIF KYLIX}
  end;

  {$IFDEF MSWINDOWS}
  TJclDCCIL = class(TJclDCC32)
  private
    FMaxCLRVersion: string;
  protected
    function GetExeName: string; override;
    function GetMaxCLRVersion: string;
  public
    function MakeProject(const ProjectName, OutputDir, ExtraOptions: string): Boolean; reintroduce;
    procedure SetDefaultOptions; override;
    property MaxCLRVersion: string read GetMaxCLRVersion;
  end;

  TJclBDSInstallation = class(TJclBorRADToolInstallation)
  private
    FDualPackageInstallation: Boolean;
    FHelp2Manager: TJclHelp2Manager;
    FDCCIL: TJclDCCIL;
    FPdbCreate: Boolean;
    procedure SetDualPackageInstallation(const Value: Boolean);
    function GetCppPathsKeyName: string;
    function GetCppBrowsingPath: TJclBorRADToolPath;
    function GetCppSearchPath: TJclBorRADToolPath;
    function GetCppLibraryPath: TJclBorRADToolPath;
    procedure SetCppBrowsingPath(const Value: TJclBorRADToolPath);
    procedure SetCppSearchPath(const Value: TJclBorRADToolPath);
    procedure SetCppLibraryPath(const Value: TJclBorRADToolPath);
    function GetMaxDelphiCLRVersion: string;
    function GetDCCIL: TJclDCCIL;

    function GetMsBuildEnvOptionsFileName: string;
    function GetMsBuildEnvOption(const OptionName: string): string;
    procedure SetMsBuildEnvOption(const OptionName, Value: string);
  protected
    function GetDCPOutputPath: string; override;
    function GetBPLOutputPath: string; override;
    function GetEnvironmentVariables: TStrings; override;
    function CompileDelphiPackage(const PackageName, BPLPath, DCPPath, ExtraOptions: string): Boolean; override;
    function CompileDelphiProject(const ProjectName, OutputDir: string;
      const DcpSearchPath: string): Boolean; override;
    function GetVclIncludeDir: string; override;
    function GetName: string; override;
    procedure SetOutputCallback(const Value: TTextHandler); override;

    function GetDebugDCUPath: TJclBorRADToolPath; override;
    procedure SetDebugDCUPath(const Value: TJclBorRADToolPath); override;
    function GetLibrarySearchPath: TJclBorRADToolPath; override;
    procedure SetLibrarySearchPath(const Value: TJclBorRADToolPath); override;
    function GetLibraryBrowsingPath: TJclBorRADToolPath; override;
    procedure SetLibraryBrowsingPath(const Value: TJclBorRADToolPath); override;

    function GetValid: Boolean; override;
  public
    constructor Create(const AConfigDataLocation: string; ARootKey: Cardinal = 0); override;
    destructor Destroy; override;
    class function PackageSourceFileExtension: string; override;
    class function ProjectSourceFileExtension: string; override;
    class function RadToolKind: TJclBorRadToolKind; override;
    class function GetLatestUpdatePackForVersion(Version: Integer): Integer; override;
    function GetDefaultProjectsDir: string; override;
    function GetCommonProjectsDir: string; override;
    class function GetDefaultProjectsDirectory(const RootDir: string; IDEVersionNumber: Integer): string;
    class function GetCommonProjectsDirectory(const RootDir: string; IDEVersionNumber: Integer): string;
    {class }function RadToolName: string; override;

    function AddToCppSearchPath(const Path: string): Boolean;
    function AddToCppBrowsingPath(const Path: string): Boolean;
    function AddToCppLibraryPath(const Path: string): Boolean;
    function RemoveFromCppSearchPath(const Path: string): Boolean;
    function RemoveFromCppBrowsingPath(const Path: string): Boolean;
    function RemoveFromCppLibraryPath(const Path: string): Boolean;

    property CppSearchPath: TJclBorRADToolPath read GetCppSearchPath write SetCppSearchPath;
    property CppBrowsingPath: TJclBorRADToolPath read GetCppBrowsingPath write SetCppBrowsingPath;
    // Only exists in BDS 5 and upper
    property CppLibraryPath: TJclBorRADToolPath read GetCppLibraryPath write SetCppLibraryPath;

    function RegisterPackage(const BinaryFileName, Description: string): Boolean; override;
    function UnregisterPackage(const BinaryFileName: string): Boolean; override;
    function CleanPackageCache(const BinaryFileName: string): Boolean;

    function CompileDelphiDotNetProject(const ProjectName, OutputDir: string; PEFormat: TJclBorPlatform = bp32bit;
      const CLRVersion: string = ''; const ExtraOptions: string = ''): Boolean;

    property DualPackageInstallation: Boolean read FDualPackageInstallation write SetDualPackageInstallation;
    property Help2Manager: TJclHelp2Manager read FHelp2Manager;
    property DCCIL: TJclDCCIL read GetDCCIL;
    property MaxDelphiCLRVersion: string read GetMaxDelphiCLRVersion;
    property PdbCreate: Boolean read FPdbCreate write FPdbCreate;
  end;
  {$ENDIF MSWINDOWS}

  TTraverseMethod = function(Installation: TJclBorRADToolInstallation): Boolean of object;

  TJclBorRADToolInstallations = class(TObject)
  private
    FList: TObjectList;
    function GetBDSInstallationFromVersion(
      VersionNumber: Integer): TJclBorRADToolInstallation;
    function GetBDSVersionInstalled(VersionNumber: Integer): Boolean;
    function GetCount: Integer;
    function GetInstallations(Index: Integer): TJclBorRADToolInstallation;
    function GetBCBVersionInstalled(VersionNumber: Integer): Boolean;
    function GetDelphiVersionInstalled(VersionNumber: Integer): Boolean;
    function GetBCBInstallationFromVersion(VersionNumber: Integer): TJclBorRADToolInstallation;
    function GetDelphiInstallationFromVersion(VersionNumber: Integer): TJclBorRADToolInstallation;
  protected
    procedure ReadInstallations;
  public
    constructor Create;
    destructor Destroy; override;
    function AnyInstanceRunning: Boolean;
    function AnyUpdatePackNeeded(var Text: string): Boolean;
    function Iterate(TraverseMethod: TTraverseMethod): Boolean;
    property Count: Integer read GetCount;
    property Installations[Index: Integer]: TJclBorRADToolInstallation read GetInstallations; default;
    property BCBInstallationFromVersion[VersionNumber: Integer]: TJclBorRADToolInstallation
      read GetBCBInstallationFromVersion;
    property DelphiInstallationFromVersion[VersionNumber: Integer]: TJclBorRADToolInstallation
      read GetDelphiInstallationFromVersion;
    property BDSInstallationFromVersion[VersionNumber: Integer]: TJclBorRADToolInstallation
      read GetBDSInstallationFromVersion;
    property BCBVersionInstalled[VersionNumber: Integer]: Boolean read GetBCBVersionInstalled;
    property DelphiVersionInstalled[VersionNumber: Integer]: Boolean read GetDelphiVersionInstalled;
    property BDSVersionInstalled[VersionNumber: Integer]: Boolean read GetBDSVersionInstalled;
  end;

{$IFDEF KEEP_DEPRECATED}
function BPLFileName(const BPLPath, PackageFileName: string): string;
{$ENDIF KEEP_DEPRECATE}
function BinaryFileName(const OutputPath, ProjectFileName: string): string;

function IsDelphiPackage(const FileName: string): Boolean;
function IsDelphiProject(const FileName: string): Boolean;
function IsBCBPackage(const FileName: string): Boolean;
function IsBCBProject(const FileName: string): Boolean;

procedure GetDPRFileInfo(const DPRFileName: string; out BinaryExtension: string;
  const LibSuffix: PString = nil);
procedure GetBPRFileInfo(const BPRFileName: string; out BinaryFileName: string;
  const Description: PString = nil);
procedure GetDPKFileInfo(const DPKFileName: string; out RunOnly: Boolean;
  const LibSuffix: PString = nil; const Description: PString = nil);
procedure GetBPKFileInfo(const BPKFileName: string; out RunOnly: Boolean;
  const BinaryFileName: PString = nil; const Description: PString = nil);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclBorlandTools.pas $';
    Revision: '$Revision: 2397 $';
    Date: '$Date: 2008-07-20 22:13:14 +0200 (dim., 20 juil. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysConst,
  {$IFDEF MSWINDOWS}
  Registry,
  JclRegistry,
  JclDebug,
  {$ENDIF MSWINDOWS}
  {$IFDEF HAS_UNIT_LIBC}
  Libc,
  {$ENDIF HAS_UNIT_LIBC}
  JclFileUtils, JclLogic, JclResources, JclStrings, JclWideStrings, JclSysInfo, JclSimpleXml;

// Internal

type
  TUpdatePack = record
    Version: Byte;
    LatestUpdatePack: Integer;
  end;
  {$IFDEF KYLIX}
  TKylixVersion = 1..3;
  {$ENDIF KYLIX}

  {$IFDEF MSWINDOWS}
  TBDSVersionInfo = record
    Name: string;
    VersionStr: string;
    Version: Integer;
    CoreIdeVersion: string;
    Supported: Boolean;
  end;
  {$ENDIF MSWINDOWS}

const
  {$IFDEF MSWINDOWS}
  {$IFNDEF RTL140_UP}
  PathSep = ';';
  {$ENDIF ~RTL140_UP}

  MSHelpSystemKeyName = '\SOFTWARE\Microsoft\Windows\Help';

  BCBKeyName          = '\SOFTWARE\Borland\C++Builder';
  BDSKeyName          = '\SOFTWARE\Borland\BDS';
  DelphiKeyName       = '\SOFTWARE\Borland\Delphi';

  BDSVersions: array [1..5] of TBDSVersionInfo = (
    (
      Name: RsCSharpName;
      VersionStr: '1.0';
      Version: 1;
      CoreIdeVersion: '71';
      Supported: True),
    (
      Name: RsDelphiName;
      VersionStr: '8';
      Version: 8;
      CoreIdeVersion: '71';
      Supported: True),
    (
      Name: RsDelphiName;
      VersionStr: '2005';
      Version: 9;
      CoreIdeVersion: '90';
      Supported: True),
    (
      Name: RsBDSName;
      VersionStr: '2006';
      Version: 10;
      CoreIdeVersion: '100';
      Supported: True),
    (
      Name: RsRSName;
      VersionStr: '2007';
      Version: 11;
      CoreIdeVersion: '100';
      Supported: True)
  );
  {$ENDIF MSWINDOWS}

  {$IFDEF KYLIX}
  RootDirValueName           = 'DelphiRoot';
  {$ELSE}
  RootDirValueName           = 'RootDir';
  {$ENDIF KYLIX}

  EditionValueName           = 'Edition';
  VersionValueName           = 'Version';

  DebuggingKeyName           = 'Debugging';
  DebugDCUPathValueName      = 'Debug DCUs Path';

  GlobalsKeyName             = 'Globals';

  LibraryKeyName             = 'Library';
  LibrarySearchPathValueName = 'Search Path';
  LibraryBrowsingPathValueName = 'Browsing Path';
  LibraryBPLOutputValueName  = 'Package DPL Output';
  LibraryDCPOutputValueName  = 'Package DCP Output';
  BDSDebugDCUPathValueName   = 'Debug DCU Path';

  CppPathsKeyName            = 'CppPaths';
  CppPathsV5UpperKeyName     = 'C++\Paths';
  CppBrowsingPathValueName   = 'BrowsingPath';
  CppSearchPathValueName     = 'SearchPath';
  CppLibraryPathValueName    = 'LibraryPath';

  TransferKeyName            = 'Transfer';
  TransferCountValueName     = 'Count';
  TransferPathValueName      = 'Path%d';
  TransferParamsValueName    = 'Params%d';
  TransferTitleValueName     = 'Title%d';
  TransferWorkDirValueName   = 'WorkingDir%d';

  DisabledPackagesKeyName    = 'Disabled Packages';
  EnvVariablesKeyName        = 'Environment Variables';
  EnvVariableBDSValueName    = 'BDS';
  EnvVariableBDSPROJDIRValueName = 'BDSPROJECTSDIR';
  EnvVariableBDSCOMDIRValueName = 'BDSCOMMONDIR';
  KnownPackagesKeyName       = 'Known Packages';
  KnownIDEPackagesKeyName    = 'Known IDE Packages';
  ExpertsKeyName             = 'Experts';
  PackageCacheKeyName        = 'Package Cache';

  PaletteKeyName             = 'Palette';
  PaletteHiddenTag           = '.Hidden';

  ConfigurationExtension     = '.cfg';
  {$IFDEF MSWINDOWS}
  AsmExeName                 = 'tasm32.exe';
  BCC32ExeName               = 'bcc32.exe';
  DCC32ExeName               = 'dcc32.exe';
  DCCILExeName               = 'dccil.exe';
  Bpr2MakExeName             = 'bpr2mak.exe';
  MakeExeName                = 'make.exe';
  DelphiOptionsFileExtension = '.dof';
  {$IFDEF BCB}
  BorRADToolRepositoryFileName = 'bcb.dro';
  {$ELSE BCB}
  BorRADToolRepositoryFileName = 'delphi32.dro';
  {$ENDIF BCB}
  HelpContentFileName        = '%s\Help\%s%d.ohc';
  HelpIndexFileName          = '%s\Help\%s%d.ohi';
  HelpLinkFileName           = '%s\Help\%s%d.ohl';
  HelpProjectFileName        = '%s\Help\%s%d.ohp';
  HelpGidFileName            = '%s\Help\%s%d.gid';      
  {$ENDIF MSWINDOWS}

  {$IFDEF KYLIX}
  IDs: array [TKylixVersion] of Integer = (60, 65, 69);
  LibSuffixes: array [TKylixVersion] of string[3] = ('6.0', '6.5', '6.9');

  BCC32ExeName               = 'bc++';
  DCC32ExeName               = 'dcc';
  Bpr2MakExeName             = 'bpr2mak';
  MakeExeName                = 'make';

  DelphiIdeExeName           = 'delphi';
  BCBIdeExeName              = 'bcblin';
  DelphiOptionsFileExtension = '.kof';

  KylixHelpNamePart          = 'k%d';
  {$ENDIF KYLIX}

  DelphiLibSuffixOption   = '{$LIBSUFFIX ''';
  DelphiDescriptionOption = '{$DESCRIPTION ''';
  DelphiRunOnlyOption     = '{$RUNONLY}';
  DelphiBinaryExtOption   = '{$E ';
  BCBLFlagsOption     = '<LFLAGS ';
  BCBDSwitchOption    = '-D';
  BCBLibSuffixOption  = 'LibSuffix=';
  BCBGprSwitchOption  = '-Gpr';
  BCBProjectOption    = '<PROJECT ';

  // BDSProj options
  BDSProjPersonalityInfoNodeName = 'PersonalityInfo';
  BDSProjOptionNodeName = 'Option';
  BDSProjNameProperty = 'Name';
  BDSProjPersonalityValue = 'Personality';
  BDSProjUnitOutputDirValue = 'UnitOutputDir';
  BDSProjSearchPathValue = 'SearchPath';
  BDSProjPackagesValue = 'Packages';
  BDSProjConditionalsValue = 'Conditionals';
  BDSProjUsePackagesValue = 'UsePackages';
  BDSProjDirectoriesNodeName = 'Directories';

  // DProj options
  DProjProjectExtensionsNodeName = 'ProjectExtensions';
  DProjPersonalityNodeName = 'Borland.Personality';
  DProjDelphiPersonalityValue = 'Delphi.Personality';
  DProjDelphiDotNetPersonalityValue = 'DelphiDotNet.Personality';
  DProjPropertyGroupNodeName = 'PropertyGroup';
  DProjConditionValueName = 'Condition';
  DProjUsePackageNodeName = 'DCC_UsePackage';
  DProjDcuOutputDirNodeName = 'DCC_DcuOutput';
  DProjUnitSearchPathNodeName = 'DCC_UnitSearchPath';
  DProjDefineNodeName = 'DCC_Define';
  DProjConfigurationNodeName = 'Configuration';
  DProjPlatformNodeName = 'Platform';

  // MsBuild options
  MsBuildWin32DCPOutputNodeName = 'Win32DCPOutput';
  MsBuildWin32LibraryPathNodeName = 'Win32LibraryPath';
  MsBuildWin32BrowsingPathNodeName = 'Win32BrowsingPath';
  MsBuildWin32DebugDCUPathNodeName = 'Win32DebugDCUPath';
  MsBuildWin32DLLOutputPathNodeName = 'Win32DLLOutputPath';
  MsBuildCBuilderBPLOutputPathNodeName = 'CBuilderBPLOutputPath';
  MsBuildCBuilderBrowsingPathNodeName = 'CBuilderBrowsingPath';
  MsBuildCBuilderLibraryPathNodeName = 'CBuilderLibraryPath';
  MsBuildPropertyGroupNodeName = 'PropertyGroup';

function AnsiStartsText(const SubStr, S: string): Boolean;
begin
  if Length(SubStr) <= Length(S) then
    Result := AnsiStrLIComp(PChar(S), PChar(SubStr), Length(SubStr)) = 0
  else
    Result := False;
end;

procedure GetDPRFileInfo(const DPRFileName: string; out BinaryExtension: string;
  const LibSuffix: PString = nil);
var
  Index: Integer;
  S: string;
  DPRFile: TStrings;
const
  ProgramText = 'program';
  LibraryText = 'library';
begin
  DPRFile := TStringList.Create;
  try
    DPRFile.LoadFromFile(DPRFileName);                              

    if Assigned(LibSuffix) then
      LibSuffix^ := '';

    BinaryExtension := '';

    for Index := 0 to DPRFile.Count - 1 do
    begin
      S := TrimRight(DPRFile.Strings[Index]);
      if AnsiStartsText(ProgramText, S) and (BinaryExtension = '') then
        BinaryExtension := BinaryExtensionExecutable;
      if AnsiStartsText(LibraryText, S) and (BinaryExtension = '') then
        BinaryExtension := BinaryExtensionLibrary;
      if AnsiStartsText(DelphiBinaryExtOption, S) then
        BinaryExtension :=
          StrTrimQuotes(Copy(S, Length(DelphiBinaryExtOption), Length(S) - Length(DelphiBinaryExtOption)));
      if Assigned(LibSuffix) and AnsiStartsText(DelphiLibSuffixOption, S) then
        LibSuffix^ :=
          StrTrimQuotes(Copy(S, Length(DelphiLibSuffixOption), Length(S) - Length(DelphiLibSuffixOption)));
    end;
  finally
    DPRFile.Free;
  end;
end;

procedure GetBPRFileInfo(const BPRFileName: string; out BinaryFileName: string;
  const Description: PString = nil);
var
  I, J: Integer;
  S, SubS1, SubS2, SubS3: string;
  BPKFile: TStringList;
  LProjectPos, BinaryFileNamePos, EndFileNamePos, LFlagsPos, DSwitchPos: Integer;
  SemiColonPos, AmpPos: Integer;
begin
  BPKFile := TStringList.Create;
  try
    BPKFile.LoadFromFile(BPRFileName);
    BinaryFileName := '';
    if Assigned(Description) then
      Description^ := '';
    for I := 0 to BPKFile.Count - 1 do
    begin
      S := BPKFile[I];

      LProjectPos := Pos(BCBProjectOption, S);
      if LProjectPos > 0 then
      begin
        SubS1 := Copy(S, LProjectPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;

        BinaryFileNamePos := Pos('"', SubS1);
        if BinaryFileNamePos > 0 then
        begin
          SubS2 := Copy(SubS1, BinaryFileNamePos + 1, Length(SubS1) - BinaryFileNamePos);
          EndFileNamePos := Pos('"', SubS2);

          if EndFileNamePos > 0 then
            BinaryFileName := Copy(SubS2, 1, EndFileNamePos - 1);
        end;
      end;

      LFlagsPos := Pos(BCBLFlagsOption, S);
      if LFlagsPos > 0 then
      begin
        SubS1 := Copy(S, LFlagsPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;
        DSwitchPos := Pos(BCBDSwitchOption, SubS1);
        if DSwitchPos > 0 then
        begin
          SubS2 := Copy(SubS1, DSwitchPos, Length(SubS1));
          SemiColonPos := Pos(';', SubS2);
          if SemiColonPos > 0 then
          begin
            SubS3 := Copy(SubS2, SemiColonPos + 1, Length(SubS2));
            AmpPos := Pos('&', SubS3);
            if (Description <> nil) and (AmpPos > 0) then
              Description^ := Copy(SubS3, 1, AmpPos - 1);
          end;
        end;
      end;
    end;
  finally
    BPKFile.Free;
  end;
end;

procedure GetDPKFileInfo(const DPKFileName: string; out RunOnly: Boolean;
  const LibSuffix: PString = nil; const Description: PString = nil);
var
  I: Integer;
  S: string;
  DPKFile: TStringList;
begin
  DPKFile := TStringList.Create;
  try
    DPKFile.LoadFromFile(DPKFileName);
    if Assigned(Description) then
      Description^ := '';
    if Assigned(LibSuffix) then
      LibSuffix^ := '';
    RunOnly := False;
    for I := 0 to DPKFile.Count - 1 do
    begin
      S := TrimRight(DPKFile.Strings[I]);
      if Assigned(Description) and (Pos(DelphiDescriptionOption, S) = 1) then
        Description^ := Copy(S, Length(DelphiDescriptionOption), Length(S) - Length(DelphiDescriptionOption))
      else
      if Assigned(LibSuffix) and (Pos(DelphiLibSuffixOption, S) = 1) then
        LibSuffix^ := StrTrimQuotes(Copy(S, Length(DelphiLibSuffixOption), Length(S) - Length(DelphiLibSuffixOption)))
      else
      if Pos(DelphiRunOnlyOption, S) = 1 then
        RunOnly := True;
    end;
  finally
    DPKFile.Free;
  end;
end;

procedure GetBPKFileInfo(const BPKFileName: string; out RunOnly: Boolean;
  const BinaryFileName: PString = nil; const Description: PString = nil);
var
  I, J: Integer;
  S, SubS1, SubS2, SubS3: string;
  BPKFile: TStringList;
  LFlagsPos, DSwitchPos, SemiColonPos, AmpPos, GprPos: Integer;
  LProjectPos, BinaryFileNamePos, EndFileNamePos: Integer;
begin
  BPKFile := TStringList.Create;
  try
    BPKFile.LoadFromFile(BPKFileName);
    if Assigned(Description) then
      Description^ := '';
    if Assigned(BinaryFileName) then
      BinaryFileName^ := '';
    RunOnly := False;
    for I := 0 to BPKFile.Count - 1 do
    begin
      S := BPKFile[I];

      LProjectPos := Pos(BCBProjectOption, S);
      if Assigned(BinaryFileName) and (LProjectPos > 0) then
      begin
        SubS1 := Copy(S, LProjectPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;

        BinaryFileNamePos := Pos('"', SubS1);
        if BinaryFileNamePos > 0 then
        begin
          SubS2 := Copy(SubS1, BinaryFileNamePos + 1, Length(SubS1) - BinaryFileNamePos);
          EndFileNamePos := Pos('"', SubS2);

          if EndFileNamePos > 0 then
            BinaryFileName^ := Copy(SubS2, 1, EndFileNamePos - 1);
        end;
      end;

      LFlagsPos := Pos(BCBLFlagsOption, S);
      if LFlagsPos > 0 then
      begin
        SubS1 := Copy(S, LFlagsPos, Length(S));
        J := 1;
        while (Pos('>', SubS1) = 0) and ((I + J) < BPKFile.Count) do
        begin
          SubS1 := SubS1 + BPKFile[I + J];
          Inc(J);
        end;
        DSwitchPos := Pos(BCBDSwitchOption, SubS1);
        GprPos := Pos(BCBGprSwitchOption, SubS1);
        if DSwitchPos > 0 then
        begin
          SubS2 := Copy(SubS1, DSwitchPos, Length(SubS1));
          SemiColonPos := Pos(';', SubS2);
          if SemiColonPos > 0 then
          begin
            SubS3 := Copy(SubS2, SemiColonPos + 1, Length(SubS2));
            AmpPos := Pos('&', SubS3);
            if (Description <> nil) and (AmpPos > 0) then
              Description^ := Copy(SubS3, 1, AmpPos - 1);
          end;
        end;
        if GprPos > 0 then
          RunOnly := True;
      end;
    end;
  finally
    BPKFile.Free;
  end;
end;

function BPLFileName(const BPLPath, PackageFileName: string): string;
var
  PackageExtension, LibSuffix: string;
  RunOnly: Boolean;
begin
  PackageExtension := ExtractFileExt(PackageFileName);
  if SameText(PackageExtension, SourceExtensionDelphiPackage) then
  begin
    GetDPKFileInfo(PackageFileName, RunOnly, @LibSuffix);
    Result := PathExtractFileNameNoExt(PackageFileName) + LibSuffix + BinaryExtensionPackage;
  end
  else
  if SameText(PackageExtension, SourceExtensionBCBPackage) then
    GetBPKFileInfo(PackageFileName, RunOnly, @Result)
  else
    raise EJclBorRadException.CreateResFmt(@RsEUnknownPackageExtension, [PackageExtension]);

  Result := PathAddSeparator(BPLPath) + Result;
end;

function BinaryFileName(const OutputPath, ProjectFileName: string): string;
var
  ProjectExtension, LibSuffix, BinaryExtension: string;
  RunOnly: Boolean;
begin
  ProjectExtension := ExtractFileExt(ProjectFileName);
  if SameText(ProjectExtension, SourceExtensionDelphiPackage) then
  begin
    GetDPKFileInfo(ProjectFileName, RunOnly, @LibSuffix);
    Result := PathExtractFileNameNoExt(ProjectFileName) + LibSuffix + BinaryExtensionPackage;
  end
  else
  if SameText(ProjectExtension, SourceExtensionDelphiProject) then
  begin
    GetDPRFileInfo(ProjectFileName, BinaryExtension, @LibSuffix);
    Result := PathExtractFileNameNoExt(ProjectFileName) + LibSuffix + BinaryExtension;
  end
  else
  if SameText(ProjectExtension, SourceExtensionBCBPackage) then
    GetBPKFileInfo(ProjectFileName, RunOnly, @Result)
  else
  if SameText(ProjectExtension, SourceExtensionBCBProject) then
    GetBPRFileInfo(ProjectFileName, Result)
  else
    raise EJclBorRadException.CreateResFmt(@RsEUnknownProjectExtension, [ProjectExtension]);

  Result := PathAddSeparator(OutputPath) + Result;
end;

function IsDelphiPackage(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionDelphiPackage);
  { TODO : Add some plausibility tests }
  { like
  var
    F: TextFile;
    FirstLine: string;

  if FileExists(FileName) then
  begin
    AssignFile(F, FileName);
    Reset(F);
    ReadLn(F, FirstLine);
    Result := Pos('package ', FirstLine) = 1;
    CloseFile(F);
  end;
  }
end;

function IsDelphiProject(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionDelphiProject);
end;

function IsBCBPackage(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionBCBPackage);
end;

function IsBCBProject(const FileName: string): Boolean;
begin
  Result := SameText(ExtractFileExt(FileName), SourceExtensionBCBProject);
end;

{$IFDEF MSWINDOWS}

type
  TFindResStartRec = record
    StartStr: WideString;
    MatchStr: WideString;
  end;
  PFindResStartRec = ^TFindResStartRec;

// helper function to check strings starting "StartStr" in current string table
function FindResStartCallBack(hModule: HMODULE; lpszType, lpszName: PChar;
  lParam: PFindResStartRec): BOOL; stdcall;
var
  ResInfo, ResHData, ResSize, ResIndex: Cardinal;
  ResData: PWord;
  StrLength: Word;
  MatchLen: Integer;
begin
  Result := True;
  MatchLen := Length(lParam^.StartStr);

  ResInfo := FindResource(hModule, lpszName, lpszType);
  if ResInfo <> 0 then
  begin
    ResHData := LoadResource(hModule, ResInfo);
    if ResHData <> 0 then
    begin
      ResData := LockResource(ResHData);
      if Assigned(ResData) then
      begin
        // string tables are a concatenation of maximum 16 prefixed-length widestrings
        ResSize := SizeofResource(hModule, ResInfo) div 2;
        ResIndex := 0;
        // iterate all concatenated strings
        while ResIndex < ResSize do
        begin
          StrLength := ResData^;
          Inc(ResData);
          Inc(ResIndex);
          if (StrLength >= MatchLen) and
            (StrLICompW(PWideChar(lParam^.StartStr), PWideChar(ResData), MatchLen) = 0) then
          begin
            // we have a match
            SetLength(lParam^.MatchStr, StrLength);
            Move(ResData^, lParam^.MatchStr[1], StrLength * SizeOf(lParam^.MatchStr[1]));
            Result := False;
            Break;
          end;
          Inc(ResData, StrLength);
          Inc(ResIndex, StrLength);
        end;
      end;
    end;
  end;
end;

// find in specified module "FileName" a resourcestring starting with StartStr
function FindResStart(const FileName: string; const StartStr: WideString): WideString;
var
  H: HMODULE;
  FindResRec: TFindResStartRec;
begin
  FindResRec.StartStr := StartStr;
  FindResRec.MatchStr := '';

  H := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
  if H <> 0 then
    try
      EnumResourceNames(H, RT_STRING, @FindResStartCallBack, Integer(@FindResRec));
    finally
      FreeLibrary(H);
    end;

  Result := FindResRec.MatchStr;
end;

type
  WideStringArray = array of WideString;

  TLoadResRec = record
    EnglishStr: WideStringArray;
    ResId: array of Integer;
  end;
  PLoadResRec = ^TLoadResRec;

// helper function to find strings in current string table
function LoadResCallBack(hModule: HMODULE; lpszType, lpszName: PChar;
  lParam: PLoadResRec): BOOL; stdcall;
var
  ResInfo, ResHData, ResSize, ResIndex: Cardinal;
  ResData: PWord;
  StrLength: Word;
  StrIndex, ResOffset, MatchCount, MatchLen: Integer;
begin
  Result := True;
  MatchCount := 0;

  ResInfo := FindResource(hModule, lpszName, lpszType);
  if ResInfo <> 0 then
  begin
    ResHData := LoadResource(hModule, ResInfo);
    if ResHData <> 0 then
    begin
      ResData := LockResource(ResHData);
      if Assigned(ResData) then
      begin
        ResSize := SizeofResource(hModule, ResInfo) div 2;
        ResIndex := 0;
        ResOffset := 0;
        while ResIndex < ResSize do
        begin
          StrLength := ResData^;
          Inc(ResData);
          Inc(ResIndex);
          // for each requested strings
          for StrIndex := Low(lParam^.EnglishStr) to High(lParam^.EnglishStr) do
          begin
            MatchLen := Length(lParam^.EnglishStr[StrIndex]);
            if (lParam^.ResId[StrIndex] = 0) and (StrLength = MatchLen)
              and (StrLICompW(PWideChar(lParam^.EnglishStr[StrIndex]), PWideChar(ResData), MatchLen) = 0) then
            begin // http://support.microsoft.com/kb/q196774/
              lParam^.ResId[StrIndex] := (PWord(@lpszName)^ - 1) * 16 + ResOffset;
              Inc(MatchCount);
              if MatchCount = Length(lParam^.EnglishStr) then
              begin
                Result := False;
                Break; // all requests were translated to ResId
              end;
            end;
          end;
          Inc(ResOffset);
          Inc(ResData, StrLength);
          Inc(ResIndex, StrLength);
        end;
      end;
    end;
  end;
end;

function LoadResStrings(const BaseBinName: string;
  const ResEn: array of WideString): WideStringArray;
var
  H: HMODULE;
  LocaleName: array [0..4] of Char;
  FileName: string;
  Index, NbRes: Integer;
  LoadResRec: TLoadResRec;
begin
  NbRes := Length(ResEn);
  SetLength(LoadResRec.EnglishStr, NbRes);
  SetLength(LoadResRec.ResId, NbRes);
  SetLength(Result, NbRes);

  for Index := Low(ResEn) to High(ResEn) do
    LoadResRec.EnglishStr[Index] := ResEn[Index];

  H := LoadLibraryEx(PChar(ChangeFileExt(BaseBinName, BinaryExtensionPackage)), 0,
    LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
  if H <> 0 then
    try
      EnumResourceNames(H, RT_STRING, @LoadResCallBack, Integer(@LoadResRec));
    finally
      FreeLibrary(H);
    end;

  FileName := '';

  FillChar(LocaleName, SizeOf(LocaleName[0]), 0);
  GetLocaleInfo(GetThreadLocale, LOCALE_SABBREVLANGNAME, LocaleName, SizeOf(LocaleName));
  if LocaleName[0] <> #0 then
  begin
    FileName := BaseBinName;
    if FileExists(FileName + LocaleName) then
      FileName := FileName + LocaleName
    else
    begin
      LocaleName[2] := #0;
      if FileExists(FileName + LocaleName) then
        FileName := FileName + LocaleName
      else
        FileName := '';
    end;
  end;

  if FileName <> '' then
  begin
    H := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_DATAFILE or DONT_RESOLVE_DLL_REFERENCES);
    if H <> 0 then
      try
        for Index := 0 to NbRes - 1 do
        begin
          SetLength(Result[Index], 1024);
          SetLength(Result[Index],
            LoadStringW(H, LoadResRec.ResId[Index], PWideChar(Result[Index]), Length(Result[Index]) - 1));
        end;
      finally
        FreeLibrary(H);
      end;
  end
  else
    Result := LoadResRec.EnglishStr;
end;

function RegGetValueNamesAndValues(const RootKey: HKEY; const Key: string; const List: TStrings): Boolean;
var
  I: Integer;
  TempList: TStringList;
  Name: string;
  DataType: DWORD;
begin
  TempList := TStringList.Create;
  try
    Result := RegKeyExists(RootKey, Key) and RegGetValueNames(RootKey, Key, TempList);
    if Result then
    begin
      for I := 0 to TempList.Count - 1 do
      begin
        Name := TempList[I];
        if RegGetDataType(RootKey, Key, Name, DataType) and
          ((DataType = REG_SZ) or (DataType = REG_EXPAND_SZ) or (DataType = REG_BINARY)) then
          TempList[I] := Name + '=' + RegReadStringDef(RootKey, Key, Name, '');
      end;
      List.AddStrings(TempList);
    end;
  finally
    TempList.Free;
  end;
end;
{$ENDIF MSWINDOWS}

//=== { TJclBorRADToolInstallationObject } ===================================

constructor TJclBorRADToolInstallationObject.Create(AInstallation: TJclBorRADToolInstallation);
begin
  FInstallation := AInstallation;
end;

{$IFDEF MSWINDOWS}

//=== { TJclBorlandOpenHelp } ================================================

function TJclBorlandOpenHelp.AddHelpFile(const HelpFileName, IndexName: string): Boolean;
var
  CntFileName, HelpName, CntName: string;
  List: TStringList;

  procedure AddToList(const FileName, Text: string);
  var
    I, Attr: Integer;
    Found: Boolean;
  begin
    List.LoadFromFile(FileName);
    Found := False;
    for I := 0 to List.Count - 1 do
      if AnsiSameText(Trim(List[I]), Text) then
      begin
        Found := True;
        Break;
      end;
    if not Found then
    begin
      List.Add(Text);
      Attr := FileGetAttr(FileName);
      FileSetAttr(FileName, faArchive);
      List.SaveToFile(FileName);
      FileSetAttr(FileName, Attr);
    end;
  end;

begin
  CntFileName := ChangeFileExt(HelpFileName, '.cnt');
  Result := FileExists(HelpFileName) and FileExists(CntFileName);
  if Result then
  begin
    HelpName := ExtractFileName(HelpFileName);
    CntName := ExtractFileName(CntFileName);
    RegWriteString(HKEY_LOCAL_MACHINE, MSHelpSystemKeyName, HelpName, ExtractFilePath(HelpFileName));
    RegWriteString(HKEY_LOCAL_MACHINE, MSHelpSystemKeyName, CntName, ExtractFilePath(CntFileName));
    List := TStringList.Create;
    try
      AddToList(ContentFileName, Format(':Include %s', [CntName]));
      AddToList(LinkFileName, Format(':Link %s', [HelpName]));
      AddToList(IndexFileName, Format(':Index %s=%s', [IndexName, HelpName]));
      SetFileLastWrite(ProjectFileName, Now);
      FileDelete(GidFileName);
    finally
      List.Free;
    end;
  end;
end;

function TJclBorlandOpenHelp.GetContentFileName: string;
begin
  Result := ReadFileName(HelpContentFileName);
end;

function TJclBorlandOpenHelp.GetGidFileName: string;
begin
  Result := ReadFileName(HelpGidFileName);
end;

function TJclBorlandOpenHelp.GetIndexFileName: string;
begin
  Result := ReadFileName(HelpIndexFileName);
end;

function TJclBorlandOpenHelp.GetLinkFileName: string;
begin
  Result := ReadFileName(HelpLinkFileName);
end;

function TJclBorlandOpenHelp.GetProjectFileName: string;
begin
  Result := ReadFileName(HelpProjectFileName);
end;

function TJclBorlandOpenHelp.ReadFileName(const FormatName: string): string;
var
  S: string;
begin
  with Installation do
  begin
    case RadToolKind of
      brDelphi:
        if VersionNumber <= 6 then
          S := 'delphi'
        else
          S := 'd';
      brCppBuilder:
        S := 'bcb';
      else
      //brBorlandDevStudio :
        raise EJclBorRadException.CreateRes(@RsENoOpenHelp);
    end;
    Result := Format(FormatName, [RootDir, S, VersionNumber]);
  end;
end;

function TJclBorlandOpenHelp.RemoveHelpFile(const HelpFileName, IndexName: string): Boolean;
var
  CntFileName, HelpName, CntName: string;
  List: TStringList;

  procedure RemoveFromList(const FileName, Text: string);
  var
    I, Attr: Integer;
    Found: Boolean;
  begin
    List.LoadFromFile(FileName);
    Found := False;
    for I := 0 to List.Count - 1 do
      if AnsiSameText(Trim(List[I]), Text) then
      begin
        Found := True;
        List.Delete(I);
        Break;
      end;
    if Found then
    begin
      Attr := FileGetAttr(FileName);
      FileSetAttr(FileName, faArchive);
      List.SaveToFile(FileName);
      FileSetAttr(FileName, Attr);
    end;
  end;

begin
  CntFileName := ChangeFileExt(HelpFileName, '.cnt');
  Result := FileExists(HelpFileName) and FileExists(CntFileName);
  if Result then
  begin
    HelpName := ExtractFileName(HelpFileName);
    CntName := ExtractFileName(CntFileName);
    //RegDeleteEntry(HKEY_LOCAL_MACHINE, MSHelpSystemKeyName, HelpName);
    //RegDeleteEntry(HKEY_LOCAL_MACHINE, MSHelpSystemKeyName, CntName);
    List := TStringList.Create;
    try
      RemoveFromList(ContentFileName, Format(':Include %s', [CntName]));
      RemoveFromList(LinkFileName, Format(':Link %s', [HelpName]));
      RemoveFromList(IndexFileName, Format(':Index %s=%s', [IndexName, HelpName]));
      SetFileLastWrite(ProjectFileName, Now);
      FileDelete(GidFileName);
    finally
      List.Free;
    end;
  end;
end;

//== { TJclHelp2Manager } ====================================================

const
  Help2BorlandNameSpace = 'Borland.BDS%d';
  Help2DefaultKeyWord   = '_DEFAULT';

constructor TJclHelp2Manager.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FHxRegisterSession := nil;
  FHxRegister := nil;
  FHxPlugin := nil;
  if Assigned(Installation) then
    FIdeNameSpace := Format(Help2BorlandNameSpace, [Installation.IDEVersionNumber])
  else
    FIdeNameSpace := '';
end;

constructor TJclHelp2Manager.Create;
begin
  Create(nil);
end;

destructor TJclHelp2Manager.Destroy;
begin
  FHxRegisterSession := nil;
  FHxRegister := nil;
  FHxPlugin := nil;
  inherited Destroy;
end;

function TJclHelp2Manager.CommitTransaction: Boolean;
begin
  Result := RequireObject([hoRegisterSession]);
  if Result then
  begin
    try
      FHxRegisterSession.CommitTransaction;
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.CreateTransaction: Boolean;
begin
  Result := RequireObject([hoRegisterSession]);
  if Result then
  begin
    try
      FHxRegisterSession.CreateTransaction('');
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.GetHxPlugin: IHxPlugin;
begin
  RequireObject([hoPlugin]);
  Result := FHxPlugin;
end;

function TJclHelp2Manager.GetHxRegister: IHxRegister;
begin
  RequireObject([hoRegister]);
  Result := FHxRegister;
end;

function TJclHelp2Manager.GetHxRegisterSession: IHxRegisterSession;
begin
  RequireObject([hoRegisterSession]);
  Result := FHxRegisterSession;
end;

function TJclHelp2Manager.PlugNameSpaceIn(const SourceNameSpace, TargetNameSpace: WideString): Boolean;
var
  Help2Default: WideString;
begin
  Result := RequireObject([hoPlugin]);
  if Result then
  begin
    try
      Help2Default := Help2DefaultKeyWord;
      FHxPlugin.RegisterHelpPlugIn(TargetNameSpace, Help2Default,
        SourceNameSpace, Help2Default, '', 0);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.PlugNameSpaceInBorlandHelp(
  const NameSpace: WideString): Boolean;
begin
  Result := Assigned(FInstallation) and (Installation.RadToolKind = brBorlandDevStudio) and
    PlugNameSpaceIn(NameSpace, IdeNamespace);
end;

function TJclHelp2Manager.RegisterHelpFile(const NameSpace, Identifier: WideString;
  const LangId: Integer; const HxSFile, HxIFile: WideString): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RegisterHelpFileSet(NameSpace, Identifier, LangId, HxSFile,
        HxIFile, '', '', 0, 0, 0, 0);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.RegisterNameSpace(const Name, Collection, Description: WideString): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RegisterNamespace(Name, Collection, Description);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.RequireObject(HelpObjects: TJclHelp2Objects): Boolean;
begin
  // dependencies
  if (hoRegister in HelpObjects) or (hoPlugin in HelpObjects) then
    Include(HelpObjects, hoRegisterSession);

  Result := True;

  if (hoRegisterSession in HelpObjects) and not Assigned(FHxRegisterSession) then
  begin
    try
      FHxRegisterSession := CoHxRegisterSession.Create;
    except
      Result := False;
    end;
  end;

  if Result and (hoRegister in HelpObjects) and not Assigned(FHxRegister) then
  begin
    try
      Result := Supports(FHxRegisterSession.GetRegistrationObject(HxRegisterSession_IHxRegister),
        IHxRegister, FHxRegister);
    except
      Result := False;
    end;
  end;

  if Result and (hoPlugin in HelpObjects) and not Assigned(FHxPlugin) then
  begin
    try
      Result := Supports(FHxRegisterSession.GetRegistrationObject(HxRegisterSession_IHxPlugIn),
        IHxPlugin, FHxPlugin);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.UnPlugNameSpace(const SourceNameSpace, TargetNameSpace: WideString): Boolean;
var
  Help2Default: WideString;
begin
  Result := RequireObject([hoPlugin]);
  if Result then
  begin
    try
      Help2Default := Help2DefaultKeyWord;
      FHxPlugin.RemoveHelpPlugIn(TargetNameSpace, Help2Default,
        SourceNameSpace, Help2Default, '');
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.UnPlugNameSpaceFromBorlandHelp(const NameSpace: WideString): Boolean;
begin
  Result := Assigned(FInstallation) and (Installation.RadToolKind = brBorlandDevStudio) and
    UnPlugNameSpace(NameSpace, IdeNamespace);
end;

function TJclHelp2Manager.UnregisterHelpFile(const NameSpace, Identifier: WideString;
  const LangId: Integer): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RemoveHelpFile(NameSpace, Identifier, LangId);
    except
      Result := False;
    end;
  end;
end;

function TJclHelp2Manager.UnregisterNameSpace(const Name: WideString): Boolean;
begin
  Result := RequireObject([hoRegister]);
  if Result then
  begin
    try
      FHxRegister.RemoveNamespace(Name);
    except
      Result := False;
    end;
  end;
end;

{$ENDIF MSWINDOWS}

//== { TJclBorRADToolIdeTool } ===============================================

constructor TJclBorRADToolIdeTool.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FKey := TransferKeyName;
end;

procedure TJclBorRADToolIdeTool.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    raise EJclError.CreateRes(@RsEIndexOufOfRange);
end;

function TJclBorRADToolIdeTool.GetCount: Integer;
begin
  Result := Installation.ConfigData.ReadInteger(Key, TransferCountValueName, 0);
end;

function TJclBorRADToolIdeTool.GetParameters(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferParamsValueName, [Index]), '');
end;

function TJclBorRADToolIdeTool.GetPath(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferPathValueName, [Index]), '');
end;

function TJclBorRADToolIdeTool.GetTitle(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferTitleValueName, [Index]), '');
end;

function TJclBorRADToolIdeTool.GetWorkingDir(Index: Integer): string;
begin
  CheckIndex(Index);
  Result := Installation.ConfigData.ReadString(Key, Format(TransferWorkDirValueName, [Index]), '');
end;

function TJclBorRADToolIdeTool.IndexOfPath(const Value: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if SamePath(Path[I], Value) then
    begin
      Result := I;
      Break;
    end;
end;

function TJclBorRADToolIdeTool.IndexOfTitle(const Value: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Count - 1 do
    if Title[I] = Value then
    begin
      Result := I;
      Break;
    end;
end;

procedure TJclBorRADToolIdeTool.RemoveIndex(const Index: Integer);
var
  I: Integer;
begin
  for I := Index to Count - 2 do
  begin
    Parameters[I] := Parameters[I + 1];
    Path[I] := Path[I + 1];
    Title[I] := Title[I + 1];
    WorkingDir[Index] := WorkingDir[I + 1];
  end;
  Count := Count - 1;
end;

procedure TJclBorRADToolIdeTool.SetCount(const Value: Integer);
begin
  if Value > Count then
    Installation.ConfigData.WriteInteger(Key, TransferCountValueName, Value);
end;

procedure TJclBorRADToolIdeTool.SetParameters(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferParamsValueName, [Index]), Value);
end;

procedure TJclBorRADToolIdeTool.SetPath(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferPathValueName, [Index]), Value);
end;

procedure TJclBorRADToolIdeTool.SetTitle(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferTitleValueName, [Index]), Value);
end;

procedure TJclBorRADToolIdeTool.SetWorkingDir(Index: Integer; const Value: string);
begin
  CheckIndex(Index);
  Installation.ConfigData.WriteString(Key, Format(TransferWorkDirValueName, [Index]), Value);
end;

//=== { TJclBorRADToolIdePackages } ==========================================

constructor TJclBorRADToolIdePackages.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FDisabledPackages := TStringList.Create;
  FDisabledPackages.Sorted := True;
  FDisabledPackages.Duplicates := dupIgnore;
  FKnownPackages := TStringList.Create;
  FKnownPackages.Sorted := True;
  FKnownPackages.Duplicates := dupIgnore;
  FKnownIDEPackages := TStringList.Create;
  FKnownIDEPackages.Sorted := True;
  FKnownIDEPackages.Duplicates := dupIgnore;
  FExperts := TStringList.Create;
  FExperts.Sorted := True;
  FExperts.Duplicates := dupIgnore;
  ReadPackages;
end;

destructor TJclBorRADToolIdePackages.Destroy;
begin
  FreeAndNil(FDisabledPackages);
  FreeAndNil(FKnownPackages);
  FreeAndNil(FKnownIDEPackages);
  FreeAndNil(FExperts);
  inherited Destroy;
end;

function TJclBorRADToolIdePackages.AddPackage(const FileName, Description: string): Boolean;
begin
  Result := True;
  RemoveDisabled(FileName);
  Installation.ConfigData.WriteString(KnownPackagesKeyName, FileName, Description);
  ReadPackages;
end;

function TJclBorRADToolIdePackages.AddExpert(const FileName, Description: string): Boolean;
begin
  Result := True;
  RemoveDisabled(FileName);
  Installation.ConfigData.WriteString(ExpertsKeyName, Description, FileName);
  ReadPackages;
end;

function TJclBorRADToolIdePackages.AddIDEPackage(const FileName, Description: string): Boolean;
begin
  Result := True;
  RemoveDisabled(FileName);
  Installation.ConfigData.WriteString(KnownIDEPackagesKeyName, FileName, Description);
  ReadPackages;
end;

function TJclBorRADToolIdePackages.GetCount: Integer;
begin
  Result := FKnownPackages.Count;
end;

function TJclBorRADToolIdePackages.GetExpertCount: Integer;
begin
  Result := FExperts.Count;
end;

function TJclBorRADToolIdePackages.GetExpertDescriptions(Index: Integer): string;
begin
  Result := FExperts.Names[Index];
end;

function TJclBorRADToolIdePackages.GetExpertFileNames(Index: Integer): string;
begin
  Result := PackageEntryToFileName(FExperts.Values[FExperts.Names[Index]]);
end;

function TJclBorRADToolIdePackages.GetIDECount: Integer;
begin
  Result := FKnownIDEPackages.Count;
end;

function TJclBorRADToolIdePackages.GetPackageDescriptions(Index: Integer): string;
begin
  Result := FKnownPackages.Values[FKnownPackages.Names[Index]];
end;

function TJclBorRADToolIdePackages.GetIDEPackageDescriptions(Index: Integer): string;
begin
  Result := FKnownPackages.Values[FKnownIDEPackages.Names[Index]];
end;

function TJclBorRADToolIdePackages.GetPackageDisabled(Index: Integer): Boolean;
begin
  Result := Boolean(FKnownPackages.Objects[Index]);
end;

function TJclBorRADToolIdePackages.GetPackageFileNames(Index: Integer): string;
begin
  Result := PackageEntryToFileName(FKnownPackages.Names[Index]);
end;

function TJclBorRADToolIdePackages.GetIDEPackageFileNames(Index: Integer): string;
begin
  Result := PackageEntryToFileName(FKnownIDEPackages.Names[Index]);
end;

function TJclBorRADToolIdePackages.PackageEntryToFileName(const Entry: string): string;
begin
  Result := Installation.SubstitutePath(Entry);
end;

procedure TJclBorRADToolIdePackages.ReadPackages;
var
  I: Integer;

  procedure ReadPackageList(const Name: string; List: TStringList);
  var
    ListIsSorted: Boolean;
  begin
    ListIsSorted := List.Sorted;
    List.Sorted := False;
    List.Clear;
    Installation.ConfigData.ReadSectionValues(Name, List);
    List.Sorted := ListIsSorted;
  end;

begin
  if Installation.RadToolKind = brBorlandDevStudio then
    ReadPackageList(KnownIDEPackagesKeyName, FKnownIDEPackages);
  ReadPackageList(KnownPackagesKeyName, FKnownPackages);
  ReadPackageList(DisabledPackagesKeyName, FDisabledPackages);
  ReadPackageList(ExpertsKeyName, FExperts);
  for I := 0 to Count - 1 do
    if FDisabledPackages.IndexOfName(FKnownPackages.Names[I]) <> -1 then
      FKnownPackages.Objects[I] := Pointer(True);
end;

procedure TJclBorRADToolIdePackages.RemoveDisabled(const FileName: string);
var
  I: Integer;
begin
  for I := 0 to FDisabledPackages.Count - 1 do
    if SamePath(FileName, PackageEntryToFileName(FDisabledPackages.Names[I])) then
    begin
      Installation.ConfigData.DeleteKey(DisabledPackagesKeyName, FDisabledPackages.Names[I]);
      ReadPackages;
      Break;
    end;
end;

function TJclBorRADToolIdePackages.RemoveExpert(const FileName: string): Boolean;
var
  I: Integer;
  KnownExpertDescription, KnownExpert, KnownExpertFileName: string;
begin
  Result := False;
  for I := 0 to FExperts.Count - 1 do
  begin
    KnownExpertDescription := FExperts.Names[I];
    KnownExpert := FExperts.Values[KnownExpertDescription];
    KnownExpertFileName := PackageEntryToFileName(KnownExpert);
    if SamePath(FileName, KnownExpertFileName) then
    begin
      RemoveDisabled(KnownExpertFileName);
      Installation.ConfigData.DeleteKey(ExpertsKeyName, KnownExpertDescription);
      ReadPackages;
      Result := True;
      Break;
    end;
  end;
end;

function TJclBorRADToolIdePackages.RemovePackage(const FileName: string): Boolean;
var
  I: Integer;
  KnownPackage, KnownPackageFileName: string;
begin
  Result := False;
  for I := 0 to FKnownPackages.Count - 1 do
  begin
    KnownPackage := FKnownPackages.Names[I];
    KnownPackageFileName := PackageEntryToFileName(KnownPackage);
    if SamePath(FileName, KnownPackageFileName) then
    begin
      RemoveDisabled(KnownPackageFileName);
      Installation.ConfigData.DeleteKey(KnownPackagesKeyName, KnownPackage);
      ReadPackages;
      Result := True;
      Break;
    end;
  end;
end;

function TJclBorRADToolIdePackages.RemoveIDEPackage(const FileName: string): Boolean;
var
  I: Integer;
  KnownIDEPackage, KnownIDEPackageFileName: string;
begin
  Result := False;
  for I := 0 to FKnownIDEPackages.Count - 1 do
  begin
    KnownIDEPackage := FKnownIDEPackages.Names[I];
    KnownIDEPackageFileName := PackageEntryToFileName(KnownIDEPackage);
    if SamePath(FileName, KnownIDEPackageFileName) then
    begin
      RemoveDisabled(KnownIDEPackageFileName);
      Installation.ConfigData.DeleteKey(KnownIDEPackagesKeyName, KnownIDEPackage);
      ReadPackages;
      Result := True;
      Break;
    end;
  end;
end;

//=== { TJclBorlandCommandLineTool } =========================================

constructor TJclBorlandCommandLineTool.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FOptions := TStringList.Create;
end;

destructor TJclBorlandCommandLineTool.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TJclBorlandCommandLineTool.AddPathOption(const Option, Path: string);
var
  S: string;

  {$IFDEF MSWINDOWS}
  // to avoid the 126 character limit of DCC32 (and eventually other command line tools)
  // which shows up with misleading error messages ("Fatal: System.pas not found") or
  // might even cause AVs
  procedure ConvertToShortPathNames(var Paths: string);
  var
    List: TStringList;
    I: Integer;
  begin
    List := TStringList.Create;
    try
      StrToStrings(Paths, PathSep, List);
      for I := 0 to List.Count - 1 do
        List[I] := PathGetShortName(List[I]);
      Paths := StringsToStr(List, PathSep);
    finally
      List.Free;
    end;
  end;
  {$ENDIF MSWINDOWS}

begin
  S := PathRemoveSeparator(Path);
  {$IFDEF MSWINDOWS}
  S := LowerCase(S); // file names are case insensitive
  ConvertToShortPathNames(S);
  {$ENDIF MSWINDOWS}
  { TODO : If we were sure that options are always case-insensitive
           for Borland tools, we could use UpperCase(Option) below. }
  S := Format('-%s"%s"', [Option, S]);
  // avoid duplicate entries
  if Options.IndexOf(S) = -1 then
    Options.Add(S);
end;

procedure TJclBorlandCommandLineTool.CheckOutputValid;
begin
  if Assigned(FOutputCallback) then
    raise EJclCommandLineToolError.CreateResFmt(@RsECmdLineToolOutputInvalid, [GetExeName]);
end;

function TJclBorlandCommandLineTool.Execute(const CommandLine: string): Boolean;
var
  LaunchCommand: string;
begin
  LaunchCommand := Format('%s %s', [FileName, CommandLine]);
  if Assigned(FOutputCallback) then
  begin
    FOutputCallback(LaunchCommand);
    Result := JclSysUtils.Execute(LaunchCommand, FOutputCallback) = 0;
  end
  else
    Result := JclSysUtils.Execute(LaunchCommand, FOutput) = 0;
end;

function TJclBorlandCommandLineTool.GetExeName: string;
begin
  Result := '';
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ENDIF MSWINDOWS}
end;

function TJclBorlandCommandLineTool.GetFileName: string;
begin
  Result := Installation.BinFolderName + GetExeName;
  if Pos(' ', Result) > 0 then
    Result := AnsiQuotedStr(Result, '"');
end;

function TJclBorlandCommandLineTool.GetOptions: TStrings;
begin
  Result := FOptions;
end;

function TJclBorlandCommandLineTool.GetOutput: string;
begin
  CheckOutputValid;
  Result := FOutput;
end;

function TJclBorlandCommandLineTool.GetOutputCallback: TTextHandler;
begin
  Result := FOutputCallback;
end;

procedure TJclBorlandCommandLineTool.SetOutputCallback(const CallbackMethod: TTextHandler);
begin
  FOutputCallback := CallbackMethod;
end;

//=== { TJclBCC32 } ============================================================

constructor TJclBCC32.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
end;

function TJclBCC32.GetExeName: string;
begin
  Result := BCC32ExeName;
end;

{$IFDEF KEEP_DEPRECATED}
function TJclBCC32.SupportsLibSuffix: Boolean;
begin
  Result := Installation.SupportsLibSuffix;
end;
{$ENDIF KEEP_DEPRECATED}

//=== { TJclDCC32 } ============================================================

procedure TJclDCC32.AddProjectOptions(const ProjectFileName, DCPPath: string);

type
  TProjectOptions = record
    UsePackages: Boolean;
    UnitOutputDir: string;
    SearchPath: string;
    DynamicPackages: string;
    SearchDcpPath: string;
    Conditionals: string;
  end;

  function AddDProjOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
  var
    DProjFileName, ProjectConfiguration, ProjectPlatform, PersonalityName: string;
    OptionsXmlFile: TJclSimpleXML;
    ProjectExtensionsNode, PropertyGroupNode, PersonalityNode, ChildNode: TJclSimpleXMLElem;
    NodeIndex: Integer;
    ConditionProperty: TJclSimpleXMLProp;
  begin
    DProjFileName := ChangeFileExt(ProjectFileName, SourceExtensionDProject);
    Result := FileExists(DProjFileName) and (Installation.IDEVersionNumber >= 5)
      and (Installation.RadToolKind = brBorlandDevStudio);
    if Result then
    begin
      OptionsXmlFile := TJclSimpleXML.Create;
      try
        OptionsXmlFile.LoadFromFile(DProjFileName);
        OptionsXmlFile.Options := OptionsXmlFile.Options - [sxoAutoCreate];
        PersonalityName := '';
        ProjectExtensionsNode := OptionsXmlFile.Root.Items.ItemNamed[DProjProjectExtensionsNodeName];
        if Assigned(ProjectExtensionsNode) then
        begin
          PersonalityNode := ProjectExtensionsNode.Items.ItemNamed[DProjPersonalityNodeName];
          if Assigned(PersonalityNode) then
            PersonalityName := PersonalityNode.Value;
        end;
        if AnsiSameText(PersonalityName, DProjDelphiPersonalityValue) or
          AnsiSameText(PersonalityName, DProjDelphiDotNetPersonalityValue) then
        begin
          ProjectConfiguration := '';
          ProjectPlatform := '';
          for NodeIndex := 0 to OptionsXmlFile.Root.Items.Count - 1 do
          begin
            PropertyGroupNode := OptionsXmlFile.Root.Items.Item[NodeIndex];
            if AnsiSameText(PropertyGroupNode.Name, DProjPropertyGroupNodeName) then
            begin
              ConditionProperty := PropertyGroupNode.Properties.ItemNamed[DProjConditionValueName];
              if Assigned(ConditionProperty) then
              begin
                if (ProjectConfiguration <> '') and (ProjectPlatform <> '') and
                  (AnsiPos(Format('%s|%s', [ProjectConfiguration, ProjectPlatform]), ConditionProperty.Value) > 0) then
                begin
                  // this is the active configuration, check for overrides
                  ChildNode := PropertyGroupNode.Items.ItemNamed[DProjUsePackageNodeName];
                  if Assigned(ChildNode) then
                    ProjectOptions.DynamicPackages := ChildNode.Value;
                  ProjectOptions.UsePackages := ProjectOptions.DynamicPackages <> '';
                  ChildNode := PropertyGroupNode.Items.ItemNamed[DProjDcuOutputDirNodeName];
                  if Assigned(ChildNode) then
                    ProjectOptions.UnitOutputDir := ChildNode.Value;
                  ChildNode := PropertyGroupNode.Items.ItemNamed[DProjUnitSearchPathNodeName];
                  if Assigned(ChildNode) then
                    ProjectOptions.SearchPath := ChildNode.Value;
                  ChildNode := PropertyGroupNode.Items.ItemNamed[DProjDefineNodeName];
                  if Assigned(ChildNode) then
                    ProjectOptions.Conditionals := ChildNode.Value;
                end;
              end
              else
              begin
                // check for default configurations
                ChildNode := PropertyGroupNode.Items.ItemNamed[DProjConfigurationNodeName];
                if Assigned(ChildNode) then
                  ProjectConfiguration := ChildNode.Value;
                ChildNode := PropertyGroupNode.Items.ItemNamed[DProjPlatformNodeName];
                if Assigned(ChildNode) then
                  ProjectPlatform := ChildNode.Value;
              end;
            end;
          end;
        end;
      finally
        OptionsXmlFile.Free;
      end;
    end;
  end;

  function AddBDSProjOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
  var
    BDSProjFileName, PersonalityName: string;
    OptionsXmlFile: TJclSimpleXML;
    PersonalityInfoNode, OptionNode, ChildNode, PersonalityNode, DirectoriesNode: TJclSimpleXMLElem;
    NodeIndex: Integer;
    NameProperty: TJclSimpleXMLProp;
  begin
    BDSProjFileName := ChangeFileExt(ProjectFileName, SourceExtensionBDSProject);
    Result := FileExists(BDSProjFileName);
    if Result then
    begin
      OptionsXmlFile := TJclSimpleXML.Create;
      try
        OptionsXmlFile.LoadFromFile(BDSProjFileName);
        OptionsXmlFile.Options := OptionsXmlFile.Options - [sxoAutoCreate];
        PersonalityInfoNode := OptionsXmlFile.Root.Items.ItemNamed[BDSProjPersonalityInfoNodeName];
        PersonalityName := '';
        if Assigned(PersonalityInfoNode) then
        begin
          OptionNode := PersonalityInfoNode.Items.ItemNamed[BDSProjOptionNodeName];
          if Assigned(OptionNode) then
            for NodeIndex := 0 to OptionNode.Items.Count - 1 do
            begin
              ChildNode := OptionNode.Items.Item[NodeIndex];
              if SameText(ChildNode.Name, BDSProjOptionNodeName) then
              begin
                NameProperty := ChildNode.Properties.ItemNamed[BDSProjNameProperty];
                if Assigned(NameProperty) and SameText(NameProperty.Value, BDSProjPersonalityValue) then
                begin
                  PersonalityName := ChildNode.Value;
                  Break;
                end;
              end;
            end;
        end;
        if PersonalityName <> '' then
        begin
          PersonalityNode := OptionsXmlFile.Root.Items.ItemNamed[PersonalityName];
          if Assigned(PersonalityNode) then
          begin
            DirectoriesNode := PersonalityNode.Items.ItemNamed[BDSProjDirectoriesNodeName];
            if Assigned(DirectoriesNode) then
              for NodeIndex := 0 to DirectoriesNode.Items.Count - 1 do
              begin
                ChildNode := DirectoriesNode.Items.Item[NodeIndex];
                if SameText(ChildNode.Name, BDSProjDirectoriesNodeName) then
                begin
                  NameProperty := ChildNode.Properties.ItemNamed[BDSProjNameProperty];
                  if Assigned(NameProperty) then
                  begin
                    if SameText(NameProperty.Value, BDSProjUnitOutputDirValue) then
                      ProjectOptions.UnitOutputDir := ChildNode.Value
                    else
                    if SameText(NameProperty.Value, BDSProjSearchPathValue) then
                      ProjectOptions.SearchPath := ChildNode.Value
                    else
                    if SameText(NameProperty.Value, BDSProjPackagesValue) then
                      ProjectOptions.DynamicPackages := ChildNode.Value
                    else
                    if SameText(NameProperty.Value, BDSProjConditionalsValue) then
                      ProjectOptions.Conditionals := ChildNode.Value
                    else
                    if SameText(NameProperty.Value, BDSProjUsePackagesValue) then
                      ProjectOptions.UsePackages := StrToBoolean(ChildNode.Value);
                  end;
                end;
              end;
          end;
        end;
      finally
        OptionsXmlFile.Free;
      end;
    end;
  end;

  function AddDOFOptions(const ProjectFileName: string; var ProjectOptions: TProjectOptions): Boolean;
  var
    DOFFileName: string;
    OptionsFile: TIniFile;
  begin
    DOFFileName := ChangeFileExt(ProjectFileName, DelphiOptionsFileExtension);
    Result := FileExists(DOFFileName);
    if Result then
    begin
      OptionsFile := TIniFile.Create(DOFFileName);
      try
        ProjectOptions.SearchPath := OptionsFile.ReadString(DOFDirectoriesSection, DOFSearchPathName, '');
        ProjectOptions.UnitOutputDir := OptionsFile.ReadString(DOFDirectoriesSection, DOFUnitOutputDirKey, '');
        ProjectOptions.Conditionals := OptionsFile.ReadString(DOFDirectoriesSection, DOFConditionals, '');
        ProjectOptions.UsePackages := OptionsFile.ReadString(DOFCompilerSection, DOFPackageNoLinkKey, '') = '1';
        ProjectOptions.DynamicPackages := OptionsFile.ReadString(DOFLinkerSection, DOFPackagesKey, '');
      finally
        OptionsFile.Free;
      end;
    end;
  end;
var
  ConfigurationFileName: string;
  ProjectOptions: TProjectOptions;
begin
  ConfigurationFileName := ChangeFileExt(ProjectFileName, ConfigurationExtension);
  if FileExists(ConfigurationFileName) then
    FileDelete(ConfigurationFileName);

  ProjectOptions.UsePackages := False;
  ProjectOptions.UnitOutputDir := '';
  ProjectOptions.SearchPath := '';
  ProjectOptions.DynamicPackages := '';
  ProjectOptions.SearchDcpPath := '';
  ProjectOptions.Conditionals := '';

  if AddDProjOptions(ProjectFileName, ProjectOptions) or
     AddBDSProjOptions(ProjectFileName, ProjectOptions) or
     AddDOFOptions(ProjectFileName, ProjectOptions) then
  begin
    if ProjectOptions.UnitOutputDir <> '' then
      AddPathOption('N', ProjectOptions.UnitOutputDir);
    if ProjectOptions.SearchPath <> '' then
    begin
      AddPathOption('I', ProjectOptions.SearchPath);
      AddPathOption('R', ProjectOptions.SearchPath);
    end;
    if ProjectOptions.Conditionals <> '' then
      Options.Add(Format('-D%s', [ProjectOptions.Conditionals]));
    if SamePath(DCPPath, Installation.DCPOutputPath) then
      ProjectOptions.SearchDcpPath := DCPPath
    else
      ProjectOptions.SearchDcpPath := StrEnsureSuffix(PathSep, DCPPath) + Installation.DCPOutputPath;
    AddPathOption('U', StrEnsureSuffix(PathSep, ProjectOptions.SearchDcpPath) + ProjectOptions.SearchPath);
    if ProjectOptions.UsePackages and (ProjectOptions.DynamicPackages <> '') then
      Options.Add(Format('-LU"%s"', [ProjectOptions.DynamicPackages]));
  end;
end;

function TJclDCC32.Compile(const ProjectFileName: string): Boolean;
begin
  // Note: PathGetShortName may not return the short path if it's a network
  // drive. Hence we always double quote the path, regardless of the compiling
  // environment.
  Result := Execute(StrDoubleQuote(StrTrimQuotes(ProjectFileName)));
end;

constructor TJclDCC32.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  SetDefaultOptions; // in case $(DELPHI)\bin\dcc32.cfg (replace as appropriate) is invalid
end;

function TJclDCC32.Execute(const CommandLine: string): Boolean;
  function IsPathOption(const S: string; out Len: Integer): Boolean;
  begin
    Result := False;
    if (Length(S) >= 2) and (S[1] = '-') then
      case UpCase(S[2]) of
        'E', 'I', 'O', 'R', 'U':
          begin
            Result := True;
            Len := 2;
          end;
        'L':
          if Length(S) >= 3 then
          begin
            Result := UpCase(S[3]) in ['E', 'N'];
            Len := 3;
          end;
        'N':
          begin
            Result := True;
            if (Length(S) >= 3) and (S[3] in ['0'..'9', 'H', 'O', 'B']) then
              Len := 3
            else
              Len := 2;
          end;
      end;
  end;
var
  OptionIndex, PathIndex, SwitchLen: Integer;
  PathList: TStrings;
  Option, Arguments, CurrentFolder: string;
begin
  FOutput := '';
  Arguments := '';
  CurrentFolder := GetCurrentFolder;

  PathList := TStringList.Create;
  try
    for OptionIndex := 0 to Options.Count - 1 do
    begin
      Option := Options.Strings[OptionIndex];
      if IsPathOption(Option, SwitchLen) then
      begin

        StrToStrings(StrTrimQuotes(Copy(Option, SwitchLen + 1, Length(Option) - SwitchLen)), PathSep, PathList);
        // change to relative paths to avoid DCC32 126 character path limit
        for PathIndex := 0 to PathList.Count - 1 do
          PathList.Strings[PathIndex] := PathGetRelativePath(CurrentFolder, ExpandFileName(PathList[PathIndex]));
        if PathList.Count > 0 then
          Arguments := Format('%s %s"%s"', [Arguments, Copy(Option, 1, SwitchLen),
            StringsToStr(PathList, PathSep)]);
      end
      else
      begin
        {$IFDEF KYLIX}
        // escaping $ chars
        if (Length(Option) > 2) and (Option[1] = '-') and (Option[2] = '$') then
          Option := '-\' + Copy(Option, 2, Length(Option) - 1);
        {$ENDIF KYLIX}
        Arguments := Format('%s %s', [Arguments, Option]);
      end;
    end;
  finally
    PathList.Free;
  end;

  Result := inherited Execute(CommandLine + Arguments);
end;

function TJclDCC32.GetExeName: string;
begin
  Result := DCC32ExeName;
end;

function TJclDCC32.MakePackage(const PackageName, BPLPath, DCPPath: string; ExtraOptions: string): Boolean;
var
  SaveDir: string;
begin
  SaveDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(PackageName) + '.');
  try
    Options.Clear;
    SetDefaultOptions;
    AddProjectOptions(PackageName, DCPPath);
    AddPathOption('LN', DCPPath);
    AddPathOption('LE', BPLPath);
    Options.Add(ExtraOptions);
    Result := Compile(PackageName);
  finally
    SetCurrentDir(SaveDir);
  end;
end;

function TJclDCC32.MakeProject(const ProjectName, OutputDir, DcpSearchPath: string;
  ExtraOptions: string): Boolean;
var
  SaveDir: string;
begin
  SaveDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(ProjectName) + '.');
  try
    Options.Clear;
    SetDefaultOptions;
    AddProjectOptions(ProjectName, DcpSearchPath);
    AddPathOption('E', OutputDir);
    Options.Add(ExtraOptions);
    Result := Compile(ProjectName);
  finally
    SetCurrentDir(SaveDir);
  end;
end;

procedure TJclDCC32.SetDefaultOptions;
begin
  Options.Clear;
  if (Installation.RadToolKind = brBorlandDevStudio) and (Installation.VersionNumber >= 4) then
    Options.Add('--no-config');
  AddPathOption('U', Installation.LibFolderName);
  if Installation.RadToolKind = brCppBuilder then
  begin
    AddPathOption('U', Installation.LibFolderName + PathAddSeparator('obj'));
    {$IFNDEF KYLIX}
    if (Installation.RadToolKind <> brBorlandDevStudio) and
      (Installation.VersionNumber = 5) then
      Options.Add('-LUvcl50')
    else
      Options.Add('-LUrtl');
    {$ENDIF ~KYLIX}
  end;
end;

{$IFDEF KEEP_DEPRECATED}
function TJclDCC32.SupportsLibSuffix: Boolean;
begin
  Result := Installation.SupportsLibSuffix;
end;
{$ENDIF KEEP_DEPRECATED}

{$IFDEF MSWINDOWS}
//=== { TJclDCCIL } ==========================================================

function TJclDCCIL.GetExeName: string;
begin
  Result := DCCILExeName;
end;

function TJclDCCIL.GetMaxCLRVersion: string;
var
  StartPos, EndPos: Integer;
begin
  if FMaxCLRVersion <> '' then
  begin
    Result := FMaxCLRVersion;
    Exit;
  end;

  Result := FindResStart(Installation.BinFolderName + GetExeName, '  --clrversion');

  StartPos := Pos(':', Result);
  if StartPos = 0 then
    StartPos := Pos('=', Result);

  if StartPos > 0 then
    Result := Copy(Result, StartPos + 1, Length(Result) - StartPos);

  EndPos := Pos(' ', Result);
  if EndPos > 0 then
    SetLength(Result, EndPos - 1);

  if Result = '' then
    Result := 'v1.1.4322'; // do not localize

  FMaxCLRVersion := Result;
end;

function TJclDCCIL.MakeProject(const ProjectName, OutputDir,
  ExtraOptions: string): Boolean;
var
  SaveDir: string;
begin
  SaveDir := GetCurrentDir;
  SetCurrentDir(ExtractFilePath(ProjectName) + '.');
  try
    Options.Clear;
    SetDefaultOptions;
    AddProjectOptions(ProjectName, '');
    AddPathOption('E', OutputDir);
    Options.Add(ExtraOptions);
    Result := Compile(ProjectName);
  finally
    SetCurrentDir(SaveDir);
  end;
end;

procedure TJclDCCIL.SetDefaultOptions;
begin
  Options.Clear;
  AddPathOption('U', Installation.LibFolderName);
end;

{$ENDIF MSWINDOWS}

//=== { TJclBorlandMake } ====================================================

function TJclBorlandMake.GetExeName: string;
begin
  Result := MakeExeName;
end;

//=== { TJclBpr2Mak } ========================================================

function TJclBpr2Mak.GetExeName: string;
begin
  Result := Bpr2MakExeName;
end;

//=== { TJclBorRADToolPalette } ==============================================

constructor TJclBorRADToolPalette.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  FKey := PaletteKeyName;
  FTabNames := TStringList.Create;
  FTabNames.Sorted := True;
  ReadTabNames;
end;

destructor TJclBorRADToolPalette.Destroy;
begin
  FreeAndNil(FTabNames);
  inherited Destroy;
end;

procedure TJclBorRADToolPalette.ComponentsOnTabToStrings(Index: Integer; Strings: TStrings;
  IncludeUnitName: Boolean; IncludeHiddenComponents: Boolean);
var
  TempList: TStringList;

  procedure ProcessList(Hidden: Boolean);
  var
    D, I: Integer;
    List, S: string;
  begin
    if Hidden then
      List := HiddenComponentsOnTab[Index]
    else
      List := ComponentsOnTab[Index];
    List := StrEnsureSuffix(';', List);
    while Length(List) > 1 do
    begin
      D := Pos(';', List);
      S := Trim(Copy(List, 1, D - 1));
      if not IncludeUnitName then
        Delete(S, 1, Pos('.', S));
      if Hidden then
      begin
        I := TempList.IndexOf(S);
        if I = -1 then
          TempList.AddObject(S, Pointer(True))
        else
          TempList.Objects[I] := Pointer(True);
      end
      else
        TempList.Add(S);
      Delete(List, 1, D);
    end;
  end;

begin
  TempList := TStringList.Create;
  try
    TempList.Duplicates := dupError;
    ProcessList(False);
    TempList.Sorted := True;
    if IncludeHiddenComponents then
      ProcessList(True);
    Strings.AddStrings(TempList);
  finally
    TempList.Free;
  end;
end;

function TJclBorRADToolPalette.DeleteTabName(const TabName: string): Boolean;
var
  I: Integer;
begin
  I := FTabNames.IndexOf(TabName);
  Result := I >= 0;
  if Result then
  begin
    Installation.ConfigData.DeleteKey(Key, FTabNames[I]);
    Installation.ConfigData.DeleteKey(Key, FTabNames[I] + PaletteHiddenTag);
    FTabNames.Delete(I);
  end;
end;

function TJclBorRADToolPalette.GetComponentsOnTab(Index: Integer): string;
begin
  Result := Installation.ConfigData.ReadString(Key, FTabNames[Index], '');
end;

function TJclBorRADToolPalette.GetHiddenComponentsOnTab(Index: Integer): string;
begin
  Result := Installation.ConfigData.ReadString(Key, FTabNames[Index] + PaletteHiddenTag, '');
end;

function TJclBorRADToolPalette.GetTabNameCount: Integer;
begin
  Result := FTabNames.Count;
end;

function TJclBorRADToolPalette.GetTabNames(Index: Integer): string;
begin
  Result := FTabNames[Index];
end;

procedure TJclBorRADToolPalette.ReadTabNames;
var
  TempList: TStringList;
  I: Integer;
  S: string;
begin
  if Installation.ConfigData.SectionExists(Key) then
  begin
    TempList := TStringList.Create;
    try
      Installation.ConfigData.ReadSection(Key, TempList);
      for I := 0 to TempList.Count - 1 do
      begin
        S := TempList[I];
        if Pos(PaletteHiddenTag, S) = 0 then
          FTabNames.Add(S);
      end;
    finally
      TempList.Free;
    end;
  end;
end;

function TJclBorRADToolPalette.TabNameExists(const TabName: string): Boolean;
begin
  Result := FTabNames.IndexOf(TabName) <> -1;
end;

//=== { TJclBorRADToolRepository } ===========================================

constructor TJclBorRADToolRepository.Create(AInstallation: TJclBorRADToolInstallation);
begin
  inherited Create(AInstallation);
  {$IFDEF KYLIX}
  FFileName := AInstallation.ConfigFileName('dro');
  {$ELSE}
  FFileName := AInstallation.BinFolderName + BorRADToolRepositoryFileName;
  {$ENDIF KYLIX}
  FPages := TStringList.Create;
  IniFile.ReadSection(BorRADToolRepositoryPagesSection, FPages);
  CloseIniFile;
end;

destructor TJclBorRADToolRepository.Destroy;
begin
  FreeAndNil(FPages);
  FreeAndNil(FIniFile);
  inherited Destroy;
end;

procedure TJclBorRADToolRepository.AddObject(const FileName, ObjectType, PageName, ObjectName,
  IconFileName, Description, Author, Designer: string; const Ancestor: string);
var
  SectionName: string;
begin
  GetIniFile;
  SectionName := AnsiUpperCase(PathRemoveExtension(FileName));
  FIniFile.EraseSection(FileName);
  FIniFile.EraseSection(SectionName);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectType, ObjectType);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectName, ObjectName);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectPage, PageName);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectIcon, IconFileName);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectDescr, Description);
  FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectAuthor, Author);
  if Ancestor <> '' then
    FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectAncestor, Ancestor);
  if (Installation.RadToolKind = brBorlandDevStudio) or (Installation.VersionNumber >= 6) then
    FIniFile.WriteString(SectionName, BorRADToolRepositoryObjectDesigner, Designer);
  FIniFile.WriteBool(SectionName, BorRADToolRepositoryObjectNewForm, False);
  FIniFile.WriteBool(SectionName, BorRADToolRepositoryObjectMainForm, False);
  CloseIniFile;
end;

procedure TJclBorRADToolRepository.CloseIniFile;
begin
  FreeAndNil(FIniFile);
end;

function TJclBorRADToolRepository.FindPage(const Name: string; OptionalIndex: Integer): string;
var
  I: Integer;
begin
  I := Pages.IndexOf(Name);
  if I >= 0 then
    Result := Pages[I]
  else
  if OptionalIndex < Pages.Count then
    Result := Pages[OptionalIndex]
  else
    Result := '';
end;

function TJclBorRADToolRepository.GetIniFile: TIniFile;
begin
  if not Assigned(FIniFile) then
    FIniFile := TIniFile.Create(FileName);
  Result := FIniFile;
end;

function TJclBorRADToolRepository.GetPages: TStrings;
begin
  Result := FPages;
end;

procedure TJclBorRADToolRepository.RemoveObjects(const PartialPath, FileName, ObjectType: string);
var
  Sections: TStringList;
  I: Integer;
  SectionName, FileNamePart, PathPart, DialogFileName: string;
begin
  Sections := TStringList.Create;
  try
    GetIniFile;
    FIniFile.ReadSections(Sections);
    for I := 0 to Sections.Count - 1 do
    begin
      SectionName := Sections[I];
      if FIniFile.ReadString(SectionName, BorRADToolRepositoryObjectType, '') = ObjectType then
      begin
        FileNamePart := PathExtractFileNameNoExt(SectionName);
        PathPart := StrRight(PathAddSeparator(ExtractFilePath(SectionName)), Length(PartialPath));
        DialogFileName := PathExtractFileNameNoExt(FileName);
        if StrSame(FileNamePart, DialogFileName) and StrSame(PathPart, PartialPath) then
          FIniFile.EraseSection(SectionName);
      end;
    end;
  finally
    Sections.Free;
  end;
end;

//=== { TJclBorRADToolInstallation } =========================================

constructor TJclBorRADToolInstallation.Create(const AConfigDataLocation: string; ARootKey: Cardinal);
begin
  inherited Create;
  FConfigDataLocation := AConfigDataLocation;
  {$IFDEF KYLIX}
  FConfigData := TMemIniFile.Create(AConfigDataLocation);
  {$ELSE ~KYLIX}
  FConfigData := TRegistryIniFile.Create(AConfigDataLocation);
  if ARootKey = 0 then
    FRootKey := HKCU
  else
    FRootKey := ARootKey;
  TRegistryIniFile(FConfigData).RegIniFile.RootKey := RootKey;
  TRegistryIniFile(FConfigData).RegIniFile.OpenKey(AConfigDataLocation, True);
  {$ENDIF ~KYLIX}
  FGlobals := TStringList.Create;
  ReadInformation;
  FIdeTools := TJclBorRADToolIdeTool.Create(Self);
  {$IFDEF MSWINDOWS}
  FOpenHelp := TJclBorlandOpenHelp.Create(Self);
  {$ENDIF ~MSWINDOWS}
  FMapCreate := False;
  {$IFDEF MSWINDOWS}
  FJdbgCreate := False;
  FJdbgInsert := False;
  FMapDelete := False;
  if FileExists(BinFolderName + AsmExeName) then
    Include(FCommandLineTools, clAsm);
  {$ENDIF ~MSWINDOWS}
  if FileExists(BinFolderName + BCC32ExeName) then
    Include(FCommandLineTools, clBcc32);
  if FileExists(BinFolderName + DCC32ExeName) then
    Include(FCommandLineTools, clDcc32);
  {$IFDEF MSWINDOWS}
  if FileExists(BinFolderName + DCCILExeName) then
    Include(FCommandLineTools, clDccIL);
  {$ENDIF ~MSWINDOWS}
  if FileExists(BinFolderName + MakeExeName) then
    Include(FCommandLineTools, clMake);
  if FileExists(BinFolderName + Bpr2MakExeName) then
    Include(FCommandLineTools, clProj2Mak);
end;

destructor TJclBorRADToolInstallation.Destroy;
begin
  FreeAndNil(FRepository);
  FreeAndNil(FDCC32);
  FreeAndNil(FBCC32);
  FreeAndNil(FBpr2Mak);
  FreeAndNil(FIdePackages);
  FreeAndNil(FIdeTools);
  {$IFDEF MSWINDOWS}
  FreeAndNil(FOpenHelp);
  {$ENDIF MSWINDOWS}
  FreeAndNil(FPalette);
  FreeAndNil(FGlobals);
  {$IFDEF KYLIX}
  FConfigData.UpdateFile; // TMemIniFile.Destroy doesn't call UpdateFile
  {$ENDIF KYLIX}
  FreeAndNil(FEnvironmentVariables);
  FreeAndNil(FConfigData);
  inherited Destroy;
end;

function TJclBorRADToolInstallation.AddToDebugDCUPath(const Path: string): Boolean;
var
  TempDebugDCUPath: TJclBorRADToolPath;
begin
  TempDebugDCUPath := DebugDCUPath;
  PathListIncludeItems(TempDebugDCUPath, Path);
  Result := True;
  DebugDCUPath := TempDebugDCUPath;
end;

function TJclBorRADToolInstallation.AddToLibrarySearchPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclBorRADToolPath;
begin
  TempLibraryPath := LibrarySearchPath;
  PathListIncludeItems(TempLibraryPath, Path);
  Result := True;
  LibrarySearchPath := TempLibraryPath;
end;

function TJclBorRADToolInstallation.AddToLibraryBrowsingPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclBorRADToolPath;
begin
  TempLibraryPath := LibraryBrowsingPath;
  PathListIncludeItems(TempLibraryPath, Path);
  Result := True;
  LibraryBrowsingPath := TempLibraryPath;
end;

function TJclBorRADToolInstallation.AnyInstanceRunning: Boolean;
var
  Processes: TStringList;
  I: Integer;
begin
  Result := False;
  Processes := TStringList.Create;
  try
    if RunningProcessesList(Processes) then
    begin
      for I := 0 to Processes.Count - 1 do
        if AnsiSameText(IdeExeFileName, Processes[I]) then
        begin
          Result := True;
          Break;
        end;
    end;
  finally
    Processes.Free;
  end;
end;

{$IFDEF KYLIX}
function TJclBorRADToolInstallation.ConfigFileName(const Extension: string): string;
begin
  Result := '';
end;
{$ENDIF KYLIX}

class procedure TJclBorRADToolInstallation.ExtractPaths(const Path: TJclBorRADToolPath; List: TStrings);
begin
  StrToStrings(Path, PathSep, List);
end;

function TJclBorRADToolInstallation.CompileBCBPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  SaveDir, PackagePath, MakeFileName: string;
begin
  OutputString(Format(RsCompilingPackage, [PackageName]));

  if not IsBCBPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsENotABCBPackage, [PackageName]);

  PackagePath := PathRemoveSeparator(ExtractFilePath(PackageName));
  SaveDir := GetCurrentDir;
  SetCurrentDir(PackagePath);
  try
    MakeFileName := StrTrimQuotes(ChangeFileExt(PackageName, '.mak'));
    if clProj2Mak in CommandLineTools then       // let bpr2mak generate make file from .bpk
      // Kylix bpr2mak doesn't like full file names
      Result := Bpr2Mak.Execute(StringsToStr(Bpr2Mak.Options, ' ') + ' ' + ExtractFileName(PackageName))
    else
      // If make file exists (and doesn't need to be created by bpr2mak)
      Result := FileExists(MakeFileName);

    if MapCreate then
      Make.Options.Add('-DMAPFLAGS=-s');

    Result := Result and
      Make.Execute(Format('%s -f%s', [StringsToStr(Make.Options, ' '), StrDoubleQuote(MakeFileName)])) and
      ProcessMapFile(BinaryFileName(BPLPath, PackageName));
  finally
    SetCurrentDir(SaveDir);
  end;

  if Result then
    OutputString(RsCompilationOk)
  else
    OutputString(RsCompilationFailed);
end;

function TJclBorRADToolInstallation.CompileBCBProject(const ProjectName, OutputDir, DcpSearchPath: string): Boolean;
var
  SaveDir, PackagePath, MakeFileName: string;
begin
  OutputString(Format(RsCompilingProject, [ProjectName]));

  if not IsBCBProject(ProjectName) then
    raise EJclBorRADException.CreateResFmt(@RsENotADelphiProject, [ProjectName]);

  PackagePath := PathRemoveSeparator(ExtractFilePath(ProjectName));
  SaveDir := GetCurrentDir;
  SetCurrentDir(PackagePath);
  try
    MakeFileName := StrTrimQuotes(ChangeFileExt(ProjectName, '.mak'));
    if clProj2Mak in CommandLineTools then       // let bpr2mak generate make file from .bpk
      // Kylix bpr2mak doesn't like full file names
      Result := Bpr2Mak.Execute(StringsToStr(Bpr2Mak.Options, ' ') + ' ' + ExtractFileName(ProjectName))
    else
      // If make file exists (and doesn't need to be created by bpr2mak)
      Result := FileExists(MakeFileName);

    if MapCreate then
      Make.Options.Add('-DMAPFLAGS=-s');

    Result := Result and
      Make.Execute(Format('%s -f%s', [StringsToStr(Make.Options, ' '), StrDoubleQuote(MakeFileName)])) and
      ProcessMapFile(BinaryFileName(OutputDir, ProjectName));
  finally
    SetCurrentDir(SaveDir);
  end;

  if Result then
    OutputString(RsCompilationOk)
  else
    OutputString(RsCompilationFailed);
end;

function TJclBorRADToolInstallation.CompileDelphiPackage(const PackageName,
  BPLPath, DCPPath: string): Boolean;
begin
  Result := CompileDelphiPackage(PackageName, BPLPath, DCPPath, '');
end;

function TJclBorRADToolInstallation.CompileDelphiPackage(const PackageName,
  BPLPath, DCPPath, ExtraOptions: string): Boolean;
var
  NewOptions: string;
begin
  OutputString(Format(RsCompilingPackage, [PackageName]));

  if not IsDelphiPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsENotADelphiPackage, [PackageName]);

  if MapCreate then
    NewOptions := ExtraOptions + ' -GD'
  else
    NewOptions := ExtraOptions;

  Result := DCC32.MakePackage(PackageName, BPLPath, DCPPath, NewOptions) and
    ProcessMapFile(BinaryFileName(BPLPath, PackageName));

  if Result then
    OutputString(RsCompilationOk)
  else
    OutputString(RsCompilationFailed);
end;

function TJclBorRADToolInstallation.CompileDelphiProject(const ProjectName,
  OutputDir, DcpSearchPath: string): Boolean;
var
  ExtraOptions: string;
begin
  OutputString(Format(RsCompilingProject, [ProjectName]));

  if not IsDelphiProject(ProjectName) then
    raise EJclBorRADException.CreateResFmt(@RsENotADelphiProject, [ProjectName]);

  if MapCreate then
    ExtraOptions := '-GD'
  else
    ExtraOptions := '';

  Result := DCC32.MakeProject(ProjectName, OutputDir, DcpSearchPath, ExtraOptions) and
    ProcessMapFile(BinaryFileName(OutputDir, ProjectName));

  if Result then
    OutputString(RsCompilationOk)
  else
    OutputString(RsCompilationFailed);
end;

function TJclBorRADToolInstallation.CompilePackage(const PackageName, BPLPath,
  DCPPath: string): Boolean;
var
  PackageExtension: string;
begin
  PackageExtension := ExtractFileExt(PackageName);
  if SameText(PackageExtension, SourceExtensionBCBPackage) then
    Result := CompileBCBPackage(PackageName, BPLPath, DCPPath)
  else
  if SameText(PackageExtension, SourceExtensionDelphiPackage) then
    Result := CompileDelphiPackage(PackageName, BPLPath, DCPPath)
  else
    raise EJclBorRadException.CreateResFmt(@RsEUnknownPackageExtension, [PackageExtension]);
end;

function TJclBorRADToolInstallation.CompileProject(const ProjectName,
  OutputDir, DcpSearchPath: string): Boolean;
var
  ProjectExtension: string;
begin
  ProjectExtension := ExtractFileExt(ProjectName);
  if SameText(ProjectExtension, SourceExtensionBCBProject) then
    Result := CompileBCBProject(ProjectName, OutputDir, DcpSearchPath)
  else
  if SameText(ProjectExtension, SourceExtensionDelphiProject) then
    Result := CompileDelphiProject(ProjectName, OutputDir, DcpSearchPath)
  else
    raise EJclBorRadException.CreateResFmt(@RsEUnknownProjectExtension, [ProjectExtension]);
end;

function TJclBorRADToolInstallation.FindFolderInPath(Folder: string; List: TStrings): Integer;
var
  I: Integer;
begin
  Result := -1;
  Folder := PathRemoveSeparator(Folder);
  for I := 0 to List.Count - 1 do
    if SamePath(Folder, PathRemoveSeparator(SubstitutePath(List[I]))) then
    begin
      Result := I;
      Break;
    end;
end;

function TJclBorRADToolInstallation.GetBPLOutputPath: string;
begin
  Result := SubstitutePath(ConfigData.ReadString(LibraryKeyName, LibraryBPLOutputValueName, ''));
end;

function TJclBorRADToolInstallation.GetBpr2Mak: TJclBpr2Mak;
begin
  if not Assigned(FBpr2Mak) then
  begin
    if not (clProj2Mak in CommandLineTools) then
      raise EJclBorRadException.CreateResFmt(@RsENotFound, [Bpr2MakExeName]);
    FBpr2Mak := TJclBpr2Mak.Create(Self);
  end;
  Result := FBpr2Mak;
end;

function TJclBorRADToolInstallation.GetBCC32: TJclBCC32;
begin
  if not Assigned(FBCC32) then
  begin
    if not (clBcc32 in CommandLineTools) then
      raise EJclBorRadException.CreateResFmt(@RsENotFound, [Bcc32ExeName]);
    FBCC32 := TJclBCC32.Create(Self);
  end;
  Result := FBCC32;
end;

function TJclBorRADToolInstallation.GetCommonProjectsDir: string;
begin
  Result := DefaultProjectsDir;
end;

function TJclBorRADToolInstallation.GetDCC32: TJclDCC32;
begin
  if not Assigned(FDCC32) then
  begin
    if not (clDcc32 in CommandLineTools) then
      raise EJclBorRadException.CreateResFmt(@RsENotFound, [Dcc32ExeName]);
    FDCC32 := TJclDCC32.Create(Self);
  end;
  Result := FDCC32;
end;

function TJclBorRADToolInstallation.GetDCPOutputPath: string;
begin
  Result := SubstitutePath(ConfigData.ReadString(LibraryKeyName, LibraryDCPOutputValueName, ''));
end;

function TJclBorRADToolInstallation.GetDebugDCUPath: string;
begin
  Result := ConfigData.ReadString(DebuggingKeyName, DebugDCUPathValueName, '');
end;

function TJclBorRADToolInstallation.GetDefaultProjectsDir: string;
begin
  {$IFDEF KYLIX}
  Result := GetPersonalFolder;
  {$ELSE ~KYLIX}
  Result := Globals.Values['DefaultProjectsDirectory'];
  if Result = '' then
    Result := PathAddSeparator(RootDir) + 'Projects';
  {$ENDIF ~KYLIX}
end;

function TJclBorRADToolInstallation.GetDescription: TJclBorRADToolPath;
begin
  Result := Format('%s %s', [Name, EditionAsText]);
  if InstalledUpdatePack > 0 then
    Result := Result + ' ' + Format(RsUpdatePackName, [InstalledUpdatePack]);
end;

function TJclBorRADToolInstallation.GetEditionAsText: string;
begin
  {$IFDEF KYLIX}
  case Edition of
    deOPEN:
      Result := RsOpenEdition;
    dePRO:
      Result := RsProfessional;
    deSVR:
      if VersionNumber >= 2 then
        Result := RsEnterprise
      else
        Result := RsServerDeveloper;
  end;
  {$ELSE}
  Result := FEditionStr;
  if Length(FEditionStr) = 3 then
    case Edition of
      deSTD:
        if (VersionNumber >= 6) or (RadToolKind = brBorlandDevStudio) then
          Result := RsPersonal
        else
          Result := RsStandard;
      dePRO:
        Result := RsProfessional;
      deCSS:
        if (VersionNumber >= 5) or (RadToolKind = brBorlandDevStudio) then
          Result := RsEnterprise
        else
          Result := RsClientServer;
      deARC:
        Result := RsArchitect;
    end;
  {$ENDIF KYLIX}
end;

function TJclBorRADToolInstallation.GetEnvironmentVariables: TStrings;
var
  EnvNames: TStringList;
  EnvVarKeyName: string;
  I: Integer;
begin
  if FEnvironmentVariables = nil then
  begin
    FEnvironmentVariables := TStringList.Create;
    if ((VersionNumber >= 6) or (RadToolKind = brBorlandDevStudio)) and
      ConfigData.SectionExists(EnvVariablesKeyName) then
    begin
      EnvNames := TStringList.Create;
      try
        ConfigData.ReadSection(EnvVariablesKeyName, EnvNames);
        for I := 0 to EnvNames.Count - 1 do
        begin
          EnvVarKeyName := EnvNames[I];
          FEnvironmentVariables.Values[EnvVarKeyName] :=
            ConfigData.ReadString(EnvVariablesKeyName, EnvVarKeyName, '');
        end;
      finally
        EnvNames.Free;
      end;
    end;
  end;
  Result := FEnvironmentVariables;
end;

function TJclBorRADToolInstallation.GetGlobals: TStrings;
begin
  Result := FGlobals;
end;

function TJclBorRADToolInstallation.GetIdeExeFileName: string;
{$IFDEF KYLIX}
const
  IdeFileNames: array [brDelphi..brCppBuilder] of string = (DelphiIdeExeName, BCBIdeExeName);
begin
  Result := FBinFolderName + IdeFileNames[RADToolKind];
end;
{$ENDIF KYLIX}
{$IFDEF MSWINDOWS}
begin
  Result := Globals.Values['App'];
end;
{$ENDIF MSWINDOWS}

function TJclBorRADToolInstallation.GetIdeExeBuildNumber: string;
begin
  {$IFDEF KYLIX}
  { TODO : determine Kylix IDE build # }
  Result := '?';
  {$ELSE}
  Result := VersionFixedFileInfoString(IdeExeFileName, vfFull);
  {$ENDIF KYLIX}
end;

function TJclBorRADToolInstallation.GetIdePackages: TJclBorRADToolIdePackages;
begin
  if not Assigned(FIdePackages) then
    FIdePackages := TJclBorRADToolIdePackages.Create(Self);
  Result := FIdePackages;
end;

function TJclBorRADToolInstallation.GetIsTurboExplorer: Boolean;
begin
  Result := (RadToolKind = brBorlandDevStudio) and (VersionNumber = 4) and not (clDcc32 in CommandLineTools);
end;

function TJclBorRADToolInstallation.GetLatestUpdatePack: Integer;
begin
  Result := GetLatestUpdatePackForVersion(VersionNumber);
end;

class function TJclBorRADToolInstallation.GetLatestUpdatePackForVersion(Version: Integer): Integer;
begin
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  // dummy; BCB doesn't like abstract class functions
  {$ELSE MSWINDOWS}
  Result := 0;
  {$ENDIF MSWINDOWS}
end;

function TJclBorRADToolInstallation.GetLibrarySearchPath: TJclBorRADToolPath;
begin
  Result := ConfigData.ReadString(LibraryKeyName, LibrarySearchPathValueName, '');
end;

function TJclBorRADToolInstallation.GetMake: IJclCommandLineTool;
begin
  if not Assigned(FMake) then
  begin
    if not (clMake in CommandLineTools) then
      raise EJclBorRadException.CreateResFmt(@RsENotFound, [MakeExeName]);
    {$IFDEF KYLIX}
    FMake := TJclCommandLineTool.Create(MakeExeName);
    {$ELSE ~KYLIX}
    FMake := TJclBorlandMake.Create(Self);
    // Set option "-l+", which enables use of long command lines.  Should be
    // default, but there have been reports indicating that's not always the case.
    FMake.Options.Add('-l+');
    {$ENDIF ~KYLIX}
  end;
  Result := FMake;
end;

function TJclBorRADToolInstallation.GetLibraryBrowsingPath: TJclBorRADToolPath;
begin
  Result := ConfigData.ReadString(LibraryKeyName, LibraryBrowsingPathValueName, '');
end;

function TJclBorRADToolInstallation.GetName: string;
begin
  {$IFDEF KYLIX}
  Result := Format(RsKylixVersionName, [IDEVersionNumber, RADToolName]);
  {$ELSE ~KYLIX}
  Result := Format('%s %d', [RADToolName, IDEVersionNumber]);
  {$ENDIF ~KYLIX}
end;

function TJclBorRADToolInstallation.GetPalette: TJclBorRADToolPalette;
begin
  if not Assigned(FPalette) then
    FPalette := TJclBorRADToolPalette.Create(Self);
  Result := FPalette;
end;

function TJclBorRADToolInstallation.GetRepository: TJclBorRADToolRepository;
begin
  if not Assigned(FRepository) then
    FRepository := TJclBorRADToolRepository.Create(Self);
  Result := FRepository;
end;

function TJclBorRADToolInstallation.GetSupportsLibSuffix: Boolean;
begin
  {$IFDEF KYLIX}
  Result := True;
  {$ELSE}
  Result := (RadToolKind = brBorlandDevStudio) or (VersionNumber >= 6);
  {$ENDIF KYLIX}
end;

function TJclBorRADToolInstallation.GetUpdateNeeded: Boolean;
begin
  Result := InstalledUpdatePack < LatestUpdatePack;
end;

function TJclBorRADToolInstallation.GetValid: Boolean;
begin
  Result := (ConfigData.FileName <> '') and (RootDir <> '') and FileExists(IdeExeFileName);
end;

function TJclBorRADToolInstallation.GetVclIncludeDir: string;
begin
  Result := RootDir + RsVclIncludeDir;
  if not DirectoryExists(Result) then
    Result := '';
end;

function TJclBorRADToolInstallation.InstallBCBExpert(const ProjectName, OutputDir, DcpSearchPath: string): Boolean;
var
  Unused, Description: string;
begin
  OutputString(Format(RsExpertInstallationStarted, [ProjectName]));

  GetBPRFileInfo(ProjectName, Unused, @Description);

  Result := CompileBCBProject(ProjectName, OutputDir, DcpSearchPath) and
    RegisterExpert(BinaryFileName(OutputDir, ProjectName), Description);

  OutputString(RsExpertInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallBCBIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  RunOnly: Boolean;
  Unused, Description: string;
begin
  OutputString(Format(RsIdePackageInstallationStarted, [PackageName]));

  GetBPKFileInfo(PackageName, RunOnly, @Unused, @Description);
  if RunOnly then
    raise EJclBorRadException.CreateResFmt(@RsECannotInstallRunOnly, [PackageName]);

  Result := CompileBCBPackage(PackageName, BPLPath, DCPPath) and
    RegisterIdePackage(BinaryFileName(BPLPath, PackageName), Description);

  OutputString(RsIdePackageInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallBCBPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  RunOnly: Boolean;
  Unused, Description: string;
begin
  OutputString(Format(RsPackageInstallationStarted, [PackageName]));

  GetBPKFileInfo(PackageName, RunOnly, @Unused, @Description);
  if RunOnly then
    raise EJclBorRadException.CreateResFmt(@RsECannotInstallRunOnly, [PackageName]);

  Result := CompileBCBPackage(PackageName, BPLPath, DCPPath) and
    RegisterPackage(BinaryFileName(BPLPath, PackageName), Description);

  OutputString(RsPackageInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallDelphiExpert(const ProjectName, OutputDir, DcpSearchPath: string): Boolean;
var
  BaseName: string;
begin
  OutputString(Format(RsExpertInstallationStarted, [ProjectName]));

  BaseName := PathExtractFileNameNoExt(ProjectName);

  Result := CompileDelphiProject(ProjectName, OutputDir, DcpSearchPath) and
    RegisterExpert(BinaryFileName(OutputDir, ProjectName), BaseName);

  OutputString(RsExpertInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallDelphiIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  RunOnly: Boolean;
  Unused, Description: string;
begin
  OutputString(Format(RsIdePackageInstallationStarted, [PackageName]));

  GetDPKFileInfo(PackageName, RunOnly, @Unused, @Description);
  if RunOnly then
    raise EJclBorRadException.CreateResFmt(@RsECannotInstallRunOnly, [PackageName]);

  Result := CompileDelphiPackage(PackageName, BPLPath, DCPPath) and
    RegisterIdePackage(BinaryFileName(BPLPath, PackageName), Description);

  OutputString(RsIdePackageInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallDelphiPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  RunOnly: Boolean;
  Unused, Description: string;
begin
  OutputString(Format(RsPackageInstallationStarted, [PackageName]));

  GetDPKFileInfo(PackageName, RunOnly, @Unused, @Description);
  if RunOnly then
    raise EJclBorRadException.CreateResFmt(@RsECannotInstallRunOnly, [PackageName]);

  Result := CompileDelphiPackage(PackageName, BPLPath, DCPPath) and
    RegisterPackage(BinaryFileName(BPLPath, PackageName), Description);

  OutputString(RsPackageInstallationFinished);
end;

function TJclBorRADToolInstallation.InstallExpert(const ProjectName, OutputDir, DcpSearchPath: string): Boolean;
var
  ProjectExtension: string;
begin
  ProjectExtension := ExtractFileExt(ProjectName);
  if SameText(ProjectExtension, SourceExtensionBCBProject) then
    Result := InstallBCBExpert(ProjectName, OutputDir, DcpSearchPath)
  else
  if SameText(ProjectExtension, SourceExtensionDelphiProject) then
    Result := InstallDelphiExpert(ProjectName, OutputDir, DcpSearchPath)
  else
    raise EJclBorRADException.CreateResFmt(@RsEUnknownProjectExtension, [ProjectExtension]);
end;

function TJclBorRADToolInstallation.InstallIDEPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  PackageExtension: string;
begin
  PackageExtension := ExtractFileExt(PackageName);
  if SameText(PackageExtension, SourceExtensionBCBPackage) then
    Result := InstallBCBIdePackage(PackageName, BPLPath, DCPPath)
  else
  if SameText(PackageExtension, SourceExtensionDelphiPackage) then
    Result := InstallDelphiIdePackage(PackageName, BPLPath, DCPPath)
  else
    raise EJclBorRADException.CreateResFmt(@RsEUnknownIdePackageExtension, [PackageExtension]);
end;

function TJclBorRADToolInstallation.InstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  PackageExtension: string;
begin
  PackageExtension := ExtractFileExt(PackageName);
  if SameText(PackageExtension, SourceExtensionBCBPackage) then
    Result := InstallBCBPackage(PackageName, BPLPath, DCPPath)
  else
  if SameText(PackageExtension, SourceExtensionDelphiPackage) then
    Result := InstallDelphiPackage(PackageName, BPLPath, DCPPath)
  else
    raise EJclBorRADException.CreateResFmt(@RsEUnknownPackageExtension, [PackageExtension]);
end;

{$IFDEF KEEP_DEPRECATED}
function TJclBorRADToolInstallation.IsBDSPersonality: Boolean;
begin
  {$IFDEF MSWINDOWS}
  Result := InheritsFrom(TJclBDSInstallation);
  {$ELSE}
  Result := False;
  {$ENDIF MSWINDOWS}
end;
{$ENDIF KEEP_DEPRECATED}

function TJclBorRADToolInstallation.LibFolderName: string;
begin
  Result := PathAddSeparator(RootDir) + PathAddSeparator('lib');
end;

function TJclBorRADToolInstallation.ObjFolderName: string;
begin
  Result := LibFolderName + PathAddSeparator('obj');
end;

function TJclBorRADToolInstallation.ProcessMapFile(const BinaryFileName: string): Boolean;
{$IFDEF MSWINDOWS}
var
  MAPFileName, LinkerBugUnit: string;
  MAPFileSize, JclDebugDataSize: Integer;
{$ENDIF MSWINDOWS}
begin
  {$IFDEF MSWINDOWS}
  if JdbgCreate then
  begin
    MAPFileName := ChangeFileExt(BinaryFileName, CompilerExtensionMAP);

    if JdbgInsert then
    begin
      OutputString(Format(RsInsertingJdbg, [BinaryFileName]));
      Result := InsertDebugDataIntoExecutableFile(BinaryFileName, MAPFileName,
        LinkerBugUnit, MAPFileSize, JclDebugDataSize);
      OutputString(Format(RsJdbgInfo, [LinkerBugUnit, MAPFileSize, JclDebugDataSize]));
    end
    else
    begin
      OutputString(Format(RsCreatingJdbg, [BinaryFileName]));
      Result := ConvertMapFileToJdbgFile(MAPFileName);
    end;
    if Result then
    begin
      OutputString(RsJdbgInfoOk);
      if MapDelete then
        OutputFileDelete(MAPFileName);
    end
    else
      OutputString(RsJdbgInfoFailed);
  end
  else
    Result := True;
  {$ELSE MSWINDOWS}
  Result := True;
  {$ENDIF MSWINDOWS}
end;

function TJclBorRADToolInstallation.OutputFileDelete(const FileName: string): Boolean;
begin
  OutputString(Format(RsDeletingFile, [FileName]));
  Result := FileDelete(FileName);
  if Result then
    OutputString(RsFileDeletionOk)
  else
    OutputString(RsFileDeletionFailed);
end;

procedure TJclBorRADToolInstallation.OutputString(const AText: string);
begin
  if Assigned(FOutputCallback) then
    OutputCallback(AText);
end;

class function TJclBorRADToolInstallation.PackageSourceFileExtension: string;
begin
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ELSE MSWINDOWS}
  Result := '';
  {$ENDIF MSWINDOWS}
end;

class function TJclBorRADToolInstallation.ProjectSourceFileExtension: string;
begin
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ELSE MSWINDOWS}
  Result := '';
  {$ENDIF MSWINDOWS}
end;

class function TJclBorRADToolInstallation.RADToolKind: TJclBorRADToolKind;
begin
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ELSE MSWINDOWS}
  Result := brDelphi;
  {$ENDIF MSWINDOWS}
end;

{class }function TJclBorRADToolInstallation.RADToolName: string;
begin
  {$IFDEF MSWINDOWS}
  raise EAbstractError.CreateResFmt(@SAbstractError, ['']); // BCB doesn't support abstract keyword
  {$ELSE MSWINDOWS}
  Result := '';
  {$ENDIF MSWINDOWS}
end;

procedure TJclBorRADToolInstallation.ReadInformation;
const
  {$IFDEF KYLIX}
  BinDir = 'bin/';
  {$ELSE ~KYLIX}
  BinDir = 'bin\';
  {$ENDIF ~KYLIX}
  UpdateKeyName = 'Update #';
  BDSUpdateKeyName = 'UpdatePackInstalled';
var
  KeyLen, I: Integer;
  Key: string;
  Ed: TJclBorRADToolEdition;

  function FormatVersionNumber(const Num: Integer): string;
  begin
    Result := '';
    case RadToolKind of
      {$IFDEF KYLIX}
      brDelphi:
        Result := Format('kd%d', [Num]);
      brCppBuilder:
        Result := Format('kc%d', [Num]);
      {$ELSE ~KYLIX}
      brDelphi:
        Result := Format('d%d', [Num]);
      brCppBuilder:
        Result := Format('c%d', [Num]);
      {$ENDIF ~KYLIX}
      brBorlandDevStudio:
        case Num of
          1:
            Result := 'cs1';
        else
          Result := Format('d%d', [Num + 6]);  // BDS 2 goes to D8
        end;
    end;
  end;

begin
  Key := ConfigData.FileName;
  {$IFDEF KYLIX}
  ConfigData.ReadSectionValues(GlobalsKeyName, Globals);
  if Length(Key) >= 3 then
  begin
    case Key[Length(Key)-2] of
      '0' :
        FVersionNumber := 1;
      '5' :
        FVersionNumber := 2;
      '9' :
        FVersionNumber := 3;
    else
      FVersionNumber := 0;
    end;
  end;
  FIDEVersionNumber := VersionNumber;

  {$ELSE ~KYLIX}
  RegGetValueNamesAndValues(HKEY_LOCAL_MACHINE, Key, Globals);

  KeyLen := Length(Key);
  if (KeyLen > 3) and StrIsDigit(Key[KeyLen - 2]) and (Key[KeyLen - 1] = '.') and (Key[KeyLen] = '0') then
    FIDEVersionNumber := Ord(Key[KeyLen - 2]) - Ord('0')
  else
    FIDEVersionNumber := 0;

 // If this is Spacely, then consider the version is equal to 4 (BDS2006)
 // as it is a non breaking version (dcu wise)

 { ahuser: Delphi 2007 is a non breaking version in the case that you can use
   BDS 2006 compiled units in Delphi 2007. But it completely breaks the BDS 2006
   installation because if BDS 2006 uses the Delphi 2007 compile DCUs the
   resulting executable is broken and will do strange things. So treat Delphi 2007
   as version 11 what it actually is. }
 {if (FIDEVersionNumber = 5) and (RadToolKind = brBorlandDevStudio) then
    FVersionNumber := 4
  else}
    FVersionNumber := FIDEVersionNumber;

  {$ENDIF ~KYLIX}

  FVersionNumberStr := FormatVersionNumber(VersionNumber);
  FIDEVersionNumberStr := FormatVersionNumber(IDEVersionNumber);

  FRootDir := PathRemoveSeparator(Globals.Values[RootDirValueName]);
  FBinFolderName := PathAddSeparator(RootDir) + BinDir;

  FEditionStr := Globals.Values[EditionValueName];
  if FEditionStr = '' then
    FEditionStr := Globals.Values[VersionValueName];
  { TODO : Edition detection for BDS }
  for Ed := Low(Ed) to High(Ed) do
    if StrIPos(BorRADToolEditionIDs[Ed], FEditionStr) = 1 then
      FEdition := Ed;

  if RadToolKind = brBorlandDevStudio then
    FInstalledUpdatePack := StrToIntDef(Globals.Values[BDSUpdateKeyName], 0)
  else
    for I := 0 to Globals.Count - 1 do
    begin
      Key := Globals.Names[I];
      KeyLen := Length(UpdateKeyName);
      if (Pos(UpdateKeyName, Key) = 1) and (Length(Key) > KeyLen) and StrIsDigit(Key[KeyLen + 1]) then
        FInstalledUpdatePack := Max(FInstalledUpdatePack, Integer(Ord(Key[KeyLen + 1]) - 48));
    end;
end;

function TJclBorRADToolInstallation.RegisterExpert(const ProjectName, OutputDir, Description: string): Boolean;
begin
  Result := RegisterExpert(BinaryFileName(OutputDir, ProjectName), Description);
end;

function TJclBorRADToolInstallation.RegisterExpert(const BinaryFileName, Description: string): Boolean;
var
  InternalDescription: string;
begin
  OutputString(Format(RsRegisteringExpert, [BinaryFileName]));

  if Description = '' then
    InternalDescription := PathExtractFileNameNoExt(BinaryFileName)
  else
    InternalDescription := Description;

  Result := IdePackages.AddExpert(BinaryFileName, InternalDescription);
  if Result then
    OutputString(RsRegistrationOk)
  else
    OutputString(RsRegistrationFailed);
end;

function TJclBorRADToolInstallation.RegisterIDEPackage(const PackageName, BPLPath, Description: string): Boolean;
begin
  Result := RegisterIDEPackage(BinaryFileName(BPLPath, PackageName), Description);
end;

function TJclBorRADToolInstallation.RegisterIDEPackage(const BinaryFileName, Description: string): Boolean;
var
  InternalDescription: string;
begin
  OutputString(Format(RsRegisteringIdePackage, [BinaryFileName]));

  if Description = '' then
    InternalDescription := PathExtractFileNameNoExt(BinaryFileName)
  else
    InternalDescription := Description;

  Result := IdePackages.AddIDEPackage(BinaryFileName, InternalDescription);
  if Result then
    OutputString(RsRegistrationOk)
  else
    OutputString(RsRegistrationFailed);
end;

function TJclBorRADToolInstallation.RegisterPackage(const PackageName, BPLPath, Description: string): Boolean;
begin
  Result := RegisterPackage(BinaryFileName(BPLPath, PackageName), Description);
end;

function TJclBorRADToolInstallation.RegisterPackage(const BinaryFileName, Description: string): Boolean;
var
  InternalDescription: string;
begin
  OutputString(Format(RsRegisteringPackage, [BinaryFileName]));

  if Description = '' then
    InternalDescription := PathExtractFileNameNoExt(BinaryFileName)
  else
    InternalDescription := Description;

  Result := IdePackages.AddPackage(BinaryFileName, InternalDescription);
  if Result then
    OutputString(RsRegistrationOk)
  else
    OutputString(RsRegistrationFailed);
end;

function TJclBorRADToolInstallation.RemoveFromDebugDCUPath(const Path: string): Boolean;
var
  TempDebugDCUPath: TJclBorRADToolPath;
begin
  TempDebugDCUPath := DebugDCUPath;
  Result := RemoveFromPath(TempDebugDCUPath, Path);
  DebugDCUPath := TempDebugDCUPath;
end;

function TJclBorRADToolInstallation.RemoveFromLibrarySearchPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclBorRADToolPath;
begin
  TempLibraryPath := LibrarySearchPath;
  Result := RemoveFromPath(TempLibraryPath, Path);
  LibrarySearchPath := TempLibraryPath;
end;

function TJclBorRADToolInstallation.RemoveFromLibraryBrowsingPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclBorRADToolPath;
begin
  TempLibraryPath := LibraryBrowsingPath;
  Result := RemoveFromPath(TempLibraryPath, Path);
  LibraryBrowsingPath := TempLibraryPath;
end;

function TJclBorRADToolInstallation.RemoveFromPath(var Path: string; const ItemsToRemove: string): Boolean;
var
  PathItems, RemoveItems: TStringList;
  Folder: string;
  I, J: Integer;
begin
  Result := False;
  PathItems := nil;
  RemoveItems := nil;
  try
    PathItems := TStringList.Create;
    RemoveItems := TStringList.Create;
    ExtractPaths(Path, PathItems);
    ExtractPaths(ItemsToRemove, RemoveItems);
    for I := 0 to RemoveItems.Count - 1 do
    begin
      Folder := RemoveItems[I];
      J := FindFolderInPath(Folder, PathItems);
      if J <> -1 then
      begin
        PathItems.Delete(J);
        Result := True;
      end;
    end;
    Path := StringsToStr(PathItems, PathSep, False);
  finally
    PathItems.Free;
    RemoveItems.Free;
  end;
end;

procedure TJclBorRADToolInstallation.SetDebugDCUPath(const Value: TJclBorRADToolPath);
begin
  ConfigData.WriteString(DebuggingKeyName, DebugDCUPathValueName, Value);
end;

procedure TJclBorRADToolInstallation.SetLibrarySearchPath(const Value: TJclBorRADToolPath);
begin
  ConfigData.WriteString(LibraryKeyName, LibrarySearchPathValueName, Value);
end;

procedure TJclBorRADToolInstallation.SetOutputCallback(const Value: TTextHandler);
begin
  FOutputCallback := Value;
  //if clAsm in CommandLineTools then
  //  Asm.OutputCallback := Value;
  if clBcc32 in CommandLineTools then
    Bcc32.OutputCallback := Value;
  if clDcc32 in CommandLineTools then
    Dcc32.OutputCallback := Value;
  //if clDccIL in CommandLineTools then
  //  DccIL.OutputCallback := Value;
  if clMake in CommandLineTools then
    Make.OutputCallback := Value;
  if clProj2Mak in CommandLineTools then
    Bpr2Mak.OutputCallback := Value;
end;

procedure TJclBorRADToolInstallation.SetLibraryBrowsingPath(const Value: TJclBorRADToolPath);
begin
  ConfigData.WriteString(LibraryKeyName, LibraryBrowsingPathValueName, Value);
end;

function TJclBorRADToolInstallation.SubstitutePath(const Path: string): string;
var
  I: Integer;
  Name: string;
begin
  Result := Path;
  if Pos('$(', Result) > 0 then
    with EnvironmentVariables do
      for I := 0 to Count - 1 do
      begin
        Name := Names[I];
        Result := StringReplace(Result, Format('$(%s)', [Name]), Values[Name], [rfReplaceAll, rfIgnoreCase]);
      end;
  // remove duplicate path delimiters '\\'
  Result := StringReplace(Result, DirDelimiter + DirDelimiter, DirDelimiter, [rfReplaceAll]);
end;

{$IFDEF KEEP_DEPRECATED}
function TJclBorRADToolInstallation.SupportsBCB: Boolean;
begin
  Result := clBCC32 in CommandLineTools;
end;
{$ENDIF KEEP_DEPRECATED}

function TJclBorRADToolInstallation.SupportsVCL: Boolean;
const
  VclDcp = 'vcl.dcp';
begin
  {$IFDEF KYLIX}
  Result := False;
  {$ELSE ~KYLIX}
  Result := (RadToolKind = brBorlandDevStudio) or (VersionNumber >= 6) and
    (FileExists(LibFolderName + VclDcp) or FileExists(ObjFolderName + VclDcp));
  {$ENDIF ~KYLIX}
end;

function TJclBorRADToolInstallation.SupportsVisualCLX: Boolean;
const
  VisualClxDcp = 'visualclx.dcp';
begin
  {$IFDEF KYLIX}
  Result := True;
  {$ELSE}
  Result := (Edition <> deSTD) and (VersionNumber in [6, 7]) and (RadToolKind <> brBorlandDevStudio) and
    (FileExists(LibFolderName + VisualClxDcp) or FileExists(ObjFolderName + VisualClxDcp));
  {$ENDIF KYLIX}
end;

function TJclBorRADToolInstallation.UninstallBCBExpert(const ProjectName, OutputDir: string): Boolean;
var
  DllFileName: string;
begin
  OutputString(Format(RsExpertUninstallationStarted, [ProjectName]));

  if not IsBCBProject(ProjectName) then
    raise EJclBorRADException.CreateResFmt(@RsENotABCBProject, [ProjectName]);

  DllFileName := BinaryFileName(OutputDir, ProjectName);
  // important: remove from experts /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := UnregisterExpert(DllFileName);

  if Result then
    OutputFileDelete(DllFileName);

  OutputString(RsExpertUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallBCBIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  MAPFileName, TDSFileName,
  BPIFileName, LIBFileName, BPLFileName: string;
  RunOnly: Boolean;
begin
  OutputString(Format(RsIdePackageUninstallationStarted, [PackageName]));

  if not IsBCBPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsENotABCBPackage, [PackageName]);

  GetBPKFileInfo(PackageName, RunOnly);

  BPLFileName := BinaryFileName(BPLPath, PackageName);

  // important: remove from IDE packages /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := (RunOnly or UnregisterIdePackage(BPLFileName));

  // Don't delete binaries if removal of design time package failed
  if Result then
  begin
    OutputFileDelete(BPLFileName);

    BPIFileName := PathAddSeparator(DCPPath) + PathExtractFileNameNoExt(PackageName) + CompilerExtensionBPI;
    OutputFileDelete(BPIFileName);

    LIBFileName := ChangeFileExt(BPIFileName, CompilerExtensionLIB);
    OutputFileDelete(LIBFileName);

    MAPFileName := ChangeFileExt(BPLFileName, CompilerExtensionMAP);
    OutputFileDelete(MAPFileName);

    TDSFileName := ChangeFileExt(BPLFileName, CompilerExtensionTDS);
    OutputFileDelete(TDSFileName);
  end;

  OutputString(RsIdePackageUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallBCBPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  MAPFileName, TDSFileName, TmpBinaryFileName,
  BPIFileName, LIBFileName, BPLFileName: string;
  RunOnly: Boolean;
begin
  OutputString(Format(RsPackageUninstallationStarted, [PackageName]));

  if not IsBCBPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsENotABCBPackage, [PackageName]);

  GetBPKFileInfo(PackageName, RunOnly, @TmpBinaryFileName);

  BPLFileName := BinaryFileName(BPLPath, PackageName);

  // important: remove from IDE packages /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := (RunOnly or UnregisterPackage(BPLFileName));

  // Don't delete binaries if removal of design time package failed
  if Result then
  begin
    OutputFileDelete(BPLFileName);

    BPIFileName := PathAddSeparator(DCPPath) + PathExtractFileNameNoExt(PackageName) + CompilerExtensionBPI;
    OutputFileDelete(BPIFileName);

    LIBFileName := ChangeFileExt(BPIFileName, CompilerExtensionLIB);
    OutputFileDelete(LIBFileName);

    MAPFileName := ChangeFileExt(BPLFileName, CompilerExtensionMAP);
    OutputFileDelete(MAPFileName);

    TDSFileName := ChangeFileExt(BPLFileName, CompilerExtensionTDS);
    OutputFileDelete(TDSFileName);
  end;

  OutputString(RsPackageUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallDelphiExpert(const ProjectName, OutputDir: string): Boolean;
var
  DllFileName: string;
begin
  OutputString(Format(RsExpertUninstallationStarted, [ProjectName]));

  if not IsDelphiProject(ProjectName) then
    raise EJclBorRADException.CreateResFmt(@RsENotADelphiProject, [ProjectName]);

  DllFileName := BinaryFileName(OutputDir, ProjectName);
  // important: remove from experts /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := UnregisterExpert(DllFileName);

  if Result then
    OutputFileDelete(DllFileName);

  OutputString(RsExpertUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallDelphiIdePackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  MAPFileName,
  BPLFileName, DCPFileName: string;
  BaseName: string;
  RunOnly: Boolean;
begin
  OutputString(Format(RsIdePackageUninstallationStarted, [PackageName]));

  if not IsDelphiPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsENotADelphiPackage, [PackageName]);

  GetDPKFileInfo(PackageName, RunOnly);
  BaseName := PathExtractFileNameNoExt(PackageName);

  BPLFileName := BinaryFileName(BPLPath, PackageName);

  // important: remove from IDE packages /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := RunOnly or UnregisterIdePackage(BPLFileName);

  // Don't delete binaries if removal of design time package failed
  if Result then
  begin
    OutputFileDelete(BPLFileName);

    DCPFileName := PathAddSeparator(DCPPath) + BaseName + CompilerExtensionDCP;
    OutputFileDelete(DCPFileName);

    MAPFileName := ChangeFileExt(BPLFileName, CompilerExtensionMAP);
    OutputFileDelete(MAPFileName);
  end;

  OutputString(RsIdePackageUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallDelphiPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  MAPFileName, BPLFileName, DCPFileName: string;
  BaseName: string;
  RunOnly: Boolean;
begin
  OutputString(Format(RsPackageUninstallationStarted, [PackageName]));

  if not IsDelphiPackage(PackageName) then
    raise EJclBorRADException.CreateResFmt(@RsENotADelphiPackage, [PackageName]);

  GetDPKFileInfo(PackageName, RunOnly);
  BaseName := PathExtractFileNameNoExt(PackageName);

  BPLFileName := BinaryFileName(BPLPath, PackageName);

  // important: remove from IDE packages /before/ deleting;
  //            otherwise PathGetLongPathName won't work
  Result := RunOnly or UnregisterPackage(BPLFileName);

  // Don't delete binaries if removal of design time package failed
  if Result then
  begin
    OutputFileDelete(BPLFileName);

    DCPFileName := PathAddSeparator(DCPPath) + BaseName + CompilerExtensionDCP;
    OutputFileDelete(DCPFileName);

    MAPFileName := ChangeFileExt(BPLFileName, CompilerExtensionMAP);
    OutputFileDelete(MAPFileName);
  end;

  OutputString(RsPackageUninstallationFinished);
end;

function TJclBorRADToolInstallation.UninstallExpert(const ProjectName, OutputDir: string): Boolean;
var
  ProjectExtension: string;
begin
  ProjectExtension := ExtractFileExt(ProjectName);
  if SameText(ProjectExtension, SourceExtensionBCBProject) then
    Result := UninstallBCBExpert(ProjectName, OutputDir)
  else
  if SameText(ProjectExtension, SourceExtensionDelphiProject) then
    Result := UninstallDelphiExpert(ProjectName, OutputDir)
  else
    raise EJclBorRadException.CreateResFmt(@RsEUnknownProjectExtension, [ProjectExtension]);
end;

function TJclBorRADToolInstallation.UninstallIDEPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  PackageExtension: string;
begin
  PackageExtension := ExtractFileExt(PackageName);
  if SameText(PackageExtension, SourceExtensionBCBPackage) then
    Result := UninstallBCBIdePackage(PackageName, BPLPath, DCPPath)
  else
  if SameText(PackageExtension, SourceExtensionDelphiPackage) then
    Result := UninstallDelphiIdePackage(PackageName, BPLPath, DCPPath)
  else
    raise EJclBorRadException.CreateResFmt(@RsEUnknownIdePackageExtension, [PackageExtension]);
end;

function TJclBorRADToolInstallation.UninstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
var
  PackageExtension: string;
begin
  PackageExtension := ExtractFileExt(PackageName);
  if SameText(PackageExtension, SourceExtensionBCBPackage) then
    Result := UninstallBCBPackage(PackageName, BPLPath, DCPPath)
  else
  if SameText(PackageExtension, SourceExtensionDelphiPackage) then
    Result := UninstallDelphiPackage(PackageName, BPLPath, DCPPath)
  else
    raise EJclBorRadException.CreateResFmt(@RsEUnknownPackageExtension, [PackageExtension]);
end;

function TJclBorRADToolInstallation.UnregisterExpert(const ProjectName, OutputDir: string): Boolean;
begin
  Result := UnregisterExpert(BinaryFileName(OutputDir, ProjectName));
end;

function TJclBorRADToolInstallation.UnregisterExpert(const BinaryFileName: string): Boolean;
begin
  OutputString(Format(RsUnregisteringExpert, [BinaryFileName]));

  Result := IdePackages.RemoveExpert(BinaryFileName);
  if Result then
    OutputString(RsUnregistrationOk)
  else
    OutputString(RsUnregistrationFailed);
end;

function TJclBorRADToolInstallation.UnregisterIDEPackage(const PackageName, BPLPath: string): Boolean;
begin
  Result := UnregisterIDEPackage(BinaryFileName(BPLPath, PackageName));
end;

function TJclBorRADToolInstallation.UnregisterIDEPackage(const BinaryFileName: string): Boolean;
begin
  OutputString(Format(RsUnregisteringIDEPackage, [BinaryFileName]));

  Result := IdePackages.RemoveIDEPackage(BinaryFileName);
  if Result then
    OutputString(RsUnregistrationOk)
  else
    OutputString(RsUnregistrationFailed);
end;

function TJclBorRADToolInstallation.UnregisterPackage(const PackageName, BPLPath: string): Boolean;
begin
  Result := UnregisterPackage(BinaryFileName(BPLPath, PackageName));
end;

function TJclBorRADToolInstallation.UnregisterPackage(const BinaryFileName: string): Boolean;
begin
  OutputString(Format(RsUnregisteringPackage, [BinaryFileName]));

  Result := IdePackages.RemovePackage(BinaryFileName);
  if Result then
    OutputString(RsUnregistrationOk)
  else
    OutputString(RsUnregistrationFailed);
end;

//=== { TJclBCBInstallation } ================================================

constructor TJclBCBInstallation.Create(const AConfigDataLocation: string; ARootKey: Cardinal);
begin
  inherited Create(AConfigDataLocation, ARootKey);
  FPersonalities := [bpBCBuilder32];
  if clDcc32 in CommandLineTools then
    Include(FPersonalities, bpDelphi32);
end;

destructor TJclBCBInstallation.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF KYLIX}
function TJclBCBInstallation.ConfigFileName(const Extension: string): string;
begin
  Result := Format('%s/.borland/bcb%d%s', [GetPersonalFolder, IDs[VersionNumber], Extension]);
end;
{$ENDIF KYLIX}

function TJclBCBInstallation.GetEnvironmentVariables: TStrings;
begin
  Result := inherited GetEnvironmentVariables;
  if Assigned(Result) then
    Result.Values['BCB'] := PathRemoveSeparator(RootDir);
end;

class function TJclBCBInstallation.GetLatestUpdatePackForVersion(Version: Integer): Integer;
begin
  case Version of
    5:
      Result := 0;
    6:
      Result := 4;
    10:
      Result := 0;
  else
    Result := 0;
  end;
end;

class function TJclBCBInstallation.PackageSourceFileExtension: string;
begin
  Result := SourceExtensionBCBPackage;
end;

class function TJclBCBInstallation.ProjectSourceFileExtension: string;
begin
  Result := SourceExtensionBCBProject;
end;

class function TJclBCBInstallation.RadToolKind: TJclBorRadToolKind;
begin
  Result := brCppBuilder;
end;

function TJclBCBInstallation.RADToolName: string;
begin
  Result := RsBCBName;
end;

//=== { TJclDelphiInstallation } =============================================

constructor TJclDelphiInstallation.Create(const AConfigDataLocation: string; ARootKey: Cardinal);
begin
  inherited Create(AConfigDataLocation, ARootKey);
  FPersonalities := [bpDelphi32];
end;

destructor TJclDelphiInstallation.Destroy;
begin
  inherited Destroy;
end;

{$IFDEF KYLIX}
function TJclDelphiInstallation.ConfigFileName(const Extension: string): string;
begin
  Result := Format('%s/.borland/delphi%d%s', [GetPersonalFolder, IDs[VersionNumber], Extension]);
end;
{$ENDIF KYLIX}

function TJclDelphiInstallation.GetEnvironmentVariables: TStrings;
begin
  Result := inherited GetEnvironmentVariables;
  if Assigned(Result) then
    Result.Values['DELPHI'] := PathRemoveSeparator(RootDir);
end;

class function TJclDelphiInstallation.GetLatestUpdatePackForVersion(Version: Integer): Integer;
begin
  case Version of
    5:
      Result := 1;
    6:
      Result := 2;
    7:
      Result := 0;
  else
    Result := 0;
  end;
end;

function TJclDelphiInstallation.InstallPackage(const PackageName, BPLPath, DCPPath: string): Boolean;
begin
  Result := InstallDelphiPackage(PackageName, BPLPath, DCPPath);
end;

class function TJclDelphiInstallation.PackageSourceFileExtension: string;
begin
  Result := SourceExtensionDelphiPackage;
end;

class function TJclDelphiInstallation.ProjectSourceFileExtension: string;
begin
  Result := SourceExtensionDelphiProject;
end;

class function TJclDelphiInstallation.RadToolKind: TJclBorRadToolKind;
begin
  Result := brDelphi;
end;

function TJclDelphiInstallation.RADToolName: string;
begin
  Result := RsDelphiName;
end;

//=== { TJclBDSInstallation } ==================================================

{$IFDEF MSWINDOWS}

constructor TJclBDSInstallation.Create(const AConfigDataLocation: string; ARootKey: Cardinal = 0);
const
  PersonalitiesSection = 'Personalities';
begin
  inherited Create(AConfigDataLocation, ARootKey);
  FHelp2Manager := TJclHelp2Manager.Create(Self);

  if ConfigData.ReadString(PersonalitiesSection, 'C#Builder', '') <> '' then
    Include(FPersonalities, bpCSBuilder32);
  if ConfigData.ReadString(PersonalitiesSection, 'BCB', '') <> '' then
    Include(FPersonalities, bpBCBuilder32);
  if ConfigData.ReadString(PersonalitiesSection, 'Delphi.Win32', '') <> '' then
    Include(FPersonalities, bpDelphi32);
  if (ConfigData.ReadString(PersonalitiesSection, 'Delphi.NET', '') <> '') or
    (ConfigData.ReadString(PersonalitiesSection, 'Delphi8', '') <> '') then
  begin
    Include(FPersonalities, bpDelphiNet32);
    if VersionNumber >= 5 then
      Include(FPersonalities, bpDelphiNet64);
  end;

  if clDcc32 in CommandLineTools then
    Include(FPersonalities, bpDelphi32);
end;

destructor TJclBDSInstallation.Destroy;
begin
  FreeAndNil(FDCCIL);
  FreeAndNil(FHelp2Manager);
  inherited Destroy;
end;

function TJclBDSInstallation.AddToCppBrowsingPath(const Path: string): Boolean;
var
  TempCppPath: TJclBorRADToolPath;
begin
  if bpBCBuilder32 in Personalities then
  begin
    TempCppPath := CppBrowsingPath;
    PathListIncludeItems(TempCppPath, Path);
    Result := True;
    CppBrowsingPath := TempCppPath;
  end
  else
    Result := False;
end;

function TJclBDSInstallation.AddToCppSearchPath(const Path: string): Boolean;
var
  TempCppPath: TJclBorRADToolPath;
begin
  if bpBCBuilder32 in Personalities then
  begin
    TempCppPath := CppSearchPath;
    PathListIncludeItems(TempCppPath, Path);
    Result := True;
    CppSearchPath := TempCppPath;
  end
  else
    Result := False;
end;

function TJclBDSInstallation.AddToCppLibraryPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclBorRADToolPath;
begin
  if (bpBCBuilder32 in Personalities) and (IDEVersionNumber >= 5) then
  begin
    TempLibraryPath := CppLibraryPath;
    PathListIncludeItems(TempLibraryPath, Path);
    Result := True;
    CppLibraryPath := TempLibraryPath;
  end
  else
    Result := False;
end;

function TJclBDSInstallation.CleanPackageCache(const BinaryFileName: string): Boolean;
var
  FileName, KeyName: string;
begin
  Result := True;

  if VersionNumber >= 3 then
  begin
    FileName := ExtractFileName(BinaryFileName);

    try
      OutputString(Format(RsCleaningPackageCache, [FileName]));
      KeyName := PathAddSeparator(ConfigDataLocation) + PackageCacheKeyName + '\' + FileName;

      if RegKeyExists(RootKey, KeyName) then
        Result := RegDeleteKeyTree(RootKey, KeyName);

      if Result then
        OutputString(RsCleaningOk)
      else
        OutputString(RsCleaningFailed);
    except
      // trap possible exceptions
    end;
  end;
end;

function TJclBDSInstallation.CompileDelphiDotNetProject(const ProjectName,
  OutputDir: string; PEFormat: TJclBorPlatform; const CLRVersion,
  ExtraOptions: string): Boolean;
var
  DCCILOptions, PlatformOption, PdbOption: string;
begin
  if VersionNumber >= 2 then   // C#Builder 1 doesn't have any Delphi.net compiler
  begin
    if IsDelphiProject(ProjectName) then
      OutputString(Format(RsCompilingProject, [ProjectName]))
    else
    if IsDelphiPackage(ProjectName) then
      OutputString(Format(RsCompilingPackage, [ProjectName]))
    else
      raise EJclBorRADException.CreateResFmt(@RsENotADelphiProject, [ProjectName]);

    PlatformOption := '';
    case PEFormat of
      bp32bit:
        if VersionNumber >= 3 then
          PlatformOption := 'x86';
      bp64bit:
        if VersionNumber >= 3 then
          PlatformOption := 'x64'
        else
          raise EJclBorRADException.CreateRes(@RsEx64PlatformNotValid);
    end;

    if PdbCreate then
      PdbOption := '-V'
    else
      PdbOption := '';

    DCCILOptions := Format('%s --platform:%s %s', [ExtraOptions, PlatformOption, PdbOption]);

    Result := DCCIL.MakeProject(ProjectName, OutputDir, DCCILOptions);

    if Result then
      OutputString(RsCompilationOk)
    else
      OutputString(RsCompilationFailed);
  end
  else
    raise EJclBorRADException.CreateRes(@RsENoSupportedPersonality);
end;

function TJclBDSInstallation.CompileDelphiPackage(const PackageName, BPLPath, DCPPath, ExtraOptions: string): Boolean;
var
  NewOptions: string;
begin
  if DualPackageInstallation then
  begin
    if not (bpBCBuilder32 in Personalities) then
      raise EJclBorRadException.CreateResFmt(@RsEDualPackageNotSupported, [Name]);

    NewOptions := Format('%s -JL -NB"%s" -NO"%s"',
      [ExtraOptions, PathRemoveSeparator(DcpPath),
       PathRemoveSeparator(DcpPath)]);
  end
  else
    NewOptions := ExtraOptions;

  Result := inherited CompileDelphiPackage(PackageName, BPLPath, DCPPath, NewOptions);
end;

function TJclBDSInstallation.CompileDelphiProject(const ProjectName, OutputDir, DcpSearchPath: string): Boolean;
var
  ExtraOptions: string;
begin
  if VersionNumber <= 2 then
  begin
    OutputString(Format(RsCompilingProject, [ProjectName]));

    if not IsDelphiProject(ProjectName) then
      raise EJclBorRADException.CreateResFmt(@RsENotADelphiProject, [ProjectName]);

    if MapCreate then
      ExtraOptions := '-GD'
    else
      ExtraOptions := '';

    Result := DCC32.MakeProject(ProjectName, OutputDir, DcpSearchPath, ExtraOptions) and
      ProcessMapFile(BinaryFileName(OutputDir, ProjectName));

    if Result then
      OutputString(RsCompilationOk)
    else
      OutputString(RsCompilationFailed);
  end
  else
    Result := inherited CompileDelphiProject(ProjectName, DcpSearchPath, OutputDir);
end;

function TJclBDSInstallation.GetBPLOutputPath: string;
begin
  // BDS 1 (C#Builder 1) and BDS 2 (Delphi 8) don't have a valid BPL output path
  // set in the registry
  case IDEVersionNumber of
    1, 2:
      Result := PathAddSeparator(GetDefaultProjectsDir) + 'bpl';
    3, 4:
      Result := inherited GetBPLOutputPath;
    5:
      begin
        Result := SubstitutePath(GetMsBuildEnvOption(MsBuildCBuilderBPLOutputPathNodeName));
        if Result = '' then
          Result := SubstitutePath(GetMsBuildEnvOption(MsBuildWin32DLLOutputPathNodeName));
      end;
  else
    Result := SubstitutePath(GetMsBuildEnvOption(MsBuildWin32DLLOutputPathNodeName));
  end;
end;

function TJclBDSInstallation.GetCommonProjectsDir: string;
begin
  Result := GetCommonProjectsDirectory(RootDir, IDEVersionNumber);
end;

class function TJclBDSInstallation.GetCommonProjectsDirectory(const RootDir: string;
  IDEVersionNumber: Integer): string;
var
  RsVarsOutput, ComSpec: string;
  Lines: TStrings;
begin
  if IDEVersionNumber >= 5 then
  begin
    Result := '';
    if GetEnvironmentVar('COMSPEC', ComSpec) and (JclSysUtils.Execute(Format('%s /C "%s%sbin%srsvars.bat && set BDS"',
      [ComSpec, ExtractShortPathName(RootDir), DirDelimiter, DirDelimiter]), RsVarsOutput) = 0) then
    begin
      Lines := TStringList.Create;
      try
        Lines.Text := RsVarsOutput;
        Result := Lines.Values[EnvVariableBDSCOMDIRValueName];
      finally
        Lines.Free;
      end;
    end;

    if Result = '' then
    begin
      Result := LoadResStrings(RootDir + '\Bin\coreide' + BDSVersions[IDEVersionNumber].CoreIdeVersion + '.',
        ['RAD Studio'])[0];

      Result := Format('%s%s%d.0',
        [PathAddSeparator(GetCommonDocumentsFolder), PathAddSeparator(Result), IDEVersionNumber]);
    end;
  end
  else
    Result := GetDefaultProjectsDirectory(RootDir, IDEVersionNumber);
end;

function TJclBDSInstallation.GetCppPathsKeyName: string;
begin
  if IDEVersionNumber >= 5 then
    Result := CppPathsV5UpperKeyName
  else
    Result := CppPathsKeyName;
end;

function TJclBDSInstallation.GetCppBrowsingPath: TJclBorRADToolPath;
begin
  Result := ConfigData.ReadString(GetCppPathsKeyName, CppBrowsingPathValueName, '');
end;

function TJclBDSInstallation.GetCppSearchPath: TJclBorRADToolPath;
begin
  Result := ConfigData.ReadString(GetCppPathsKeyName, CppSearchPathValueName, '');
end;

function TJclBDSInstallation.GetCppLibraryPath: TJclBorRADToolPath;
begin
  Result := ConfigData.ReadString(GetCppPathsKeyName, CppLibraryPathValueName, '');
end;

function TJclBDSInstallation.GetDCCIL: TJclDCCIL;
begin
  if not Assigned(FDCCIL) then
  begin
    if not (clDccIL in CommandLineTools) then
      raise EJclBorRadException.CreateResFmt(@RsENotFound, [DccILExeName]);
    FDCCIL := TJclDCCIL.Create(Self);
  end;
  Result := FDCCIL;
end;

function TJclBDSInstallation.GetDCPOutputPath: string;
begin
  case IDEVersionNumber of
    1, 2:
      // hard-coded
      Result := PathAddSeparator(RootDir) + 'lib';
    3, 4:
      // use registry
      Result := inherited GetDCPOutputPath;
    //5:
  else
    // use EnvOptions.proj
    Result := SubstitutePath(GetMsBuildEnvOption(MsBuildWin32DCPOutputNodeName));
  end;
end;

function TJclBDSInstallation.GetDebugDCUPath: TJclBorRADToolPath;
begin
  if IDEVersionNumber >= 5 then
    // use EnvOptions.proj
    Result := GetMsBuildEnvOption(MsBuildWin32DebugDCUPathNodeName)
  else
    // use registry
    Result := ConfigData.ReadString(LibraryKeyName, BDSDebugDCUPathValueName, '');
end;

function TJclBDSInstallation.GetDefaultProjectsDir: string;
begin
  Result := GetDefaultProjectsDirectory(RootDir, IDEVersionNumber);
end;

class function TJclBDSInstallation.GetDefaultProjectsDirectory(const RootDir: string;
  IDEVersionNumber: Integer): string;
var
  LocStr: WideStringArray;
begin
  LocStr := LoadResStrings(RootDir + '\Bin\coreide' + BDSVersions[IDEVersionNumber].CoreIdeVersion + '.',
    ['Borland Studio Projects', 'RAD Studio', 'Projects']);

  if IDEVersionNumber < 5 then
    Result := LocStr[0]
  else
    Result := LocStr[1] + AnsiBackslash + LocStr[2];

  Result := PathAddSeparator(GetPersonalFolder) + Result;
end;

function TJclBDSInstallation.GetEnvironmentVariables: TStrings;
begin
  Result := inherited GetEnvironmentVariables;
  if Assigned(Result) then
  begin
    // adding default values
    if Result.Values[EnvVariableBDSValueName] = '' then
      Result.Values[EnvVariableBDSValueName] := PathRemoveSeparator(RootDir);
    if Result.Values[EnvVariableBDSPROJDIRValueName] = '' then
      Result.Values[EnvVariableBDSPROJDIRValueName] := DefaultProjectsDir;
    if Result.Values[EnvVariableBDSCOMDIRValueName] = '' then
      Result.Values[EnvVariableBDSCOMDIRValueName] := CommonProjectsDir;
  end;
end;

class function TJclBDSInstallation.GetLatestUpdatePackForVersion(Version: Integer): Integer;
begin
  case Version of
    9:
      Result := 1;   // personal version is only update pack 1
    10:
      Result := 1;  // update 1 is out
  else
    Result := 0;
  end;
end;

function TJclBDSInstallation.GetValid: Boolean;
begin
  Result := (inherited GetValid) and ((IDEVersionNumber < 5) or FileExists(GetMsBuildEnvOptionsFileName));
end;

function TJclBDSInstallation.GetLibraryBrowsingPath: TJclBorRADToolPath;
begin
  if IDEVersionNumber >= 5 then
    // use EnvOptions.proj
    Result := GetMsBuildEnvOption(MsBuildWin32BrowsingPathNodeName)
  else
    // use registry
    Result := inherited GetLibraryBrowsingPath;
end;

function TJclBDSInstallation.GetLibrarySearchPath: TJclBorRADToolPath;
begin
  if IDEVersionNumber >= 5 then
    // use EnvOptions.proj
    Result := GetMsBuildEnvOption(MsBuildWin32LibraryPathNodeName)
  else
    // use registry
    Result := inherited GetLibrarySearchPath;
end;

function TJclBDSInstallation.GetMaxDelphiCLRVersion: string;
begin
  Result := DCCIL.GetMaxCLRVersion;
end;

function TJclBDSInstallation.GetName: string;
begin
  // The name comes from the IDEVersionNumber
  if IDEVersionNumber in [Low(BDSVersions)..High(BDSVersions)] then
    Result := Format('%s %s', [RadToolName, BDSVersions[IDEVersionNumber].VersionStr])
  else
    Result := Format('%s ***%s***', [RadToolName, IDEVersionNumber]);
end;

function TJclBDSInstallation.GetMsBuildEnvOption(const OptionName: string): string;
var
  EnvOptionsFile: TJclSimpleXML;
  PropertyGroupNode, PropertyNode: TJclSimpleXMLElem;
begin
  Result := '';

  EnvOptionsFile := TJclSimpleXML.Create;
  try
    EnvOptionsFile.LoadFromFile(GetMsBuildEnvOptionsFileName);
    EnvOptionsFile.Options := EnvOptionsFile.Options - [sxoAutoCreate];

    PropertyGroupNode := EnvOptionsFile.Root.Items.ItemNamed[MsBuildPropertyGroupNodeName];
    if Assigned(PropertyGroupNode) then
    begin
      PropertyNode := PropertyGroupNode.Items.ItemNamed[OptionName];
      if Assigned(PropertyNode) then
        Result := PropertyNode.Value;
    end;
  finally
    EnvOptionsFile.Free;
  end;
end;

function TJclBDSInstallation.GetMsBuildEnvOptionsFileName: string;
var
  AppdataFolder: string;
begin
  if IDEVersionNumber >= 5 then
  begin
    if (RootKey = 0) or (RootKey = HKCU) then
      AppdataFolder := GetAppdataFolder
    else
      AppdataFolder := RegReadString(RootKey, 'Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders', 'AppData');

    Result := Format('%sBorland\BDS\%d.0\EnvOptions.proj',
      [PathAddSeparator(AppdataFolder), IDEVersionNumber]);
  end
  else
    raise EJclBorRADException.CreateRes(@RsMsBuildNotSupported);
end;

function TJclBDSInstallation.GetVclIncludeDir: string;
begin
  if not (bpBCBuilder32 in Personalities) then
    raise EJclBorRadException.CreateResFmt(@RsEDualPackageNotSupported, [Name]);
  Result := inherited GetVclIncludeDir;
end;

class function TJclBDSInstallation.PackageSourceFileExtension: string;
begin
  Result := SourceExtensionDelphiPackage;
end;

class function TJclBDSInstallation.ProjectSourceFileExtension: string;
begin
  Result := SourceExtensionDelphiProject;
end;

class function TJclBDSInstallation.RadToolKind: TJclBorRadToolKind;
begin
  Result := brBorlandDevStudio;
end;

function TJclBDSInstallation.RadToolName: string;
begin
  // The name comes from IDEVersionNumber
  if IDEVersionNumber in [Low(BDSVersions)..High(BDSVersions)] then
  begin
    Result := BDSVersions[IDEVersionNumber].Name;
    // IDE Version 5 comes in three flavors: 
    // - Delphi only  (Spacely)
    // - C++Builder only  (Cogswell)
    // - Delphi and C++Builder
    if (IDEVersionNumber = 5) and (Personalities = [bpDelphi32]) then
      Result := RsDelphiName
    else
    if (IDEVersionNumber = 5) and (Personalities = [bpBCBuilder32]) then
      Result := RsBCBName;
  end
  else
    Result := RsBDSName;
end;

function TJclBDSInstallation.RegisterPackage(const BinaryFileName, Description: string): Boolean;
begin
  if VersionNumber >= 3 then
    CleanPackageCache(BinaryFileName);

  Result := inherited RegisterPackage(BinaryFileName, Description);
end;

function TJclBDSInstallation.RemoveFromCppBrowsingPath(const Path: string): Boolean;
var
  TempCppPath: TJclBorRADToolPath;
begin
  if bpBCBuilder32 in Personalities then
  begin
    TempCppPath := CppBrowsingPath;
    Result := RemoveFromPath(TempCppPath, Path);
    CppBrowsingPath := TempCppPath;
  end
  else
    Result := False;
end;

function TJclBDSInstallation.RemoveFromCppSearchPath(const Path: string): Boolean;
var
  TempCppPath: TJclBorRADToolPath;
begin
  if bpBCBuilder32 in Personalities then
  begin
    TempCppPath := CppSearchPath;
    Result := RemoveFromPath(TempCppPath, Path);
    CppSearchPath := TempCppPath;
  end
  else
    Result := False;
end;

function TJclBDSInstallation.RemoveFromCppLibraryPath(const Path: string): Boolean;
var
  TempLibraryPath: TJclBorRADToolPath;
begin
  if (bpBCBuilder32 in Personalities) and (IDEVersionNumber >= 5) then
  begin
    TempLibraryPath := CppLibraryPath;
    Result := RemoveFromPath(TempLibraryPath, Path);
    CppLibraryPath := TempLibraryPath;
  end
  else
    Result := False;
end;

procedure TJclBDSInstallation.SetCppBrowsingPath(const Value: TJclBorRADToolPath);
begin
  // update registry
  ConfigData.WriteString(GetCppPathsKeyName, CppBrowsingPathValueName, Value);
  // update EnvOptions.dproj
  if IDEVersionNumber >= 5 then
    SetMsBuildEnvOption(MsBuildCBuilderBrowsingPathNodeName, Value);
end;

procedure TJclBDSInstallation.SetCppSearchPath(const Value: TJclBorRADToolPath);
begin
  ConfigData.WriteString(GetCppPathsKeyName, CppSearchPathValueName, Value);
end;

procedure TJclBDSInstallation.SetCppLibraryPath(const Value: TJclBorRADToolPath);
begin
  // update registry
  ConfigData.WriteString(GetCppPathsKeyName, CppLibraryPathValueName, Value);
  // update EnvOptions.dproj
  if IDEVersionNumber >= 5 then
    SetMsBuildEnvOption(MsBuildCBuilderLibraryPathNodeName, Value);
end;

procedure TJclBDSInstallation.SetDebugDCUPath(const Value: TJclBorRADToolPath);
begin
  // update registry
  ConfigData.WriteString(LibraryKeyName, BDSDebugDCUPathValueName, Value);
  // update EnvOptions.dproj
  if IDEVersionNumber >= 5 then
    SetMsBuildEnvOption(MsBuildWin32DebugDCUPathNodeName, Value);
end;

procedure TJclBDSInstallation.SetDualPackageInstallation(const Value: Boolean);
begin
  if Value and not (bpBCBuilder32 in Personalities) then
    raise EJclBorRadException.CreateResFmt(@RsEDualPackageNotSupported, [Name]);
  FDualPackageInstallation := Value;
end;

procedure TJclBDSInstallation.SetLibraryBrowsingPath(const Value: TJclBorRADToolPath);
begin
  // update registry
  inherited SetLibraryBrowsingPath(Value);
  // update EnvOptions.dproj
  if IDEVersionNumber >= 5 then
    SetMsBuildEnvOption(MsBuildWin32BrowsingPathNodeName, Value);
end;

procedure TJclBDSInstallation.SetLibrarySearchPath(const Value: TJclBorRADToolPath);
begin
  // update registry
  inherited SetLibrarySearchPath(Value);
  // update EnvOptions.dproj
  if IDEVersionNumber >= 5 then
    SetMsBuildEnvOption(MsBuildWin32LibraryPathNodeName, Value);
end;

procedure TJclBDSInstallation.SetMsBuildEnvOption(const OptionName, Value: string);
var
  EnvOptionsFileName: string;
  EnvOptionsFile: TJclSimpleXML;
  PropertyGroupNode, PropertyNode: TJclSimpleXMLElem;
begin
  EnvOptionsFile := TJclSimpleXML.Create;
  try
    EnvOptionsFileName := GetMsBuildEnvOptionsFileName;
    EnvOptionsFile.LoadFromFile(EnvOptionsFileName);
    EnvOptionsFile.Options := EnvOptionsFile.Options + [sxoAutoCreate];

    PropertyGroupNode := EnvOptionsFile.Root.Items.ItemNamed[MsBuildPropertyGroupNodeName];
    PropertyNode := PropertyGroupNode.Items.ItemNamed[OptionName];

    PropertyNode.Value := Value;

    EnvOptionsFile.SaveToFile(EnvOptionsFileName);
  finally
    EnvOptionsFile.Free;
  end;
end;

procedure TJclBDSInstallation.SetOutputCallback(const Value: TTextHandler);
begin
  inherited SetOutputCallback(Value);
  if clDccIL in CommandLineTools then
    DCCIL.OutputCallback := Value;
end;

function TJclBDSInstallation.UnregisterPackage(const BinaryFileName: string): Boolean;
begin
  if IDEVersionNumber >= 3 then
    CleanPackageCache(BinaryFileName);
  Result := inherited UnregisterPackage(BinaryFileName);
end;

{$ENDIF MSWINDOWS}

//=== { TJclBorRADToolInstallations } ========================================

constructor TJclBorRADToolInstallations.Create;
begin
  FList := TObjectList.Create;
  ReadInstallations;
end;

destructor TJclBorRADToolInstallations.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TJclBorRADToolInstallations.AnyInstanceRunning: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Installations[I].AnyInstanceRunning then
    begin
      Result := True;
      Break;
    end;
end;

function TJclBorRADToolInstallations.AnyUpdatePackNeeded(var Text: string): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if Installations[I].UpdateNeeded then
    begin
      Result := True;
      Text := Format(RsNeedUpdate, [Installations[I].LatestUpdatePack, Installations[I].Name]);
      Break;
    end;
end;

function TJclBorRADToolInstallations.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TJclBorRADToolInstallations.GetBCBInstallationFromVersion(VersionNumber: Integer): TJclBorRADToolInstallation;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    case Installations[I].RadToolKind of
      brCppBuilder:
        if Installations[I].IDEVersionNumber = VersionNumber then
        begin
          Result := Installations[I];
          Break;
        end;
      brBorlandDevStudio:
        if (VersionNumber >= 10) and (Installations[I].IDEVersionNumber = (VersionNumber - 6)) then
        begin
          Result := Installations[I];
          Break;
        end;
    end;
end;

function TJclBorRADToolInstallations.GetDelphiInstallationFromVersion(
  VersionNumber: Integer): TJclBorRADToolInstallation;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    case Installations[I].RadToolKind of
      brDelphi:
        if Installations[I].IDEVersionNumber = VersionNumber then
        begin
          Result := Installations[I];
          Break;
        end;
      brBorlandDevStudio:
        if (VersionNumber >= 8) and (Installations[I].IDEVersionNumber = (VersionNumber - 6)) then
        begin
          Result := Installations[I];
          Break;
        end;
    end;
end;

function TJclBorRADToolInstallations.GetInstallations(Index: Integer): TJclBorRADToolInstallation;
begin
  Result := TJclBorRADToolInstallation(FList[Index]);
end;

function TJclBorRADToolInstallations.GetBCBVersionInstalled(VersionNumber: Integer): Boolean;
begin
  Result := BCBInstallationFromVersion[VersionNumber] <> nil;
end;

function TJclBorRADToolInstallations.GetBDSInstallationFromVersion(VersionNumber: Integer): TJclBorRADToolInstallation;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if (Installations[I].IDEVersionNumber = VersionNumber) and
      (Installations[I].RadToolKind = brBorlandDevStudio) then
    begin
      Result := Installations[I];
      Break;
    end;
end;

function TJclBorRADToolInstallations.GetBDSVersionInstalled(VersionNumber: Integer): Boolean;
begin
  Result := BDSInstallationFromVersion[VersionNumber] <> nil;
end;

function TJclBorRADToolInstallations.GetDelphiVersionInstalled(VersionNumber: Integer): Boolean;
begin
  Result := DelphiInstallationFromVersion[VersionNumber] <> nil;
end;

function TJclBorRADToolInstallations.Iterate(TraverseMethod: TTraverseMethod): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to Count - 1 do
    Result := Result and TraverseMethod(Installations[I]);
end;

procedure TJclBorRADToolInstallations.ReadInstallations;
{$IFDEF KYLIX}
var
  I: Integer;

  procedure CheckForInstallation(RADToolKind: TJclBorRADToolKind; VersionNumber: Integer);
  const
    RcBaseFileNames: array [brDelphi..brCppBuilder] of string = ('delphi', 'bcb');
  var
    Item: TJclBorRADToolInstallation;
    RcFileName: string;
  begin
    RcFileName := Format('%s/.borland/%s%drc', [GetPersonalFolder, RcBaseFileNames[RADToolKind], IDs[VersionNumber]]);
    if FileExists(RcFileName) then
    begin
      if RADToolKind = brCppBuilder then
        Item := TJclBCBInstallation.Create(RcFileName)
      else
        Item := TJclDelphiInstallation.Create(RcFileName);
      Item.FVersionNumber := VersionNumber;
      Item.FIDEVersionNumber := VersionNumber;
      FList.Add(Item);
    end;
  end;

begin
  FList.Clear;
  for I := Low(TKylixVersion) to High(TKylixVersion) do
    CheckForInstallation(brDelphi, I);
  CheckForInstallation(brCppBuilder, 3); // Kylix 3 only
end;
{$ELSE ~KYLIX}
var
  VersionNumbers: TStringList;

  function EnumVersions(const KeyName: string; const Personalities: array of string;
    CreateClass: TJclBorRADToolInstallationClass): Boolean;
  var
    I, J: Integer;
    VersionKeyName, PersonalitiesKeyName: string;
    PersonalitiesList: TStrings;
    Installation: TJclBorRADToolInstallation;
  begin
    Result := False;
    if RegKeyExists(HKEY_LOCAL_MACHINE, KeyName) and
      RegGetKeyNames(HKEY_LOCAL_MACHINE, KeyName, VersionNumbers) then
      for I := 0 to VersionNumbers.Count - 1 do
        if StrIsSubSet(VersionNumbers[I], ['.', '0'..'9']) then
        begin
          VersionKeyName := KeyName + DirDelimiter + VersionNumbers[I];
          if RegKeyExists(HKEY_LOCAL_MACHINE, VersionKeyName) then
          begin
            if Length(Personalities) = 0 then
            begin
              try
                Installation := CreateClass.Create(VersionKeyName);
                if Installation.Valid then
                  FList.Add(Installation);
              finally
                Result := True;
              end;
            end
            else
            begin
              PersonalitiesList := TStringList.Create;
              try
                PersonalitiesKeyName := VersionKeyName + '\Personalities';
                if RegKeyExists(HKEY_LOCAL_MACHINE, PersonalitiesKeyName) then
                  RegGetValueNames(HKEY_LOCAL_MACHINE, PersonalitiesKeyName, PersonalitiesList);
              
                for J := Low(Personalities) to High(Personalities) do
                  if PersonalitiesList.IndexOf(Personalities[J]) >= 0 then
                  begin
                    try
                      Installation := CreateClass.Create(VersionKeyName);
                      if Installation.Valid then
                        FList.Add(Installation)
                      else
                        Installation.Free;
                    finally
                      Result := True;
                    end;
                    Break;
                  end;
              finally
                PersonalitiesList.Free;
              end;
            end;
          end;
        end;
  end;

begin
  FList.Clear;
  VersionNumbers := TStringList.Create;
  try
    EnumVersions(DelphiKeyName, [], TJclDelphiInstallation);
    EnumVersions(BCBKeyName, [], TJclBCBInstallation);
    EnumVersions(BDSKeyName, ['Delphi.Win32', 'BCB', 'Delphi8', 'C#Builder'], TJclBDSInstallation);
  finally
    VersionNumbers.Free;
  end;
end;
{$ENDIF ~KYLIX}

//=== { TJclCommandLineTool } ================================================

constructor TJclCommandLineTool.Create(const AExeName: string);
begin
  inherited Create;
  FOptions := TStringList.Create;
  FExeName := AExeName;
end;

destructor TJclCommandLineTool.Destroy;
begin
  FreeAndNil(FOptions);
  inherited Destroy;
end;

procedure TJclCommandLineTool.AddPathOption(const Option, Path: string);
var
  S: string;
begin
  S := PathRemoveSeparator(Path);
  {$IFDEF MSWINDOWS}
  S := LowerCase(S); // file names are case insensitive
  {$ENDIF MSWINDOWS}
  S := Format('-%s%s', [Option, S]);
  // avoid duplicate entries (note that search is case sensitive)
  if GetOptions.IndexOf(S) = -1 then
    GetOptions.Add(S);
end;

function TJclCommandLineTool.Execute(const CommandLine: string): Boolean;
begin
  if Assigned(FOutputCallback) then
    Result := JclSysUtils.Execute(Format('"%s" %s', [ExeName, CommandLine]), FOutputCallback) = 0
  else
    Result := JclSysUtils.Execute(Format('"%s" %s', [ExeName, CommandLine]), FOutput) = 0;
end;

function TJclCommandLineTool.GetExeName: string;
begin
  Result := FExeName;
end;

function TJclCommandLineTool.GetOptions: TStrings;
begin
  Result := FOptions;
end;

function TJclCommandLineTool.GetOutput: string;
begin
  Result := FOutput;
end;

function TJclCommandLineTool.GetOutputCallback: TTextHandler;
begin
  Result := FOutputCallback;
end;

procedure TJclCommandLineTool.SetOutputCallback(const CallbackMethod: TTextHandler);
begin
  FOutputCallback := CallbackMethod;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

