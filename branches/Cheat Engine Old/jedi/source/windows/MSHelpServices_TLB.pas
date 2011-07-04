{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit MSHelpServices_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 25/02/2006 20:01:26 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files\Fichiers communs\Microsoft Shared\Help\hxds.dll (1)
// LIBID: {31411197-A502-11D2-BBCA-00C04F8EC294}
// LCID: 0
// Helpfile: 
// HelpString: Microsoft Help Data Services 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\system32\stdole2.tlb)
// Parent TypeLibrary:
//   (0) v1.0 MSHelpControls, (C:\Program Files\Fichiers communs\Microsoft Shared\Help\hxvz.dll)
// Errors:
//   Hint: Parameter 'var' of IHxTopic.SetProperty changed to 'var_'
//   Hint: Parameter 'var' of IHxAttribute.SetProperty changed to 'var_'
//   Hint: Parameter 'var' of IHxCollection.SetProperty changed to 'var_'
//   Hint: Parameter 'var' of IHxAttrName.SetProperty changed to 'var_'
//   Hint: Parameter 'var' of IHxAttrValue.SetProperty changed to 'var_'
//   Hint: Parameter 'type' of IHxRegisterSession.GetRegistrationObject changed to 'type_'
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{ $WARN SYMBOL_PLATFORM OFF}
{ $WRITEABLECONST ON}
{ $VARPROPSETTER ON}

{$I jedi.inc}

{$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
{$WEAKPACKAGEUNIT ON}
{$ENDIF SUPPORTS_WEAKPACKAGEUNIT}

interface

uses ActiveX, Classes;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  MSHelpServicesMajorVersion = 1;
  MSHelpServicesMinorVersion = 0;

  LIBID_MSHelpServices: TGUID = '{31411197-A502-11D2-BBCA-00C04F8EC294}';

  IID_IHxHierarchy: TGUID = '{314111B2-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxTopic: TGUID = '{31411196-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxAttributeList: TGUID = '{314111AB-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxAttribute: TGUID = '{314111A9-A502-11D2-BBCA-00C04F8EC294}';
  IID_IEnumHxAttribute: TGUID = '{314111AD-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegister: TGUID = '{314111BC-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxIndex: TGUID = '{314111CC-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxSession: TGUID = '{31411192-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxTopicList: TGUID = '{31411194-A502-11D2-BBCA-00C04F8EC294}';
  IID_IEnumHxTopic: TGUID = '{31411195-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxQuery: TGUID = '{31411193-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxCollection: TGUID = '{314111A1-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxAttrNameList: TGUID = '{314111CE-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxAttrName: TGUID = '{314111D2-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxAttrValueList: TGUID = '{314111D4-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxAttrValue: TGUID = '{314111D8-A502-11D2-BBCA-00C04F8EC294}';
  IID_IEnumHxAttrValue: TGUID = '{314111D6-A502-11D2-BBCA-00C04F8EC294}';
  IID_IEnumHxAttrName: TGUID = '{314111D0-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxFilters: TGUID = '{314111E3-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegFilterList: TGUID = '{31411212-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegFilter: TGUID = '{31411221-A502-11D2-BBCA-00C04F8EC294}';
  IID_IEnumHxRegFilter: TGUID = '{3141121C-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxSampleCollection: TGUID = '{314111E6-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxSample: TGUID = '{314111E8-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegistryWalker: TGUID = '{314111EF-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegNamespaceList: TGUID = '{314111F3-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegNamespace: TGUID = '{314111F1-A502-11D2-BBCA-00C04F8EC294}';
  IID_IEnumHxRegNamespace: TGUID = '{314111F5-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegTitle: TGUID = '{31411202-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegTitleList: TGUID = '{31411203-A502-11D2-BBCA-00C04F8EC294}';
  IID_IEnumHxRegTitle: TGUID = '{31411204-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegPlugIn: TGUID = '{3141120A-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegPlugInList: TGUID = '{3141120B-A502-11D2-BBCA-00C04F8EC294}';
  IID_IEnumHxRegPlugIn: TGUID = '{3141120C-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegisterSession: TGUID = '{31411218-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxPlugIn: TGUID = '{314111DA-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxInitialize: TGUID = '{314111AE-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxCancel: TGUID = '{31411225-A502-11D2-BBCA-00C04F8EC294}';
  DIID_IHxSessionEvents: TGUID = '{314111ED-A502-11D2-BBCA-00C04F8EC294}';
  DIID_IHxRegisterSessionEvents: TGUID = '{31411223-A502-11D2-BBCA-00C04F8EC294}';
  CLASS_HxSession: TGUID = '{31411198-A502-11D2-BBCA-00C04F8EC294}';
  CLASS_HxRegistryWalker: TGUID = '{314111F0-A502-11D2-BBCA-00C04F8EC294}';
  CLASS_HxRegisterSession: TGUID = '{31411219-A502-11D2-BBCA-00C04F8EC294}';
  IID_IHxRegisterProtocol: TGUID = '{31411227-A502-11D2-BBCA-00C04F8EC294}';
  CLASS_HxRegisterProtocol: TGUID = '{31411228-A502-11D2-BBCA-00C04F8EC294}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum HxHierarchyNodeType
type
  HxHierarchyNodeType = TOleEnum;
const
  HxHierarchy_Book = $00000003;
  HxHierarchy_BookPage = $00000004;
  HxHierarchy_Page = $00000005;
  HxHierarchy_Unknown = $00000008;

// Constants for enum HxHierarchyPropId
type
  HxHierarchyPropId = TOleEnum;
const
  HxHierarchyTocFont = $00000000;
  HxHierarchyTocFontSize = $00000001;
  HxHierarchyTocLangId = $00000002;
  HxHierarchyTocCharSet = $00000003;
  HxHierarchyTocId = $00000004;
  HxHierarchyTocFileVer = $00000005;
  HxHierarchyTocIconFile = $00000006;
  HxHierarchyTocParentNodeIcon = $00000007;
  HxHierarchyTocIcon = $00000008;

// Constants for enum HxTopicGetTitleType
type
  HxTopicGetTitleType = TOleEnum;
const
  HxTopicGetTOCTitle = $00000000;
  HxTopicGetRLTitle = $00000001;
  HxTopicGetHTMTitle = $00000002;

// Constants for enum HxTopicGetTitleDefVal
type
  HxTopicGetTitleDefVal = TOleEnum;
const
  HxTopicGetTitleFullURL = $00000000;
  HxTopicGetTitleFileName = $00000001;
  HxTopicGetTitleNoDefault = $00000002;

// Constants for enum HxQueryPropId
type
  HxQueryPropId = TOleEnum;
const
  HxPropIdQueryFirst = $00000000;

// Constants for enum HxTopicPropId
type
  HxTopicPropId = TOleEnum;
const
  HxTopicPropIdFirst = $00000000;

// Constants for enum HxHierarchy_PrintNode_Options
type
  HxHierarchy_PrintNode_Options = TOleEnum;
const
  HxHierarchy_PrintNode_Option_Node = $00000000;
  HxHierarchy_PrintNode_Option_Children = $00000001;

// Constants for enum HxQuery_Options
type
  HxQuery_Options = TOleEnum;
const
  HxQuery_No_Option = $00000000;
  HxQuery_FullTextSearch_Title_Only = $00000001;
  HxQuery_FullTextSearch_Enable_Stemming = $00000002;
  HxQuery_FullTextSearch_SearchPrevious = $00000004;
  HxQuery_KeywordSearch_CaseSensitive = $0000000A;

// Constants for enum HxCollectionPropId
type
  HxCollectionPropId = TOleEnum;
const
  HxCollectionProp_NamespaceName = $00000001;
  HxCollectionProp_Font = $00000002;
  HxCollectionProp_FontSize = $00000003;
  HxCollectionProp_LangId = $00000004;
  HxCollectionProp_CharSet = $00000005;
  HxCollectionProp_Id = $00000006;
  HxCollectionProp_FileVer = $00000007;
  HxCollectionProp_CopyRight = $00000008;

// Constants for enum HxRegFilterPropId
type
  HxRegFilterPropId = TOleEnum;
const
  HxRegFilterName = $00000000;
  HxRegFilterQuery = $00000001;

// Constants for enum HxIndexPropId
type
  HxIndexPropId = TOleEnum;
const
  HxIndexFont = $00000000;
  HxIndexFontSize = $00000001;
  HxIndexLangId = $00000002;
  HxIndexCharSet = $00000003;
  HxIndexTitleStr = $00000004;
  HxIndexIsVisible = $00000005;
  HxIndexId = $00000006;

// Constants for enum HxSampleFileCopyOption
type
  HxSampleFileCopyOption = TOleEnum;
const
  HxSampleFileCopyNoOption = $00000000;
  HxSampleFileCopyOverwrite = $00000001;
  HxSampleFileCopyFileOnly = $00000002;

// Constants for enum HxRegNamespacePropId
type
  HxRegNamespacePropId = TOleEnum;
const
  HxRegNamespaceTitleList = $00000000;
  HxRegNamespacePlugInList = $00000001;
  HxRegNamespaceName = $00000002;
  HxRegNamespaceCollection = $00000003;
  HxRegNamespaceDescription = $00000004;
  HxRegNamespaceFilterList = $00000008;

// Constants for enum HxRegTitlePropId
type
  HxRegTitlePropId = TOleEnum;
const
  HxRegTitleFileName = $00000000;
  HxRegTitleIndexName = $00000001;
  HxRegTitleQueryName = $00000002;
  HxRegTitleId = $00000003;
  HxRegTitleLangId = $00000004;
  HxRegAttrQueryName = $00000005;
  HxRegTitleHxsMediaLoc = $00000006;
  HxRegTitleHxqMediaLoc = $00000007;
  HxRegTitleHxrMediaLoc = $00000008;
  HxRegTitleSampleMediaLoc = $00000009;

// Constants for enum HxRegPlugInPropId
type
  HxRegPlugInPropId = TOleEnum;
const
  HxRegPlugInName = $00000000;

// Constants for enum HxRegisterSession_InterfaceType
type
  HxRegisterSession_InterfaceType = TOleEnum;
const
  HxRegisterSession_IHxRegister = $00000000;
  HxRegisterSession_IHxFilters = $00000001;
  HxRegisterSession_IHxPlugIn = $00000002;

// Constants for enum HxCancelStatus
type
  HxCancelStatus = TOleEnum;
const
  HxCancelStatus_Continue = $00000000;
  HxCancelStatus_Cancel = $00000001;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IHxHierarchy = interface;
  IHxHierarchyDisp = dispinterface;
  IHxTopic = interface;
  IHxTopicDisp = dispinterface;
  IHxAttributeList = interface;
  IHxAttributeListDisp = dispinterface;
  IHxAttribute = interface;
  IHxAttributeDisp = dispinterface;
  IEnumHxAttribute = interface;
  IHxRegister = interface;
  IHxRegisterDisp = dispinterface;
  IHxIndex = interface;
  IHxIndexDisp = dispinterface;
  IHxSession = interface;
  IHxSessionDisp = dispinterface;
  IHxTopicList = interface;
  IHxTopicListDisp = dispinterface;
  IEnumHxTopic = interface;
  IHxQuery = interface;
  IHxQueryDisp = dispinterface;
  IHxCollection = interface;
  IHxCollectionDisp = dispinterface;
  IHxAttrNameList = interface;
  IHxAttrNameListDisp = dispinterface;
  IHxAttrName = interface;
  IHxAttrNameDisp = dispinterface;
  IHxAttrValueList = interface;
  IHxAttrValueListDisp = dispinterface;
  IHxAttrValue = interface;
  IHxAttrValueDisp = dispinterface;
  IEnumHxAttrValue = interface;
  IEnumHxAttrName = interface;
  IHxFilters = interface;
  IHxFiltersDisp = dispinterface;
  IHxRegFilterList = interface;
  IHxRegFilterListDisp = dispinterface;
  IHxRegFilter = interface;
  IHxRegFilterDisp = dispinterface;
  IEnumHxRegFilter = interface;
  IHxSampleCollection = interface;
  IHxSampleCollectionDisp = dispinterface;
  IHxSample = interface;
  IHxSampleDisp = dispinterface;
  IHxRegistryWalker = interface;
  IHxRegistryWalkerDisp = dispinterface;
  IHxRegNamespaceList = interface;
  IHxRegNamespaceListDisp = dispinterface;
  IHxRegNamespace = interface;
  IHxRegNamespaceDisp = dispinterface;
  IEnumHxRegNamespace = interface;
  IHxRegTitle = interface;
  IHxRegTitleDisp = dispinterface;
  IHxRegTitleList = interface;
  IHxRegTitleListDisp = dispinterface;
  IEnumHxRegTitle = interface;
  IHxRegPlugIn = interface;
  IHxRegPlugInDisp = dispinterface;
  IHxRegPlugInList = interface;
  IHxRegPlugInListDisp = dispinterface;
  IEnumHxRegPlugIn = interface;
  IHxRegisterSession = interface;
  IHxRegisterSessionDisp = dispinterface;
  IHxPlugIn = interface;
  IHxPlugInDisp = dispinterface;
  IHxInitialize = interface;
  IHxInitializeDisp = dispinterface;
  IHxCancel = interface;
  IHxCancelDisp = dispinterface;
  IHxSessionEvents = dispinterface;
  IHxRegisterSessionEvents = dispinterface;
  IHxRegisterProtocol = interface;
  IHxRegisterProtocolDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  HxSession = IHxSession;
  HxRegistryWalker = IHxRegistryWalker;
  HxRegisterSession = IHxRegisterSession;
  HxRegisterProtocol = IHxRegisterProtocol;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  PUserType1 = ^TGUID; {*}
  POleVariant1 = ^OleVariant; {*}


// *********************************************************************//
// Interface: IHxHierarchy
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111B2-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxHierarchy = interface(IDispatch)
    ['{314111B2-A502-11D2-BBCA-00C04F8EC294}']
    function GetRoot: Integer; safecall;
    function GetParent(hNode: Integer): Integer; safecall;
    function GetSibling(hNode: Integer): Integer; safecall;
    function GetFirstChild(hNode: Integer): Integer; safecall;
    function GetNextFromUrl(const pURL: WideString): Integer; safecall;
    function GetPrevFromUrl(const pURL: WideString): Integer; safecall;
    function GetType(hNode: Integer): HxHierarchyNodeType; safecall;
    function IsNew(hNode: Integer): WordBool; safecall;
    function HasChild(hNode: Integer): WordBool; safecall;
    function GetSyncInfo(const pURL: WideString): PSafeArray; safecall;
    function GetTitle(hNode: Integer): WideString; safecall;
    function GetImageIndexes(hNode: Integer; out pOpen: Integer): Integer; safecall;
    function GetURL(hNode: Integer): WideString; safecall;
    function OnNavigation(const pbstrURL: WideString): WordBool; safecall;
    procedure OnHierarchyNavigation(hNode: Integer); safecall;
    function GetProperty(propid: HxHierarchyPropId; hNode: Integer): OleVariant; safecall;
    function GetNextFromNode(hNode: Integer): Integer; safecall;
    function GetPrevFromNode(hNode: Integer): Integer; safecall;
    function GetTopic(hNode: Integer): IHxTopic; safecall;
    function GetOpenImageIndex(hNode: Integer): Integer; safecall;
    function GetClosedImageIndex(hNode: Integer): Integer; safecall;
    procedure PrintNode(hwnd: Integer; hNode: Integer; options: HxHierarchy_PrintNode_Options); safecall;
  end;

// *********************************************************************//
// DispIntf:  IHxHierarchyDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111B2-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxHierarchyDisp = dispinterface
    ['{314111B2-A502-11D2-BBCA-00C04F8EC294}']
    function GetRoot: Integer; dispid 66561;
    function GetParent(hNode: Integer): Integer; dispid 66562;
    function GetSibling(hNode: Integer): Integer; dispid 66563;
    function GetFirstChild(hNode: Integer): Integer; dispid 66564;
    function GetNextFromUrl(const pURL: WideString): Integer; dispid 66565;
    function GetPrevFromUrl(const pURL: WideString): Integer; dispid 66566;
    function GetType(hNode: Integer): HxHierarchyNodeType; dispid 66567;
    function IsNew(hNode: Integer): WordBool; dispid 66568;
    function HasChild(hNode: Integer): WordBool; dispid 66569;
    function GetSyncInfo(const pURL: WideString): {??PSafeArray}OleVariant; dispid 66570;
    function GetTitle(hNode: Integer): WideString; dispid 66571;
    function GetImageIndexes(hNode: Integer; out pOpen: Integer): Integer; dispid 66572;
    function GetURL(hNode: Integer): WideString; dispid 66573;
    function OnNavigation(const pbstrURL: WideString): WordBool; dispid 66574;
    procedure OnHierarchyNavigation(hNode: Integer); dispid 66575;
    function GetProperty(propid: HxHierarchyPropId; hNode: Integer): OleVariant; dispid 66576;
    function GetNextFromNode(hNode: Integer): Integer; dispid 66577;
    function GetPrevFromNode(hNode: Integer): Integer; dispid 66578;
    function GetTopic(hNode: Integer): IHxTopic; dispid 66579;
    function GetOpenImageIndex(hNode: Integer): Integer; dispid 66580;
    function GetClosedImageIndex(hNode: Integer): Integer; dispid 66581;
    procedure PrintNode(hwnd: Integer; hNode: Integer; options: HxHierarchy_PrintNode_Options); dispid 66582;
  end;
  {$EXTERNALSYM IHxHierarchyDisp}

// *********************************************************************//
// Interface: IHxTopic
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411196-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxTopic = interface(IDispatch)
    ['{31411196-A502-11D2-BBCA-00C04F8EC294}']
    function Get_Title(optType: HxTopicGetTitleType; optDef: HxTopicGetTitleDefVal): WideString; safecall;
    function Get_URL: WideString; safecall;
    function Get_Location: WideString; safecall;
    function Get_Rank: Integer; safecall;
    function Get_Attributes: IHxAttributeList; safecall;
    procedure GetInfo(out pTitle: WideString; out pURL: WideString; out pLocation: WideString; 
                      out pRank: Integer); safecall;
    function GetProperty(propid: HxTopicPropId): OleVariant; safecall;
    procedure SetProperty(propid: HxTopicPropId; var_: OleVariant); safecall;
    function HasAttribute(const Name: WideString; const Value: WideString): WordBool; safecall;
    function HasAttrName(const Name: WideString): WordBool; safecall;
    procedure HighlightDocument(const pIDispatch: IDispatch); safecall;
    property Title[optType: HxTopicGetTitleType; optDef: HxTopicGetTitleDefVal]: WideString read Get_Title;
    property URL: WideString read Get_URL;
    property Location: WideString read Get_Location;
    property Rank: Integer read Get_Rank;
    property Attributes: IHxAttributeList read Get_Attributes;
  end;

// *********************************************************************//
// DispIntf:  IHxTopicDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411196-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxTopicDisp = dispinterface
    ['{31411196-A502-11D2-BBCA-00C04F8EC294}']
    property Title[optType: HxTopicGetTitleType; optDef: HxTopicGetTitleDefVal]: WideString readonly dispid 68097;
    property URL: WideString readonly dispid 68098;
    property Location: WideString readonly dispid 68099;
    property Rank: Integer readonly dispid 68100;
    property Attributes: IHxAttributeList readonly dispid 68101;
    procedure GetInfo(out pTitle: WideString; out pURL: WideString; out pLocation: WideString; 
                      out pRank: Integer); dispid 68102;
    function GetProperty(propid: HxTopicPropId): OleVariant; dispid 68103;
    procedure SetProperty(propid: HxTopicPropId; var_: OleVariant); dispid 68104;
    function HasAttribute(const Name: WideString; const Value: WideString): WordBool; dispid 68105;
    function HasAttrName(const Name: WideString): WordBool; dispid 68106;
    procedure HighlightDocument(const pIDispatch: IDispatch); dispid 68107;
  end;
  {$EXTERNALSYM IHxTopicDisp}

// *********************************************************************//
// Interface: IHxAttributeList
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111AB-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttributeList = interface(IDispatch)
    ['{314111AB-A502-11D2-BBCA-00C04F8EC294}']
    function Get_Count: Integer; safecall;
    function ItemAt(index: Integer): IHxAttribute; safecall;
    function EnumAttribute(filter: Integer; options: Integer): IEnumHxAttribute; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Item(index: OleVariant): IHxAttribute; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IHxAttributeListDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111AB-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttributeListDisp = dispinterface
    ['{314111AB-A502-11D2-BBCA-00C04F8EC294}']
    property Count: Integer readonly dispid 70400;
    function ItemAt(index: Integer): IHxAttribute; dispid 70401;
    function EnumAttribute(filter: Integer; options: Integer): IEnumHxAttribute; dispid 70402;
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(index: OleVariant): IHxAttribute; dispid 70403;
  end;
  {$EXTERNALSYM IHxAttributeListDisp}

// *********************************************************************//
// Interface: IHxAttribute
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111A9-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttribute = interface(IDispatch)
    ['{314111A9-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxQueryPropId): OleVariant; safecall;
    procedure SetProperty(propid: HxQueryPropId; var_: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    function Get_Value: WideString; safecall;
    function Get_DisplayName: WideString; safecall;
    function Get_DisplayValue: WideString; safecall;
    property Name: WideString read Get_Name;
    property Value: WideString read Get_Value;
    property DisplayName: WideString read Get_DisplayName;
    property DisplayValue: WideString read Get_DisplayValue;
  end;

// *********************************************************************//
// DispIntf:  IHxAttributeDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111A9-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttributeDisp = dispinterface
    ['{314111A9-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxQueryPropId): OleVariant; dispid 69888;
    procedure SetProperty(propid: HxQueryPropId; var_: OleVariant); dispid 69889;
    property Name: WideString readonly dispid 69890;
    property Value: WideString readonly dispid 69891;
    property DisplayName: WideString readonly dispid 69892;
    property DisplayValue: WideString readonly dispid 69893;
  end;
  {$EXTERNALSYM IHxAttributeDisp}

// *********************************************************************//
// Interface: IEnumHxAttribute
// Flags:     (16) Hidden
// GUID:      {314111AD-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IEnumHxAttribute = interface(IUnknown)
    ['{314111AD-A502-11D2-BBCA-00C04F8EC294}']
    function Next(celt: LongWord; out ppIHxAttribute: IHxAttribute; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Clone(out ppEnum: IEnumHxAttribute): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IHxRegister
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111BC-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegister = interface(IDispatch)
    ['{314111BC-A502-11D2-BBCA-00C04F8EC294}']
    procedure RegisterNamespace(const bstrNamespace: WideString; const bstrCollection: WideString; 
                                const bstrDescription: WideString); safecall;
    function IsNamespace(const bstrNamespace: WideString): WordBool; safecall;
    function GetCollection(const bstrNamespace: WideString): WideString; safecall;
    function GetDescription(const bstrNamespace: WideString): WideString; safecall;
    procedure RemoveNamespace(const bstrNamespace: WideString); safecall;
    procedure RegisterHelpFile(const bstrNamespace: WideString; const bstrId: WideString; 
                               LangId: Integer; const bstrHelpFile: WideString); safecall;
    function RegisterMedia(const bstrNamespace: WideString; const bstrFriendly: WideString; 
                           const bstrPath: WideString): Integer; safecall;
    procedure RemoveHelpFile(const bstrNamespace: WideString; const bstrId: WideString; 
                             LangId: Integer); safecall;
    procedure RegisterHelpFileSet(const bstrNamespace: WideString; const bstrId: WideString; 
                                  LangId: Integer; const bstrHxs: WideString; 
                                  const bstrHxi: WideString; const bstrHxq: WideString; 
                                  const bstrHxr: WideString; lHxsMediaId: Integer; 
                                  lHxqMediaId: Integer; lHxrMediaId: Integer; 
                                  lSampleMediaId: Integer); safecall;
  end;

// *********************************************************************//
// DispIntf:  IHxRegisterDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111BC-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegisterDisp = dispinterface
    ['{314111BC-A502-11D2-BBCA-00C04F8EC294}']
    procedure RegisterNamespace(const bstrNamespace: WideString; const bstrCollection: WideString; 
                                const bstrDescription: WideString); dispid 66817;
    function IsNamespace(const bstrNamespace: WideString): WordBool; dispid 66818;
    function GetCollection(const bstrNamespace: WideString): WideString; dispid 66830;
    function GetDescription(const bstrNamespace: WideString): WideString; dispid 66829;
    procedure RemoveNamespace(const bstrNamespace: WideString); dispid 66819;
    procedure RegisterHelpFile(const bstrNamespace: WideString; const bstrId: WideString; 
                               LangId: Integer; const bstrHelpFile: WideString); dispid 66822;
    function RegisterMedia(const bstrNamespace: WideString; const bstrFriendly: WideString; 
                           const bstrPath: WideString): Integer; dispid 66823;
    procedure RemoveHelpFile(const bstrNamespace: WideString; const bstrId: WideString; 
                             LangId: Integer); dispid 66825;
    procedure RegisterHelpFileSet(const bstrNamespace: WideString; const bstrId: WideString; 
                                  LangId: Integer; const bstrHxs: WideString; 
                                  const bstrHxi: WideString; const bstrHxq: WideString; 
                                  const bstrHxr: WideString; lHxsMediaId: Integer; 
                                  lHxqMediaId: Integer; lHxrMediaId: Integer; 
                                  lSampleMediaId: Integer); dispid 66831;
  end;
  {$EXTERNALSYM IHxRegisterDisp}

// *********************************************************************//
// Interface: IHxIndex
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111CC-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxIndex = interface(IDispatch)
    ['{314111CC-A502-11D2-BBCA-00C04F8EC294}']
    function GetSession: IHxSession; safecall;
    function Get_Count: Integer; safecall;
    function GetStringFromSlot(iSlot: Integer): WideString; safecall;
    function GetLevelFromSlot(iSlot: Integer): Integer; safecall;
    function GetSlotFromString(const bszLink: WideString): Integer; safecall;
    function GetTopicsFromSlot(uiSlot: Integer): IHxTopicList; safecall;
    function GetTopicsFromString(const bszLink: WideString; options: Integer): IHxTopicList; safecall;
    function GetInfoFromSlot(iSlot: Integer; out piLevel: Integer): WideString; safecall;
    function GetProperty(propid: HxIndexPropId): OleVariant; safecall;
    function GetCrossRef(iSlot: Integer): WideString; safecall;
    function GetFullStringFromSlot(iSlot: Integer; const sep: WideString): WideString; safecall;
    function GetCrossRefSlot(iSlot: Integer): Integer; safecall;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IHxIndexDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111CC-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxIndexDisp = dispinterface
    ['{314111CC-A502-11D2-BBCA-00C04F8EC294}']
    function GetSession: IHxSession; dispid 67072;
    property Count: Integer readonly dispid 67073;
    function GetStringFromSlot(iSlot: Integer): WideString; dispid 67074;
    function GetLevelFromSlot(iSlot: Integer): Integer; dispid 67078;
    function GetSlotFromString(const bszLink: WideString): Integer; dispid 67075;
    function GetTopicsFromSlot(uiSlot: Integer): IHxTopicList; dispid 67076;
    function GetTopicsFromString(const bszLink: WideString; options: Integer): IHxTopicList; dispid 67077;
    function GetInfoFromSlot(iSlot: Integer; out piLevel: Integer): WideString; dispid 67079;
    function GetProperty(propid: HxIndexPropId): OleVariant; dispid 67080;
    function GetCrossRef(iSlot: Integer): WideString; dispid 67081;
    function GetFullStringFromSlot(iSlot: Integer; const sep: WideString): WideString; dispid 67082;
    function GetCrossRefSlot(iSlot: Integer): Integer; dispid 67083;
  end;
  {$EXTERNALSYM IHxIndexDisp}

// *********************************************************************//
// Interface: IHxSession
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411192-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxSession = interface(IDispatch)
    ['{31411192-A502-11D2-BBCA-00C04F8EC294}']
    procedure Initialize(const NameSpace: WideString; options: Integer); safecall;
    function Query(const keywords: WideString; const NavDataMoniker: WideString; options: Integer; 
                   const FilterMoniker: WideString): IHxTopicList; safecall;
    function QueryForTopic(const keywords: WideString; const NavDataMoniker: WideString; 
                           options: Integer; const FilterMoniker: WideString): IHxTopic; safecall;
    function QueryForUrl(const keywords: WideString; const NavDataMoniker: WideString; 
                         options: Integer; const FilterMoniker: WideString): WideString; safecall;
    function GetNavigationInterface(const NavDataMoniker: WideString; 
                                    const FilterMoniker: WideString; var refiid: TGUID): IDispatch; safecall;
    function GetNavigationObject(const NavDataMoniker: WideString; const FilterMoniker: WideString): IDispatch; safecall;
    function GetQueryObject(const NavDataMoniker: WideString; const FilterMoniker: WideString): IHxQuery; safecall;
    function Get_Collection: IHxCollection; safecall;
    function Get_LangId: Smallint; safecall;
    procedure Set_LangId(piHelpLangId: Smallint); safecall;
    function GetFilterList: IHxRegFilterList; safecall;
    property Collection: IHxCollection read Get_Collection;
    property LangId: Smallint read Get_LangId write Set_LangId;
  end;

// *********************************************************************//
// DispIntf:  IHxSessionDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411192-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxSessionDisp = dispinterface
    ['{31411192-A502-11D2-BBCA-00C04F8EC294}']
    procedure Initialize(const NameSpace: WideString; options: Integer); dispid 65792;
    function Query(const keywords: WideString; const NavDataMoniker: WideString; options: Integer; 
                   const FilterMoniker: WideString): IHxTopicList; dispid 65793;
    function QueryForTopic(const keywords: WideString; const NavDataMoniker: WideString; 
                           options: Integer; const FilterMoniker: WideString): IHxTopic; dispid 65794;
    function QueryForUrl(const keywords: WideString; const NavDataMoniker: WideString; 
                         options: Integer; const FilterMoniker: WideString): WideString; dispid 65795;
    function GetNavigationInterface(const NavDataMoniker: WideString; 
                                    const FilterMoniker: WideString; var refiid: {??TGUID}OleVariant): IDispatch; dispid 65796;
    function GetNavigationObject(const NavDataMoniker: WideString; const FilterMoniker: WideString): IDispatch; dispid 65797;
    function GetQueryObject(const NavDataMoniker: WideString; const FilterMoniker: WideString): IHxQuery; dispid 65798;
    property Collection: IHxCollection readonly dispid 65799;
    property LangId: Smallint dispid 65803;
    function GetFilterList: IHxRegFilterList; dispid 65805;
  end;
  {$EXTERNALSYM IHxSessionDisp}

// *********************************************************************//
// Interface: IHxTopicList
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411194-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxTopicList = interface(IDispatch)
    ['{31411194-A502-11D2-BBCA-00C04F8EC294}']
    function Item(index: OleVariant): IHxTopic; safecall;
    function ItemAt(index: Integer): IHxTopic; safecall;
    function EnumTopics(filter: Integer; options: Integer): IEnumHxTopic; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IHxTopicListDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411194-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxTopicListDisp = dispinterface
    ['{31411194-A502-11D2-BBCA-00C04F8EC294}']
    function Item(index: OleVariant): IHxTopic; dispid 0;
    function ItemAt(index: Integer): IHxTopic; dispid 67584;
    function EnumTopics(filter: Integer; options: Integer): IEnumHxTopic; dispid 67585;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 67586;
  end;
  {$EXTERNALSYM IHxTopicListDisp}

// *********************************************************************//
// Interface: IEnumHxTopic
// Flags:     (16) Hidden
// GUID:      {31411195-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IEnumHxTopic = interface(IUnknown)
    ['{31411195-A502-11D2-BBCA-00C04F8EC294}']
    function Next(celt: LongWord; out ppIHxTopic: IHxTopic; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Clone(out ppEnum: IEnumHxTopic): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IHxQuery
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411193-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxQuery = interface(IDispatch)
    ['{31411193-A502-11D2-BBCA-00C04F8EC294}']
    function Query(const keywords: WideString; options: HxQuery_Options): IHxTopicList; safecall;
    function QueryForTopic(const keywords: WideString; options: HxQuery_Options): IHxTopic; safecall;
    function QueryForUrl(const keywords: WideString; options: HxQuery_Options): WideString; safecall;
  end;

// *********************************************************************//
// DispIntf:  IHxQueryDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411193-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxQueryDisp = dispinterface
    ['{31411193-A502-11D2-BBCA-00C04F8EC294}']
    function Query(const keywords: WideString; options: HxQuery_Options): IHxTopicList; dispid 67328;
    function QueryForTopic(const keywords: WideString; options: HxQuery_Options): IHxTopic; dispid 67329;
    function QueryForUrl(const keywords: WideString; options: HxQuery_Options): WideString; dispid 67330;
  end;
  {$EXTERNALSYM IHxQueryDisp}

// *********************************************************************//
// Interface: IHxCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111A1-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxCollection = interface(IDispatch)
    ['{314111A1-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxCollectionPropId): OleVariant; safecall;
    procedure SetProperty(propid: HxCollectionPropId; var_: OleVariant); safecall;
    function Get_URL: WideString; safecall;
    function Get_AttributeNames: IHxAttrNameList; safecall;
    function Get_Filters: IHxFilters; safecall;
    function Get_Title: WideString; safecall;
    procedure MergeIndex; safecall;
    function GetFilterTopicCount(const bstrQuery: WideString): Integer; safecall;
    property URL: WideString read Get_URL;
    property AttributeNames: IHxAttrNameList read Get_AttributeNames;
    property Filters: IHxFilters read Get_Filters;
    property Title: WideString read Get_Title;
  end;

// *********************************************************************//
// DispIntf:  IHxCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111A1-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxCollectionDisp = dispinterface
    ['{314111A1-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxCollectionPropId): OleVariant; dispid 68352;
    procedure SetProperty(propid: HxCollectionPropId; var_: OleVariant); dispid 68353;
    property URL: WideString readonly dispid 68354;
    property AttributeNames: IHxAttrNameList readonly dispid 68357;
    property Filters: IHxFilters readonly dispid 68358;
    property Title: WideString readonly dispid 68359;
    procedure MergeIndex; dispid 68360;
    function GetFilterTopicCount(const bstrQuery: WideString): Integer; dispid 68361;
  end;
  {$EXTERNALSYM IHxCollectionDisp}

// *********************************************************************//
// Interface: IHxAttrNameList
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111CE-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttrNameList = interface(IDispatch)
    ['{314111CE-A502-11D2-BBCA-00C04F8EC294}']
    function Get_Count: Integer; safecall;
    function ItemAt(index: Integer): IHxAttrName; safecall;
    function EnumAttrName(filter: Integer; options: Integer): IEnumHxAttrName; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Item(index: OleVariant): IHxAttrName; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IHxAttrNameListDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111CE-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttrNameListDisp = dispinterface
    ['{314111CE-A502-11D2-BBCA-00C04F8EC294}']
    property Count: Integer readonly dispid 71168;
    function ItemAt(index: Integer): IHxAttrName; dispid 71169;
    function EnumAttrName(filter: Integer; options: Integer): IEnumHxAttrName; dispid 71170;
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(index: OleVariant): IHxAttrName; dispid 71171;
  end;
  {$EXTERNALSYM IHxAttrNameListDisp}

// *********************************************************************//
// Interface: IHxAttrName
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111D2-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttrName = interface(IDispatch)
    ['{314111D2-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxQueryPropId): OleVariant; safecall;
    procedure SetProperty(propid: HxQueryPropId; var_: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    function Get_DisplayName: WideString; safecall;
    function Get_Flag: Integer; safecall;
    function Get_AttributeValues: IHxAttrValueList; safecall;
    property Name: WideString read Get_Name;
    property DisplayName: WideString read Get_DisplayName;
    property Flag: Integer read Get_Flag;
    property AttributeValues: IHxAttrValueList read Get_AttributeValues;
  end;

// *********************************************************************//
// DispIntf:  IHxAttrNameDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111D2-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttrNameDisp = dispinterface
    ['{314111D2-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxQueryPropId): OleVariant; dispid 70656;
    procedure SetProperty(propid: HxQueryPropId; var_: OleVariant); dispid 70657;
    property Name: WideString readonly dispid 70658;
    property DisplayName: WideString readonly dispid 70659;
    property Flag: Integer readonly dispid 70660;
    property AttributeValues: IHxAttrValueList readonly dispid 70661;
  end;
  {$EXTERNALSYM IHxAttrNameDisp}

// *********************************************************************//
// Interface: IHxAttrValueList
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111D4-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttrValueList = interface(IDispatch)
    ['{314111D4-A502-11D2-BBCA-00C04F8EC294}']
    function Get_Count: Integer; safecall;
    function ItemAt(index: Integer): IHxAttrValue; safecall;
    function EnumAttrValue(filter: Integer; options: Integer): IEnumHxAttrValue; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Item(index: OleVariant): IHxAttrValue; safecall;
    property Count: Integer read Get_Count;
    property _NewEnum: IUnknown read Get__NewEnum;
  end;

// *********************************************************************//
// DispIntf:  IHxAttrValueListDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111D4-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttrValueListDisp = dispinterface
    ['{314111D4-A502-11D2-BBCA-00C04F8EC294}']
    property Count: Integer readonly dispid 71936;
    function ItemAt(index: Integer): IHxAttrValue; dispid 71937;
    function EnumAttrValue(filter: Integer; options: Integer): IEnumHxAttrValue; dispid 71938;
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(index: OleVariant): IHxAttrValue; dispid 71939;
  end;
  {$EXTERNALSYM IHxAttrValueListDisp}

// *********************************************************************//
// Interface: IHxAttrValue
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111D8-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttrValue = interface(IDispatch)
    ['{314111D8-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxQueryPropId): OleVariant; safecall;
    procedure SetProperty(propid: HxQueryPropId; var_: OleVariant); safecall;
    function Get_Value: WideString; safecall;
    function Get_DisplayValue: WideString; safecall;
    function Get_Flag: Integer; safecall;
    property Value: WideString read Get_Value;
    property DisplayValue: WideString read Get_DisplayValue;
    property Flag: Integer read Get_Flag;
  end;

// *********************************************************************//
// DispIntf:  IHxAttrValueDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111D8-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxAttrValueDisp = dispinterface
    ['{314111D8-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxQueryPropId): OleVariant; dispid 71424;
    procedure SetProperty(propid: HxQueryPropId; var_: OleVariant); dispid 71425;
    property Value: WideString readonly dispid 71426;
    property DisplayValue: WideString readonly dispid 71427;
    property Flag: Integer readonly dispid 71428;
  end;
  {$EXTERNALSYM IHxAttrValueDisp}

// *********************************************************************//
// Interface: IEnumHxAttrValue
// Flags:     (16) Hidden
// GUID:      {314111D6-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IEnumHxAttrValue = interface(IUnknown)
    ['{314111D6-A502-11D2-BBCA-00C04F8EC294}']
    function Next(celt: LongWord; out ppIHxAttrValue: IHxAttrValue; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Clone(out ppEnum: IEnumHxAttrValue): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IEnumHxAttrName
// Flags:     (16) Hidden
// GUID:      {314111D0-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IEnumHxAttrName = interface(IUnknown)
    ['{314111D0-A502-11D2-BBCA-00C04F8EC294}']
    function Next(celt: LongWord; out ppIHxAttrName: IHxAttrName; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Clone(out ppEnum: IEnumHxAttrName): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IHxFilters
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111E3-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxFilters = interface(IDispatch)
    ['{314111E3-A502-11D2-BBCA-00C04F8EC294}']
    function Count: Integer; safecall;
    function GetFilter(iIndex: Integer; out pbstrName: WideString): WideString; safecall;
    function GetFilterName(iIndex: Integer): WideString; safecall;
    function GetFilterQuery(iIndex: Integer): WideString; safecall;
    procedure RegisterFilter(const bstrName: WideString; const bstrQuery: WideString); safecall;
    procedure RemoveFilter(const bstrName: WideString); safecall;
    function FindFilter(const bstrName: WideString): WideString; safecall;
    procedure SetNamespace(const bstrName: WideString); safecall;
    procedure SetCollectionFiltersFlag(vb: WordBool); safecall;
  end;

// *********************************************************************//
// DispIntf:  IHxFiltersDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111E3-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxFiltersDisp = dispinterface
    ['{314111E3-A502-11D2-BBCA-00C04F8EC294}']
    function Count: Integer; dispid 66048;
    function GetFilter(iIndex: Integer; out pbstrName: WideString): WideString; dispid 66049;
    function GetFilterName(iIndex: Integer): WideString; dispid 66054;
    function GetFilterQuery(iIndex: Integer): WideString; dispid 66055;
    procedure RegisterFilter(const bstrName: WideString; const bstrQuery: WideString); dispid 66050;
    procedure RemoveFilter(const bstrName: WideString); dispid 66051;
    function FindFilter(const bstrName: WideString): WideString; dispid 66052;
    procedure SetNamespace(const bstrName: WideString); dispid 66053;
    procedure SetCollectionFiltersFlag(vb: WordBool); dispid 66057;
  end;
  {$EXTERNALSYM IHxFiltersDisp}

// *********************************************************************//
// Interface: IHxRegFilterList
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411212-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegFilterList = interface(IDispatch)
    ['{31411212-A502-11D2-BBCA-00C04F8EC294}']
    function Item(index: OleVariant): IHxRegFilter; safecall;
    function ItemAt(index: Integer): IHxRegFilter; safecall;
    function EnumRegFilter(filter: Integer; options: Integer): IEnumHxRegFilter; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    function FindFilter(const bstrFilterName: WideString): IHxRegFilter; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IHxRegFilterListDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411212-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegFilterListDisp = dispinterface
    ['{31411212-A502-11D2-BBCA-00C04F8EC294}']
    function Item(index: OleVariant): IHxRegFilter; dispid 0;
    function ItemAt(index: Integer): IHxRegFilter; dispid 75776;
    function EnumRegFilter(filter: Integer; options: Integer): IEnumHxRegFilter; dispid 75777;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 75778;
    function FindFilter(const bstrFilterName: WideString): IHxRegFilter; dispid 75779;
  end;
  {$EXTERNALSYM IHxRegFilterListDisp}

// *********************************************************************//
// Interface: IHxRegFilter
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411221-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegFilter = interface(IDispatch)
    ['{31411221-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxRegFilterPropId): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  IHxRegFilterDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411221-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegFilterDisp = dispinterface
    ['{31411221-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxRegFilterPropId): OleVariant; dispid 75520;
  end;
  {$EXTERNALSYM IHxRegFilterDisp}

// *********************************************************************//
// Interface: IEnumHxRegFilter
// Flags:     (16) Hidden
// GUID:      {3141121C-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IEnumHxRegFilter = interface(IUnknown)
    ['{3141121C-A502-11D2-BBCA-00C04F8EC294}']
    function Next(celt: LongWord; out ppIHxRegFilter: IHxRegFilter; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Clone(out ppEnum: IEnumHxRegFilter): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IHxSampleCollection
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111E6-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxSampleCollection = interface(IDispatch)
    ['{314111E6-A502-11D2-BBCA-00C04F8EC294}']
    function GetSampleFromId(const bstrTopicUrl: WideString; const bstrId: WideString; 
                             const bstrSFLName: WideString): IHxSample; safecall;
  end;

// *********************************************************************//
// DispIntf:  IHxSampleCollectionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111E6-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxSampleCollectionDisp = dispinterface
    ['{314111E6-A502-11D2-BBCA-00C04F8EC294}']
    function GetSampleFromId(const bstrTopicUrl: WideString; const bstrId: WideString; 
                             const bstrSFLName: WideString): IHxSample; dispid 72448;
  end;
  {$EXTERNALSYM IHxSampleCollectionDisp}

// *********************************************************************//
// Interface: IHxSample
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111E8-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxSample = interface(IDispatch)
    ['{314111E8-A502-11D2-BBCA-00C04F8EC294}']
    function Get_SampleId: WideString; safecall;
    function Get_LoadString: WideString; safecall;
    function Get_DestinationDir: WideString; safecall;
    function Get_ProjectFileExt: WideString; safecall;
    function Get_FileCount: Integer; safecall;
    function GetFileNameAtIndex(index: Integer): WideString; safecall;
    procedure CopyFileAtIndex(index: Integer; const bstrDest: WideString; 
                              option: HxSampleFileCopyOption); safecall;
    function ChooseDirectory(const bstrDefaultDir: WideString; const bstrTitle: WideString): WideString; safecall;
    function GetFileTextAtIndex(index: Integer): WideString; safecall;
    property SampleId: WideString read Get_SampleId;
    property LoadString: WideString read Get_LoadString;
    property DestinationDir: WideString read Get_DestinationDir;
    property ProjectFileExt: WideString read Get_ProjectFileExt;
    property FileCount: Integer read Get_FileCount;
  end;

// *********************************************************************//
// DispIntf:  IHxSampleDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111E8-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxSampleDisp = dispinterface
    ['{314111E8-A502-11D2-BBCA-00C04F8EC294}']
    property SampleId: WideString readonly dispid 72704;
    property LoadString: WideString readonly dispid 72705;
    property DestinationDir: WideString readonly dispid 72706;
    property ProjectFileExt: WideString readonly dispid 72707;
    property FileCount: Integer readonly dispid 72709;
    function GetFileNameAtIndex(index: Integer): WideString; dispid 72710;
    procedure CopyFileAtIndex(index: Integer; const bstrDest: WideString; 
                              option: HxSampleFileCopyOption); dispid 72711;
    function ChooseDirectory(const bstrDefaultDir: WideString; const bstrTitle: WideString): WideString; dispid 72713;
    function GetFileTextAtIndex(index: Integer): WideString; dispid 72714;
  end;
  {$EXTERNALSYM IHxSampleDisp}

// *********************************************************************//
// Interface: IHxRegistryWalker
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111EF-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegistryWalker = interface(IDispatch)
    ['{314111EF-A502-11D2-BBCA-00C04F8EC294}']
    function Get_RegisteredNamespaceList(const bstrStart: WideString): IHxRegNamespaceList; safecall;
    property RegisteredNamespaceList[const bstrStart: WideString]: IHxRegNamespaceList read Get_RegisteredNamespaceList;
  end;

// *********************************************************************//
// DispIntf:  IHxRegistryWalkerDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111EF-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegistryWalkerDisp = dispinterface
    ['{314111EF-A502-11D2-BBCA-00C04F8EC294}']
    property RegisteredNamespaceList[const bstrStart: WideString]: IHxRegNamespaceList readonly dispid 72960;
  end;
  {$EXTERNALSYM IHxRegistryWalkerDisp}

// *********************************************************************//
// Interface: IHxRegNamespaceList
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111F3-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegNamespaceList = interface(IDispatch)
    ['{314111F3-A502-11D2-BBCA-00C04F8EC294}']
    function Item(index: OleVariant): IHxRegNamespace; safecall;
    function ItemAt(index: Integer): IHxRegNamespace; safecall;
    function EnumRegNamespace(filter: Integer; options: Integer): IEnumHxRegNamespace; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IHxRegNamespaceListDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111F3-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegNamespaceListDisp = dispinterface
    ['{314111F3-A502-11D2-BBCA-00C04F8EC294}']
    function Item(index: OleVariant): IHxRegNamespace; dispid 0;
    function ItemAt(index: Integer): IHxRegNamespace; dispid 73472;
    function EnumRegNamespace(filter: Integer; options: Integer): IEnumHxRegNamespace; dispid 73473;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 73474;
  end;
  {$EXTERNALSYM IHxRegNamespaceListDisp}

// *********************************************************************//
// Interface: IHxRegNamespace
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111F1-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegNamespace = interface(IDispatch)
    ['{314111F1-A502-11D2-BBCA-00C04F8EC294}']
    function Get_Name: WideString; safecall;
    function GetProperty(propid: HxRegNamespacePropId): OleVariant; safecall;
    function IsTitle(const bstrTitle: WideString): WordBool; safecall;
    property Name: WideString read Get_Name;
  end;

// *********************************************************************//
// DispIntf:  IHxRegNamespaceDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111F1-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegNamespaceDisp = dispinterface
    ['{314111F1-A502-11D2-BBCA-00C04F8EC294}']
    property Name: WideString readonly dispid 73216;
    function GetProperty(propid: HxRegNamespacePropId): OleVariant; dispid 73217;
    function IsTitle(const bstrTitle: WideString): WordBool; dispid 73218;
  end;
  {$EXTERNALSYM IHxRegNamespaceDisp}

// *********************************************************************//
// Interface: IEnumHxRegNamespace
// Flags:     (16) Hidden
// GUID:      {314111F5-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IEnumHxRegNamespace = interface(IUnknown)
    ['{314111F5-A502-11D2-BBCA-00C04F8EC294}']
    function Next(celt: LongWord; out ppIHxRegNamespace: IHxRegNamespace; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Clone(out ppEnum: IEnumHxRegNamespace): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IHxRegTitle
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411202-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegTitle = interface(IDispatch)
    ['{31411202-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxRegTitlePropId): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  IHxRegTitleDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411202-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegTitleDisp = dispinterface
    ['{31411202-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxRegTitlePropId): OleVariant; dispid 73984;
  end;
  {$EXTERNALSYM IHxRegTitleDisp}

// *********************************************************************//
// Interface: IHxRegTitleList
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411203-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegTitleList = interface(IDispatch)
    ['{31411203-A502-11D2-BBCA-00C04F8EC294}']
    function Item(index: OleVariant): IHxRegTitle; safecall;
    function ItemAt(index: Integer): IHxRegTitle; safecall;
    function EnumRegTitle(filter: Integer; options: Integer): IEnumHxRegTitle; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IHxRegTitleListDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411203-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegTitleListDisp = dispinterface
    ['{31411203-A502-11D2-BBCA-00C04F8EC294}']
    function Item(index: OleVariant): IHxRegTitle; dispid 0;
    function ItemAt(index: Integer): IHxRegTitle; dispid 74240;
    function EnumRegTitle(filter: Integer; options: Integer): IEnumHxRegTitle; dispid 74241;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 74242;
  end;
  {$EXTERNALSYM IHxRegTitleListDisp}

// *********************************************************************//
// Interface: IEnumHxRegTitle
// Flags:     (16) Hidden
// GUID:      {31411204-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IEnumHxRegTitle = interface(IUnknown)
    ['{31411204-A502-11D2-BBCA-00C04F8EC294}']
    function Next(celt: LongWord; out ppIHxRegTitle: IHxRegTitle; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Clone(out ppEnum: IEnumHxRegTitle): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IHxRegPlugIn
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3141120A-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegPlugIn = interface(IDispatch)
    ['{3141120A-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxRegPlugInPropId): OleVariant; safecall;
  end;

// *********************************************************************//
// DispIntf:  IHxRegPlugInDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3141120A-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegPlugInDisp = dispinterface
    ['{3141120A-A502-11D2-BBCA-00C04F8EC294}']
    function GetProperty(propid: HxRegPlugInPropId): OleVariant; dispid 74752;
  end;
  {$EXTERNALSYM IHxRegPluginDisp}

// *********************************************************************//
// Interface: IHxRegPlugInList
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3141120B-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegPlugInList = interface(IDispatch)
    ['{3141120B-A502-11D2-BBCA-00C04F8EC294}']
    function Item(index: OleVariant): IHxRegPlugIn; safecall;
    function ItemAt(index: Integer): IHxRegPlugIn; safecall;
    function EnumRegPlugIn(filter: Integer; options: Integer): IEnumHxRegPlugIn; safecall;
    function Get__NewEnum: IUnknown; safecall;
    function Get_Count: Integer; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  IHxRegPlugInListDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3141120B-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegPlugInListDisp = dispinterface
    ['{3141120B-A502-11D2-BBCA-00C04F8EC294}']
    function Item(index: OleVariant): IHxRegPlugIn; dispid 0;
    function ItemAt(index: Integer): IHxRegPlugIn; dispid 75008;
    function EnumRegPlugIn(filter: Integer; options: Integer): IEnumHxRegPlugIn; dispid 75009;
    property _NewEnum: IUnknown readonly dispid -4;
    property Count: Integer readonly dispid 75010;
  end;
  {$EXTERNALSYM IHxRegPluginListDisp}

// *********************************************************************//
// Interface: IEnumHxRegPlugIn
// Flags:     (16) Hidden
// GUID:      {3141120C-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IEnumHxRegPlugIn = interface(IUnknown)
    ['{3141120C-A502-11D2-BBCA-00C04F8EC294}']
    function Next(celt: LongWord; out ppIHxRegPlugIn: IHxRegPlugIn; out pceltFetched: LongWord): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Skip(celt: LongWord): HResult; stdcall;
    function Clone(out ppEnum: IEnumHxRegPlugIn): HResult; stdcall;
  end;

// *********************************************************************//
// Interface: IHxRegisterSession
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411218-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegisterSession = interface(IDispatch)
    ['{31411218-A502-11D2-BBCA-00C04F8EC294}']
    function CreateTransaction(const bstrInToken: WideString): WideString; safecall;
    function PostponeTransaction: WideString; safecall;
    procedure ContinueTransaction(const bstrToken: WideString); safecall;
    procedure CommitTransaction; safecall;
    procedure RevertTransaction; safecall;
    function GetRegistrationObject(type_: HxRegisterSession_InterfaceType): IDispatch; safecall;
  end;

// *********************************************************************//
// DispIntf:  IHxRegisterSessionDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411218-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegisterSessionDisp = dispinterface
    ['{31411218-A502-11D2-BBCA-00C04F8EC294}']
    function CreateTransaction(const bstrInToken: WideString): WideString; dispid 75265;
    function PostponeTransaction: WideString; dispid 75268;
    procedure ContinueTransaction(const bstrToken: WideString); dispid 75269;
    procedure CommitTransaction; dispid 75266;
    procedure RevertTransaction; dispid 75267;
    function GetRegistrationObject(type_: HxRegisterSession_InterfaceType): IDispatch; dispid 75270;
  end;
  {$EXTERNALSYM IHxRegisterSessionDisp}

// *********************************************************************//
// Interface: IHxPlugIn
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {314111DA-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxPlugIn = interface(IDispatch)
    ['{314111DA-A502-11D2-BBCA-00C04F8EC294}']
    procedure RegisterHelpPlugIn(const bstrProductNamespace: WideString; 
                                 const bstrProductHxt: WideString; const bstrNamespace: WideString; 
                                 const bstrHxt: WideString; const bstrHxa: WideString; 
                                 options: Integer); safecall;
    procedure RemoveHelpPlugIn(const bstrProductNamespace: WideString; 
                               const bstrProductHxt: WideString; const bstrNamespace: WideString; 
                               const bstrHxt: WideString; const bstrHxa: WideString); safecall;
  end;

// *********************************************************************//
// DispIntf:  IHxPlugInDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {314111DA-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxPlugInDisp = dispinterface
    ['{314111DA-A502-11D2-BBCA-00C04F8EC294}']
    procedure RegisterHelpPlugIn(const bstrProductNamespace: WideString; 
                                 const bstrProductHxt: WideString; const bstrNamespace: WideString; 
                                 const bstrHxt: WideString; const bstrHxa: WideString; 
                                 options: Integer); dispid 66304;
    procedure RemoveHelpPlugIn(const bstrProductNamespace: WideString; 
                               const bstrProductHxt: WideString; const bstrNamespace: WideString; 
                               const bstrHxt: WideString; const bstrHxa: WideString); dispid 66305;
  end;
  {$EXTERNALSYM IHxPlugInDisp}

// *********************************************************************//
// Interface: IHxInitialize
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111AE-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxInitialize = interface(IDispatch)
    ['{314111AE-A502-11D2-BBCA-00C04F8EC294}']
    procedure Initialize(const InitString: WideString; options: Integer); safecall;
    function Get_filter: WideString; safecall;
    procedure Set_filter(const pFilterMoniker: WideString); safecall;
    property filter: WideString read Get_filter write Set_filter;
  end;

// *********************************************************************//
// DispIntf:  IHxInitializeDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {314111AE-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxInitializeDisp = dispinterface
    ['{314111AE-A502-11D2-BBCA-00C04F8EC294}']
    procedure Initialize(const InitString: WideString; options: Integer); dispid 72192;
    property filter: WideString dispid 72193;
  end;
  {$EXTERNALSYM IHxInitializeDisp}

// *********************************************************************//
// Interface: IHxCancel
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411225-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxCancel = interface(IDispatch)
    ['{31411225-A502-11D2-BBCA-00C04F8EC294}']
    function Get_Cancel: HxCancelStatus; safecall;
    procedure Set_Cancel(pbCancel: HxCancelStatus); safecall;
    property Cancel: HxCancelStatus read Get_Cancel write Set_Cancel;
  end;

// *********************************************************************//
// DispIntf:  IHxCancelDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411225-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxCancelDisp = dispinterface
    ['{31411225-A502-11D2-BBCA-00C04F8EC294}']
    property Cancel: HxCancelStatus dispid 76032;
  end;
  {$EXTERNALSYM IHxCancelDisp}

// *********************************************************************//
// DispIntf:  IHxSessionEvents
// Flags:     (4096) Dispatchable
// GUID:      {314111ED-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxSessionEvents = dispinterface
    ['{314111ED-A502-11D2-BBCA-00C04F8EC294}']
    procedure QueryCancel(const pSession: IDispatch; const pCancel: IDispatch; status: Integer); dispid 65800;
    procedure IndexMergeStatus(const pSession: IDispatch; const pCancel: IDispatch; status: Integer); dispid 65801;
    procedure PrintMergeStatus(const pSession: IDispatch; const pCancel: IDispatch; status: Integer); dispid 65802;
    procedure MergeIndexFileName(const pDisp: IDispatch; const bstrFile: WideString); dispid 65804;
  end;
  {$EXTERNALSYM IHxSessionEvents}

// *********************************************************************//
// DispIntf:  IHxRegisterSessionEvents
// Flags:     (4096) Dispatchable
// GUID:      {31411223-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegisterSessionEvents = dispinterface
    ['{31411223-A502-11D2-BBCA-00C04F8EC294}']
    procedure FiltersChanged(const pDisp: IDispatch; var pvar: OleVariant); dispid 75271;
  end;
  {$EXTERNALSYM IHxRegisterSessionEvents}

// *********************************************************************//
// Interface: IHxRegisterProtocol
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411227-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegisterProtocol = interface(IDispatch)
    ['{31411227-A502-11D2-BBCA-00C04F8EC294}']
    procedure Register; safecall;
    procedure Unregister; safecall;
  end;

// *********************************************************************//
// DispIntf:  IHxRegisterProtocolDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {31411227-A502-11D2-BBCA-00C04F8EC294}
// *********************************************************************//
  IHxRegisterProtocolDisp = dispinterface
    ['{31411227-A502-11D2-BBCA-00C04F8EC294}']
    procedure Register; dispid 1610743808;
    procedure Unregister; dispid 1610743809;
  end;
  {$EXTERNALSYM IHxRegisterProtocolDisp}

// *********************************************************************//
// The Class CoHxSession provides a Create and CreateRemote method to          
// create instances of the default interface IHxSession exposed by              
// the CoClass HxSession. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoHxSession = class
    class function Create: IHxSession;
    class function CreateRemote(const MachineName: string): IHxSession;
  end;

// *********************************************************************//
// The Class CoHxRegistryWalker provides a Create and CreateRemote method to          
// create instances of the default interface IHxRegistryWalker exposed by              
// the CoClass HxRegistryWalker. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoHxRegistryWalker = class
    class function Create: IHxRegistryWalker;
    class function CreateRemote(const MachineName: string): IHxRegistryWalker;
  end;

// *********************************************************************//
// The Class CoHxRegisterSession provides a Create and CreateRemote method to          
// create instances of the default interface IHxRegisterSession exposed by              
// the CoClass HxRegisterSession. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoHxRegisterSession = class
    class function Create: IHxRegisterSession;
    class function CreateRemote(const MachineName: string): IHxRegisterSession;
  end;

// *********************************************************************//
// The Class CoHxRegisterProtocol provides a Create and CreateRemote method to          
// create instances of the default interface IHxRegisterProtocol exposed by              
// the CoClass HxRegisterProtocol. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoHxRegisterProtocol = class
    class function Create: IHxRegisterProtocol;
    class function CreateRemote(const MachineName: string): IHxRegisterProtocol;
  end;

implementation

uses ComObj;

class function CoHxSession.Create: IHxSession;
begin
  Result := CreateComObject(CLASS_HxSession) as IHxSession;
end;

class function CoHxSession.CreateRemote(const MachineName: string): IHxSession;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_HxSession) as IHxSession;
end;

class function CoHxRegistryWalker.Create: IHxRegistryWalker;
begin
  Result := CreateComObject(CLASS_HxRegistryWalker) as IHxRegistryWalker;
end;

class function CoHxRegistryWalker.CreateRemote(const MachineName: string): IHxRegistryWalker;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_HxRegistryWalker) as IHxRegistryWalker;
end;

class function CoHxRegisterSession.Create: IHxRegisterSession;
begin
  Result := CreateComObject(CLASS_HxRegisterSession) as IHxRegisterSession;
end;

class function CoHxRegisterSession.CreateRemote(const MachineName: string): IHxRegisterSession;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_HxRegisterSession) as IHxRegisterSession;
end;

class function CoHxRegisterProtocol.Create: IHxRegisterProtocol;
begin
  Result := CreateComObject(CLASS_HxRegisterProtocol) as IHxRegisterProtocol;
end;

class function CoHxRegisterProtocol.CreateRemote(const MachineName: string): IHxRegisterProtocol;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_HxRegisterProtocol) as IHxRegisterProtocol;
end;

end.
