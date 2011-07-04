{**************************************************************************************************}
{                                                                                                  }
{  Borland Delphi Runtime Library                                                                  }
{  SNMP functions interface unit                                                                   }
{                                                                                                  }
{  The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License") }
{  you may not use this file except in compliance with the License. You may obtain a copy of the   }
{  License at http://www.mozilla.org/MPL/                                                          }
{                                                                                                  }
{  Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF  }
{  ANY KIND, either express or implied. See the License for the specific language governing rights }
{  and limitations under the License.                                                              }
{                                                                                                  }
{  The Original Code is: snmp.h.                                                                   }
{  The Initial Developer of the Original Code is Microsoft. Portions created by Microsoft are      }
{  Copyright (C) 1992-1999 Microsoft Corporation. All Rights Reserved.                             }
{                                                                                                  }
{  The Original Pascal code is: Snmp.pas, released 2001-10-05.                                     }
{  The Initial Developer of the Original Pascal code is Petr Vones                                 }
{  (petrdott v att mujmail dott cz). Portions created by Petr Vones are Copyright (C) 2001 Petr    }
{  Vones. All Rights Reserved.                                                                     }
{                                                                                                  }
{  Obtained through:                                                                               }
{    Joint Endeavour of Delphi Innovators (Project JEDI)                                           }
{                                                                                                  }
{  You may retrieve the latest version of this file at the Project JEDI homepage, located at       }
{  http://delphi-jedi.org                                                                          }
{                                                                                                  }
{  Contributor(s):                                                                                 }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2007-09-17 23:41:02 +0200 (lun., 17 sept. 2007)                         $ }
{ Revision:      $Rev:: 2175                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

unit Snmp;

interface

{$I jcl.inc}

{$DEFINE SNMP_DYNAMIC_LINK}
{$DEFINE SNMP_DYNAMIC_LINK_EXPLICIT}
{$DEFINE SNMPSTRICT}

{$ALIGN ON}
{$MINENUMSIZE 4}
{$IFNDEF SNMP_DYNAMIC_LINK}
{$IFDEF SUPPORTS_WEAKPACKAGEUNIT}
  {$WEAKPACKAGEUNIT ON}
{$ENDIF SUPPORTS_WEAKPACKAGEUNIT}
{$ENDIF ~SNMP_DYNAMIC_LINK}

uses
  Windows, SysUtils;

(*$HPPEMIT '#include <snmp.h>'*)

type
  PAsnOctetString = ^TAsnOctetString;
  TAsnOctetString = record
    stream: PChar;
    length: UINT;
    dynamic_: Boolean;
  end;

  PAsnObjectIdentifier = ^TAsnObjectIdentifier;
  TAsnObjectIdentifier = record
    idLength: UINT;
    ids: PUINT;
  end;

  TAsnInteger32        = LongInt;
  {$EXTERNALSYM TAsnInteger32}
  TAsnUnsigned32       = ULONG;
  {$EXTERNALSYM TAsnUnsigned32}
  TAsnCounter64        = ULARGE_INTEGER;
  {$EXTERNALSYM TAsnCounter64}
  TAsnCounter32        = TAsnUnsigned32;
  {$EXTERNALSYM TAsnCounter32}
  TAsnGauge32          = TAsnUnsigned32;
  {$EXTERNALSYM TAsnGauge32}
  TAsnTimeticks        = TAsnUnsigned32;
  {$EXTERNALSYM TAsnTimeticks}
  TAsnBits             = TAsnOctetString;
  {$EXTERNALSYM TAsnBits}
  TAsnSequence         = TAsnOctetString;
  {$EXTERNALSYM TAsnSequence}
  TAsnImplicitSequence = TAsnOctetString;
  {$EXTERNALSYM TAsnImplicitSequence}
  TAsnIPAddress        = TAsnOctetString;
  {$EXTERNALSYM TAsnIPAddress}
  TAsnNetworkAddress   = TAsnOctetString;
  {$EXTERNALSYM TAsnNetworkAddress}
  TAsnDisplayString    = TAsnOctetString;
  {$EXTERNALSYM TAsnDisplayString}
  TAsnOpaque           = TAsnOctetString;
  {$EXTERNALSYM TAsnOpaque}

  PAsnAny = ^TAsnAny;
  TAsnAny = record
    asnType: Byte;
    case Integer of
      0: (number: TAsnInteger32);          // ASN_INTEGER, ASN_INTEGER32
      1: (unsigned32: TAsnUnsigned32);     // ASN_UNSIGNED32
      2: (counter64: TAsnCounter64);       // ASN_COUNTER64
      3: (string_: TAsnOctetString);       // ASN_OCTETSTRING
      4: (bits: TAsnBits);                 // ASN_BITS
      5: (object_: TAsnObjectIdentifier);  // ASN_OBJECTIDENTIFIER
      6: (sequence: TAsnSequence);         // ASN_SEQUENCE
      7: (address: TAsnIPAddress);         // ASN_IPADDRESS
      8: (counter: TAsnCounter32);         // ASN_COUNTER32
      9: (gauge: TAsnGauge32);             // ASN_GAUGE32
     10: (ticks: TAsnTimeticks);           // ASN_TIMETICKS
     11: (arbitrary: TAsnOpaque);          // ASN_OPAQUE
  end;

  TAsnObjectName = TAsnObjectIdentifier;
  TAsnObjectSyntax = TAsnAny;

  PSnmpVarBind = ^TSnmpVarBind;
  TSnmpVarBind = record
    name: TAsnObjectName;
    value: TAsnObjectSyntax;
  end;

  PSnmpVarBindList = ^TSnmpVarBindList;
  TSnmpVarBindList = record
    list: PSnmpVarBind;
    len: UINT;
  end;

const

{ ASN/BER Base Types }

  ASN_UNIVERSAL                   = $00;
  {$EXTERNALSYM ASN_UNIVERSAL}
  ASN_APPLICATION                 = $40;
  {$EXTERNALSYM ASN_APPLICATION}
  ASN_CONTEXT                     = $80;
  {$EXTERNALSYM ASN_CONTEXT}
  ASN_PRIVATE                     = $C0;
  {$EXTERNALSYM ASN_PRIVATE}

  ASN_PRIMITIVE                   = $00;
  {$EXTERNALSYM ASN_PRIMITIVE}
  ASN_CONSTRUCTOR                 = $20;
  {$EXTERNALSYM ASN_CONSTRUCTOR}

{ PDU Type Values }

  SNMP_PDU_GET                    = (ASN_CONTEXT or ASN_CONSTRUCTOR or $0);
  {$EXTERNALSYM SNMP_PDU_GET}
  SNMP_PDU_GETNEXT                = (ASN_CONTEXT or ASN_CONSTRUCTOR or $1);
  {$EXTERNALSYM SNMP_PDU_GETNEXT}
  SNMP_PDU_RESPONSE               = (ASN_CONTEXT or ASN_CONSTRUCTOR or $2);
  {$EXTERNALSYM SNMP_PDU_RESPONSE}
  SNMP_PDU_SET                    = (ASN_CONTEXT or ASN_CONSTRUCTOR or $3);
  {$EXTERNALSYM SNMP_PDU_SET}
  SNMP_PDU_V1TRAP                 = (ASN_CONTEXT or ASN_CONSTRUCTOR or $4);
  {$EXTERNALSYM SNMP_PDU_V1TRAP}
  SNMP_PDU_GETBULK                = (ASN_CONTEXT or ASN_CONSTRUCTOR or $5);
  {$EXTERNALSYM SNMP_PDU_GETBULK}
  SNMP_PDU_INFORM                 = (ASN_CONTEXT or ASN_CONSTRUCTOR or $6);
  {$EXTERNALSYM SNMP_PDU_INFORM}
  SNMP_PDU_TRAP                   = (ASN_CONTEXT or ASN_CONSTRUCTOR or $7);
  {$EXTERNALSYM SNMP_PDU_TRAP}

{ SNMP Simple Syntax Values }

  ASN_INTEGER                     = (ASN_UNIVERSAL or ASN_PRIMITIVE or $02);
  {$EXTERNALSYM ASN_INTEGER}
  ASN_BITS                        = (ASN_UNIVERSAL or ASN_PRIMITIVE or $03);
  {$EXTERNALSYM ASN_BITS}
  ASN_OCTETSTRING                 = (ASN_UNIVERSAL or ASN_PRIMITIVE or $04);
  {$EXTERNALSYM ASN_OCTETSTRING}
  ASN_NULL                        = (ASN_UNIVERSAL or ASN_PRIMITIVE or $05);
  {$EXTERNALSYM ASN_NULL}
  ASN_OBJECTIDENTIFIER            = (ASN_UNIVERSAL or ASN_PRIMITIVE or $06);
  {$EXTERNALSYM ASN_OBJECTIDENTIFIER}
  ASN_INTEGER32                   = ASN_INTEGER;
  {$EXTERNALSYM ASN_INTEGER32}

{ SNMP Constructor Syntax Values }

  ASN_SEQUENCE                    = (ASN_UNIVERSAL or ASN_CONSTRUCTOR or $10);
  {$EXTERNALSYM ASN_SEQUENCE}
  ASN_SEQUENCEOF                  = ASN_SEQUENCE;
  {$EXTERNALSYM ASN_SEQUENCEOF}

{ SNMP Application Syntax Values }

  ASN_IPADDRESS                   = (ASN_APPLICATION or ASN_PRIMITIVE or $00);
  {$EXTERNALSYM ASN_IPADDRESS}
  ASN_COUNTER32                   = (ASN_APPLICATION or ASN_PRIMITIVE or $01);
  {$EXTERNALSYM ASN_COUNTER32}
  ASN_GAUGE32                     = (ASN_APPLICATION or ASN_PRIMITIVE or $02);
  {$EXTERNALSYM ASN_GAUGE32}
  ASN_TIMETICKS                   = (ASN_APPLICATION or ASN_PRIMITIVE or $03);
  {$EXTERNALSYM ASN_TIMETICKS}
  ASN_OPAQUE                      = (ASN_APPLICATION or ASN_PRIMITIVE or $04);
  {$EXTERNALSYM ASN_OPAQUE}
  ASN_COUNTER64                   = (ASN_APPLICATION or ASN_PRIMITIVE or $06);
  {$EXTERNALSYM ASN_COUNTER64}
  ASN_UNSIGNED32                  = (ASN_APPLICATION or ASN_PRIMITIVE or $07);
  {$EXTERNALSYM ASN_UNSIGNED32}

{ SNMP Exception Conditions }

  SNMP_EXCEPTION_NOSUCHOBJECT     = (ASN_CONTEXT or ASN_PRIMITIVE or $00);
  {$EXTERNALSYM SNMP_EXCEPTION_NOSUCHOBJECT}
  SNMP_EXCEPTION_NOSUCHINSTANCE   = (ASN_CONTEXT or ASN_PRIMITIVE or $01);
  {$EXTERNALSYM SNMP_EXCEPTION_NOSUCHINSTANCE}
  SNMP_EXCEPTION_ENDOFMIBVIEW     = (ASN_CONTEXT or ASN_PRIMITIVE or $02);
  {$EXTERNALSYM SNMP_EXCEPTION_ENDOFMIBVIEW}

{ SNMP Request Types (used in SnmpExtensionQueryEx) }

  SNMP_EXTENSION_GET              = SNMP_PDU_GET;
  {$EXTERNALSYM SNMP_EXTENSION_GET}
  SNMP_EXTENSION_GET_NEXT         = SNMP_PDU_GETNEXT;
  {$EXTERNALSYM SNMP_EXTENSION_GET_NEXT}
  SNMP_EXTENSION_GET_BULK         = SNMP_PDU_GETBULK;
  {$EXTERNALSYM SNMP_EXTENSION_GET_BULK}
  SNMP_EXTENSION_SET_TEST         = (ASN_PRIVATE or ASN_CONSTRUCTOR or $0);
  {$EXTERNALSYM SNMP_EXTENSION_SET_TEST}
  SNMP_EXTENSION_SET_COMMIT       = SNMP_PDU_SET;
  {$EXTERNALSYM SNMP_EXTENSION_SET_COMMIT}
  SNMP_EXTENSION_SET_UNDO         = (ASN_PRIVATE or ASN_CONSTRUCTOR or $1);
  {$EXTERNALSYM SNMP_EXTENSION_SET_UNDO}
  SNMP_EXTENSION_SET_CLEANUP      = (ASN_PRIVATE or ASN_CONSTRUCTOR or $2);
  {$EXTERNALSYM SNMP_EXTENSION_SET_CLEANUP}

{ SNMP Error Codes }

  SNMP_ERRORSTATUS_NOERROR                    = 0;
  {$EXTERNALSYM SNMP_ERRORSTATUS_NOERROR}
  SNMP_ERRORSTATUS_TOOBIG                     = 1;
  {$EXTERNALSYM SNMP_ERRORSTATUS_TOOBIG}
  SNMP_ERRORSTATUS_NOSUCHNAME                 = 2;
  {$EXTERNALSYM SNMP_ERRORSTATUS_NOSUCHNAME}
  SNMP_ERRORSTATUS_BADVALUE                   = 3;
  {$EXTERNALSYM SNMP_ERRORSTATUS_BADVALUE}
  SNMP_ERRORSTATUS_READONLY                   = 4;
  {$EXTERNALSYM SNMP_ERRORSTATUS_READONLY}
  SNMP_ERRORSTATUS_GENERR                     = 5;
  {$EXTERNALSYM SNMP_ERRORSTATUS_GENERR}
  SNMP_ERRORSTATUS_NOACCESS                   = 6;
  {$EXTERNALSYM SNMP_ERRORSTATUS_NOACCESS}
  SNMP_ERRORSTATUS_WRONGTYPE                  = 7;
  {$EXTERNALSYM SNMP_ERRORSTATUS_WRONGTYPE}
  SNMP_ERRORSTATUS_WRONGLENGTH                = 8;
  {$EXTERNALSYM SNMP_ERRORSTATUS_WRONGLENGTH}
  SNMP_ERRORSTATUS_WRONGENCODING              = 9;
  {$EXTERNALSYM SNMP_ERRORSTATUS_WRONGENCODING}
  SNMP_ERRORSTATUS_WRONGVALUE                 = 10;
  {$EXTERNALSYM SNMP_ERRORSTATUS_WRONGVALUE}
  SNMP_ERRORSTATUS_NOCREATION                 = 11;
  {$EXTERNALSYM SNMP_ERRORSTATUS_NOCREATION}
  SNMP_ERRORSTATUS_INCONSISTENTVALUE          = 12;
  {$EXTERNALSYM SNMP_ERRORSTATUS_INCONSISTENTVALUE}
  SNMP_ERRORSTATUS_RESOURCEUNAVAILABLE        = 13;
  {$EXTERNALSYM SNMP_ERRORSTATUS_RESOURCEUNAVAILABLE}
  SNMP_ERRORSTATUS_COMMITFAILED               = 14;
  {$EXTERNALSYM SNMP_ERRORSTATUS_COMMITFAILED}
  SNMP_ERRORSTATUS_UNDOFAILED                 = 15;
  {$EXTERNALSYM SNMP_ERRORSTATUS_UNDOFAILED}
  SNMP_ERRORSTATUS_AUTHORIZATIONERROR         = 16;
  {$EXTERNALSYM SNMP_ERRORSTATUS_AUTHORIZATIONERROR}
  SNMP_ERRORSTATUS_NOTWRITABLE                = 17;
  {$EXTERNALSYM SNMP_ERRORSTATUS_NOTWRITABLE}
  SNMP_ERRORSTATUS_INCONSISTENTNAME           = 18;
  {$EXTERNALSYM SNMP_ERRORSTATUS_INCONSISTENTNAME}

{ SNMPv1 Trap Types }

  SNMP_GENERICTRAP_COLDSTART                  = 0;
  {$EXTERNALSYM SNMP_GENERICTRAP_COLDSTART}
  SNMP_GENERICTRAP_WARMSTART                  = 1;
  {$EXTERNALSYM SNMP_GENERICTRAP_WARMSTART}
  SNMP_GENERICTRAP_LINKDOWN                   = 2;
  {$EXTERNALSYM SNMP_GENERICTRAP_LINKDOWN}
  SNMP_GENERICTRAP_LINKUP                     = 3;
  {$EXTERNALSYM SNMP_GENERICTRAP_LINKUP}
  SNMP_GENERICTRAP_AUTHFAILURE                = 4;
  {$EXTERNALSYM SNMP_GENERICTRAP_AUTHFAILURE}
  SNMP_GENERICTRAP_EGPNEIGHLOSS               = 5;
  {$EXTERNALSYM SNMP_GENERICTRAP_EGPNEIGHLOSS}
  SNMP_GENERICTRAP_ENTERSPECIFIC              = 6;
  {$EXTERNALSYM SNMP_GENERICTRAP_ENTERSPECIFIC}

{ SNMP Access Types }

  SNMP_ACCESS_NONE                            = 0;
  {$EXTERNALSYM SNMP_ACCESS_NONE}
  SNMP_ACCESS_NOTIFY                          = 1;
  {$EXTERNALSYM SNMP_ACCESS_NOTIFY}
  SNMP_ACCESS_READ_ONLY                       = 2;
  {$EXTERNALSYM SNMP_ACCESS_READ_ONLY}
  SNMP_ACCESS_READ_WRITE                      = 3;
  {$EXTERNALSYM SNMP_ACCESS_READ_WRITE}
  SNMP_ACCESS_READ_CREATE                     = 4;
  {$EXTERNALSYM SNMP_ACCESS_READ_CREATE}

{ SNMP API Return Code Definitions }

type
  SNMPAPI                                     = Integer;
  {$EXTERNALSYM SNMPAPI}
const
  SNMPAPI_NOERROR                             = True;
  {$EXTERNALSYM SNMPAPI_NOERROR}
  SNMPAPI_ERROR                               = False;
  {$EXTERNALSYM SNMPAPI_ERROR}

{ SNMP Extension API Type Definitions }

type
  TSnmpExtensionInit = function (dwUptimeReference: DWORD; var phSubagentTrapEvent: THandle;
    var pFirstSupportedRegion: PAsnObjectIdentifier): Boolean; stdcall;

  TSnmpExtensionInitEx = function (var pNextSupportedRegion: PAsnObjectIdentifier): Boolean; stdcall;

  TSnmpExtensionMonitor = function (pAgentMgmtData: Pointer): Boolean; stdcall;

  TSnmpExtensionQuery = function (bPduType: Byte; var pVarBindList: TSnmpVarBindList;
    var pErrorStatus: TAsnInteger32; var pErrorIndex: TAsnInteger32): Boolean; stdcall;

  TSnmpExtensionQueryEx = function (nRequestType: UINT; nTransactionId: UINT; var pVarBindList: PSnmpVarBindList;
    var pContextInfo: PAsnOctetString; var pErrorStatus: TAsnInteger32; var pErrorIndex: TAsnInteger32): Boolean; stdcall;

  TSnmpExtensionTrap = function (pEnterpriseOid: PAsnObjectIdentifier; var pGenericTrapId: TAsnInteger32;
     var pSpecificTrapId: TAsnInteger32; var pTimeStamp: TAsnTimeticks; var pVarBindList: PSnmpVarBindList): Boolean; stdcall;

  TSnmpExtensionClose = procedure; stdcall;

{ SNMP API Prototypes }

{$IFDEF SNMP_DYNAMIC_LINK}

var
  SnmpUtilOidCpy: function(pOidDst: PAsnObjectIdentifier; pOidSrc: PAsnObjectIdentifier): SNMPAPI; stdcall;
  SnmpUtilOidAppend: function(pOidDst: PAsnObjectIdentifier; pOidSrc: PAsnObjectIdentifier): SNMPAPI; stdcall;
  SnmpUtilOidNCmp: function(pOid1, pOid2: PAsnObjectIdentifier; nSubIds: UINT): SNMPAPI; stdcall;
  SnmpUtilOidCmp: function(pOid1, pOid2: PAsnObjectIdentifier): SNMPAPI; stdcall;
  SnmpUtilOidFree: procedure(pOid: TAsnObjectIdentifier); stdcall;
  SnmpUtilOctetsCmp: function(pOctets1, pOctets2: PAsnOctetString): SNMPAPI; stdcall;
  SnmpUtilOctetsNCmp: function(pOctets1, pOctets2: PAsnOctetString; nChars: UINT): SNMPAPI; stdcall;
  SnmpUtilOctetsCpy: function(pOctetsDst, pOctetsSrc: PAsnOctetString): SNMPAPI; stdcall;
  SnmpUtilOctetsFree: procedure(pOctets: PAsnOctetString); stdcall;
  SnmpUtilAsnAnyCpy: function(pAnyDst, pAnySrc: PAsnAny): SNMPAPI; stdcall;
  SnmpUtilAsnAnyFree: procedure(pAny: PAsnAny); stdcall;
  SnmpUtilVarBindCpy: function(pVbDst: PSnmpVarBind; pVbSrc: PSnmpVarBind): SNMPAPI; stdcall;
  SnmpUtilVarBindFree: procedure(pVb: PSnmpVarBind); stdcall;
  SnmpUtilVarBindListCpy: function(pVblDst: PSnmpVarBindList; pVblSrc: PSnmpVarBindList): SNMPAPI; stdcall;
  SnmpUtilVarBindListFree: procedure(pVbl: PSnmpVarBindList); stdcall;
  SnmpUtilMemFree: procedure(pMem: Pointer); stdcall;
  SnmpUtilMemAlloc: function(nBytes: UINT): Pointer; stdcall;
  SnmpUtilMemReAlloc: function(pMem: Pointer; nBytes: UINT): Pointer; stdcall;
  SnmpUtilOidToA: function(Oid: PAsnObjectIdentifier): PChar; stdcall;
  SnmpUtilIdsToA: function(Ids: PUINT; IdLength: UINT): PChar; stdcall;
  SnmpUtilPrintOid: procedure(Oid: PAsnObjectIdentifier); stdcall;
  SnmpUtilPrintAsnAny: procedure(pAny: PAsnAny); stdcall;
  SnmpSvcGetUptime: function: DWORD; stdcall;
  SnmpSvcSetLogLevel: procedure(nLogLevel: Integer); stdcall;
  SnmpSvcSetLogType: procedure(nLogType: Integer); stdcall;

{$ELSE}

function SnmpUtilOidCpy(pOidDst: PAsnObjectIdentifier; pOidSrc: PAsnObjectIdentifier): SNMPAPI; stdcall;
function SnmpUtilOidAppend(pOidDst: PAsnObjectIdentifier; pOidSrc: PAsnObjectIdentifier): SNMPAPI; stdcall;
function SnmpUtilOidNCmp(pOid1, pOid2: PAsnObjectIdentifier; nSubIds: UINT): SNMPAPI; stdcall;
function SnmpUtilOidCmp(pOid1, pOid2: PAsnObjectIdentifier): SNMPAPI; stdcall;
procedure SnmpUtilOidFree(pOid: TAsnObjectIdentifier); stdcall;
function SnmpUtilOctetsCmp(pOctets1, pOctets2: PAsnOctetString): SNMPAPI; stdcall;
function SnmpUtilOctetsNCmp(pOctets1, pOctets2: PAsnOctetString; nChars: UINT): SNMPAPI; stdcall;
function SnmpUtilOctetsCpy(pOctetsDst, pOctetsSrc: PAsnOctetString): SNMPAPI; stdcall;
procedure SnmpUtilOctetsFree(pOctets: PAsnOctetString); stdcall;
function SnmpUtilAsnAnyCpy(pAnyDst, pAnySrc: PAsnAny): SNMPAPI; stdcall;
procedure SnmpUtilAsnAnyFree(pAny: PAsnAny); stdcall;
function SnmpUtilVarBindCpy(pVbDst: PSnmpVarBind; pVbSrc: PSnmpVarBind): SNMPAPI; stdcall;
procedure SnmpUtilVarBindFree(pVb: PSnmpVarBind); stdcall;
function SnmpUtilVarBindListCpy(pVblDst: PSnmpVarBindList; pVblSrc: PSnmpVarBindList): SNMPAPI; stdcall;
procedure SnmpUtilVarBindListFree(pVbl: PSnmpVarBindList); stdcall;
procedure SnmpUtilMemFree(pMem: Pointer); stdcall;
function SnmpUtilMemAlloc(nBytes: UINT): Pointer; stdcall;
function SnmpUtilMemReAlloc(pMem: Pointer; nBytes: UINT): Pointer; stdcall;
function SnmpUtilOidToA(Oid: PAsnObjectIdentifier): PChar; stdcall;
function SnmpUtilIdsToA(Ids: PUINT; IdLength: UINT): PChar; stdcall;
procedure SnmpUtilPrintOid(Oid: PAsnObjectIdentifier); stdcall;
procedure SnmpUtilPrintAsnAny(pAny: PAsnAny); stdcall;
function SnmpSvcGetUptime: DWORD; stdcall;
procedure SnmpSvcSetLogLevel(nLogLevel: Integer); stdcall;
procedure SnmpSvcSetLogType(nLogType: Integer); stdcall;

{$ENDIF SNMP_DYNAMIC_LINK}

{$EXTERNALSYM SnmpUtilOidCpy}
{$EXTERNALSYM SnmpUtilOidAppend}
{$EXTERNALSYM SnmpUtilOidNCmp}
{$EXTERNALSYM SnmpUtilOidCmp}
{$EXTERNALSYM SnmpUtilOidFree}
{$EXTERNALSYM SnmpUtilOctetsCmp}
{$EXTERNALSYM SnmpUtilOctetsNCmp}
{$EXTERNALSYM SnmpUtilOctetsCpy}
{$EXTERNALSYM SnmpUtilOctetsFree}
{$EXTERNALSYM SnmpUtilAsnAnyCpy}
{$EXTERNALSYM SnmpUtilAsnAnyFree}
{$EXTERNALSYM SnmpUtilVarBindCpy}
{$EXTERNALSYM SnmpUtilVarBindFree}
{$EXTERNALSYM SnmpUtilVarBindListCpy}
{$EXTERNALSYM SnmpUtilVarBindListFree}
{$EXTERNALSYM SnmpUtilMemFree}
{$EXTERNALSYM SnmpUtilMemAlloc}
{$EXTERNALSYM SnmpUtilMemReAlloc}
{$EXTERNALSYM SnmpUtilOidToA}
{$EXTERNALSYM SnmpUtilIdsToA}
{$EXTERNALSYM SnmpUtilPrintOid}
{$EXTERNALSYM SnmpUtilPrintAsnAny}
{$EXTERNALSYM SnmpSvcGetUptime}
{$EXTERNALSYM SnmpSvcSetLogLevel}
{$EXTERNALSYM SnmpSvcSetLogType}

{ SNMP Debugging Definitions }

const
  SNMP_LOG_SILENT                 = $0;
  {$EXTERNALSYM SNMP_LOG_SILENT}
  SNMP_LOG_FATAL                  = $1;
  {$EXTERNALSYM SNMP_LOG_FATAL}
  SNMP_LOG_ERROR                  = $2;
  {$EXTERNALSYM SNMP_LOG_ERROR}
  SNMP_LOG_WARNING                = $3;
  {$EXTERNALSYM SNMP_LOG_WARNING}
  SNMP_LOG_TRACE                  = $4;
  {$EXTERNALSYM SNMP_LOG_TRACE}
  SNMP_LOG_VERBOSE                = $5;
  {$EXTERNALSYM SNMP_LOG_VERBOSE}

  SNMP_OUTPUT_TO_CONSOLE          = $1;
  {$EXTERNALSYM SNMP_OUTPUT_TO_CONSOLE}
  SNMP_OUTPUT_TO_LOGFILE          = $2;
  {$EXTERNALSYM SNMP_OUTPUT_TO_LOGFILE}
  SNMP_OUTPUT_TO_EVENTLOG         = $4;  // no longer supported
  {$EXTERNALSYM SNMP_OUTPUT_TO_EVENTLOG}
  SNMP_OUTPUT_TO_DEBUGGER         = $8;
  {$EXTERNALSYM SNMP_OUTPUT_TO_DEBUGGER}

{ SNMP Debugging Prototypes }

{$IFNDEF SNMP_DYNAMIC_LINK}

procedure SnmpUtilDbgPrint(nLogLevel: Integer; szFormat: PChar); stdcall;

{$ELSE SNMP_DYNAMIC_LINK}

var
  SnmpUtilDbgPrint: procedure (nLogLevel: Integer; szFormat: PChar); stdcall;

{$ENDIF ~SNMP_DYNAMIC_LINK}

{$EXTERNALSYM SnmpUtilDbgPrint}

{ Miscellaneous definitions }

const
  DEFINE_NULLOID: TAsnObjectIdentifier = (idLength: 0; ids: nil);
  {$EXTERNALSYM DEFINE_NULLOID}
  DEFINE_NULLOCTETS: TAsnOctetString = (stream: nil; length: 0; dynamic_: False);
  {$EXTERNALSYM DEFINE_NULLOCTETS}

  DEFAULT_SNMP_PORT_UDP       = 161;
  {$EXTERNALSYM DEFAULT_SNMP_PORT_UDP}
  DEFAULT_SNMP_PORT_IPX       = 36879;
  {$EXTERNALSYM DEFAULT_SNMP_PORT_IPX}
  DEFAULT_SNMPTRAP_PORT_UDP   = 162;
  {$EXTERNALSYM DEFAULT_SNMPTRAP_PORT_UDP}
  DEFAULT_SNMPTRAP_PORT_IPX   = 36880;
  {$EXTERNALSYM DEFAULT_SNMPTRAP_PORT_IPX}
  SNMP_MAX_OID_LEN            = 128;
  {$EXTERNALSYM SNMP_MAX_OID_LEN}

{ API Error Code Definitions }

  SNMP_MEM_ALLOC_ERROR            = 1;
  {$EXTERNALSYM SNMP_MEM_ALLOC_ERROR}
  SNMP_BERAPI_INVALID_LENGTH      = 10;
  {$EXTERNALSYM SNMP_BERAPI_INVALID_LENGTH}
  SNMP_BERAPI_INVALID_TAG         = 11;
  {$EXTERNALSYM SNMP_BERAPI_INVALID_TAG}
  SNMP_BERAPI_OVERFLOW            = 12;
  {$EXTERNALSYM SNMP_BERAPI_OVERFLOW}
  SNMP_BERAPI_SHORT_BUFFER        = 13;
  {$EXTERNALSYM SNMP_BERAPI_SHORT_BUFFER}
  SNMP_BERAPI_INVALID_OBJELEM     = 14;
  {$EXTERNALSYM SNMP_BERAPI_INVALID_OBJELEM}
  SNMP_PDUAPI_UNRECOGNIZED_PDU    = 20;
  {$EXTERNALSYM SNMP_PDUAPI_UNRECOGNIZED_PDU}
  SNMP_PDUAPI_INVALID_ES          = 21;
  {$EXTERNALSYM SNMP_PDUAPI_INVALID_ES}
  SNMP_PDUAPI_INVALID_GT          = 22;
  {$EXTERNALSYM SNMP_PDUAPI_INVALID_GT}
  SNMP_AUTHAPI_INVALID_VERSION    = 30;
  {$EXTERNALSYM SNMP_AUTHAPI_INVALID_VERSION}
  SNMP_AUTHAPI_INVALID_MSG_TYPE   = 31;
  {$EXTERNALSYM SNMP_AUTHAPI_INVALID_MSG_TYPE}
  SNMP_AUTHAPI_TRIV_AUTH_FAILED   = 32;
  {$EXTERNALSYM SNMP_AUTHAPI_TRIV_AUTH_FAILED}

{ Support for old definitions (support disabled via SNMPSTRICT) }

{$IFNDEF SNMPSTRICT}

{$IFNDEF SNMP_DYNAMIC_LINK}

var
  SNMP_oidcpy: function (pOidDst: PAsnObjectIdentifier; pOidSrc: PAsnObjectIdentifier): SNMPAPI; stdcall;
  SNMP_oidappend: function (pOidDst: PAsnObjectIdentifier; pOidSrc: PAsnObjectIdentifier): SNMPAPI; stdcall;
  SNMP_oidncmp: function (pOid1, pOid2: PAsnObjectIdentifier; nSubIds: UINT): SNMPAPI; stdcall;
  SNMP_oidcmp: function (pOid1, pOid2: PAsnObjectIdentifier): SNMPAPI; stdcall;
  SNMP_oidfree: procedure (pOid: TAsnObjectIdentifier); stdcall;

  SNMP_CopyVarBind: function (pVbDst: PSnmpVarBind; pVbSrc: PSnmpVarBind): SNMPAPI; stdcall;
  SNMP_FreeVarBind: procedure (pVb: PSnmpVarBind); stdcall;
  SNMP_CopyVarBindList: function (pVblDst: PSnmpVarBindList; pVblSrc: PSnmpVarBindList): SNMPAPI; stdcall;
  SNMP_FreeVarBindList: procedure (pVbl: PSnmpVarBindList); stdcall;

  SNMP_printany: procedure (pAny: PAsnAny); stdcall;

  SNMP_free: procedure (pMem: Pointer); stdcall;
  SNMP_malloc: function (nBytes: UINT): Pointer; stdcall;
  SNMP_realloc: function (pMem: Pointer; nBytes: UINT): Pointer; stdcall;

  SNMP_DBG_free: procedure (pMem: Pointer); stdcall;
  SNMP_DBG_malloc: function (nBytes: UINT): Pointer; stdcall;
  SNMP_DBG_realloc: function (pMem: Pointer; nBytes: UINT): Pointer; stdcall;

{$ELSE}

function SNMP_oidcpy(pOidDst: PAsnObjectIdentifier; pOidSrc: PAsnObjectIdentifier): SNMPAPI; stdcall;
function SNMP_oidappend(pOidDst: PAsnObjectIdentifier; pOidSrc: PAsnObjectIdentifier): SNMPAPI; stdcall;
function SNMP_oidncmp(pOid1, pOid2: PAsnObjectIdentifier; nSubIds: UINT): SNMPAPI; stdcall;
function SNMP_oidcmp(pOid1, pOid2: PAsnObjectIdentifier): SNMPAPI; stdcall;
procedure SNMP_oidfree(pOid: TAsnObjectIdentifier); stdcall;

function SNMP_CopyVarBind(pVbDst: PSnmpVarBind; pVbSrc: PSnmpVarBind): SNMPAPI; stdcall;
procedure SNMP_FreeVarBind(pVb: PSnmpVarBind); stdcall;
function SNMP_CopyVarBindList(pVblDst: PSnmpVarBindList; pVblSrc: PSnmpVarBindList): SNMPAPI; stdcall;
procedure SNMP_FreeVarBindList(pVbl: PSnmpVarBindList); stdcall;

procedure SNMP_printany(pAny: PAsnAny); stdcall;

procedure SNMP_free(pMem: Pointer); stdcall;
function SNMP_malloc(nBytes: UINT): Pointer; stdcall;
function SNMP_realloc(pMem: Pointer; nBytes: UINT): Pointer; stdcall;

procedure SNMP_DBG_free(pMem: Pointer); stdcall;
function SNMP_DBG_malloc(nBytes: UINT): Pointer; stdcall;
function SNMP_DBG_realloc(pMem: Pointer; nBytes: UINT): Pointer; stdcall;

{$ENDIF SNMP_DYNAMIC_LINK}

{$EXTERNALSYM SNMP_oidcpy}
{$EXTERNALSYM SNMP_oidappend}
{$EXTERNALSYM SNMP_oidncmp}
{$EXTERNALSYM SNMP_oidcmp}
{$EXTERNALSYM SNMP_oidfree}

{$EXTERNALSYM SNMP_CopyVarBind}
{$EXTERNALSYM SNMP_FreeVarBind}
{$EXTERNALSYM SNMP_CopyVarBindList}
{$EXTERNALSYM SNMP_FreeVarBindList}

{$EXTERNALSYM SNMP_printany}

{$EXTERNALSYM SNMP_free}
{$EXTERNALSYM SNMP_malloc}
{$EXTERNALSYM SNMP_realloc}

{$EXTERNALSYM SNMP_DBG_free}
{$EXTERNALSYM SNMP_DBG_malloc}
{$EXTERNALSYM SNMP_DBG_realloc}

const
  ASN_RFC1155_IPADDRESS           = ASN_IPADDRESS;
  {$EXTERNALSYM ASN_RFC1155_IPADDRESS}
  ASN_RFC1155_COUNTER             = ASN_COUNTER32;
  {$EXTERNALSYM ASN_RFC1155_COUNTER}
  ASN_RFC1155_GAUGE               = ASN_GAUGE32;
  {$EXTERNALSYM ASN_RFC1155_GAUGE}
  ASN_RFC1155_TIMETICKS           = ASN_TIMETICKS;
  {$EXTERNALSYM ASN_RFC1155_TIMETICKS}
  ASN_RFC1155_OPAQUE              = ASN_OPAQUE;
  {$EXTERNALSYM ASN_RFC1155_OPAQUE}
  ASN_RFC1213_DISPSTRING          = ASN_OCTETSTRING;
  {$EXTERNALSYM ASN_RFC1213_DISPSTRING}

  ASN_RFC1157_GETREQUEST          = SNMP_PDU_GET;
  {$EXTERNALSYM ASN_RFC1157_GETREQUEST}
  ASN_RFC1157_GETNEXTREQUEST      = SNMP_PDU_GETNEXT;
  {$EXTERNALSYM ASN_RFC1157_GETNEXTREQUEST}
  ASN_RFC1157_GETRESPONSE         = SNMP_PDU_RESPONSE;
  {$EXTERNALSYM ASN_RFC1157_GETRESPONSE}
  ASN_RFC1157_SETREQUEST          = SNMP_PDU_SET;
  {$EXTERNALSYM ASN_RFC1157_SETREQUEST}
  ASN_RFC1157_TRAP                = SNMP_PDU_V1TRAP;
  {$EXTERNALSYM ASN_RFC1157_TRAP}

  ASN_CONTEXTSPECIFIC             = ASN_CONTEXT;
  {$EXTERNALSYM ASN_CONTEXTSPECIFIC}
  ASN_PRIMATIVE                   = ASN_PRIMITIVE;
  {$EXTERNALSYM ASN_PRIMATIVE}

type
  RFC1157VarBindList              = TSnmpVarBindList;
  {$EXTERNALSYM RFC1157VarBindList}
  RFC1157VarBind                  = TSnmpVarBind;
  {$EXTERNALSYM RFC1157VarBind}
  TAsnInteger                     = TAsnInteger32;
  {$EXTERNALSYM TAsnInteger}
  TAsnCounter                     = TAsnCounter32;
  {$EXTERNALSYM TAsnCounter}
  TAsnGauge                       = TAsnGauge32;
  {$EXTERNALSYM TAsnGauge}

{$ENDIF ~SNMPSTRICT}

{ SNMP Extension API Prototypes }

var
  SnmpExtensionInit: TSnmpExtensionInit;
  {$EXTERNALSYM SnmpExtensionInit}
  SnmpExtensionInitEx: TSnmpExtensionInitEx;
  {$EXTERNALSYM SnmpExtensionInitEx}
  SnmpExtensionMonitor: TSnmpExtensionMonitor;
  {$EXTERNALSYM SnmpExtensionMonitor}
  SnmpExtensionQuery: TSnmpExtensionQuery;
  {$EXTERNALSYM SnmpExtensionQuery}
  SnmpExtensionQueryEx: TSnmpExtensionQueryEx;
  {$EXTERNALSYM SnmpExtensionQueryEx}
  SnmpExtensionTrap: TSnmpExtensionTrap;
  {$EXTERNALSYM SnmpExtensionTrap}
  SnmpExtensionClose: TSnmpExtensionClose;
  {$EXTERNALSYM SnmpExtensionClose}

function SnmpExtensionLoaded: Boolean;
function LoadSnmpExtension(const LibName: string): Boolean;
function UnloadSnmpExtension: Boolean;

{$IFDEF SNMP_DYNAMIC_LINK}
function SnmpLoaded: Boolean;
{$IFDEF SNMP_DYNAMIC_LINK_EXPLICIT}
function LoadSnmp: Boolean;
function UnloadSnmp: Boolean;
{$ENDIF SNMP_DYNAMIC_LINK_EXPLICIT}
{$ENDIF SNMP_DYNAMIC_LINK}

implementation

const
  snmpapilib = 'snmpapi.dll';

var
  ExtensionLibHandle: THandle;

function SnmpExtensionLoaded: Boolean;
begin
  Result := ExtensionLibHandle <> 0;
end;

function LoadSnmpExtension(const LibName: string): Boolean;
begin
  Result := UnloadSnmpExtension;
  if Result then
  begin
    ExtensionLibHandle := SafeLoadLibrary(LibName);
    Result := SnmpExtensionLoaded;
    if Result then
    begin
      @SnmpExtensionInit := GetProcAddress(ExtensionLibHandle, 'SnmpExtensionInit');
      @SnmpExtensionInitEx := GetProcAddress(ExtensionLibHandle, 'SnmpExtensionInitEx');
      @SnmpExtensionMonitor := GetProcAddress(ExtensionLibHandle, 'SnmpExtensionMonitor');
      @SnmpExtensionQuery := GetProcAddress(ExtensionLibHandle, 'SnmpExtensionQuery');
      @SnmpExtensionQueryEx := GetProcAddress(ExtensionLibHandle, 'SnmpExtensionQueryEx');
      @SnmpExtensionTrap := GetProcAddress(ExtensionLibHandle, 'SnmpExtensionTrap');
      @SnmpExtensionClose := GetProcAddress(ExtensionLibHandle, 'SnmpExtensionClose');
      Result := Assigned(SnmpExtensionInit);
      if not Result then
        UnloadSnmpExtension;
    end;
  end;
end;

function UnloadSnmpExtension: Boolean;
begin
  if SnmpExtensionLoaded then
  begin
    Result := FreeLibrary(ExtensionLibHandle);
    ExtensionLibHandle := 0;
    @SnmpExtensionInit := nil;
    @SnmpExtensionInitEx := nil;
    @SnmpExtensionMonitor := nil;
    @SnmpExtensionQuery := nil;
    @SnmpExtensionQueryEx := nil;
    @SnmpExtensionTrap := nil;
    @SnmpExtensionClose := nil;
  end
  else
    Result := True;
end;

{$IFDEF SNMP_DYNAMIC_LINK}

var
  SnmpLibHandle: THandle;

function SnmpLoaded: Boolean;
begin
  Result := SnmpLibHandle <> 0;
end;

function UnloadSnmp: Boolean;
begin
  Result := True;
  if SnmpLoaded then
  begin
    Result := FreeLibrary(SnmpLibHandle);
    SnmpLibHandle := 0;
    @SnmpUtilOidCpy := nil;
    @SnmpUtilOidAppend := nil;
    @SnmpUtilOidNCmp := nil;
    @SnmpUtilOidCmp := nil;
    @SnmpUtilOidFree := nil;
    @SnmpUtilOctetsCmp := nil;
    @SnmpUtilOctetsNCmp := nil;
    @SnmpUtilOctetsCpy := nil;
    @SnmpUtilOctetsFree := nil;
    @SnmpUtilAsnAnyCpy := nil;
    @SnmpUtilAsnAnyFree := nil;
    @SnmpUtilVarBindCpy := nil;
    @SnmpUtilVarBindFree := nil;
    @SnmpUtilVarBindListCpy := nil;
    @SnmpUtilVarBindListFree := nil;
    @SnmpUtilMemFree := nil;
    @SnmpUtilMemAlloc := nil;
    @SnmpUtilMemReAlloc := nil;
    @SnmpUtilOidToA := nil;
    @SnmpUtilIdsToA := nil;
    @SnmpUtilPrintOid := nil;
    @SnmpUtilPrintAsnAny := nil;
    @SnmpSvcGetUptime := nil;
    @SnmpSvcSetLogLevel := nil;
    @SnmpSvcSetLogType := nil;
    @SnmpUtilDbgPrint := nil;
    {$IFNDEF SNMPSTRICT}
    @SNMP_oidcpy := nil;
    @SNMP_oidappend := nil;
    @SNMP_oidncmp := nil;
    @SNMP_oidcmp := nil;
    @SNMP_oidfree := nil;
    @SNMP_CopyVarBind := nil;
    @SNMP_FreeVarBind := nil;
    @SNMP_CopyVarBindList := nil;
    @SNMP_FreeVarBindList := nil;
    @SNMP_printany := nil;
    @SNMP_free := nil;
    @SNMP_malloc := nil;
    @SNMP_realloc := nil;
    @SNMP_DBG_free := nil;
    @SNMP_DBG_malloc := nil;
    @SNMP_DBG_realloc := nil;
   {$ENDIF ~SNMPSTRICT}
  end;
end;

function LoadSnmp: Boolean;
begin
  Result := SnmpLoaded;
  if not Result then
  begin
    SnmpLibHandle := SafeLoadLibrary(snmpapilib);
    if SnmpLoaded then
    begin
      @SnmpUtilOidCpy := GetProcAddress(SnmpLibHandle, 'SnmpUtilOidCpy');
      @SnmpUtilOidAppend := GetProcAddress(SnmpLibHandle, 'SnmpUtilOidAppend');
      @SnmpUtilOidNCmp := GetProcAddress(SnmpLibHandle, 'SnmpUtilOidNCmp');
      @SnmpUtilOidCmp := GetProcAddress(SnmpLibHandle, 'SnmpUtilOidCmp');
      @SnmpUtilOidFree := GetProcAddress(SnmpLibHandle, 'SnmpUtilOidFree');
      @SnmpUtilOctetsCmp := GetProcAddress(SnmpLibHandle, 'SnmpUtilOctetsCmp');
      @SnmpUtilOctetsNCmp := GetProcAddress(SnmpLibHandle, 'SnmpUtilOctetsNCmp');
      @SnmpUtilOctetsCpy := GetProcAddress(SnmpLibHandle, 'SnmpUtilOctetsCpy');
      @SnmpUtilOctetsFree := GetProcAddress(SnmpLibHandle, 'SnmpUtilOctetsFree');
      @SnmpUtilAsnAnyCpy := GetProcAddress(SnmpLibHandle, 'SnmpUtilAsnAnyCpy');
      @SnmpUtilAsnAnyFree := GetProcAddress(SnmpLibHandle, 'SnmpUtilAsnAnyFree');
      @SnmpUtilVarBindCpy := GetProcAddress(SnmpLibHandle, 'SnmpUtilVarBindCpy');
      @SnmpUtilVarBindFree := GetProcAddress(SnmpLibHandle, 'SnmpUtilVarBindFree');
      @SnmpUtilVarBindListCpy := GetProcAddress(SnmpLibHandle, 'SnmpUtilVarBindListCpy');
      @SnmpUtilVarBindListFree := GetProcAddress(SnmpLibHandle, 'SnmpUtilVarBindListFree');
      @SnmpUtilMemFree := GetProcAddress(SnmpLibHandle, 'SnmpUtilMemFree');
      @SnmpUtilMemAlloc := GetProcAddress(SnmpLibHandle, 'SnmpUtilMemAlloc');
      @SnmpUtilMemReAlloc := GetProcAddress(SnmpLibHandle, 'SnmpUtilMemReAlloc');
      @SnmpUtilOidToA := GetProcAddress(SnmpLibHandle, 'SnmpUtilOidToA');
      @SnmpUtilIdsToA := GetProcAddress(SnmpLibHandle, 'SnmpUtilIdsToA');
      @SnmpUtilPrintOid := GetProcAddress(SnmpLibHandle, 'SnmpUtilPrintOid');
      @SnmpUtilPrintAsnAny := GetProcAddress(SnmpLibHandle, 'SnmpUtilPrintAsnAny');
      @SnmpSvcGetUptime := GetProcAddress(SnmpLibHandle, 'SnmpSvcGetUptime');
      @SnmpSvcSetLogLevel := GetProcAddress(SnmpLibHandle, 'SnmpSvcSetLogLevel');
      @SnmpSvcSetLogType := GetProcAddress(SnmpLibHandle, 'SnmpSvcSetLogType');
      @SnmpUtilDbgPrint := GetProcAddress(SnmpLibHandle, 'SnmpUtilDbgPrint');
      {$IFNDEF SNMPSTRICT}
      @SNMP_oidcpy := GetProcAddress(SnmpLibHandle, 'SnmpUtilOidCpy');
      @SNMP_oidappend := GetProcAddress(SnmpLibHandle, 'SnmpUtilOidAppend');
      @SNMP_oidncmp := GetProcAddress(SnmpLibHandle, 'SnmpUtilOidNCmp');
      @SNMP_oidcmp := GetProcAddress(SnmpLibHandle, 'SnmpUtilOidCmp');
      @SNMP_oidfree := GetProcAddress(SnmpLibHandle, 'SnmpUtilOidFree');
      @SNMP_CopyVarBind := GetProcAddress(SnmpLibHandle, 'SnmpUtilVarBindCpy');
      @SNMP_FreeVarBind := GetProcAddress(SnmpLibHandle, 'SnmpUtilVarBindFree');
      @SNMP_CopyVarBindList := GetProcAddress(SnmpLibHandle, 'SnmpUtilVarBindListCpy');
      @SNMP_FreeVarBindList := GetProcAddress(SnmpLibHandle, 'SnmpUtilVarBindListFree');
      @SNMP_printany := GetProcAddress(SnmpLibHandle, 'SnmpUtilPrintAsnAny');
      @SNMP_free := GetProcAddress(SnmpLibHandle, 'SnmpUtilMemFree');
      @SNMP_malloc := GetProcAddress(SnmpLibHandle, 'SnmpUtilMemAlloc');
      @SNMP_realloc := GetProcAddress(SnmpLibHandle, 'SnmpUtilMemReAlloc');
      @SNMP_DBG_free := GetProcAddress(SnmpLibHandle, 'SnmpUtilMemFree');
      @SNMP_DBG_malloc := GetProcAddress(SnmpLibHandle, 'SnmpUtilMemAlloc');
      @SNMP_DBG_realloc := GetProcAddress(SnmpLibHandle, 'SnmpUtilMemReAlloc');
      {$ENDIF ~SNMPSTRICT}
      Result := True;
   end;
  end;
end;

{$ELSE}

function SnmpUtilOidCpy; external snmpapilib name 'SnmpUtilOidCpy';
function SnmpUtilOidAppend; external snmpapilib name 'SnmpUtilOidAppend';
function SnmpUtilOidNCmp; external snmpapilib name 'SnmpUtilOidNCmp';
function SnmpUtilOidCmp; external snmpapilib name 'SnmpUtilOidCmp';
procedure SnmpUtilOidFree; external snmpapilib name 'SnmpUtilOidFree';
function SnmpUtilOctetsCmp; external snmpapilib name 'SnmpUtilOctetsCmp';
function SnmpUtilOctetsNCmp; external snmpapilib name 'SnmpUtilOctetsNCmp';
function SnmpUtilOctetsCpy; external snmpapilib name 'SnmpUtilOctetsCpy';
procedure SnmpUtilOctetsFree; external snmpapilib name 'SnmpUtilOctetsFree';
function SnmpUtilAsnAnyCpy; external snmpapilib name 'SnmpUtilAsnAnyCpy';
procedure SnmpUtilAsnAnyFree; external snmpapilib name 'SnmpUtilAsnAnyFree';
function SnmpUtilVarBindCpy; external snmpapilib name 'SnmpUtilVarBindCpy';
procedure SnmpUtilVarBindFree; external snmpapilib name 'SnmpUtilVarBindFree';
function SnmpUtilVarBindListCpy; external snmpapilib name 'SnmpUtilVarBindListCpy';
procedure SnmpUtilVarBindListFree; external snmpapilib name 'SnmpUtilVarBindListFree';
procedure SnmpUtilMemFree; external snmpapilib name 'SnmpUtilMemFree';
function SnmpUtilMemAlloc; external snmpapilib name 'SnmpUtilMemAlloc';
function SnmpUtilMemReAlloc; external snmpapilib name 'SnmpUtilMemReAlloc';
function SnmpUtilOidToA; external snmpapilib name 'SnmpUtilOidToA';
function SnmpUtilIdsToA; external snmpapilib name 'SnmpUtilIdsToA';
procedure SnmpUtilPrintOid; external snmpapilib name 'SnmpUtilPrintOid';
procedure SnmpUtilPrintAsnAny; external snmpapilib name 'SnmpUtilPrintAsnAny';
function SnmpSvcGetUptime; external snmpapilib name 'SnmpSvcGetUptime';
procedure SnmpSvcSetLogLevel; external snmpapilib name 'SnmpSvcSetLogLevel';
procedure SnmpSvcSetLogType; external snmpapilib name 'SnmpSvcSetLogType';
procedure SnmpUtilDbgPrint; external snmpapilib name 'SnmpUtilDbgPrint';

{$IFNDEF SNMPSTRICT}
function SNMP_oidcpy; external snmpapilib name 'SnmpUtilOidCpy';
function SNMP_oidappend; external snmpapilib name 'SnmpUtilOidAppend';
function SNMP_oidncmp; external snmpapilib name 'SnmpUtilOidNCmp';
function SNMP_oidcmp; external snmpapilib name 'SnmpUtilOidCmp';
procedure SNMP_oidfree; external snmpapilib name 'SnmpUtilOidFree';
function SNMP_CopyVarBind; external snmpapilib name 'SnmpUtilVarBindCpy';
procedure SNMP_FreeVarBind; external snmpapilib name 'SnmpUtilVarBindFree';
function SNMP_CopyVarBindList; external snmpapilib name 'SnmpUtilVarBindListCpy';
procedure SNMP_FreeVarBindList; external snmpapilib name 'SnmpUtilVarBindListFree';
procedure SNMP_printany; external snmpapilib name 'SnmpUtilPrintAsnAny';
procedure SNMP_free; external snmpapilib name 'SnmpUtilMemFree';
function SNMP_malloc; external snmpapilib name 'SnmpUtilMemAlloc';
function SNMP_realloc; external snmpapilib name 'SnmpUtilMemReAlloc';
procedure SNMP_DBG_free; external snmpapilib name 'SnmpUtilMemFree';
function SNMP_DBG_malloc; external snmpapilib name 'SnmpUtilMemAlloc';
function SNMP_DBG_realloc; external snmpapilib name 'SnmpUtilMemReAlloc';
{$ENDIF ~SNMPSTRICT}

{$ENDIF SNMP_DYNAMIC_LINK}

{$IFDEF SNMP_DYNAMIC_LINK}
{$IFNDEF SNMP_DYNAMIC_LINK_EXPLICIT}

initialization
  LoadSnmp;

finalization
  UnloadSnmp;

{$ENDIF ~SNMP_DYNAMIC_LINK_EXPLICIT}
{$ENDIF SNMP_DYNAMIC_LINK}

end.
