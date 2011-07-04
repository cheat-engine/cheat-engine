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
{ The Original Code is JclCIL.pas.                                                                 }
{                                                                                                  }
{ The Initial Developer of the Original Code is Flier Lu (<flier_lu att yahoo dott com dott cn>).  }
{ Portions created by Flier Lu are Copyright (C) Flier Lu. All Rights Reserved.                    }
{                                                                                                  }
{ Contributors:                                                                                    }
{   Flier Lu (flier)                                                                               }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{   Olivier Sannier (obones)                                                                       }
{   Petr Vones (pvones)                                                                            }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Microsoft .Net CIL Instruction Set information support routines and classes.                     }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-02-09 21:11:06 +0100 (sam., 09 févr. 2008)                        $ }
{ Revision:      $Rev:: 2351                                                                     $ }
{ Author:        $Author:: marcovtje                                                             $ }
{                                                                                                  }
{**************************************************************************************************}

// Last modified: $Date: 2008-02-09 21:11:06 +0100 (sam., 09 févr. 2008) $

unit JclCIL;

interface

{$I jcl.inc}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  {$IFDEF MSWINDOWS}
  Windows, 
  {$ENDIF MSWINDOWS}
  Classes, SysUtils,
  {$IFDEF HAS_UNIT_CONTNRS}
  Contnrs,
  {$ENDIF HAS_UNIT_CONTNRS}
  JclBase, JclSysUtils, JclCLR, JclMetadata;

type
  TJclOpCode =
   (opNop, opBreak,
    opLdArg_0, opLdArg_1, opLdArg_2, opLdArg_3,
    opLdLoc_0, opLdLoc_1, opLdLoc_2, opLdLoc_3,
    opStLoc_0, opStLoc_1, opStLoc_2, opStLoc_3,
    opldArg_s, opLdArga_s, opStArg_s,
    opLdLoc_s, opLdLoca_s, opStLoc_s,
    opLdNull, opLdc_I4_M1,
    opLdc_I4_0, opLdc_I4_1, opLdc_I4_2, opLdc_I4_3, opLdc_I4_4,
    opLdc_I4_5, opLdc_I4_6, opLdc_I4_7, opLdc_I4_8, opLdc_I4_s,
    opLdc_i4, opLdc_i8, opLdc_r4, opLdc_r8,
    opUnused49,
    opDup, opPop, opJmp, opCall, opCalli, opRet,
    opBr_s, opBrFalse_s, opBrTrue_s,
    opBeq_s, opBge_s, opBgt_s, opBle_s, opBlt_s,
    opBne_un_s, opBge_un_s, opBgt_un_s, opBle_un_s, opBlt_un_s,
    opBr, opBrFalse, opBrTrue,
    opBeq, opBge, opBgt, opBle, opBlt,
    opBne_un, opBge_un, opBgt_un, opBle_un, opBlt_un,
    opSwitch,
    opLdInd_i1, opLdInd_i2, opLdInd_u1, opLdInd_u2,
    opLdInd_i4, opLdInd_u4, opLdInd_i8, opLdInd_i,
    opLdInd_r4, opLdInd_r8, opLdInd_ref, opStInd_ref,
    opStInd_i1, opStInd_i2, opStInd_i4, opStInd_i8,
    opStInd_r4, opStInd_r8,
    opAdd, opSub, opMul, opDiv, opDiv_un, opRem, opRem_un,
    opAnd, opOr, opXor, opShl, opShr, opShr_un, opNeg, opNot,
    opConv_i1, opConv_i2, opConv_i4, opConv_i8,
    opConv_r4, opConv_r8, opConv_u4, opConv_u8,
    opCallVirt, opCpObj, opLdObj, opLdStr, opNewObj,
    opCastClass, opIsInst, opConv_r_un,
    opUnused58, opUnused1,
    opUnbox, opThrow,
    opLdFld, opLdFlda, opStFld, opLdsFld, opLdsFlda, opStsFld, opStObj,
    opConv_ovf_i1_un, opConv_ovf_i2_un, opConv_ovf_i4_un, opConv_ovf_i8_un,
    opConv_ovf_u1_un, opConv_ovf_u2_un, opConv_ovf_u4_un, opConv_ovf_u8_un,
    opConv_ovf_i_un, opConv_ovf_u_un,
    opBox, opNewArr, opLdLen,
    opLdElema, opLdElem_i1, opLdElem_u1, opLdElem_i2, opLdElem_u2,
    opLdElem_i4, opLdElem_u4, opLdElem_i8, opLdElem_i,
    opLdElem_r4, opLdElem_r8, opLdElem_ref,
    opStElem_i, opStElem_i1, opStElem_i2, opStElem_i4, opStElem_i8,
    opStElem_r4, opStElem_r8, opStElem_ref,
    opUnused2, opUnused3, opUnused4, opUnused5,
    opUnused6, opUnused7, opUnused8, opUnused9,
    opUnused10, opUnused11, opUnused12, opUnused13,
    opUnused14, opUnused15, opUnused16, opUnused17,
    opConv_ovf_i1, opConv_ovf_u1, opConv_ovf_i2, opConv_ovf_u2,
    opConv_ovf_i4, opConv_ovf_u4, opConv_ovf_i8, opConv_ovf_u8,
    opUnused50, opUnused18, opUnused19, opUnused20,
    opUnused21, opUnused22, opUnused23,
    opRefAnyVal, opCkFinite,
    opUnused24, opUnused25,
    opMkRefAny,
    opUnused59, opUnused60, opUnused61, opUnused62, opUnused63,
    opUnused64, opUnused65, opUnused66, opUnused67,
    opLdToken,
    opConv_u2, opConv_u1, opConv_i, opConv_ovf_i, opConv_ovf_u,
    opAdd_ovf, opAdd_ovf_un, opMul_ovf, opMul_ovf_un, opSub_ovf, opSub_ovf_un,
    opEndFinally, opLeave, opLeave_s, opStInd_i, opConv_u,
    opUnused26, opUnused27, opUnused28, opUnused29, opUnused30,
    opUnused31, opUnused32, opUnused33, opUnused34, opUnused35,
    opUnused36, opUnused37, opUnused38, opUnused39, opUnused40,
    opUnused41, opUnused42, opUnused43, opUnused44, opUnused45,
    opUnused46, opUnused47, opUnused48,
    opPrefix7, opPrefix6, opPrefix5, opPrefix4,
    opPrefix3, opPrefix2, opPrefix1, opPrefixRef,

    opArgLlist, opCeq, opCgt, opCgt_un, opClt, opClt_un,
    opLdFtn, opLdVirtFtn, optUnused56,
    opLdArg, opLdArga, opStArg, opLdLoc, opLdLoca, opStLoc,
    opLocalLoc, opUnused57, opEndFilter, opUnaligned, opVolatile,
    opTail, opInitObj, opUnused68, opCpBlk, opInitBlk, opUnused69,
    opRethrow, opUnused51, opSizeOf, opRefAnyType,
    opUnused52, opUnused53, opUnused54, opUnused55, opUnused70);

  TJclInstructionDumpILOption =
    (doLineNo, doRawBytes, doIL, doTokenValue, doComment);
  TJclInstructionDumpILOptions = set of TJclInstructionDumpILOption;

  TJclInstructionParamType =
   (ptVoid, ptI1, ptI2, ptI4, ptI8, ptU1, ptU2, ptU4, ptU8, ptR4, ptR8,
    ptToken, ptSOff, ptLOff, ptArray);

const
  InstructionDumpILAllOption =
    [doLineNo, doRawBytes, doIL, doTokenValue, doComment];

type
  TJclClrILGenerator = class;

  TJclInstruction = class(TObject)
  private
    FOpCode: TJclOpCode;
    FOffset: DWORD;
    FParam: Variant;
    FOwner: TJclClrILGenerator;
    function GetWideOpCode: Boolean;
    function GetRealOpCode: Byte;
    function GetName: string;
    function GetFullName: string;
    function GetDescription: string;
    function GetParamType: TJclInstructionParamType;
    function FormatLabel(Offset: Integer): string;
  protected
    function GetSize: DWORD; virtual;
    function DumpILOption(Option: TJclInstructionDumpILOption): string; virtual;
  public
    constructor Create(AOwner: TJclClrILGenerator; AOpCode: TJclOpCode);
    procedure Load(Stream: TStream); virtual;
    procedure Save(Stream: TStream); virtual;
    function DumpIL(Options: TJclInstructionDumpILOptions = [doIL]): string;
    property Owner: TJclClrILGenerator read FOwner;
    property OpCode: TJclOpCode read FOpCode;
    property WideOpCode: Boolean read GetWideOpCode;
    property RealOpCode: Byte read GetRealOpCode;
    property Param: Variant read FParam write FParam;
    property ParamType: TJclInstructionParamType read GetParamType;
    property Name: string read GetName;
    property FullName: string read GetFullName;
    property Description: string read GetDescription;
    property Size: DWORD read GetSize;
    property Offset: DWORD read FOffset;
  end;

  TJclUnaryInstruction = class(TJclInstruction);

  TJclBinaryInstruction = class(TJclInstruction);

  TJclClrILGenerator = class(TObject)
  private
    FMethod: TJclClrMethodBody;
    FInstructions: TObjectList;
    function GetInstructionCount: Integer;
    function GetInstruction(const Idx: Integer): TJclInstruction;
  public
    constructor Create(AMethod: TJclClrMethodBody = nil);
    destructor Destroy; override;
    function DumpIL(Options: TJclInstructionDumpILOptions): string;
    property Method: TJclClrMethodBody read FMethod;
    property Instructions[const Idx: Integer]: TJclInstruction read GetInstruction;
    property InstructionCount: Integer read GetInstructionCount;
  end;

  EJclCliInstructionError = class(EJclError);
  EJclCliInstructionStreamInvalid = class(EJclCliInstructionError);

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/windows/JclCIL.pas $';
    Revision: '$Revision: 2351 $';
    Date: '$Date: 2008-02-09 21:11:06 +0100 (sam., 09 févr. 2008) $';
    LogPath: 'JCL\source\windows'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  {$IFDEF HAS_UNIT_VARIANTS}
  Variants,
  {$ENDIF HAS_UNIT_VARIANTS}
  JclStrings, JclResources;

type
  TJclOpCodeInfoType = (itName, itFullName, itDescription);

const
  STP1 = $FE;

  OpCodeInfos: array [TJclOpCode, TJclOpCodeInfoType] of string =
   (
    ('nop',            RsCILCmdnop,         RsCILDescrnop),
    ('break',          RsCILCmdbreak,       RsCILDescrbreak),
    ('ldarg.0',        RsCILCmdldarg0,      RsCILDescrldarg0),
    ('ldarg.1',        RsCILCmdldarg1,      RsCILDescrldarg1),
    ('ldarg.2',        RsCILCmdldarg2,      RsCILDescrldarg2),
    ('ldarg.3',        RsCILCmdldarg3,      RsCILDescrldarg3),
    ('ldloc.0',        RsCILCmdldloc0,      RsCILDescrldloc0),
    ('ldloc.1',        RsCILCmdldloc1,      RsCILDescrldloc1),
    ('ldloc.2',        RsCILCmdldloc2,      RsCILDescrldloc2),
    ('ldloc.3',        RsCILCmdldloc3,      RsCILDescrldloc3),
    ('stloc.0',        RsCILCmdstloc0,      RsCILDescrstloc0),
    ('stloc.1',        RsCILCmdstloc1,      RsCILDescrstloc1),
    ('stloc.2',        RsCILCmdstloc2,      RsCILDescrstloc2),
    ('stloc.3',        RsCILCmdstloc3,      RsCILDescrstloc3),
    ('ldarg.s',        RsCILCmdldargs,      RsCILDescrldargs),
    ('ldarga.s',       RsCILCmdldargas,     RsCILDescrldargas),
    ('starg.s',        RsCILCmdstargs,      RsCILDescrstargs),
    ('ldloc.s',        RsCILCmdldlocs,      RsCILDescrldlocs),
    ('ldloca.s',       RsCILCmdldlocas,     RsCILDescrldlocas),
    ('stloc.s',        RsCILCmdstlocs,      RsCILDescrstlocs),
    ('ldnull',         RsCILCmdldnull,      RsCILDescrldnull),
    ('ldc.i4.m1',      RsCILCmdldci4m1,     RsCILDescrldci4m1),
    ('ldc.i4.0',       RsCILCmdldci40,      RsCILDescrldci40),
    ('ldc.i4.1',       RsCILCmdldci41,      RsCILDescrldci41),
    ('ldc.i4.2',       RsCILCmdldci42,      RsCILDescrldci42),
    ('ldc.i4.3',       RsCILCmdldci43,      RsCILDescrldci43),
    ('ldc.i4.4',       RsCILCmdldci44,      RsCILDescrldci44),
    ('ldc.i4.5',       RsCILCmdldci45,      RsCILDescrldci45),
    ('ldc.i4.6',       RsCILCmdldci46,      RsCILDescrldci46),
    ('ldc.i4.7',       RsCILCmdldci47,      RsCILDescrldci47),
    ('ldc.i4.8',       RsCILCmdldci48,      RsCILDescrldci48),
    ('ldc.i4.s',       RsCILCmdldci4s,      RsCILDescrldci4s),
    ('ldc.i4',         RsCILCmdldci4,       RsCILDescrldci4),
    ('ldc.i8',         RsCILCmdldci8,       RsCILDescrldci8),
    ('ldc.r4',         RsCILCmdldcr4,       RsCILDescrldcr4),
    ('ldc.r8',         RsCILCmdldcr8,       RsCILDescrldcr8),
    ('unused',         RsCILCmdunused1,     RsCILDescrunused1),
    ('dup',            RsCILCmddup,         RsCILDescrdup),
    ('pop',            RsCILCmdpop,         RsCILDescrpop),
    ('jmp',            RsCILCmdjmp,         RsCILDescrjmp),
    ('call',           RsCILCmdcall,        RsCILDescrcall),
    ('calli',          RsCILCmdcalli,       RsCILDescrcalli),
    ('ret',            RsCILCmdret,         RsCILDescrret),
    ('br.s',           RsCILCmdbrs,         RsCILDescrbrs),
    ('brfalse.s',      RsCILCmdbrfalses,    RsCILDescrbrfalses),
    ('brtrue.s',       RsCILCmdbrtrues,     RsCILDescrbrtrues),
    ('beq.s',          RsCILCmdbeqs,        RsCILDescrbeqs),
    ('bge.s',          RsCILCmdbges,        RsCILDescrbges),
    ('bgt.s',          RsCILCmdbgts,        RsCILDescrbgts),
    ('ble.s',          RsCILCmdbles,        RsCILDescrbles),
    ('blt.s',          RsCILCmdblts,        RsCILDescrblts),
    ('bne.un.s',       RsCILCmdbneuns,      RsCILDescrbneuns),
    ('bge.un.s',       RsCILCmdbgeuns,      RsCILDescrbgeuns),
    ('bgt.un.s',       RsCILCmdbgtuns,      RsCILDescrbgtuns),
    ('ble.un.s',       RsCILCmdbleuns,      RsCILDescrbleuns),
    ('blt.un.s',       RsCILCmdbltuns,      RsCILDescrbltuns),
    ('br',             RsCILCmdbr,          RsCILDescrbr),
    ('brfalse',        RsCILCmdbrfalse,     RsCILDescrbrfalse),
    ('brtrue',         RsCILCmdbrtrue,      RsCILDescrbrtrue),
    ('beq',            RsCILCmdbeq,         RsCILDescrbeq),
    ('bge',            RsCILCmdbge,         RsCILDescrbge),
    ('bgt',            RsCILCmdbgt,         RsCILDescrbgt),
    ('ble',            RsCILCmdble,         RsCILDescrble),
    ('blt',            RsCILCmdblt,         RsCILDescrblt),
    ('bne.un',         RsCILCmdbneun,       RsCILDescrbneun),
    ('bge.un',         RsCILCmdbgeun,       RsCILDescrbgeun),
    ('bgt.un',         RsCILCmdbgtun,       RsCILDescrbgtun),
    ('ble.un',         RsCILCmdbleun,       RsCILDescrbleun),
    ('blt.un',         RsCILCmdbltun,       RsCILDescrbltun),
    ('switch',         RsCILCmdswitch,      RsCILDescrswitch),
    ('ldind.i1',       RsCILCmdldindi1,     RsCILDescrldindi1),
    ('ldind.u1',       RsCILCmdldindu1,     RsCILDescrldindu1),
    ('ldind.i2',       RsCILCmdldindi2,     RsCILDescrldindi2),
    ('ldind.u2',       RsCILCmdldindu2,     RsCILDescrldindu2),
    ('ldind.i4',       RsCILCmdldindi4,     RsCILDescrldindi4),
    ('ldind.u4',       RsCILCmdldindu4,     RsCILDescrldindu4),
    ('ldind.i8',       RsCILCmdldindi8,     RsCILDescrldindi8),
    ('ldind.i',        RsCILCmdldindi,      RsCILDescrldindi),
    ('ldind.r4',       RsCILCmdldindr4,     RsCILDescrldindr4),
    ('ldind.r8',       RsCILCmdldindr8,     RsCILDescrldindr8),
    ('ldind.ref',      RsCILCmdldindref,    RsCILDescrldindref),
    ('stind.ref',      RsCILCmdstindref,    RsCILDescrstindref),
    ('stind.i1',       RsCILCmdstindi1,     RsCILDescrstindi1),
    ('stind.i2',       RsCILCmdstindi2,     RsCILDescrstindi2),
    ('stind.i4',       RsCILCmdstindi4,     RsCILDescrstindi4),
    ('stind.i8',       RsCILCmdstindi8,     RsCILDescrstindi8),
    ('stind.r4',       RsCILCmdstindr4,     RsCILDescrstindr4),
    ('stind.r8',       RsCILCmdstindr8,     RsCILDescrstindr8),
    ('add',            RsCILCmdadd,         RsCILDescradd),
    ('sub',            RsCILCmdsub,         RsCILDescrsub),
    ('mul',            RsCILCmdmul,         RsCILDescrmul),
    ('div',            RsCILCmddiv,         RsCILDescrdiv),
    ('div.un',         RsCILCmddivun,       RsCILDescrdivun),
    ('rem',            RsCILCmdrem,         RsCILDescrrem),
    ('rem.un',         RsCILCmdremun,       RsCILDescrremun),
    ('and',            RsCILCmdand,         RsCILDescrand),
    ('or',             RsCILCmdor,          RsCILDescror),
    ('xor',            RsCILCmdxor,         RsCILDescrxor),
    ('shl',            RsCILCmdshl,         RsCILDescrshl),
    ('shr',            RsCILCmdshr,         RsCILDescrshr),
    ('shr.un',         RsCILCmdshrun,       RsCILDescrshrun),
    ('neg',            RsCILCmdneg,         RsCILDescrneg),
    ('not',            RsCILCmdnot,         RsCILDescrnot),
    ('conv.i1',        RsCILCmdconvi1,      RsCILDescrconvi1),
    ('conv.i2',        RsCILCmdconvi2,      RsCILDescrconvi2),
    ('conv.i4',        RsCILCmdconvi4,      RsCILDescrconvi4),
    ('conv.i8',        RsCILCmdconvi8,      RsCILDescrconvi8),
    ('conv.r4',        RsCILCmdconvr4,      RsCILDescrconvr4),
    ('conv.r8',        RsCILCmdconvr8,      RsCILDescrconvr8),
    ('conv.u4',        RsCILCmdconvu4,      RsCILDescrconvu4),
    ('conv.u8',        RsCILCmdconvu8,      RsCILDescrconvu8),
    ('callvirt',       RsCILCmdcallvirt,    RsCILDescrcallvirt),
    ('cpobj',          RsCILCmdcpobj,       RsCILDescrcpobj),
    ('ldobj',          RsCILCmdldobj,       RsCILDescrldobj),
    ('ldstr',          RsCILCmdldstr,       RsCILDescrldstr),
    ('newobj',         RsCILCmdnewobj,      RsCILDescrnewobj),
    ('castclass',      RsCILCmdcastclass,   RsCILDescrcastclass),
    ('isinst',         RsCILCmdisinst,      RsCILDescrisinst),
    ('conv.r.un',      RsCILCmdconvrun,     RsCILDescrconvrun),
    ('unused',         RsCILCmdunused2,     RsCILDescrunused2),
    ('unused',         RsCILCmdunused3,     RsCILDescrunused3),
    ('unbox',          RsCILCmdunbox,       RsCILDescrunbox),
    ('throw',          RsCILCmdthrow,       RsCILDescrthrow),
    ('ldfld',          RsCILCmdldfld,       RsCILDescrldfld),
    ('ldflda',         RsCILCmdldflda,      RsCILDescrldflda),
    ('stfld',          RsCILCmdstfld,       RsCILDescrstfld),
    ('ldsfld',         RsCILCmdldsfld,      RsCILDescrldsfld),
    ('ldsflda',        RsCILCmdldsflda,     RsCILDescrldsflda),
    ('stsfld',         RsCILCmdstsfld,      RsCILDescrstsfld),
    ('stobj',          RsCILCmdstobj,       RsCILDescrstobj),
    ('conv.ovf.i1.un', RsCILCmdconvovfi1un, RsCILDescrconvovfi1un),
    ('conv.ovf.i2.un', RsCILCmdconvovfi2un, RsCILDescrconvovfi2un),
    ('conv.ovf.i4.un', RsCILCmdconvovfi4un, RsCILDescrconvovfi4un),
    ('conv.ovf.i8.un', RsCILCmdconvovfi8un, RsCILDescrconvovfi8un),
    ('conv.ovf.u1.un', RsCILCmdconvovfu1un, RsCILDescrconvovfu1un),
    ('conv.ovf.u2.un', RsCILCmdconvovfu2un, RsCILDescrconvovfu2un),
    ('conv.ovf.u4.un', RsCILCmdconvovfu4un, RsCILDescrconvovfu4un),
    ('conv.ovf.u8.un', RsCILCmdconvovfu8un, RsCILDescrconvovfu8un),
    ('conv.ovf.i.un',  RsCILCmdconvovfiun,  RsCILDescrconvovfiun),
    ('conv.ovf.u.un',  RsCILCmdconvovfuun,  RsCILDescrconvovfuun),
    ('box',            RsCILCmdbox,         RsCILDescrbox),
    ('newarr',         RsCILCmdnewarr,      RsCILDescrnewarr),
    ('ldlen',          RsCILCmdldlen,       RsCILDescrldlen),
    ('ldelema',        RsCILCmdldelema,     RsCILDescrldelema),
    ('ldelem.i1',      RsCILCmdldelemi1,    RsCILDescrldelemi1),
    ('ldelem.u1',      RsCILCmdldelemu1,    RsCILDescrldelemu1),
    ('ldelem.i2',      RsCILCmdldelemi2,    RsCILDescrldelemi2),
    ('ldelem.u2',      RsCILCmdldelemu2,    RsCILDescrldelemu2),
    ('ldelem.i4',      RsCILCmdldelemi4,    RsCILDescrldelemi4),
    ('ldelem.u4',      RsCILCmdldelemu4,    RsCILDescrldelemu4),
    ('ldelem.i8',      RsCILCmdldelemi8,    RsCILDescrldelemi8),
    ('ldelem.i',       RsCILCmdldelemi,     RsCILDescrldelemi),
    ('ldelem.r4',      RsCILCmdldelemr4,    RsCILDescrldelemr4),
    ('ldelem.r8',      RsCILCmdldelemr8,    RsCILDescrldelemr8),
    ('ldelem.ref',     RsCILCmdldelemref,   RsCILDescrldelemref),
    ('stelem.i',       RsCILCmdstelemi,     RsCILDescrstelemi),
    ('stelem.i1',      RsCILCmdstelemi1,    RsCILDescrstelemi1),
    ('stelem.i2',      RsCILCmdstelemi2,    RsCILDescrstelemi2),
    ('stelem.i4',      RsCILCmdstelemi4,    RsCILDescrstelemi4),
    ('stelem.i8',      RsCILCmdstelemi8,    RsCILDescrstelemi8),
    ('stelem.r4',      RsCILCmdstelemr4,    RsCILDescrstelemr4),
    ('stelem.r8',      RsCILCmdstelemr8,    RsCILDescrstelemr8),
    ('stelem.ref',     RsCILCmdstelemref,   RsCILDescrstelemref),
    ('unused',         RsCILCmdunused4,     RsCILDescrunused4),
    ('unused',         RsCILCmdunused5,     RsCILDescrunused5),
    ('unused',         RsCILCmdunused6,     RsCILDescrunused6),
    ('unused',         RsCILCmdunused7,     RsCILDescrunused7),
    ('unused',         RsCILCmdunused8,     RsCILDescrunused8),
    ('unused',         RsCILCmdunused9,     RsCILDescrunused9),
    ('unused',         RsCILCmdunused10,    RsCILDescrunused10),
    ('unused',         RsCILCmdunused11,    RsCILDescrunused11),
    ('unused',         RsCILCmdunused12,    RsCILDescrunused12),
    ('unused',         RsCILCmdunused13,    RsCILDescrunused13),
    ('unused',         RsCILCmdunused14,    RsCILDescrunused14),
    ('unused',         RsCILCmdunused15,    RsCILDescrunused15),
    ('unused',         RsCILCmdunused16,    RsCILDescrunused16),
    ('unused',         RsCILCmdunused17,    RsCILDescrunused17),
    ('unused',         RsCILCmdunused18,    RsCILDescrunused18),
    ('unused',         RsCILCmdunused19,    RsCILDescrunused19),
    ('conv.ovf.i1',    RsCILCmdconvovfi1,   RsCILDescrconvovfi1),
    ('conv.ovf.u1',    RsCILCmdconvovfu1,   RsCILDescrconvovfu1),
    ('conv.ovf.i2',    RsCILCmdconvovfi2,   RsCILDescrconvovfi2),
    ('conv.ovf.u2',    RsCILCmdconvovfu2,   RsCILDescrconvovfu2),
    ('conv.ovf.i4',    RsCILCmdconvovfi4,   RsCILDescrconvovfi4),
    ('conv.ovf.u4',    RsCILCmdconvovfu4,   RsCILDescrconvovfu4),
    ('conv.ovf.i8',    RsCILCmdconvovfi8,   RsCILDescrconvovfi8),
    ('conv.ovf.u8',    RsCILCmdconvovfu8,   RsCILDescrconvovfu8),
    ('unused',         RsCILCmdunused20,    RsCILDescrunused20),
    ('unused',         RsCILCmdunused21,    RsCILDescrunused21),
    ('unused',         RsCILCmdunused22,    RsCILDescrunused22),
    ('unused',         RsCILCmdunused23,    RsCILDescrunused23),
    ('unused',         RsCILCmdunused24,    RsCILDescrunused24),
    ('unused',         RsCILCmdunused25,    RsCILDescrunused25),
    ('unused',         RsCILCmdunused26,    RsCILDescrunused26),
    ('refanyval',      RsCILCmdrefanyval,   RsCILDescrrefanyval),
    ('ckfinite',       RsCILCmdckfinite,    RsCILDescrckfinite),
    ('unused',         RsCILCmdunused27,    RsCILDescrunused27),
    ('unused',         RsCILCmdunused28,    RsCILDescrunused28),
    ('mkrefany',       RsCILCmdmkrefany,    RsCILDescrmkrefany),
    ('unused',         RsCILCmdunused29,    RsCILDescrunused29),
    ('unused',         RsCILCmdunused30,    RsCILDescrunused30),
    ('unused',         RsCILCmdunused31,    RsCILDescrunused31),
    ('unused',         RsCILCmdunused32,    RsCILDescrunused32),
    ('unused',         RsCILCmdunused33,    RsCILDescrunused33),
    ('unused',         RsCILCmdunused34,    RsCILDescrunused34),
    ('unused',         RsCILCmdunused35,    RsCILDescrunused35),
    ('unused',         RsCILCmdunused36,    RsCILDescrunused36),
    ('unused',         RsCILCmdunused37,    RsCILDescrunused37),
    ('ldtoken',        RsCILCmdldtoken,     RsCILDescrldtoken),
    ('conv.u2',        RsCILCmdconvu2,      RsCILDescrconvu2),
    ('conv.u1',        RsCILCmdconvu1,      RsCILDescrconvu1),
    ('conv.i',         RsCILCmdconvi,       RsCILDescrconvi),
    ('conv.ovf.i',     RsCILCmdconvovfi,    RsCILDescrconvovfi),
    ('conv.ovf.u',     RsCILCmdconvovfu,    RsCILDescrconvovfu),
    ('add.ovf',        RsCILCmdaddovf,      RsCILDescraddovf),
    ('add.ovf.un',     RsCILCmdaddovfun,    RsCILDescraddovfun),
    ('mul.ovf',        RsCILCmdmulovf,      RsCILDescrmulovf),
    ('mul.ovf.un',     RsCILCmdmulovfun,    RsCILDescrmulovfun),
    ('sub.ovf',        RsCILCmdsubovf,      RsCILDescrsubovf),
    ('sub.ovf.un',     RsCILCmdsubovfun,    RsCILDescrsubovfun),
    ('endfinally',     RsCILCmdendfinally,  RsCILDescrendfinally),
    ('leave',          RsCILCmdleave,       RsCILDescrleave),
    ('leave.s',        RsCILCmdleaves,      RsCILDescrleaves),
    ('stind.i',        RsCILCmdstindi,      RsCILDescrstindi),
    ('conv.u',         RsCILCmdconvu,       RsCILDescrconvu),
    ('unused',         RsCILCmdunused38,    RsCILDescrunused38),
    ('unused',         RsCILCmdunused39,    RsCILDescrunused39),
    ('unused',         RsCILCmdunused40,    RsCILDescrunused40),
    ('unused',         RsCILCmdunused41,    RsCILDescrunused41),
    ('unused',         RsCILCmdunused42,    RsCILDescrunused42),
    ('unused',         RsCILCmdunused43,    RsCILDescrunused43),
    ('unused',         RsCILCmdunused44,    RsCILDescrunused44),
    ('unused',         RsCILCmdunused45,    RsCILDescrunused45),
    ('unused',         RsCILCmdunused46,    RsCILDescrunused46),
    ('unused',         RsCILCmdunused47,    RsCILDescrunused47),
    ('unused',         RsCILCmdunused48,    RsCILDescrunused48),
    ('unused',         RsCILCmdunused49,    RsCILDescrunused49),
    ('unused',         RsCILCmdunused50,    RsCILDescrunused50),
    ('unused',         RsCILCmdunused51,    RsCILDescrunused51),
    ('unused',         RsCILCmdunused52,    RsCILDescrunused52),
    ('unused',         RsCILCmdunused53,    RsCILDescrunused53),
    ('unused',         RsCILCmdunused54,    RsCILDescrunused54),
    ('unused',         RsCILCmdunused55,    RsCILDescrunused55),
    ('unused',         RsCILCmdunused56,    RsCILDescrunused56),
    ('unused',         RsCILCmdunused57,    RsCILDescrunused57),
    ('unused',         RsCILCmdunused58,    RsCILDescrunused58),
    ('unused',         RsCILCmdunused59,    RsCILDescrunused59),
    ('unused',         RsCILCmdunused60,    RsCILDescrunused60),
    ('prefix7',        RsCILCmdprefix7,     RsCILDescrprefix7),
    ('prefix6',        RsCILCmdprefix6,     RsCILDescrprefix6),
    ('prefix5',        RsCILCmdprefix5,     RsCILDescrprefix5),
    ('prefix4',        RsCILCmdprefix4,     RsCILDescrprefix4),
    ('prefix3',        RsCILCmdprefix3,     RsCILDescrprefix3),
    ('prefix2',        RsCILCmdprefix2,     RsCILDescrprefix2),
    ('prefix1',        RsCILCmdprefix1,     RsCILDescrprefix1),
    ('prefixref',      RsCILCmdprefixref,   RsCILDescrprefixref),

    ('arglist',        RsCILCmdarglist,     RsCILDescrarglist),
    ('ceq',            RsCILCmdceq,         RsCILDescrceq),
    ('cgt',            RsCILCmdcgt,         RsCILDescrcgt),
    ('cgt.un',         RsCILCmdcgtun,       RsCILDescrcgtun),
    ('clt',            RsCILCmdclt,         RsCILDescrclt),
    ('clt.un',         RsCILCmdcltun,       RsCILDescrcltun),
    ('ldftn',          RsCILCmdldftn,       RsCILDescrldftn),
    ('ldvirtftn',      RsCILCmdldvirtftn,   RsCILDescrldvirtftn),
    ('unused',         RsCILCmdunused61,    RsCILDescrunused61),
    ('ldarg',          RsCILCmdldarg,       RsCILDescrldarg),
    ('ldarga',         RsCILCmdldarga,      RsCILDescrldarga),
    ('starg',          RsCILCmdstarg,       RsCILDescrstarg),
    ('ldloc',          RsCILCmdldloc,       RsCILDescrldloc),
    ('ldloca',         RsCILCmdldloca,      RsCILDescrldloca),
    ('stloc',          RsCILCmdstloc,       RsCILDescrstloc),
    ('localloc',       RsCILCmdlocalloc,    RsCILDescrlocalloc),
    ('unused',         RsCILCmdunused62,    RsCILDescrunused62),
    ('endfilter',      RsCILCmdendfilter,   RsCILDescrendfilter),
    ('unaligned.',     RsCILCmdunaligned,   RsCILDescrunaligned),
    ('volatile.',      RsCILCmdvolatile,    RsCILDescrvolatile),
    ('tail.',          RsCILCmdtail,        RsCILDescrtail),
    ('initobj',        RsCILCmdinitobj,     RsCILDescrinitobj),
    ('unused',         RsCILCmdunused63,    RsCILDescrunused63),
    ('cpblk',          RsCILCmdcpblk,       RsCILDescrcpblk),
    ('initblk',        RsCILCmdinitblk,     RsCILDescrinitblk),
    ('unused',         RsCILCmdunused64,    RsCILDescrunused64),
    ('rethrow',        RsCILCmdrethrow,     RsCILDescrrethrow),
    ('unused',         RsCILCmdunused65,    RsCILDescrunused65),
    ('sizeof',         RsCILCmdsizeof,      RsCILDescrsizeof),
    ('refanytype',     RsCILCmdrefanytype,  RsCILDescrrefanytype),
    ('unused',         RsCILCmdunused66,    RsCILDescrunused66),
    ('unused',         RsCILCmdunused67,    RsCILDescrunused67),
    ('unused',         RsCILCmdunused68,    RsCILDescrunused68),
    ('unused',         RsCILCmdunused69,    RsCILDescrunused69),
    ('unused',         RsCILCmdunused70,    RsCILDescrunused70)
   );

  OpCodeParamTypes: array [TJclOpCode] of TJclInstructionParamType =
   (ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {00}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptU1,     ptU1,    {08}
    ptU1,     ptU1,     ptU1,     ptU1,     ptVoid,   ptVoid,   ptVoid,   ptVoid,  {10}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptI1,    {18}
    ptI4,     ptI8,     ptR4,     ptR8,     ptVoid,   ptVoid,   ptVoid,   ptToken, {20}
    ptToken,  ptVoid,   ptVoid,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,  {28}
    ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,   ptSOff,  {30}
    ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,  {38}
    ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptLOff,   ptVoid,   ptVoid,   ptVoid,  {40}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {48}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {50}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {58}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {60}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken, {68}
    ptToken,  ptToken,  ptToken,  ptToken,  ptToken,  ptToken,  ptVoid,   ptVoid,  {70}
    ptVoid,   ptToken,  ptVoid,   ptToken,  ptToken,  ptToken,  ptToken,  ptToken, {78}
    ptToken,  ptToken,  ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {80}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken,  ptToken,  ptVoid,   ptToken, {88}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {90}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {98}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {A0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {A8}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {B0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {B8}
    ptVoid,   ptVoid,   ptToken,  ptVoid,   ptVoid,   ptVoid,   ptToken,   ptVoid, {C0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {C8}
    ptToken,  ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {D0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptI4,     ptI1,     ptVoid,  {D8}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {E0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {E8}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {F0}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,  {F8}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken,  ptToken, {00}
    ptVoid,   ptU2,     ptU2,     ptU2,     ptU2,     ptU2,     ptU2,     ptVoid,  {08}
    ptVoid,   ptVoid,   ptI1,     ptVoid,   ptVoid,   ptToken,  ptVoid,   ptVoid,  {10}
    ptVoid,   ptVoid,   ptVoid,   ptVoid,   ptToken,  ptVoid,   ptVoid,   ptVoid,  {18}
    ptVoid,   ptVoid,   ptVoid);                                                   {20}

//===  { TJclClrILGenerator } ================================================

constructor TJclClrILGenerator.Create(AMethod: TJclClrMethodBody = nil);
var
  OpCode: Byte;
  Stream: TMemoryStream;
  Instruction: TJclInstruction;
begin
  inherited Create;
  FMethod := AMethod;
  FInstructions := TObjectList.Create;
  if Assigned(AMethod) then
  begin
    Stream := TMemoryStream.Create;
    try
      Stream.Write(Method.Code^, Method.Size);
      Stream.Seek(0, soFromBeginning);
      while Stream.Position < Stream.Size do
      begin
        OpCode := PByte(Longint(Stream.Memory) + Stream.Position)^;
        if OpCode = STP1 then
        begin
          OpCode := PByte(Longint(Stream.Memory) + Stream.Position + 1)^;
          Instruction := TJclInstruction.Create(Self, TJclOpCode(MaxByte + 1 + OpCode));
        end
        else
          Instruction := TJclInstruction.Create(Self, TJclOpCode(OpCode));
        if Assigned(Instruction) then
        begin
          FInstructions.Add(Instruction);
          Instruction.Load(Stream);
        end;
      end;
    finally
      FreeAndNil(Stream);
    end;
  end;
end;

destructor TJclClrILGenerator.Destroy;
begin
  FreeAndNil(FInstructions);
  inherited Destroy;
end;

function TJclClrILGenerator.DumpIL(Options: TJclInstructionDumpILOptions): string;
var
  I, J, Indent: Integer;

  function FlagsToName(Flags: TJclClrExceptionClauseFlags): string;
  begin
    if cfFinally in Flags then
      Result := 'finally'
    else
    if cfFilter in Flags then
      Result := 'filter'
    else
    if cfFault in Flags then
      Result := 'fault'
    else
      Result := 'catch';
  end;

  function IndentStr: string;
  begin
    Result := StrRepeat('  ', Indent);
  end;

begin
  Indent := 0;
  with TStringList.Create do
  try
    for I := 0 to InstructionCount-1 do
    begin
      for J := 0 to Method.ExceptionHandlerCount-1 do
      with Method.ExceptionHandlers[J] do
      begin
        if Instructions[I].Offset = TryBlock.Offset then
        begin
          Add(IndentStr + '.try');
          Add(IndentStr + '{');
          Inc(Indent);
        end;
        if Instructions[I].Offset = (TryBlock.Offset + TryBlock.Length) then
        begin
          Dec(Indent);
          Add(IndentStr + '}  // end .try');
        end;
        if Instructions[I].Offset = HandlerBlock.Offset then
        begin
          Add(IndentStr + FlagsToName(Flags));
          Add(IndentStr + '{');
          Inc(Indent);
        end;
        if Instructions[I].Offset = (HandlerBlock.Offset + HandlerBlock.Length) then
        begin
          Dec(Indent);
          Add(IndentStr + '}  // end ' + FlagsToName(Flags));
        end;
      end;
      Add(IndentStr + Instructions[I].DumpIL(Options));
    end;
    Result := Text;
  finally
    Free;
  end;
end;

function TJclClrILGenerator.GetInstructionCount: Integer;
begin
  Result := FInstructions.Count;
end;

function TJclClrILGenerator.GetInstruction(const Idx: Integer): TJclInstruction;
begin
  Result := TJclInstruction(FInstructions[Idx]);
end;

//=== { TJclInstruction } ====================================================

constructor TJclInstruction.Create(AOwner :TJclClrILGenerator; AOpCode: TJclOpCode);
begin
  inherited Create;
  FOwner := AOwner;
  FOpCode := AOpCode;
end;

function TJclInstruction.GetWideOpCode: Boolean;
begin
  Result := Integer(OpCode) > MaxByte;
end;

function TJclInstruction.GetRealOpCode: Byte;
begin
  if WideOpCode then
    Result := Integer(OpCode) mod (MaxByte + 1)
  else
    Result := Integer(OpCode);
end;

function TJclInstruction.GetParamType: TJclInstructionParamType;
begin
  Result := OpCodeParamTypes[OpCode];
end;

function TJclInstruction.GetName: string;
begin
  Result := OpCodeInfos[OpCode, itName];
end;

function TJclInstruction.GetFullName: string;
begin
  Result := OpCodeInfos[OpCode, itFullName];
end;

function TJclInstruction.GetDescription: string;
begin
  Result := OpCodeInfos[OpCode, itDescription]
end;

function TJclInstruction.GetSize: DWORD;
const
  OpCodeSize: array [Boolean] of DWORD = (1, 2);
begin
  case ParamType of
    ptSOff, ptI1, ptU1:
      Result := SizeOf(Byte);
    ptI2, ptU2:
      Result := SizeOf(Word);
    ptLOff, ptI4, ptToken, ptU4, ptR4:
      Result := SizeOf(DWORD);
    ptI8, ptU8, ptR8:
      Result := SizeOf(Int64);
    ptArray:
      Result := (VarArrayHighBound(FParam, 1) - VarArrayLowBound(FParam, 1) + 1 + 1) * SizeOf(Integer);
  else
    Result := 0;
  end;
  Result := OpCodeSize[OpCode in [opNop..opPrefixRef]] + Result;
end;

procedure TJclInstruction.Load(Stream: TStream);
var
  Code: Byte;
  I, ArraySize: DWORD;   { TODO : I, ArraySize = DWORD create a serious problem }
  Value: Integer;
begin
  FOffset := Stream.Position;
  try
    Stream.Read(Code, SizeOf(Code));
    if WideOpCode then
    begin
      if Code <> STP1 then
        raise EJclCliInstructionStreamInvalid.CreateRes(@RsInstructionStreamInvalid);
      Stream.Read(Code, SizeOf(Code));
    end;

    if Code <> RealOpCode then
      raise EJclCliInstructionStreamInvalid.CreateRes(@RsInstructionStreamInvalid);

    with TVarData(FParam) do
    case ParamType of
      ptU1:
        begin
          Stream.Read(VByte, SizeOf(Byte));
          VType := varByte;
        end;
      ptI2:
        begin
          Stream.Read(VSmallInt, SizeOf(SmallInt));
          VType := varSmallInt;
        end;
      ptLOff, ptI4:
        begin
          Stream.Read(VInteger, SizeOf(Integer));
          VType := varInteger;
        end;
      ptR4:
        begin
          Stream.Read(VSingle, SizeOf(Single));
          VType := varSingle;
        end;
      ptR8:                                       
        begin
          Stream.Read(VDouble, SizeOf(Double));
          VType := varDouble;
        end;
      ptArray:
        begin
          Stream.Read(ArraySize, SizeOf(ArraySize));
          FParam := VarArrayCreate([0, ArraySize-1], varInteger);
          for I := 0 to ArraySize-1 do  { TODO : ArraySize = 0 and we have a nearly endless loop }
          begin
            Stream.Read(Value, SizeOf(Value));
            FParam[I] := Value;
          end;
        end;
      {$IFDEF RTL140_UP}  { TODO -cTest : since RTL 14.0 or 15.0? }
      ptSOff, ptI1:
        begin
          Stream.Read(VShortInt, SizeOf(ShortInt));
          VType := varShortInt;
        end;
      ptU2:
        begin
          Stream.Read(VWord, SizeOf(Word));
          VType := varWord;
        end;
      ptToken, ptU4:
        begin
          Stream.Read(VLongWord, SizeOf(LongWord));
          VType := varLongWord;
        end;
      ptI8, ptU8:
        begin
          Stream.Read(VInt64, SizeOf(Int64));
          VType := varInt64;
        end;
      {$ENDIF RTL140_UP}
    end;
  except
    Stream.Position := FOffset;
    raise;
  end;
end;

procedure TJclInstruction.Save(Stream: TStream);
var
  Code: Byte;
  {$IFDEF RTL140_UP}  { TODO -cTest : since RTL 14.0 or 15.0? }
  ArraySize: DWORD;
  I, Value: Integer;
  {$ENDIF RTL140_UP}
begin
  if WideOpCode then
  begin
    Code := STP1;
    Stream.Write(Code, SizeOf(Code));
  end;

  Code := RealOpCode;;
  Stream.Write(Code, SizeOf(Code));

  case ParamType of
    ptU1:
      Stream.Write(TVarData(FParam).VByte, SizeOf(Byte));
    ptI2:
      Stream.Write(TVarData(FParam).VSmallInt, SizeOf(SmallInt));
    ptLOff, ptI4:
      Stream.Write(TVarData(FParam).VInteger, SizeOf(Integer));
    ptR4:
      Stream.Write(TVarData(FParam).VSingle, SizeOf(Single));
    ptR8:
      Stream.Write(TVarData(FParam).VDouble, SizeOf(Double));
    {$IFDEF RTL140_UP}  { TODO -cTest : since RTL 14.0 or 15.0? }
    ptSOff, ptI1:
      Stream.Write(TVarData(FParam).VShortInt, SizeOf(ShortInt));
    ptU2:
      Stream.Write(TVarData(FParam).VWord, SizeOf(Word));
    ptToken, ptU4:
      Stream.Write(TVarData(FParam).VLongWord, SizeOf(LongWord));
    ptI8, ptU8:
      Stream.Write(TVarData(FParam).VInt64, SizeOf(Int64));
    ptArray:
      begin
        ArraySize := VarArrayHighBound(FParam, 1) - VarArrayLowBound(FParam, 1) + 1;
        Stream.Write(ArraySize, SizeOf(ArraySize));
        { TODO : VarArrayHighBound to VarArrayLowBound very likely wrong }
        for I := VarArrayHighBound(FParam, 1) to VarArrayLowBound(FParam, 1) do
        begin
          Value := VarArrayGet(FParam, [I]);
          Stream.Write(Value, SizeOf(Value));
        end;
      end;
    {$ENDIF RTL140_UP}
  end;
end;

function TJclInstruction.DumpIL(Options: TJclInstructionDumpILOptions): string;
var
  Opt: TJclInstructionDumpILOption;
begin
  if doLineNo in Options then
    Result := DumpILOption(doLineNo) + ': ';
  if doRawBytes in Options then
    Result := Result + Format(' /* %.24s */ ', [DumpILOption(doRawBytes)]);
  for Opt := doIL to doTokenValue do
    Result := Result + DumpILOption(Opt) + ' ';
  if (doComment in Options) and ((FullName <> '') or (Description <> '')) then
    Result := Result + ' // ' + DumpILOption(doComment);
end;

function TJclInstruction.FormatLabel(Offset: Integer): string;
begin
  Result := 'IL_' + IntToHex(Offset, 4);
end;

function TJclInstruction.DumpILOption(Option: TJclInstructionDumpILOption): string;

  function TokenToString(Token: DWORD): string;
  begin
    Result := '(' + IntToHex(Token shr 24, 2) + ')' + IntToHex(Token mod (1 shl 24), 6);
  end;

var
  {$IFDEF RTL140_UP}  { TODO -cTest : since RTL 14.0 or 15.0? }
  I: Integer;
  Row: TJclClrTableRow;
  {$ENDIF RTL140_UP}
  CodeStr, ParamStr: string;
begin
  case Option of
    doLineNo:
      Result := 'IL_' + IntToHex(Offset, 4);
    doRawBytes:
      begin
        if WideOpCode then
          CodeStr := IntToHex(STP1, 2);

        CodeStr := CodeStr + IntToHex(RealOpCode, 2);
        CodeStr := CodeStr + StrRepeat(' ', 4 - Length(CodeStr));

        case ParamType of
          ptSOff, ptI1, ptU1:
            ParamStr := IntToHex(TVarData(FParam).VByte, 2);
          ptArray:
            ParamStr := 'Array';
          {$IFDEF RTL140_UP}  { TODO -cTest : since RTL 14.0 or 15.0? }
          ptI2, ptU2:
            ParamStr := IntToHex(TVarData(FParam).VWord, 4);
          ptLOff, ptI4, ptU4, ptR4:
            ParamStr := IntToHex(TVarData(FParam).VLongWord, 8);
          ptI8, ptU8, ptR8:
            ParamStr := IntToHex(TVarData(FParam).VInt64, 16);
          ptToken:
            ParamStr := TokenToString(TVarData(FParam).VLongWord);
          {$ENDIF RTL140_UP}
        else
          ParamStr := '';
        end;
        ParamStr := ParamStr + StrRepeat(' ', 10 - Length(ParamStr));
        Result := CodeStr + ' | ' + ParamStr;
      end;
    doIL:
      begin
        case ParamType of
        ptVoid:
          ; // do nothing
        ptLOff:
          Result := FormatLabel(Integer(Offset + Size) + TVarData(Param).VInteger - 1);
        {$IFDEF RTL140_UP}  { TODO -cTest : since RTL 14.0 or 15.0? }
        ptToken:
          begin
            if Byte(TJclPeMetadata.TokenTable(TVarData(Param).VLongWord)) = $70 then
              Result := '"' + Owner.Method.Method.Table.Stream.Metadata.UserStringAt(TJclPeMetadata.TokenIndex(TVarData(Param).VLongWord)) + '"'
            else
            begin
              Row := Owner.Method.Method.Table.Stream.Metadata.Tokens[TVarData(Param).VLongWord];
              if Assigned(Row) then
              begin
                if Row is TJclClrTableTypeDefRow then
                  Result := TJclClrTableTypeDefRow(Row).FullName
                else
                if Row is TJclClrTableTypeRefRow then
                  with TJclClrTableTypeRefRow(Row) do
                    Result := FullName
                else
                if Row is TJclClrTableMethodDefRow then
                  with TJclClrTableMethodDefRow(Row) do
                    Result := ParentToken.FullName + '.' + Name
                else
                if Row is TJclClrTableMemberRefRow then
                  with TJclClrTableMemberRefRow(Row) do
                    Result := FullName
                else
                if Row is TJclClrTableFieldDefRow then
                  with TJclClrTableFieldDefRow(Row) do
                    Result := ParentToken.FullName + '.' + Name
                else
                  Result := Row.DumpIL;
              end
              else
                Result := '';
            end;
            Result := Result + ' /* ' + IntToHex(TVarData(FParam).VLongWord, 8) + ' */';
          end;
        ptSOff:
          Result := FormatLabel(Integer(Offset + Size) + TVarData(Param).VShortInt - 1);
        ptArray:
          begin
            for I := VarArrayHighBound(FParam, 1) to VarArrayLowBound(FParam, 1) do
            begin
              Result := Result + FormatLabel(Offset + Size + VarArrayGet(FParam, [I]));
              if I <> VarArrayLowBound(FParam, 1) then
                Result := Result + ', ';
            end;
            Result := ' (' + Result + ')';
          end;
        {$ENDIF RTL140_UP}
        else
          Result := VarToStr(Param);
        end;
        Result := GetName + StrRepeat(' ', 10 - Length(GetName)) + ' ' + Result;
        Result := Result + StrRepeat(' ', 20 - Length(Result));
      end;
    doTokenValue:
      Result := ''; // do nothing
    doComment:
      if FullName = '' then
        Result := Description
      else
      if Description = '' then
        Result := FullName
      else
        Result := FullName + ' - ' + Description;
  end;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
