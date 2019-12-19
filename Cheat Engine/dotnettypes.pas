unit DotNetTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  rsDNTEnd = 'END';
  rsDNTVoid = 'void';
  rsDNTBool = 'bool';
  rsDNTChar = 'char';
  rsDNTByteSigned = 'Byte (Signed)';
  rsDNTByte = 'Byte';
  rsDNT2ByteSigned = '2 Byte (Signed)';
  rsDNT2Byte = '2 Byte';
  rsDNT4ByteSigned = '4 Byte (Signed)';
  rsDNT4Byte = '4 Byte';
  rsDNT8ByteSigned = '8 Byte (Signed)';
  rsDNT8Byte = '8 Byte';
  rsDNTFloat = 'Float';
  rsDNTDouble = 'Double';
  rsDNTString = 'String';
  rsDNTPointer = 'Pointer';
  rsDNTReference = 'Reference';
  rsDNTValueType = 'ValueType';
  rsDNTClass = 'Class';
  rsDNTVar = 'Var';
  rsDNTArray = 'Array';
  rsDNTGenericInst = 'GenericInst';
  rsDNTTypedByRef = 'Typed By Ref';
  rsDNTSigned = 'Signed';
  rsDNTUnsigned = 'Unsigned';
  rsDNTFunctionPointer = 'Function Pointer';
  rsDNTObject = 'Object';
  rsDNTSZArray = 'SZ Array';
  rsDNTMVar = 'MVar';
  rsDNTCMOD_REQD = 'CMOD_REQD';
  rsDNTCMOD_OPT = 'CMOD_OPT';
  rsDNTInternal = 'Internal';
  rsDNTMAX = 'MAX';
  rsDNTModifier = 'Modifier';
  rsDNTSentinel = 'Sentinel';
  rsDNTPinned = 'Pinned';

const
  ELEMENT_TYPE_END            = $00;
  ELEMENT_TYPE_VOID           = $01;
  ELEMENT_TYPE_BOOLEAN        = $02;
  ELEMENT_TYPE_CHAR           = $03;
  ELEMENT_TYPE_I1             = $04;
  ELEMENT_TYPE_U1             = $05;
  ELEMENT_TYPE_I2             = $06;
  ELEMENT_TYPE_U2             = $07;
  ELEMENT_TYPE_I4             = $08;
  ELEMENT_TYPE_U4             = $09;
  ELEMENT_TYPE_I8             = $0a;
  ELEMENT_TYPE_U8             = $0b;
  ELEMENT_TYPE_R4             = $0c;
  ELEMENT_TYPE_R8             = $0d;
  ELEMENT_TYPE_STRING         = $0e;

  // every type above PTR will be simple type
  ELEMENT_TYPE_PTR            = $0f;     // PTR <type>
  ELEMENT_TYPE_BYREF          = $10;     // BYREF <type>

  // Please use ELEMENT_TYPE_VALUETYPE. ELEMENT_TYPE_VALUECLASS is deprecated.
  ELEMENT_TYPE_VALUETYPE      = $11;     // VALUETYPE <class Token>
  ELEMENT_TYPE_CLASS          = $12;     // CLASS <class Token>
  ELEMENT_TYPE_VAR            = $13;     // a class type variable VAR <number>
  ELEMENT_TYPE_ARRAY          = $14;     // MDARRAY <type> <rank> <bcount> <bound1> ... <lbcount> <lb1> ...
  ELEMENT_TYPE_GENERICINST    = $15;     // GENERICINST <generic type> <argCnt> <arg1> ... <argn>
  ELEMENT_TYPE_TYPEDBYREF     = $16;     // TYPEDREF  (it takes no args) a typed referece to some other type

  ELEMENT_TYPE_I              = $18;     // native integer size
  ELEMENT_TYPE_U              = $19;     // native unsigned integer size
  ELEMENT_TYPE_FNPTR          = $1b;     // FNPTR <complete sig for the function including calling convention>
  ELEMENT_TYPE_OBJECT         = $1c;     // Shortcut for System.Object
  ELEMENT_TYPE_SZARRAY        = $1d;     // Shortcut for single dimension zero lower bound array
                                          // SZARRAY <type>
  ELEMENT_TYPE_MVAR           = $1e;     // a method type variable MVAR <number>

  // This is only for binding
  ELEMENT_TYPE_CMOD_REQD      = $1f;     // required C modifier : E_T_CMOD_REQD <mdTypeRef/mdTypeDef>
  ELEMENT_TYPE_CMOD_OPT       = $20;     // optional C modifier : E_T_CMOD_OPT <mdTypeRef/mdTypeDef>

  // This is for signatures generated internally (which will not be persisted in any way).
  ELEMENT_TYPE_INTERNAL       = $21;     // INTERNAL <typehandle>

  // Note that this is the max of base type excluding modifiers
  ELEMENT_TYPE_MAX            = $22;     // first invalid element type


  ELEMENT_TYPE_MODIFIER       = $40;
  ELEMENT_TYPE_SENTINEL       = $41;
  ELEMENT_TYPE_PINNED         = $45;

function DotNetTypeToString(dntype: dword):string;

implementation



function DotNetTypeToString(dntype: dword):string;
begin
  result:=rsDNTPointer;
  case dntype of
    ELEMENT_TYPE_END            : result:=rsDNTEnd;
    ELEMENT_TYPE_VOID           : result:=rsDNTVoid;
    ELEMENT_TYPE_BOOLEAN        : result:=rsDNTBool;
    ELEMENT_TYPE_CHAR           : result:=rsDNTChar;
    ELEMENT_TYPE_I1             : result:=rsDNTByteSigned;
    ELEMENT_TYPE_U1             : result:=rsDNTByte;
    ELEMENT_TYPE_I2             : result:=rsDNT2ByteSigned;
    ELEMENT_TYPE_U2             : result:=rsDNT2Byte;
    ELEMENT_TYPE_I4             : result:=rsDNT4ByteSigned;
    ELEMENT_TYPE_U4             : result:=rsDNT4Byte;
    ELEMENT_TYPE_I8             : result:=rsDNT8ByteSigned;
    ELEMENT_TYPE_U8             : result:=rsDNT8Byte;
    ELEMENT_TYPE_R4             : result:=rsDNTFloat;
    ELEMENT_TYPE_R8             : result:=rsDNTDouble;
    ELEMENT_TYPE_STRING         : result:=rsDNTString;
    ELEMENT_TYPE_PTR            : result:=rsDNTPointer;
    ELEMENT_TYPE_BYREF          : result:=rsDNTReference;
    ELEMENT_TYPE_VALUETYPE      : result:=rsDNTValueType;
    ELEMENT_TYPE_CLASS          : result:=rsDNTClass;
    ELEMENT_TYPE_VAR            : result:=rsDNTVar;
    ELEMENT_TYPE_ARRAY          : result:=rsDNTArray;
    ELEMENT_TYPE_GENERICINST    : result:=rsDNTGenericInst;
    ELEMENT_TYPE_TYPEDBYREF     : result:=rsDNTTypedByRef;
    ELEMENT_TYPE_I              : result:=rsDNTSigned;
    ELEMENT_TYPE_U              : result:=rsDNTUnsigned;
    ELEMENT_TYPE_FNPTR          : result:=rsDNTFunctionPointer;
    ELEMENT_TYPE_OBJECT         : result:=rsDNTObject;
    ELEMENT_TYPE_SZARRAY        : result:=rsDNTSZArray;
    ELEMENT_TYPE_MVAR           : result:=rsDNTMVar;
    ELEMENT_TYPE_CMOD_REQD      : result:=rsDNTCMOD_REQD;
    ELEMENT_TYPE_CMOD_OPT       : result:=rsDNTCMOD_OPT;
    ELEMENT_TYPE_INTERNAL       : result:=rsDNTInternal;
    ELEMENT_TYPE_MAX            : result:=rsDNTMAX;
    ELEMENT_TYPE_MODIFIER       : result:=rsDNTModifier;
    ELEMENT_TYPE_SENTINEL       : result:=rsDNTSentinel;
    ELEMENT_TYPE_PINNED         : result:=rsDNTPinned;

  end;
end;

end.

