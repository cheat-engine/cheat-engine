unit DotNetTypes;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

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
  case dntype of
    ELEMENT_TYPE_END            : result:='END';
    ELEMENT_TYPE_VOID           : result:='void';
    ELEMENT_TYPE_BOOLEAN        : result:='bool';
    ELEMENT_TYPE_CHAR           : result:='char';
    ELEMENT_TYPE_I1             : result:='Byte (Signed)';
    ELEMENT_TYPE_U1             : result:='Byte';
    ELEMENT_TYPE_I2             : result:='2 Byte (Signed)';
    ELEMENT_TYPE_U2             : result:='2 Byte';
    ELEMENT_TYPE_I4             : result:='4 Byte (Signed)';
    ELEMENT_TYPE_U4             : result:='4 Byte';
    ELEMENT_TYPE_I8             : result:='8 Byte (Signed)';
    ELEMENT_TYPE_U8             : result:='8 Byte';
    ELEMENT_TYPE_R4             : result:='Float';
    ELEMENT_TYPE_R8             : result:='Double';
    ELEMENT_TYPE_STRING         : result:='String';
    ELEMENT_TYPE_PTR            : result:='Pointer';
    ELEMENT_TYPE_BYREF          : result:='Reference';
    ELEMENT_TYPE_VALUETYPE      : result:='ValueType';
    ELEMENT_TYPE_CLASS          : result:='Class';
    ELEMENT_TYPE_VAR            : result:='Var';
    ELEMENT_TYPE_ARRAY          : result:='Array';
    ELEMENT_TYPE_GENERICINST    : result:='GenericInst';
    ELEMENT_TYPE_TYPEDBYREF     : result:='Typed By Ref';
    ELEMENT_TYPE_I              : result:='Signed';
    ELEMENT_TYPE_U              : result:='Unsigned';
    ELEMENT_TYPE_FNPTR          : result:='Function Pointer';
    ELEMENT_TYPE_OBJECT         : result:='Object';
    ELEMENT_TYPE_SZARRAY        : result:='SZ Array';
    ELEMENT_TYPE_MVAR           : result:='MVar';
    ELEMENT_TYPE_CMOD_REQD      : result:='CMOD_REQD';
    ELEMENT_TYPE_CMOD_OPT       : result:='CMOD_OPT';
    ELEMENT_TYPE_INTERNAL       : result:='Internal';
    ELEMENT_TYPE_MAX            : result:='MAX';
    ELEMENT_TYPE_MODIFIER       : result:='Modifier';
    ELEMENT_TYPE_SENTINEL       : result:='Sentinel';
    ELEMENT_TYPE_PINNED         : result:='Pinned';

  end;
end;

end.

