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
{ The Original Code is JclExprEval.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Barry Kelly.                                       }
{ Portions created by Barry Kelly are Copyright (C) Barry Kelly. All rights reserved.              }
{                                                                                                  }
{ Contributor(s):                                                                                  }
{   Barry Kelly                                                                                    }
{   Matthias Thoma (mthoma)                                                                        }
{   Petr Vones (pvones)                                                                            }
{   Robert Marquardt (marquardt)                                                                   }
{   Robert Rossmair (rrossmair)                                                                    }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ This unit contains three expression evaluators, each tailored for different usage patterns. It   }
{ also contains the component objects, so that a customized expression evaluator can be assembled  }
{ relatively easily.                                                                               }
{                                                                                                  }
{**************************************************************************************************}
{                                                                                                  }
{ Last modified: $Date:: 2008-01-30 17:01:07 +0100 (mer., 30 janv. 2008)                         $ }
{ Revision:      $Rev:: 2330                                                                     $ }
{ Author:        $Author:: outchy                                                                $ }
{                                                                                                  }
{**************************************************************************************************}

// operator priority (as implemented in this unit)
// all binary operators are associated from left to right
// all unary operators are associated from right to left

// (highest) not bnot(bitwise) +(unary) -(unary)                      (level 3)
//           * / div mod and band(bitwise) shl shr                    (level 2)
//           +(binary) -(binary) or xor bor(bitwise) bxor(bitwise)    (level 1)
// (lowest)  < <= > >= cmp = <>                                       (level 0)

// details on cmp operator:
//  "1.5 cmp 2.0" returns -1.0 because 1.5 < 2.0
//  "1.5 cmp 1.5" returns 0.0 because 1.5 = 1.5
//  "1.5 cmp 0.0" returns 1.0 because 1.5 > 0.0

unit JclExprEval;

{$I jcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils, Classes,
  JclBase, JclSysUtils, JclStrHashMap, JclResources;

const
  cExprEvalHashSize = 127;

type
  EJclExprEvalError = class(EJclError);

const
  ExprWhiteSpace = [#1..#32];

type
  TFloat = JclBase.Float;
  PFloat = JclBase.PFloat;

  TFloat32 = Single;
  PFloat32 = ^TFloat32;

  TFloat64 = Double;
  PFloat64 = ^TFloat64;

  TFloat80 = Extended;
  PFloat80 = ^TFloat80;

  TFloatFunc = function: TFloat;
  TFloat32Func = function: TFloat32;
  TFloat64Func = function: TFloat64;
  TFloat80Func = function: TFloat80;

  TUnaryFunc = function(X: TFloat): TFloat;
  TUnary32Func = function(X: TFloat32): TFloat32;
  TUnary64Func = function(X: TFloat64): TFloat64;
  TUnary80Func = function(X: TFloat80): TFloat80;

  TBinaryFunc = function(X, Y: TFloat): TFloat;
  TBinary32Func = function(X, Y: TFloat32): TFloat32;
  TBinary64Func = function(X, Y: TFloat64): TFloat64;
  TBinary80Func = function(X, Y: TFloat80): TFloat80;

  TTernaryFunc = function(X, Y, Z: TFloat): TFloat;
  TTernary32Func = function(X, Y, Z: TFloat32): TFloat32;
  TTernary64Func = function(X, Y, Z: TFloat64): TFloat64;
  TTernary80Func = function(X, Y, Z: TFloat80): TFloat80;

type
  { Forward Declarations }
  TExprLexer = class;
  TExprCompileParser = class;
  TExprEvalParser = class;
  TExprSym = class;
  TExprNode = class;
  TExprNodeFactory = class;

  TExprContext = class(TObject)
  public
    function Find(const AName: string): TExprSym; virtual; abstract;
  end;

  TExprHashContext = class(TExprContext)
  private
    FHashMap: TStringHashMap;
  public
    constructor Create(ACaseSensitive: Boolean = False; AHashSize: Integer = 127);
    destructor Destroy; override;
    procedure Add(ASymbol: TExprSym);
    procedure Remove(const AName: string);
    function Find(const AName: string): TExprSym; override;
  end;

  TExprSetContext = class(TExprContext)
  private
    FList: TList;
    FOwnsContexts: Boolean;
    function GetContexts(AIndex: Integer): TExprContext;
    function GetCount: Integer;
  public
    constructor Create(AOwnsContexts: Boolean);
    destructor Destroy; override;
    procedure Add(AContext: TExprContext);
    procedure Remove(AContext: TExprContext);
    procedure Delete(AIndex: Integer);
    function Extract(AContext: TExprContext): TExprContext;
    property Count: Integer read GetCount;
    property Contexts[AIndex: Integer]: TExprContext read GetContexts;
    property InternalList: TList read FList;
    function Find(const AName: string): TExprSym; override;
  end;

  TExprSym = class(TObject)
  private
    FIdent: string;
    FLexer: TExprLexer;
    FEvalParser: TExprEvalParser;
    FCompileParser: TExprCompileParser;
    FNodeFactory: TExprNodeFactory;
  public
    constructor Create(const AIdent: string);
    function Evaluate: TFloat; virtual; abstract;
    function Compile: TExprNode; virtual; abstract;
    property Ident: string read FIdent;
    property Lexer: TExprLexer read FLexer write FLexer;
    property CompileParser: TExprCompileParser read FCompileParser
      write FCompileParser;
    property EvalParser: TExprEvalParser read FEvalParser write FEvalParser;
    property NodeFactory: TExprNodeFactory read FNodeFactory write FNodeFactory;
  end;

  TExprToken = (
    // specials
    etEof,
    etNumber,
    etIdentifier,

    // user extension tokens
    etUser0, etUser1, etUser2, etUser3, etUser4, etUser5, etUser6, etUser7,
    etUser8, etUser9, etUser10, etUser11, etUser12, etUser13, etUser14, etUser15,
    etUser16, etUser17, etUser18, etUser19, etUser20, etUser21, etUser22, etUser23,
    etUser24, etUser25, etUser26, etUser27, etUser28, etUser29, etUser30, etUser31,

    // compound tokens
    etNotEqual, // <>
    etLessEqual, // <=
    etGreaterEqual, // >=

    // ASCII normal & ordinals

    etBang, // '!' #$21 33
    etDoubleQuote, // '"' #$22 34
    etHash, // '#' #$23 35
    etDollar, // '$' #$24 36
    etPercent, // '%' #$25 37
    etAmpersand, // '&' #$26 38
    etSingleQuote, // '''' #$27 39
    etLParen, // '(' #$28 40
    etRParen, // ')' #$29 41
    etAsterisk, // '*' #$2A 42
    etPlus, // '+' #$2B 43
    etComma, // ',' #$2C 44
    etMinus, // '-' #$2D 45
    etDot, // '.' #$2E 46
    etForwardSlash, // '/' #$2F 47

    // 48..57 - numbers...

    etColon, // ':' #$3A 58
    etSemiColon, // ';' #$3B 59
    etLessThan, // '<' #$3C 60
    etEqualTo, // '=' #$3D 61
    etGreaterThan, // '>' #$3E 62
    etQuestion, // '?' #$3F 63
    etAt, // '@' #$40 64

    // 65..90 - capital letters...

    etLBracket, // '[' #$5B 91
    etBackSlash, // '\' #$5C 92
    etRBracket, // ']' #$5D 93
    etArrow, // '^' #$5E 94
    // 95 - underscore
    etBackTick, // '`' #$60 96

    // 97..122 - small letters...

    etLBrace, // '{' #$7B 123
    etPipe, // '|' #$7C 124
    etRBrace, // '}' #$7D 125
    etTilde, // '~' #$7E 126
    et127, // '' #$7F 127
    etEuro, // 'Ä' #$80 128
    et129, // 'Å' #$81 129
    et130, // 'Ç' #$82 130
    et131, // 'É' #$83 131
    et132, // 'Ñ' #$84 132
    et133, // 'Ö' #$85 133
    et134, // 'Ü' #$86 134
    et135, // 'á' #$87 135
    et136, // 'à' #$88 136
    et137, // 'â' #$89 137
    et138, // 'ä' #$8A 138
    et139, // 'ã' #$8B 139
    et140, // 'å' #$8C 140
    et141, // 'ç' #$8D 141
    et142, // 'é' #$8E 142
    et143, // 'è' #$8F 143
    et144, // 'ê' #$90 144
    et145, // 'ë' #$91 145
    et146, // 'í' #$92 146
    et147, // 'ì' #$93 147
    et148, // 'î' #$94 148
    et149, // 'ï' #$95 149
    et150, // 'ñ' #$96 150
    et151, // 'ó' #$97 151
    et152, // 'ò' #$98 152
    et153, // 'ô' #$99 153
    et154, // 'ö' #$9A 154
    et155, // 'õ' #$9B 155
    et156, // 'ú' #$9C 156
    et157, // 'ù' #$9D 157
    et158, // 'û' #$9E 158
    et159, // 'ü' #$9F 159
    et160, // '†' #$A0 160
    et161, // '°' #$A1 161
    et162, // '¢' #$A2 162
    et163, // '£' #$A3 163
    et164, // '§' #$A4 164
    et165, // '•' #$A5 165
    et166, // '¶' #$A6 166
    et167, // 'ß' #$A7 167
    et168, // '®' #$A8 168
    et169, // '©' #$A9 169
    et170, // '™' #$AA 170
    et171, // '´' #$AB 171
    et172, // '¨' #$AC 172
    et173, // '≠' #$AD 173
    et174, // 'Æ' #$AE 174
    et175, // 'Ø' #$AF 175
    et176, // '∞' #$B0 176
    et177, // '±' #$B1 177
    et178, // '≤' #$B2 178
    et179, // '≥' #$B3 179
    et180, // '¥' #$B4 180
    et181, // 'µ' #$B5 181
    et182, // '∂' #$B6 182
    et183, // '∑' #$B7 183
    et184, // '∏' #$B8 184
    et185, // 'π' #$B9 185
    et186, // '∫' #$BA 186
    et187, // 'ª' #$BB 187
    et188, // 'º' #$BC 188
    et189, // 'Ω' #$BD 189
    et190, // 'æ' #$BE 190
    et191, // 'ø' #$BF 191
    et192, // '¿' #$C0 192
    et193, // '¡' #$C1 193
    et194, // '¬' #$C2 194
    et195, // '√' #$C3 195
    et196, // 'ƒ' #$C4 196
    et197, // '≈' #$C5 197
    et198, // '∆' #$C6 198
    et199, // '«' #$C7 199
    et200, // '»' #$C8 200
    et201, // '…' #$C9 201
    et202, // ' ' #$CA 202
    et203, // 'À' #$CB 203
    et204, // 'Ã' #$CC 204
    et205, // 'Õ' #$CD 205
    et206, // 'Œ' #$CE 206
    et207, // 'œ' #$CF 207
    et208, // '–' #$D0 208
    et209, // '—' #$D1 209
    et210, // '“' #$D2 210
    et211, // '”' #$D3 211
    et212, // '‘' #$D4 212
    et213, // '’' #$D5 213
    et214, // '÷' #$D6 214
    et215, // '◊' #$D7 215
    et216, // 'ÿ' #$D8 216
    et217, // 'Ÿ' #$D9 217
    et218, // '⁄' #$DA 218
    et219, // '€' #$DB 219
    et220, // '‹' #$DC 220
    et221, // '›' #$DD 221
    et222, // 'ﬁ' #$DE 222
    et223, // 'ﬂ' #$DF 223
    et224, // '‡' #$E0 224
    et225, // '·' #$E1 225
    et226, // '‚' #$E2 226
    et227, // '„' #$E3 227
    et228, // '‰' #$E4 228
    et229, // 'Â' #$E5 229
    et230, // 'Ê' #$E6 230
    et231, // 'Á' #$E7 231
    et232, // 'Ë' #$E8 232
    et233, // 'È' #$E9 233
    et234, // 'Í' #$EA 234
    et235, // 'Î' #$EB 235
    et236, // 'Ï' #$EC 236
    et237, // 'Ì' #$ED 237
    et238, // 'Ó' #$EE 238
    et239, // 'Ô' #$EF 239
    et240, // '' #$F0 240
    et241, // 'Ò' #$F1 241
    et242, // 'Ú' #$F2 242
    et243, // 'Û' #$F3 243
    et244, // 'Ù' #$F4 244
    et245, // 'ı' #$F5 245
    et246, // 'ˆ' #$F6 246
    et247, // '˜' #$F7 247
    et248, // '¯' #$F8 248
    et249, // '˘' #$F9 249
    et250, // '˙' #$FA 250
    et251, // '˚' #$FB 251
    et252, // '¸' #$FC 252
    et253, // '˝' #$FD 253
    et254, // '˛' #$FE 254
    et255, // 'ˇ' #$FF 255
    etInvalid // invalid token type
  );

  TExprLexer = class(TObject)
  protected
    FCurrTok: TExprToken;
    FTokenAsNumber: TFloat;
    FTokenAsString: string;
  public
    constructor Create;
    procedure NextTok; virtual; abstract;
    procedure Reset; virtual;
    property TokenAsString: string read FTokenAsString;
    property TokenAsNumber: TFloat read FTokenAsNumber;
    property CurrTok: TExprToken read FCurrTok;
  end;

  TExprNode = class(TObject)
  private
    FDepList: TList;
    function GetDepCount: Integer;
    function GetDeps(AIndex: Integer): TExprNode;
  public
    constructor Create(const ADepList: array of TExprNode);
    destructor Destroy; override;
    procedure AddDep(ADep: TExprNode);
    property DepCount: Integer read GetDepCount;
    property Deps[AIndex: Integer]: TExprNode read GetDeps; default;
    property DepList: TList read FDepList;
  end;

  TExprNodeFactory = class(TObject)
  public
    function LoadVar32(ALoc: PFloat32): TExprNode; virtual; abstract;
    function LoadVar64(ALoc: PFloat64): TExprNode; virtual; abstract;
    function LoadVar80(ALoc: PFloat80): TExprNode; virtual; abstract;

    function LoadConst32(AValue: TFloat32): TExprNode; virtual; abstract;
    function LoadConst64(AValue: TFloat64): TExprNode; virtual; abstract;
    function LoadConst80(AValue: TFloat80): TExprNode; virtual; abstract;

    function CallFloatFunc(AFunc: TFloatFunc): TExprNode; virtual; abstract;
    function CallFloat32Func(AFunc: TFloat32Func): TExprNode; virtual; abstract;
    function CallFloat64Func(AFunc: TFloat64Func): TExprNode; virtual; abstract;
    function CallFloat80Func(AFunc: TFloat80Func): TExprNode; virtual; abstract;
    function CallUnaryFunc(AFunc: TUnaryFunc; X: TExprNode): TExprNode; virtual; abstract;
    function CallUnary32Func(AFunc: TUnary32Func; X: TExprNode): TExprNode; virtual; abstract;
    function CallUnary64Func(AFunc: TUnary64Func; X: TExprNode): TExprNode; virtual; abstract;
    function CallUnary80Func(AFunc: TUnary80Func; X: TExprNode): TExprNode; virtual; abstract;
    function CallBinaryFunc(AFunc: TBinaryFunc; X, Y: TExprNode): TExprNode; virtual; abstract;
    function CallBinary32Func(AFunc: TBinary32Func; X, Y: TExprNode): TExprNode; virtual; abstract;
    function CallBinary64Func(AFunc: TBinary64Func; X, Y: TExprNode): TExprNode; virtual; abstract;
    function CallBinary80Func(AFunc: TBinary80Func; X, Y: TExprNode): TExprNode; virtual; abstract;
    function CallTernaryFunc(AFunc: TTernaryFunc; X, Y, Z: TExprNode): TExprNode; virtual; abstract;
    function CallTernary32Func(AFunc: TTernary32Func; X, Y, Z: TExprNode): TExprNode; virtual; abstract;
    function CallTernary64Func(AFunc: TTernary64Func; X, Y, Z: TExprNode): TExprNode; virtual; abstract;
    function CallTernary80Func(AFunc: TTernary80Func; X, Y, Z: TExprNode): TExprNode; virtual; abstract;

    function Add(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function Subtract(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function Multiply(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function Divide(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function IntegerDivide(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function Modulo(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function Negate(AValue: TExprNode): TExprNode; virtual; abstract;

    function Compare(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function CompareEqual(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function CompareNotEqual(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function CompareLess(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function CompareLessEqual(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function CompareGreater(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function CompareGreaterEqual(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;

    function LogicalAnd(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function LogicalOr(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function LogicalXor(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function LogicalNot(AValue: TExprNode): TExprNode; virtual; abstract;
    function BitwiseAnd(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function BitwiseOr(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function BitwiseXor(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function BitwiseNot(AValue: TExprNode): TExprNode; virtual; abstract;
    function ShiftLeft(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;
    function ShiftRight(ALeft, ARight: TExprNode): TExprNode; virtual; abstract;

    function LoadVar(ALoc: PFloat32): TExprNode; overload;
    function LoadVar(ALoc: PFloat64): TExprNode; overload;
    function LoadVar(ALoc: PFloat80): TExprNode; overload;
    function LoadConst(AValue: TFloat32): TExprNode; overload;
    function LoadConst(AValue: TFloat64): TExprNode; overload;
    function LoadConst(AValue: TFloat80): TExprNode; overload;
  end;

  TExprCompileParser = class(TObject)
  private
    FContext: TExprContext;
    FLexer: TExprLexer;
    FNodeFactory: TExprNodeFactory;
  protected
    function CompileExprLevel0(ASkip: Boolean): TExprNode; virtual;
    function CompileExprLevel1(ASkip: Boolean): TExprNode; virtual;
    function CompileExprLevel2(ASkip: Boolean): TExprNode; virtual;
    function CompileExprLevel3(ASkip: Boolean): TExprNode; virtual;
    function CompileFactor: TExprNode; virtual;
    function CompileIdentFactor: TExprNode; virtual;
  public
    constructor Create(ALexer: TExprLexer; ANodeFactory: TExprNodeFactory);
    function Compile: TExprNode; virtual;
    property Lexer: TExprLexer read FLexer;
    property NodeFactory: TExprNodeFactory read FNodeFactory;
    property Context: TExprContext read FContext write FContext;
  end;

  TExprEvalParser = class(TObject)
  private
    FContext: TExprContext;
    FLexer: TExprLexer;
  protected
    function EvalExprLevel0(ASkip: Boolean): TFloat; virtual;
    function EvalExprLevel1(ASkip: Boolean): TFloat; virtual;
    function EvalExprLevel2(ASkip: Boolean): TFloat; virtual;
    function EvalExprLevel3(ASkip: Boolean): TFloat; virtual;
    function EvalFactor: TFloat; virtual;
    function EvalIdentFactor: TFloat; virtual;
  public
    constructor Create(ALexer: TExprLexer);
    function Evaluate: TFloat; virtual;

    property Lexer: TExprLexer read FLexer;
    property Context: TExprContext read FContext write FContext;
  end;

{ some concrete class descendants follow... }

  TExprSimpleLexer = class(TExprLexer)
  protected
    FCurrPos: PChar;
    FBuf: string;
    procedure SetBuf(const ABuf: string);
  public
    constructor Create(const ABuf: string);

    procedure NextTok; override;
    procedure Reset; override;

    property Buf: string read FBuf write SetBuf;
  end;

  TExprVirtMachOp = class(TObject)
  private
    function GetOutputLoc: PFloat;
  protected
    FOutput: TFloat;
  public
    procedure Execute; virtual; abstract;
    property OutputLoc: PFloat read GetOutputLoc;
  end;

  TExprVirtMach = class(TObject)
  private
    FCodeList: TList;
    FConstList: TList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(AOp: TExprVirtMachOp);
    procedure AddConst(AOp: TExprVirtMachOp);
    procedure Clear;
    function Execute: TFloat;
  end;

  TExprVirtMachNodeFactory = class(TExprNodeFactory)
  private
    FNodeList: TList;
    function AddNode(ANode: TExprNode): TExprNode;
    procedure DoClean(AVirtMach: TExprVirtMach);
    procedure DoConsts(AVirtMach: TExprVirtMach);
    procedure DoCode(AVirtMach: TExprVirtMach);
  public
    constructor Create;
    destructor Destroy; override;

    procedure GenCode(AVirtMach: TExprVirtMach);

    function LoadVar32(ALoc: PFloat32): TExprNode; override;
    function LoadVar64(ALoc: PFloat64): TExprNode; override;
    function LoadVar80(ALoc: PFloat80): TExprNode; override;
    function LoadConst32(AValue: TFloat32): TExprNode; override;
    function LoadConst64(AValue: TFloat64): TExprNode; override;
    function LoadConst80(AValue: TFloat80): TExprNode; override;

    function CallFloatFunc(AFunc: TFloatFunc): TExprNode; override;
    function CallFloat32Func(AFunc: TFloat32Func): TExprNode; override;
    function CallFloat64Func(AFunc: TFloat64Func): TExprNode; override;
    function CallFloat80Func(AFunc: TFloat80Func): TExprNode; override;
    function CallUnaryFunc(AFunc: TUnaryFunc; X: TExprNode): TExprNode; override;
    function CallUnary32Func(AFunc: TUnary32Func; X: TExprNode): TExprNode; override;
    function CallUnary64Func(AFunc: TUnary64Func; X: TExprNode): TExprNode; override;
    function CallUnary80Func(AFunc: TUnary80Func; X: TExprNode): TExprNode; override;
    function CallBinaryFunc(AFunc: TBinaryFunc; X, Y: TExprNode): TExprNode; override;
    function CallBinary32Func(AFunc: TBinary32Func; X, Y: TExprNode): TExprNode; override;
    function CallBinary64Func(AFunc: TBinary64Func; X, Y: TExprNode): TExprNode; override;
    function CallBinary80Func(AFunc: TBinary80Func; X, Y: TExprNode): TExprNode; override;
    function CallTernaryFunc(AFunc: TTernaryFunc; X, Y, Z: TExprNode): TExprNode; override;
    function CallTernary32Func(AFunc: TTernary32Func; X, Y, Z: TExprNode): TExprNode; override;
    function CallTernary64Func(AFunc: TTernary64Func; X, Y, Z: TExprNode): TExprNode; override;
    function CallTernary80Func(AFunc: TTernary80Func; X, Y, Z: TExprNode): TExprNode; override;

    function Add(ALeft, ARight: TExprNode): TExprNode; override;
    function Subtract(ALeft, ARight: TExprNode): TExprNode; override;
    function Multiply(ALeft, ARight: TExprNode): TExprNode; override;
    function Divide(ALeft, ARight: TExprNode): TExprNode; override;
    function IntegerDivide(ALeft, ARight: TExprNode): TExprNode; override;
    function Modulo(ALeft, ARight: TExprNode): TExprNode; override;
    function Negate(AValue: TExprNode): TExprNode; override;

    function Compare(ALeft, ARight: TExprNode): TExprNode; override;
    function CompareEqual(ALeft, ARight: TExprNode): TExprNode; override;
    function CompareNotEqual(ALeft, ARight: TExprNode): TExprNode; override;
    function CompareLess(ALeft, ARight: TExprNode): TExprNode; override;
    function CompareLessEqual(ALeft, ARight: TExprNode): TExprNode; override;
    function CompareGreater(ALeft, ARight: TExprNode): TExprNode; override;
    function CompareGreaterEqual(ALeft, ARight: TExprNode): TExprNode; override;

    function LogicalAnd(ALeft, ARight: TExprNode): TExprNode; override;
    function LogicalOr(ALeft, ARight: TExprNode): TExprNode; override;
    function LogicalXor(ALeft, ARight: TExprNode): TExprNode; override;
    function LogicalNot(AValue: TExprNode): TExprNode; override;
    function BitwiseAnd(ALeft, ARight: TExprNode): TExprNode; override;
    function BitwiseOr(ALeft, ARight: TExprNode): TExprNode; override;
    function BitwiseXor(ALeft, ARight: TExprNode): TExprNode; override;
    function BitwiseNot(AValue: TExprNode): TExprNode; override;
    function ShiftLeft(ALeft, ARight: TExprNode): TExprNode; override;
    function ShiftRight(ALeft, ARight: TExprNode): TExprNode; override;
  end;

  { some concrete symbols }

  TExprConstSym = class(TExprSym)
  private
    FValue: TFloat;
  public
    constructor Create(const AIdent: string; AValue: TFloat);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprConst32Sym = class(TExprSym)
  private
    FValue: TFloat32;
  public
    constructor Create(const AIdent: string; AValue: TFloat32);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprConst64Sym = class(TExprSym)
  private
    FValue: TFloat64;
  public
    constructor Create(const AIdent: string; AValue: TFloat64);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprConst80Sym = class(TExprSym)
  private
    FValue: TFloat80;
  public
    constructor Create(const AIdent: string; AValue: TFloat80);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprVar32Sym = class(TExprSym)
  private
    FLoc: PFloat32;
  public
    constructor Create(const AIdent: string; ALoc: PFloat32);

    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprVar64Sym = class(TExprSym)
  private
    FLoc: PFloat64;
  public
    constructor Create(const AIdent: string; ALoc: PFloat64);

    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprVar80Sym = class(TExprSym)
  private
    FLoc: PFloat80;
  public
    constructor Create(const AIdent: string; ALoc: PFloat80);

    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprAbstractFuncSym = class(TExprSym)
  protected
    function EvalFirstArg: TFloat;
    function EvalNextArg: TFloat;
    function CompileFirstArg: TExprNode;
    function CompileNextArg: TExprNode;
    procedure EndArgs;
  end;

  TExprFuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TFloatFunc;
  public
    constructor Create(const AIdent: string; AFunc: TFloatFunc);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprFloat32FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TFloat32Func;
  public
    constructor Create(const AIdent: string; AFunc: TFloat32Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprFloat64FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TFloat64Func;
  public
    constructor Create(const AIdent: string; AFunc: TFloat64Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprFloat80FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TFloat80Func;
  public
    constructor Create(const AIdent: string; AFunc: TFloat80Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprUnaryFuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TUnaryFunc;
  public
    constructor Create(const AIdent: string; AFunc: TUnaryFunc);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprUnary32FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TUnary32Func;
  public
    constructor Create(const AIdent: string; AFunc: TUnary32Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprUnary64FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TUnary64Func;
  public
    constructor Create(const AIdent: string; AFunc: TUnary64Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprUnary80FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TUnary80Func;
  public
    constructor Create(const AIdent: string; AFunc: TUnary80Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprBinaryFuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TBinaryFunc;
  public
    constructor Create(const AIdent: string; AFunc: TBinaryFunc);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprBinary32FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TBinary32Func;
  public
    constructor Create(const AIdent: string; AFunc: TBinary32Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprBinary64FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TBinary64Func;
  public
    constructor Create(const AIdent: string; AFunc: TBinary64Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprBinary80FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TBinary80Func;
  public
    constructor Create(const AIdent: string; AFunc: TBinary80Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprTernaryFuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TTernaryFunc;
  public
    constructor Create(const AIdent: string; AFunc: TTernaryFunc);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprTernary32FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TTernary32Func;
  public
    constructor Create(const AIdent: string; AFunc: TTernary32Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprTernary64FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TTernary64Func;
  public
    constructor Create(const AIdent: string; AFunc: TTernary64Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TExprTernary80FuncSym = class(TExprAbstractFuncSym)
  private
    FFunc: TTernary80Func;
  public
    constructor Create(const AIdent: string; AFunc: TTernary80Func);
    function Evaluate: TFloat; override;
    function Compile: TExprNode; override;
  end;

  TEasyEvaluator = class(TObject)
  private
    FOwnContext: TExprHashContext;
    FExtContextSet: TExprSetContext;
    FInternalContextSet: TExprSetContext;
  protected
    property InternalContextSet: TExprSetContext read FInternalContextSet;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddVar(const AName: string; var AVar: TFloat32); overload;
    procedure AddVar(const AName: string; var AVar: TFloat64); overload;
    procedure AddVar(const AName: string; var AVar: TFloat80); overload;

    procedure AddConst(const AName: string; AConst: TFloat32); overload;
    procedure AddConst(const AName: string; AConst: TFloat64); overload;
    procedure AddConst(const AName: string; AConst: TFloat80); overload;

    procedure AddFunc(const AName: string; AFunc: TFloat32Func); overload;
    procedure AddFunc(const AName: string; AFunc: TFloat64Func); overload;
    procedure AddFunc(const AName: string; AFunc: TFloat80Func); overload;
    procedure AddFunc(const AName: string; AFunc: TUnary32Func); overload;
    procedure AddFunc(const AName: string; AFunc: TUnary64Func); overload;
    procedure AddFunc(const AName: string; AFunc: TUnary80Func); overload;
    procedure AddFunc(const AName: string; AFunc: TBinary32Func); overload;
    procedure AddFunc(const AName: string; AFunc: TBinary64Func); overload;
    procedure AddFunc(const AName: string; AFunc: TBinary80Func); overload;
    procedure AddFunc(const AName: string; AFunc: TTernary32Func); overload;
    procedure AddFunc(const AName: string; AFunc: TTernary64Func); overload;
    procedure AddFunc(const AName: string; AFunc: TTernary80Func); overload;
    procedure Remove(const AName: string);

    procedure Clear;
    property ExtContextSet: TExprSetContext read FExtContextSet;
  end;

  TEvaluator = class(TEasyEvaluator)
  private
    FLexer: TExprSimpleLexer;
    FParser: TExprEvalParser;
  public
    constructor Create;
    destructor Destroy; override;
    function Evaluate(const AExpr: string): TFloat;
  end;

  TCompiledEvaluator = class(TEasyEvaluator)
  private
    FExpr: string;
    FVirtMach: TExprVirtMach;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Compile(const AExpr: string);
    function Evaluate: TFloat;
  end;

  { TODO : change this definition to be just a normal function pointer, not
    a closure; will require a small executable memory allocater, and a
    couple of injected instructions. Similar concept to
    Forms.MakeObjectInstance.

    This will allow compiled expressions to be used as functions in
    contexts. Parameters won't be supported, though; I'll think about
    this. }

  TCompiledExpression = function: TFloat of object;

  TExpressionCompiler = class(TEasyEvaluator)
  private
    FExprHash: TStringHashMap;
  public
    constructor Create;
    destructor Destroy; override;
    function Compile(const AExpr: string): TCompiledExpression;
    procedure Remove(const AExpr: string);
    procedure Delete(ACompiledExpression: TCompiledExpression);
    procedure Clear;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL: https://jcl.svn.sourceforge.net:443/svnroot/jcl/tags/JCL-1.102-Build3072/jcl/source/common/JclExprEval.pas $';
    Revision: '$Revision: 2330 $';
    Date: '$Date: 2008-01-30 17:01:07 +0100 (mer., 30 janv. 2008) $';
    LogPath: 'JCL\source\common'
    );
{$ENDIF UNITVERSIONING}

implementation

{$IFDEF MSWINDOWS}
uses
  Windows; // inline of AnsiSameText
{$ENDIF MSWINDOWS}

//=== { TExprHashContext } ===================================================

constructor TExprHashContext.Create(ACaseSensitive: Boolean; AHashSize: Integer);
begin
  inherited Create;
  if ACaseSensitive then
    FHashMap := TStringHashMap.Create(CaseSensitiveTraits, AHashSize)
  else
    FHashMap := TStringHashMap.Create(CaseInsensitiveTraits, AHashSize);
end;

destructor TExprHashContext.Destroy;
begin
  FHashMap.Iterate(nil, Iterate_FreeObjects);
  FHashMap.Free;
  inherited Destroy;
end;

procedure TExprHashContext.Add(ASymbol: TExprSym);
begin
  FHashMap.Add(ASymbol.Ident, ASymbol);
end;

procedure TExprHashContext.Remove(const AName: string);
begin
  TObject(FHashMap.Remove(AName)).Free;
end;

function TExprHashContext.Find(const AName: string): TExprSym;
begin
  if not FHashMap.Find(AName, Result) then
    Result := nil;
end;

//=== { TExprSetContext } ====================================================

constructor TExprSetContext.Create(AOwnsContexts: Boolean);
begin
  inherited Create;
  FOwnsContexts := AOwnsContexts;
  FList := TList.Create;
end;

destructor TExprSetContext.Destroy;
begin
  if FOwnsContexts then
    ClearObjectList(FList);
  FList.Free;
  inherited Destroy;
end;

procedure TExprSetContext.Add(AContext: TExprContext);
begin
  FList.Add(AContext);
end;

procedure TExprSetContext.Delete(AIndex: Integer);
begin
  if FOwnsContexts then
    TObject(FList[AIndex]).Free;
  FList.Delete(AIndex);
end;

function TExprSetContext.Extract(AContext: TExprContext): TExprContext;
begin
  Result := AContext;
  FList.Remove(AContext);
end;

function TExprSetContext.Find(const AName: string): TExprSym;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
  begin
    Result := Contexts[I].Find(AName);
    if Result <> nil then
      Break;
  end;
end;

function TExprSetContext.GetContexts(AIndex: Integer): TExprContext;
begin
  Result := TExprContext(FList[AIndex]);
end;

function TExprSetContext.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TExprSetContext.Remove(AContext: TExprContext);
begin
  FList.Remove(AContext);
  if FOwnsContexts then
    AContext.Free;
end;

//=== { TExprSym } ===========================================================

constructor TExprSym.Create(const AIdent: string);
begin
  inherited Create;
  FIdent := AIdent;
end;

//=== { TExprLexer } =========================================================

constructor TExprLexer.Create;
begin
  inherited Create;
  Reset;
end;

procedure TExprLexer.Reset;
begin
  NextTok;
end;

//=== { TExprCompileParser } =================================================

constructor TExprCompileParser.Create(ALexer: TExprLexer; ANodeFactory: TExprNodeFactory);
begin
  inherited Create;
  FLexer := ALexer;
  FNodeFactory := ANodeFactory;
end;

function TExprCompileParser.Compile: TExprNode;
begin
  Result := CompileExprLevel0(False);
end;

function TExprCompileParser.CompileExprLevel0(ASkip: Boolean): TExprNode;
begin
  Result := CompileExprLevel1(ASkip);

  { Utilize some of these compound instructions to test DAG optimization
    techniques later on.

    Playing a few games after much hard work, too.
    Functional programming is fun! :-> BJK }
  while True do
    case Lexer.CurrTok of
      etEqualTo: // =
        Result := NodeFactory.CompareEqual(Result, CompileExprLevel1(True));
      etNotEqual: // <>
        Result := NodeFactory.CompareNotEqual(Result, CompileExprLevel1(True));
      etLessThan: // <
        Result := NodeFactory.CompareLess(Result, CompileExprLevel1(True));
      etLessEqual: // <=
        Result := NodeFactory.CompareLessEqual(Result, CompileExprLevel1(True));
      etGreaterThan: // >
        Result := NodeFactory.CompareGreater(Result, CompileExprLevel1(True));
      etGreaterEqual: // >=
        Result := NodeFactory.CompareGreaterEqual(Result, CompileExprLevel1(True));
      etIdentifier: // cmp
        if AnsiSameText(Lexer.TokenAsString, 'cmp') then
          Result := NodeFactory.Compare(Result, CompileExprLevel1(True))
        else
          Break;
    else
      Break;
    end;
end;

function TExprCompileParser.CompileExprLevel1(ASkip: Boolean): TExprNode;
begin
  Result := CompileExprLevel2(ASkip);

  while True do
    case Lexer.CurrTok of
      etPlus:
        Result := NodeFactory.Add(Result, CompileExprLevel2(True));
      etMinus:
        Result := NodeFactory.Subtract(Result, CompileExprLevel2(True));
      etIdentifier: // or, xor, bor, bxor
        if AnsiSameText(Lexer.TokenAsString, 'or') then
          Result := NodeFactory.LogicalOr(Result, CompileExprLevel2(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'xor') then
          Result := NodeFactory.LogicalXor(Result, CompileExprLevel2(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'bor') then
          Result := NodeFactory.BitwiseOr(Result, CompileExprLevel2(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'bxor') then
          Result := NodeFactory.BitwiseXor(Result, CompileExprLevel2(True))
        else
          Break;
    else
      Break;
    end;
end;

function TExprCompileParser.CompileExprLevel2(ASkip: Boolean): TExprNode;
begin
  Result := CompileExprLevel3(ASkip);

  while True do
    case Lexer.CurrTok of
      etAsterisk:
        Result := NodeFactory.Multiply(Result, CompileExprLevel3(True));
      etForwardSlash:
        Result := NodeFactory.Divide(Result, CompileExprLevel3(True));
      etIdentifier: // div, mod, and, shl, shr, band
        if AnsiSameText(Lexer.TokenAsString, 'div') then
          Result := NodeFactory.IntegerDivide(Result, CompileExprLevel3(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'mod') then
          Result := NodeFactory.Modulo(Result, CompileExprLevel3(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'and') then
          Result := NodeFactory.LogicalAnd(Result, CompileExprLevel3(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'shl') then
          Result := NodeFactory.ShiftLeft(Result, CompileExprLevel3(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'shr') then
          Result := NodeFactory.ShiftRight(Result, CompileExprLevel3(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'band') then
          Result := NodeFactory.BitwiseAnd(Result, CompileExprLevel3(True))
        else
          Break;
    else
      Break;
    end;
end;

function TExprCompileParser.CompileExprLevel3(ASkip: Boolean): TExprNode;
begin
  if ASkip then
    Lexer.NextTok;

  case Lexer.CurrTok of
    etPlus:
      Result := CompileExprLevel3(True);
    etMinus:
      Result := NodeFactory.Negate(CompileExprLevel3(True));
    etIdentifier: // not, bnot
      if AnsiSameText(Lexer.TokenAsString, 'not') then
        Result := NodeFactory.LogicalNot(CompileExprLevel3(True))
      else
      if AnsiSameText(Lexer.TokenAsString, 'bnot') then
        Result := NodeFactory.BitwiseNot(CompileExprLevel3(True))
      else
        Result := CompileFactor;
  else
    Result := CompileFactor;
  end;
end;

function TExprCompileParser.CompileFactor: TExprNode;
begin
  case Lexer.CurrTok of
    etIdentifier:
      Result := CompileIdentFactor;
    etLParen:
      begin
        Result := CompileExprLevel0(True);
        if Lexer.CurrTok <> etRParen then
          raise EJclExprEvalError.CreateRes(@RsExprEvalRParenExpected);
        Lexer.NextTok;
      end;
    etNumber:
      begin
        Result := NodeFactory.LoadConst(Lexer.TokenAsNumber);
        Lexer.NextTok;
      end;
  else
    raise EJclExprEvalError.CreateRes(@RsExprEvalFactorExpected);
  end;
end;

function TExprCompileParser.CompileIdentFactor: TExprNode;
var
  Sym: TExprSym;
  oldCompileParser: TExprCompileParser;
  oldLexer: TExprLexer;
  oldNodeFactory: TExprNodeFactory;
begin
  { find symbol }
  if FContext = nil then
    raise EJclExprEvalError.CreateResFmt(@RsExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);
  Sym := FContext.Find(Lexer.TokenAsString);
  if Sym = nil then
    raise EJclExprEvalError.CreateResFmt(@RsExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);

  Lexer.NextTok;

  { set symbol properties }
  oldCompileParser := Sym.CompileParser;
  oldLexer := Sym.Lexer;
  oldNodeFactory := Sym.NodeFactory;
  Sym.FLexer := Lexer;
  Sym.FCompileParser := Self;
  Sym.FNodeFactory := NodeFactory;
  try
    { compile symbol }
    Result := Sym.Compile;
  finally
    Sym.FLexer := oldLexer;
    Sym.FCompileParser := oldCompileParser;
    Sym.FNodeFactory := oldNodeFactory;
  end;
end;

//=== { TExprEvalParser } ====================================================

constructor TExprEvalParser.Create(ALexer: TExprLexer);
begin
  inherited Create;
  FLexer := ALexer;
end;

function TExprEvalParser.Evaluate: TFloat;
begin
  Result := EvalExprLevel0(False);

  if (Lexer.CurrTok <> etEof) then
  begin
    raise EJclExprEvalError.CreateResFmt(@RsExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);
  end;
end;

function TExprEvalParser.EvalExprLevel0(ASkip: Boolean): TFloat;
var
  RightValue: TFloat;
begin
  Result := EvalExprLevel1(ASkip);

  while True do
    case Lexer.CurrTok of
      etEqualTo: // =
        if Result = EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etNotEqual: // <>
        if Result <> EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etLessThan: // <
        if Result < EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etLessEqual: // <=
        if Result <= EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etGreaterThan: // >
        if Result > EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etGreaterEqual: // >=
        if Result >= EvalExprLevel1(True) then
          Result := 1.0
        else
          Result := 0.0;
      etIdentifier: // cmp
        if AnsiSameText(Lexer.TokenAsString, 'cmp') then
        begin
          RightValue := EvalExprLevel1(True);
          if Result > RightValue then
            Result := 1.0
          else
          if Result = RightValue then
            Result := 0.0
          else
            Result := -1.0;
        end
        else
          Break;
    else
      Break;
    end;
end;

function TExprEvalParser.EvalExprLevel1(ASkip: Boolean): TFloat;
begin
  Result := EvalExprLevel2(ASkip);

  while True do
    case Lexer.CurrTok of
      etPlus:
        Result := Result + EvalExprLevel2(True);
      etMinus:
        Result := Result - EvalExprLevel2(True);
      etIdentifier: // or, xor, bor, bxor
        if AnsiSameText(Lexer.TokenAsString, 'or') then
        begin
          if (EvalExprLevel2(True) <> 0) or (Result <> 0) then // prevent boolean optimisations, EvalTerm must be called
            Result := 1.0
          else
            Result := 0.0;
        end
        else
        if AnsiSameText(Lexer.TokenAsString, 'xor') then
        begin
          if (Result <> 0) xor (EvalExprLevel2(True) <> 0) then
            Result := 1.0
          else
            result := 0.0;
        end
        else
        if AnsiSameText(Lexer.TokenAsString, 'bor') then
          Result := Round(Result) or Round(EvalExprLevel2(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'bxor') then
          Result := Round(Result) xor Round(EvalExprLevel2(True))
        else
          Break;
    else
      Break;
    end;
end;

function TExprEvalParser.EvalExprLevel2(ASkip: Boolean): TFloat;
begin
  Result := EvalExprLevel3(ASkip);

  while True do
    case Lexer.CurrTok of
      etAsterisk:
        Result := Result * EvalExprLevel3(True);
      etForwardSlash:
        Result := Result / EvalExprLevel3(True);
      etIdentifier: // div, mod, and, shl, shr, band
        if AnsiSameText(Lexer.TokenAsString, 'div') then
          Result := Round(Result) div Round(EvalExprLevel3(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'mod') then
          Result := Round(Result) mod Round(EvalExprLevel3(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'and') then
        begin
          if (EvalExprLevel3(True) <> 0) and (Result <> 0) then // prevent boolean optimisations, EvalTerm must be called
            Result := 1.0
          else
            Result := 0.0;
        end
        else
        if AnsiSameText(Lexer.TokenAsString, 'shl') then
          Result := Round(Result) shl Round(EvalExprLevel3(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'shr') then
          Result := Round(Result) shr Round(EvalExprLevel3(True))
        else
        if AnsiSameText(Lexer.TokenAsString, 'band') then
          Result := Round(Result) and Round(EvalExprLevel3(True))
        else
          Break;
    else
      Break;
    end;
end;

function TExprEvalParser.EvalExprLevel3(ASkip: Boolean): TFloat;
begin
  if ASkip then
    Lexer.NextTok;

  case Lexer.CurrTok of
    etPlus:
      Result := EvalExprLevel3(True);
    etMinus:
      Result := -EvalExprLevel3(True);
    etIdentifier: // not, bnot
      if AnsiSameText(Lexer.TokenAsString, 'not') then
      begin
        if EvalExprLevel3(True) <> 0.0 then
          Result := 0.0
        else
          Result := 1.0;
      end
      else
      if AnsiSameText(Lexer.TokenAsString, 'bnot') then
        Result := not Round(EvalExprLevel3(True))
      else
        Result := EvalFactor;
  else
    Result := EvalFactor;
  end;
end;

function TExprEvalParser.EvalFactor: TFloat;
begin
  case Lexer.CurrTok of
    etIdentifier:
      Result := EvalIdentFactor;
    etLParen:
      begin
        Result := EvalExprLevel0(True);
        if Lexer.CurrTok <> etRParen then
          raise EJclExprEvalError.CreateRes(@RsExprEvalRParenExpected);
        Lexer.NextTok;
      end;
    etNumber:
      begin
        Result := Lexer.TokenAsNumber;
        Lexer.NextTok;
      end;
  else
    raise EJclExprEvalError.CreateRes(@RsExprEvalFactorExpected);
  end;
end;

function TExprEvalParser.EvalIdentFactor: TFloat;
var
  Sym: TExprSym;
  oldEvalParser: TExprEvalParser;
  oldLexer: TExprLexer;
begin
  { find symbol }
  if Context = nil then
    raise EJclExprEvalError.CreateResFmt(@RsExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);
  Sym := FContext.Find(Lexer.TokenAsString);
  if Sym = nil then
    raise EJclExprEvalError.CreateResFmt(@RsExprEvalUnknownSymbol,
      [Lexer.TokenAsString]);

  Lexer.NextTok;

  { set symbol properties }
  oldEvalParser := Sym.FEvalParser;
  oldLexer := Sym.Lexer;
  Sym.FLexer := Lexer;
  Sym.FEvalParser := Self;
  try
    { evaluate symbol }
    Result := Sym.Evaluate;
  finally
    Sym.FLexer := oldLexer;
    Sym.FEvalParser := oldEvalParser;
  end;
end;

//=== { TExprSimpleLexer } ===================================================

constructor TExprSimpleLexer.Create(const ABuf: string);
begin
  FBuf := ABuf;
  inherited Create;
end;

procedure TExprSimpleLexer.NextTok;
const
  CharToTokenMap: array [Char] of TExprToken =
  (
    {#0..#31}
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid, etInvalid,
    {#32} etInvalid,
    {#33} etBang, {#34} etDoubleQuote, {#35} etHash, {#36} etDollar,
    {#37} etPercent, {#38} etAmpersand, {#39} etSingleQuote, {#40} etLParen,
    {#41} etRParen, {#42} etAsterisk, {#43} etPlus, {#44} etComma,
    {#45} etMinus, {#46} etDot, {#47} etForwardSlash,
    // 48..57 - numbers...
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid,
    {#58} etColon, {#59} etSemiColon, {#60} etLessThan, {#61} etEqualTo,
    {#62} etGreaterThan, {#63} etQuestion, {#64} etAt,
    // 65..90 - capital letters...
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid,
    {#91} etLBracket, {#92} etBackSlash, {#93} etRBracket, {#94} etArrow,
    etInvalid, // 95 - underscore
    {#96} etBackTick,
    // 97..122 - small letters...
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid, etInvalid, etInvalid,
    etInvalid, etInvalid,
    {#123} etLBrace,
    {#124} etPipe, {#125} etRBrace, {#126} etTilde, {#127} et127,
    {#128} etEuro, {#129} et129, {#130} et130, {#131} et131,
    {#132} et132, {#133} et133, {#134} et134, {#135} et135,
    {#136} et136, {#137} et137, {#138} et138, {#139} et139,
    {#140} et140, {#141} et141, {#142} et142, {#143} et143,
    {#144} et144, {#145} et145, {#146} et146, {#147} et147,
    {#148} et148, {#149} et149, {#150} et150, {#151} et151,
    {#152} et152, {#153} et153, {#154} et154, {#155} et155,
    {#156} et156, {#157} et157, {#158} et158, {#159} et159,
    {#160} et160, {#161} et161, {#162} et162, {#163} et163,
    {#164} et164, {#165} et165, {#166} et166, {#167} et167,
    {#168} et168, {#169} et169, {#170} et170, {#171} et171,
    {#172} et172, {#173} et173, {#174} et174, {#175} et175,
    {#176} et176, {#177} et177, {#178} et178, {#179} et179,
    {#180} et180, {#181} et181, {#182} et182, {#183} et183,
    {#184} et184, {#185} et185, {#186} et186, {#187} et187,
    {#188} et188, {#189} et189, {#190} et190, {#191} et191,
    {#192} et192, {#193} et193, {#194} et194, {#195} et195,
    {#196} et196, {#197} et197, {#198} et198, {#199} et199,
    {#200} et200, {#201} et201, {#202} et202, {#203} et203,
    {#204} et204, {#205} et205, {#206} et206, {#207} et207,
    {#208} et208, {#209} et209, {#210} et210, {#211} et211,
    {#212} et212, {#213} et213, {#214} et214, {#215} et215,
    {#216} et216, {#217} et217, {#218} et218, {#219} et219,
    {#220} et220, {#221} et221, {#222} et222, {#223} et223,
    {#224} et224, {#225} et225, {#226} et226, {#227} et227,
    {#228} et228, {#229} et229, {#230} et230, {#231} et231,
    {#232} et232, {#233} et233, {#234} et234, {#235} et235,
    {#236} et236, {#237} et237, {#238} et238, {#239} et239,
    {#240} et240, {#241} et241, {#242} et242, {#243} et243,
    {#244} et244, {#245} et245, {#246} et246, {#247} et247,
    {#248} et248, {#249} et249, {#250} et250, {#251} et251,
    {#252} et252, {#253} et253, {#254} et254, {#255} et255
  );
var
  { register variable optimization }
  cp: PChar;
  start: PChar;
begin
  cp := FCurrPos;

  { skip whitespace }
  while cp^ in ExprWhiteSpace do
    Inc(cp);

  { determine token type }
  case cp^ of
    #0:
      FCurrTok := etEof;
    'a'..'z', 'A'..'Z', '_':
      begin
        start := cp;
        Inc(cp);
        while cp^ in ['0'..'9', 'a'..'z', 'A'..'Z', '_'] do
          Inc(cp);
        SetString(FTokenAsString, start, cp - start);
        FCurrTok := etIdentifier;
      end;
    '0'..'9':
      begin
        start := cp;

        { read in integer part of mantissa }
        while cp^ in ['0'..'9'] do
          Inc(cp);

        { check for and read in fraction part of mantissa }
        if (cp^ = '.') or (cp^ = DecimalSeparator) then
        begin
          Inc(cp);
          while cp^ in ['0'..'9'] do
            Inc(cp);
        end;

        { check for and read in exponent }
        if cp^ in ['e', 'E'] then
        begin
          Inc(cp);
          if cp^ in ['+', '-'] then
            Inc(cp);
          while cp^ in ['0'..'9'] do
            Inc(cp);
        end;

        { evaluate number }
        SetString(FTokenAsString, start, cp - start);
        FTokenAsNumber := StrToFloat(FTokenAsString);

        FCurrTok := etNumber;
      end;
    '<':
      begin
        Inc(cp);
        case cp^ of
          '=':
            begin
              FCurrTok := etLessEqual;
              Inc(cp);
            end;
          '>':
            begin
              FCurrTok := etNotEqual;
              Inc(cp);
            end;
        else
          FCurrTok := etLessThan;
        end;
      end;
    '>':
      begin
        Inc(cp);
        if cp^ = '=' then
        begin
          FCurrTok := etGreaterEqual;
          Inc(cp);
        end
        else
          FCurrTok := etGreaterThan;
      end;
  else
    { map character to token }
    FCurrTok := CharToTokenMap[cp^];
    Inc(cp);
  end;

  FCurrPos := cp;
end;

procedure TExprSimpleLexer.Reset;
begin
  FCurrPos := PChar(FBuf);
  inherited Reset;
end;

procedure TExprSimpleLexer.SetBuf(const ABuf: string);
begin
  FBuf := ABuf;
  Reset;
end;

//=== { TExprNode } ==========================================================

constructor TExprNode.Create(const ADepList: array of TExprNode);
var
  I: Integer;
begin
  inherited Create;
  FDepList := TList.Create;
  for I := Low(ADepList) to High(ADepList) do
    AddDep(ADepList[I]);
end;

destructor TExprNode.Destroy;
begin
  FDepList.Free;
  inherited Destroy;
end;

procedure TExprNode.AddDep(ADep: TExprNode);
begin
  FDepList.Add(ADep);
end;

function TExprNode.GetDepCount: Integer;
begin
  Result := FDepList.Count;
end;

function TExprNode.GetDeps(AIndex: Integer): TExprNode;
begin
  Result := TExprNode(FDepList[AIndex]);
end;

//=== { TExprNodeFactory } ===================================================

function TExprNodeFactory.LoadVar(ALoc: PFloat32): TExprNode;
begin
  Result := LoadVar32(ALoc);
end;

function TExprNodeFactory.LoadVar(ALoc: PFloat64): TExprNode;
begin
  Result := LoadVar64(ALoc);
end;

function TExprNodeFactory.LoadVar(ALoc: PFloat80): TExprNode;
begin
  Result := LoadVar80(ALoc);
end;

function TExprNodeFactory.LoadConst(AValue: TFloat32): TExprNode;
begin
  Result := LoadConst32(AValue);
end;

function TExprNodeFactory.LoadConst(AValue: TFloat64): TExprNode;
begin
  Result := LoadConst64(AValue);
end;

function TExprNodeFactory.LoadConst(AValue: TFloat80): TExprNode;
begin
  Result := LoadConst80(AValue);
end;

//=== { TEvaluator } =========================================================

constructor TEvaluator.Create;
begin
  inherited Create;

  FLexer := TExprSimpleLexer.Create('');
  FParser := TExprEvalParser.Create(FLexer);

  FParser.Context := InternalContextSet;
end;

destructor TEvaluator.Destroy;
begin
  FParser.Free;
  FLexer.Free;
  inherited Destroy;
end;

function TEvaluator.Evaluate(const AExpr: string): TFloat;
begin
  FLexer.Buf := AExpr;
  Result := FParser.Evaluate;
end;

//=== { TExprVirtMachOp } ====================================================

function TExprVirtMachOp.GetOutputLoc: PFloat;
begin
  Result := @FOutput;
end;

//=== Virtual machine operators follow =======================================

type
  { abstract base for var readers }
  TExprVarVmOp = class(TExprVirtMachOp)
  private
    FVarLoc: Pointer;
  public
    constructor Create(AVarLoc: Pointer);
  end;

  TExprVarVmOpClass = class of TExprVarVmOp;

  { the var readers }

  TExprVar32VmOp = class(TExprVarVmOp)
  public
    procedure Execute; override;
  end;

  TExprVar64VmOp = class(TExprVarVmOp)
  public
    procedure Execute; override;
  end;

  TExprVar80VmOp = class(TExprVarVmOp)
  public
    procedure Execute; override;
  end;

  { the const holder }
  TExprConstVmOp = class(TExprVirtMachOp)
  public
    constructor Create(AValue: TFloat);
    { null function }
    procedure Execute; override;
  end;

  { abstract unary operator }
  TExprUnaryVmOp = class(TExprVirtMachOp)
  protected
    FInput: PFloat;
  public
    constructor Create(AInput: PFloat);
    property Input: PFloat read FInput write FInput;
  end;

  TExprUnaryVmOpClass = class of TExprUnaryVmOp;

  { abstract binary operator }
  TExprBinaryVmOp = class(TExprVirtMachOp)
  protected
    FLeft: PFloat;
    FRight: PFloat;
  public
    constructor Create(ALeft, ARight: PFloat);
    property Left: PFloat read FLeft write FLeft;
    property Right: PFloat read FRight write FRight;
  end;

  TExprBinaryVmOpClass = class of TExprBinaryVmOp;

  { the 4 basic binary operators }

  TExprAddVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprSubtractVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprMultiplyVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprDivideVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprCompareVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprGreaterVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprGreaterEqualVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprLessVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprLessEqualVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprEqualVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprNotEqualVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprIntegerDivideVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprModuloVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprShiftLeftVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprShiftRightVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprBitwiseAndVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprBitwiseOrVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprBitwiseXorVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprLogicalAndVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprLogicalOrVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprLogicalXorVmOp = class(TExprBinaryVmOp)
  public
    procedure Execute; override;
  end;

  { the unary operators }

  TExprNegateVmOp = class(TExprUnaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprLogicalNotVmOp = class(TExprUnaryVmOp)
  public
    procedure Execute; override;
  end;

  TExprBitwiseNotVmOp = class(TExprUnaryVmOp)
  public
    procedure Execute; override;
  end;

  { function calls }

  TExprCallFloatVmOp = class(TExprVirtMachOp)
  private
    FFunc: TFloatFunc;
  public
    constructor Create(AFunc: TFloatFunc);
    procedure Execute; override;
  end;

  TExprCallFloat32VmOp = class(TExprVirtMachOp)
  private
    FFunc: TFloat32Func;
  public
    constructor Create(AFunc: TFloat32Func);
    procedure Execute; override;
  end;

  TExprCallFloat64VmOp = class(TExprVirtMachOp)
  private
    FFunc: TFloat64Func;
  public
    constructor Create(AFunc: TFloat64Func);
    procedure Execute; override;
  end;

  TExprCallFloat80VmOp = class(TExprVirtMachOp)
  private
    FFunc: TFloat80Func;
  public
    constructor Create(AFunc: TFloat80Func);
    procedure Execute; override;
  end;

  TExprCallUnaryVmOp = class(TExprVirtMachOp)
  private
    FFunc: TUnaryFunc;
    FX: PFloat;
  public
    constructor Create(AFunc: TUnaryFunc; X: PFloat);
    procedure Execute; override;
  end;

  TExprCallUnary32VmOp = class(TExprVirtMachOp)
  private
    FFunc: TUnary32Func;
    FX: PFloat;
  public
    constructor Create(AFunc: TUnary32Func; X: PFloat);
    procedure Execute; override;
  end;

  TExprCallUnary64VmOp = class(TExprVirtMachOp)
  private
    FFunc: TUnary64Func;
    FX: PFloat;
  public
    constructor Create(AFunc: TUnary64Func; X: PFloat);
    procedure Execute; override;
  end;

  TExprCallUnary80VmOp = class(TExprVirtMachOp)
  private
    FFunc: TUnary80Func;
    FX: PFloat;
  public
    constructor Create(AFunc: TUnary80Func; X: PFloat);
    procedure Execute; override;
  end;

  TExprCallBinaryVmOp = class(TExprVirtMachOp)
  private
    FFunc: TBinaryFunc;
    FX, FY: PFloat;
  public
    constructor Create(AFunc: TBinaryFunc; X, Y: PFloat);
    procedure Execute; override;
  end;

  TExprCallBinary32VmOp = class(TExprVirtMachOp)
  private
    FFunc: TBinary32Func;
    FX, FY: PFloat;
  public
    constructor Create(AFunc: TBinary32Func; X, Y: PFloat);
    procedure Execute; override;
  end;

  TExprCallBinary64VmOp = class(TExprVirtMachOp)
  private
    FFunc: TBinary64Func;
    FX, FY: PFloat;
  public
    constructor Create(AFunc: TBinary64Func; X, Y: PFloat);
    procedure Execute; override;
  end;

  TExprCallBinary80VmOp = class(TExprVirtMachOp)
  private
    FFunc: TBinary80Func;
    FX, FY: PFloat;
  public
    constructor Create(AFunc: TBinary80Func; X, Y: PFloat);
    procedure Execute; override;
  end;

  TExprCallTernaryVmOp = class(TExprVirtMachOp)
  private
    FFunc: TTernaryFunc;
    FX, FY, FZ: PFloat;
  public
    constructor Create(AFunc: TTernaryFunc; X, Y, Z: PFloat);
    procedure Execute; override;
  end;

  TExprCallTernary32VmOp = class(TExprVirtMachOp)
  private
    FFunc: TTernary32Func;
    FX, FY, FZ: PFloat;
  public
    constructor Create(AFunc: TTernary32Func; X, Y, Z: PFloat);
    procedure Execute; override;
  end;

  TExprCallTernary64VmOp = class(TExprVirtMachOp)
  private
    FFunc: TTernary64Func;
    FX, FY, FZ: PFloat;
  public
    constructor Create(AFunc: TTernary64Func; X, Y, Z: PFloat);
    procedure Execute; override;
  end;

  TExprCallTernary80VmOp = class(TExprVirtMachOp)
  private
    FFunc: TTernary80Func;
    FX, FY, FZ: PFloat;
  public
    constructor Create(AFunc: TTernary80Func; X, Y, Z: PFloat);
    procedure Execute; override;
  end;

//=== { TExprVar32VmOp } =====================================================

procedure TExprVar32VmOp.Execute;
begin
  FOutput := PFloat32(FVarLoc)^;
end;

//=== { TExprVar64VmOp } =====================================================

procedure TExprVar64VmOp.Execute;
begin
  FOutput := PFloat64(FVarLoc)^;
end;

//=== { TExprVar80VmOp } =====================================================

procedure TExprVar80VmOp.Execute;
begin
  FOutput := PFloat80(FVarLoc)^;
end;

//=== { TExprConstVmOp } =====================================================

constructor TExprConstVmOp.Create(AValue: TFloat);
begin
  inherited Create;
  FOutput := AValue;
end;

procedure TExprConstVmOp.Execute;
begin
end;

//=== { TExprUnaryVmOp } =====================================================

constructor TExprUnaryVmOp.Create(AInput: PFloat);
begin
  inherited Create;
  FInput := AInput;
end;

//=== { TExprBinaryVmOp } ====================================================

constructor TExprBinaryVmOp.Create(ALeft, ARight: PFloat);
begin
  inherited Create;
  FLeft := ALeft;
  FRight := ARight;
end;

//=== { TExprAddVmOp } =======================================================
procedure TExprAddVmOp.Execute;
begin
  FOutput := FLeft^ + FRight^;
end;

//=== { TExprSubtractVmOp } ==================================================

procedure TExprSubtractVmOp.Execute;
begin
  FOutput := FLeft^ - FRight^;
end;

//=== { TExprMultiplyVmOp } ==================================================

procedure TExprMultiplyVmOp.Execute;
begin
  FOutput := FLeft^ * FRight^;
end;

//=== { TExprDivideVmOp } ====================================================

procedure TExprDivideVmOp.Execute;
begin
  FOutput := FLeft^ / FRight^;
end;

//=== { TExprCompareVmOp } ===================================================

procedure TExprCompareVmOp.Execute;
begin
  if FLeft^ < FRight^ then
    FOutput := -1.0
  else
  if FLeft^ > FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

//=== { TExprCmpGreaterVmOp } ================================================

procedure TExprGreaterVmOp.Execute;
begin
  if FLeft^ > FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

//=== { TExprCmpGreaterEqualVmOp } ===========================================

procedure TExprGreaterEqualVmOp.Execute;
begin
  if FLeft^ >= FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

//=== { TExprCmpLessVmOp } ===================================================

procedure TExprLessVmOp.Execute;
begin
  if FLeft^ < FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

// === { TExprCmpLessEqualVmOp } =============================================

procedure TExprLessEqualVmOp.Execute;
begin
  if FLeft^ <= FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

//=== { TExprCmpEqualVmOp } ==================================================

procedure TExprEqualVmOp.Execute;
begin
  if FLeft^ = FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

//=== { TExprCmpNotEqualVmOp } ===============================================

procedure TExprNotEqualVmOp.Execute;
begin
  if FLeft^ <> FRight^ then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

//=== { TExprDivVmOp } =======================================================

procedure TExprIntegerDivideVmOp.Execute;
begin
  FOutput := Round(FLeft^) div Round(FRight^);
end;

//=== { TExprModVmOp } =======================================================

procedure TExprModuloVmOp.Execute;
begin
  FOutput := Round(FLeft^) mod Round(FRight^);
end;

//=== { TExprShiftLeftVmOp } =================================================

procedure TExprShiftLeftVmOp.Execute;
begin
  FOutput := Round(FLeft^) shl Round(FRight^);
end;

//=== { TExprShiftRightVmOp } ================================================

procedure TExprShiftRightVmOp.Execute;
begin
  FOutput := Round(FLeft^) shr Round(FRight^);
end;

//=== { TExprBitwiseAndVmOp } ================================================

procedure TExprBitwiseAndVmOp.Execute;
begin
  FOutput := Round(FLeft^) and Round(FRight^);
end;

//=== { TExprOrVmOp } ========================================================

procedure TExprBitwiseOrVmOp.Execute;
begin
  FOutput := Round(FLeft^) or Round(FRight^);
end;

//=== { TExprXorVmOp } =======================================================

procedure TExprBitwiseXorVmOp.Execute;
begin
  FOutput := Round(FLeft^) xor Round(FRight^);
end;

//=== { TExprLogicalAndVmOp } ================================================

procedure TExprLogicalAndVmOp.Execute;
begin
  if (FLeft^ <> 0.0) and (FRight^ <> 0) then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

//=== { TExprLogicalOrVmOp } =================================================

procedure TExprLogicalOrVmOp.Execute;
begin
  if (FLeft^ <> 0.0) or (FRight^ <> 0) then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

//=== { TExprLogicalXorVmOp } ================================================

procedure TExprLogicalXorVmOp.Execute;
begin
  if (FLeft^ <> 0.0) xor (FRight^ <> 0) then
    FOutput := 1.0
  else
    FOutput := 0.0;
end;

//=== { TExprNegateVmOp } ====================================================

procedure TExprNegateVmOp.Execute;
begin
  FOutput := - FInput^;
end;

//=== { TExprLogicalNotVmOp } ================================================

procedure TExprLogicalNotVmOp.Execute;
begin
  if FInput^ <> 0.0 then
    FOutput := 0.0
  else
    FOutput := 1.0;
end;

//=== { TExprBitwiseNotVmOp } ================================================

procedure TExprBitwiseNotVmOp.Execute;
begin
  FOutput := not Round(FInput^);
end;

//=== { TExprVarVmOp } =======================================================

constructor TExprVarVmOp.Create(AVarLoc: Pointer);
begin
  inherited Create;
  FVarLoc := AVarLoc;
end;

//=== { TExprCallFloatVmOp } =================================================

constructor TExprCallFloatVmOp.Create(AFunc: TFloatFunc);
begin
  inherited Create;
  FFunc := AFunc;
end;

procedure TExprCallFloatVmOp.Execute;
begin
  FOutput := FFunc;
end;

//=== { TExprCallFloat32VmOp } ===============================================

constructor TExprCallFloat32VmOp.Create(AFunc: TFloat32Func);
begin
  inherited Create;
  FFunc := AFunc;
end;

procedure TExprCallFloat32VmOp.Execute;
begin
  FOutput := FFunc;
end;

//=== { TExprCallFloat64VmOp } ===============================================

constructor TExprCallFloat64VmOp.Create(AFunc: TFloat64Func);
begin
  inherited Create;
  FFunc := AFunc;
end;

procedure TExprCallFloat64VmOp.Execute;
begin
  FOutput := FFunc;
end;

//=== { TExprCallFloat80VmOp } ===============================================

constructor TExprCallFloat80VmOp.Create(AFunc: TFloat80Func);
begin
  inherited Create;
  FFunc := AFunc;
end;

procedure TExprCallFloat80VmOp.Execute;
begin
  FOutput := FFunc;
end;

//=== { TExprCallUnaryVmOp } =================================================

constructor TExprCallUnaryVmOp.Create(AFunc: TUnaryFunc; X: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
end;

procedure TExprCallUnaryVmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

//=== { TExprCallUnary32VmOp } ===============================================

constructor TExprCallUnary32VmOp.Create(AFunc: TUnary32Func; X: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
end;

procedure TExprCallUnary32VmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

//=== { TExprCallUnary64VmOp } ===============================================

constructor TExprCallUnary64VmOp.Create(AFunc: TUnary64Func; X: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
end;

procedure TExprCallUnary64VmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

//=== { TExprCallUnary80VmOp } ===============================================

constructor TExprCallUnary80VmOp.Create(AFunc: TUnary80Func; X: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
end;

procedure TExprCallUnary80VmOp.Execute;
begin
  FOutput := FFunc(FX^);
end;

//=== { TExprCallBinaryVmOp } ================================================

constructor TExprCallBinaryVmOp.Create(AFunc: TBinaryFunc; X, Y: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
end;

procedure TExprCallBinaryVmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

//=== { TExprCallBinary32VmOp } ==============================================

constructor TExprCallBinary32VmOp.Create(AFunc: TBinary32Func; X, Y: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
end;

procedure TExprCallBinary32VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

//=== { TExprCallBinary64VmOp } ==============================================

constructor TExprCallBinary64VmOp.Create(AFunc: TBinary64Func; X, Y: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
end;

procedure TExprCallBinary64VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

//=== { TExprCallBinary80VmOp } ==============================================

constructor TExprCallBinary80VmOp.Create(AFunc: TBinary80Func; X, Y: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
end;

procedure TExprCallBinary80VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^);
end;

//=== { TExprCallTernaryVmOp } ===============================================

constructor TExprCallTernaryVmOp.Create(AFunc: TTernaryFunc; X, Y, Z: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
  FZ := Z;
end;

procedure TExprCallTernaryVmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

//=== { TExprCallTernary32VmOp } =============================================

constructor TExprCallTernary32VmOp.Create(AFunc: TTernary32Func; X, Y, Z: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
  FZ := Z;
end;

procedure TExprCallTernary32VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

//=== { TExprCallTernary64VmOp } =============================================

constructor TExprCallTernary64VmOp.Create(AFunc: TTernary64Func; X, Y, Z: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
  FZ := Z;
end;

procedure TExprCallTernary64VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

//=== { TExprCallTernary80VmOp } =============================================

constructor TExprCallTernary80VmOp.Create(AFunc: TTernary80Func; X, Y, Z: PFloat);
begin
  inherited Create;
  FFunc := AFunc;
  FX := X;
  FY := Y;
  FZ := Z;
end;

procedure TExprCallTernary80VmOp.Execute;
begin
  FOutput := FFunc(FX^, FY^, FZ^);
end;

{ End of virtual machine operators }

//=== { TExprVirtMach } ======================================================

constructor TExprVirtMach.Create;
begin
  inherited Create;
  FCodeList := TList.Create;
  FConstList := TList.Create;
end;

destructor TExprVirtMach.Destroy;
begin
  FreeObjectList(FCodeList);
  FreeObjectList(FConstList);
  inherited Destroy;
end;

function TExprVirtMach.Execute: TFloat;
type
  PExprVirtMachOp = ^TExprVirtMachOp;
var
  I: Integer;
  pop: PExprVirtMachOp;
begin
  if FCodeList.Count <> 0 then
  begin
    { The code that follows is the same as this, but a lot faster
    for I := 0 to FCodeList.Count - 1 do
      TExprVirtMachOp(FCodeList[I]).Execute; }
    I := FCodeList.Count;
    pop := @FCodeList.List^[0];
    while I > 0 do
    begin
      pop^.Execute;
      Inc(pop);
      Dec(I);
    end;
    Result := TExprVirtMachOp(FCodeList[FCodeList.Count - 1]).FOutput;
  end
  else
    begin
      if (FConstList.Count = 1) then
        Result := TExprVirtMachOp(FConstList[0]).FOutput
      else
        Result := 0;
    end;
end;

procedure TExprVirtMach.Add(AOp: TExprVirtMachOp);
begin
  FCodeList.Add(AOp);
end;

procedure TExprVirtMach.AddConst(AOp: TExprVirtMachOp);
begin
  FConstList.Add(AOp);
end;

procedure TExprVirtMach.Clear;
begin
  ClearObjectList(FCodeList);
  ClearObjectList(FConstList);
end;

//=== { TExprVirtMachNode } ==================================================

type
  TExprVirtMachNode = class(TExprNode)
  private
    FExprVmCode: TExprVirtMachOp;
    function GetVmDeps(AIndex: Integer): TExprVirtMachNode;
  public
    procedure GenCode(AVirtMach: TExprVirtMach); virtual; abstract;

    property ExprVmCode: TExprVirtMachOp read FExprVmCode;

    { this property saves typecasting to access ExprVmCode }
    property VmDeps[AIndex: Integer]: TExprVirtMachNode read GetVmDeps; default;
  end;

function TExprVirtMachNode.GetVmDeps(AIndex: Integer): TExprVirtMachNode;
begin
  Result := TExprVirtMachNode(FDepList[AIndex]);
end;

//=== Concrete expression nodes for virtual machine ==========================

type
  TExprUnaryVmNode = class(TExprVirtMachNode)
  private
    FUnaryClass: TExprUnaryVmOpClass;
  public
    constructor Create(AUnaryClass: TExprUnaryVmOpClass;
      const ADeps: array of TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprBinaryVmNode = class(TExprVirtMachNode)
  private
    FBinaryClass: TExprBinaryVmOpClass;
  public
    constructor Create(ABinaryClass: TExprBinaryVmOpClass;
      const ADeps: array of TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprConstVmNode = class(TExprVirtMachNode)
  private
    FValue: TFloat;
  public
    constructor Create(AValue: TFloat);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprVar32VmNode = class(TExprVirtMachNode)
  private
    FValue: PFloat32;
  public
    constructor Create(AValue: PFloat32);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprVar64VmNode = class(TExprVirtMachNode)
  private
    FValue: PFloat64;
  public
    constructor Create(AValue: PFloat64);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprVar80VmNode = class(TExprVirtMachNode)
  private
    FValue: PFloat80;
  public
    constructor Create(AValue: PFloat80);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallFloatVmNode = class(TExprVirtMachNode)
  private
    FFunc: TFloatFunc;
  public
    constructor Create(AFunc: TFloatFunc);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallFloat32VmNode = class(TExprVirtMachNode)
  private
    FFunc: TFloat32Func;
  public
    constructor Create(AFunc: TFloat32Func);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallFloat64VmNode = class(TExprVirtMachNode)
  private
    FFunc: TFloat64Func;
  public
    constructor Create(AFunc: TFloat64Func);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallFloat80VmNode = class(TExprVirtMachNode)
  private
    FFunc: TFloat80Func;
  public
    constructor Create(AFunc: TFloat80Func);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallUnaryVmNode = class(TExprVirtMachNode)
  private
    FFunc: TUnaryFunc;
  public
    constructor Create(AFunc: TUnaryFunc; X: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallUnary32VmNode = class(TExprVirtMachNode)
  private
    FFunc: TUnary32Func;
  public
    constructor Create(AFunc: TUnary32Func; X: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallUnary64VmNode = class(TExprVirtMachNode)
  private
    FFunc: TUnary64Func;
  public
    constructor Create(AFunc: TUnary64Func; X: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallUnary80VmNode = class(TExprVirtMachNode)
  private
    FFunc: TUnary80Func;
  public
    constructor Create(AFunc: TUnary80Func; X: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallBinaryVmNode = class(TExprVirtMachNode)
  private
    FFunc: TBinaryFunc;
  public
    constructor Create(AFunc: TBinaryFunc; X, Y: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallBinary32VmNode = class(TExprVirtMachNode)
  private
    FFunc: TBinary32Func;
  public
    constructor Create(AFunc: TBinary32Func; X, Y: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallBinary64VmNode = class(TExprVirtMachNode)
  private
    FFunc: TBinary64Func;
  public
    constructor Create(AFunc: TBinary64Func; X, Y: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallBinary80VmNode = class(TExprVirtMachNode)
  private
    FFunc: TBinary80Func;
  public
    constructor Create(AFunc: TBinary80Func; X, Y: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallTernaryVmNode = class(TExprVirtMachNode)
  private
    FFunc: TTernaryFunc;
  public
    constructor Create(AFunc: TTernaryFunc; X, Y, Z: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallTernary32VmNode = class(TExprVirtMachNode)
  private
    FFunc: TTernary32Func;
  public
    constructor Create(AFunc: TTernary32Func; X, Y, Z: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallTernary64VmNode = class(TExprVirtMachNode)
  private
    FFunc: TTernary64Func;
  public
    constructor Create(AFunc: TTernary64Func; X, Y, Z: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

  TExprCallTernary80VmNode = class(TExprVirtMachNode)
  private
    FFunc: TTernary80Func;
  public
    constructor Create(AFunc: TTernary80Func; X, Y, Z: TExprNode);
    procedure GenCode(AVirtMach: TExprVirtMach); override;
  end;

//== { TExprUnaryVmNode } ====================================================

constructor TExprUnaryVmNode.Create(AUnaryClass: TExprUnaryVmOpClass; const ADeps: array of TExprNode);
begin
  FUnaryClass := AUnaryClass;
  inherited Create(ADeps);
  Assert(FDepList.Count = 1);
end;

procedure TExprUnaryVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := FUnaryClass.Create(VmDeps[0].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprBinaryVmNode } ==================================================

constructor TExprBinaryVmNode.Create(ABinaryClass: TExprBinaryVmOpClass; const ADeps: array of TExprNode);
begin
  FBinaryClass := ABinaryClass;
  inherited Create(ADeps);
  Assert(FDepList.Count = 2);
end;

procedure TExprBinaryVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := FBinaryClass.Create(
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== {  TExprConstVmNode } ==================================================

constructor TExprConstVmNode.Create(AValue: TFloat);
begin
  FValue := AValue;
  inherited Create([]);
end;

procedure TExprConstVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprConstVmOp.Create(FValue);
  AVirtMach.AddConst(FExprVmCode);
end;

//=== { TExprVar32VmNode } ===================================================

constructor TExprVar32VmNode.Create(AValue: PFloat32);
begin
  FValue := AValue;
  inherited Create([]);
end;

procedure TExprVar32VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprVar32VmOp.Create(FValue);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprVar64VmNode } ===================================================

constructor TExprVar64VmNode.Create(AValue: PFloat64);
begin
  FValue := AValue;
  inherited Create([]);
end;

procedure TExprVar64VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprVar64VmOp.Create(FValue);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprVar80VmNode } ===================================================

constructor TExprVar80VmNode.Create(AValue: PFloat80);
begin
  FValue := AValue;
  inherited Create([]);
end;

procedure TExprVar80VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprVar80VmOp.Create(FValue);
  AVirtMach.Add(FExprVmCode);
end;

{ End of expression nodes for virtual machine }

//=== { TExprVirtMachNodeFactory } ===========================================

constructor TExprVirtMachNodeFactory.Create;
begin
  inherited Create;
  FNodeList := TList.Create;
end;

destructor TExprVirtMachNodeFactory.Destroy;
begin
  FreeObjectList(FNodeList);
  inherited Destroy;
end;

function TExprVirtMachNodeFactory.AddNode(ANode: TExprNode): TExprNode;
begin
  Result := ANode;
  FNodeList.Add(ANode);
end;

procedure TExprVirtMachNodeFactory.GenCode(AVirtMach: TExprVirtMach);
begin
  { TODO : optimize the expression tree into a DAG (i.e. find CSEs) and
    evaluate constant subexpressions, implement strength reduction, etc. }

  { TODO : move optimization logic (as far as possible) into ancestor classes
    once tested and interfaces are solid, so that other evaluation strategies
    can take advantage of these optimizations. }

  DoClean(AVirtMach);
  DoConsts(AVirtMach);
  DoCode(AVirtMach);
end;

function TExprVirtMachNodeFactory.LoadVar32(ALoc: PFloat32): TExprNode;
begin
  Result := AddNode(TExprVar32VmNode.Create(ALoc));
end;

function TExprVirtMachNodeFactory.LoadVar64(ALoc: PFloat64): TExprNode;
begin
  Result := AddNode(TExprVar64VmNode.Create(ALoc));
end;

function TExprVirtMachNodeFactory.LoadVar80(ALoc: PFloat80): TExprNode;
begin
  Result := AddNode(TExprVar80VmNode.Create(ALoc));
end;

function TExprVirtMachNodeFactory.LoadConst32(AValue: TFloat32): TExprNode;
begin
  Result := AddNode(TExprConstVmNode.Create(AValue));
end;

function TExprVirtMachNodeFactory.LoadConst64(AValue: TFloat64): TExprNode;
begin
  Result := AddNode(TExprConstVmNode.Create(AValue));
end;

function TExprVirtMachNodeFactory.LoadConst80(AValue: TFloat80): TExprNode;
begin
  Result := AddNode(TExprConstVmNode.Create(AValue));
end;

function TExprVirtMachNodeFactory.Add(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprAddVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.Subtract(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprSubtractVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.Multiply(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprMultiplyVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.Divide(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprDivideVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.IntegerDivide(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprIntegerDivideVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.Modulo(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprModuloVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.Negate(AValue: TExprNode): TExprNode;
begin
  Result := AddNode(TExprUnaryVmNode.Create(TExprNegateVmOp, [AValue]));
end;

procedure TExprVirtMachNodeFactory.DoClean(AVirtMach: TExprVirtMach);
var
  I: Integer;
begin
  { clean up in preparation for code generation }
  AVirtMach.Clear;
  for I := 0 to FNodeList.Count - 1 do
    TExprVirtMachNode(FNodeList[I]).FExprVmCode := nil;
end;

procedure TExprVirtMachNodeFactory.DoConsts(AVirtMach: TExprVirtMach);
var
  I: Integer;
  Node: TExprVirtMachNode;
begin
  { process consts }
  for I := 0 to FNodeList.Count - 1 do
  begin
    Node := TExprVirtMachNode(FNodeList[I]);
    if (Node is TExprConstVmNode) and (Node.ExprVmCode = nil) then
      Node.GenCode(AVirtMach);
  end;
end;

procedure TExprVirtMachNodeFactory.DoCode(AVirtMach: TExprVirtMach);
var
  I: Integer;
  Node: TExprVirtMachNode;
begin
  { process code }
  for I := 0 to FNodeList.Count - 1 do
  begin
    Node := TExprVirtMachNode(FNodeList[I]);
    if Node.ExprVmCode = nil then
      Node.GenCode(AVirtMach);
  end;
end;

function TExprVirtMachNodeFactory.CallFloatFunc(AFunc: TFloatFunc): TExprNode;
begin
  Result := AddNode(TExprCallFloatVmNode.Create(AFunc));
end;

function TExprVirtMachNodeFactory.CallFloat32Func(AFunc: TFloat32Func): TExprNode;
begin
  Result := AddNode(TExprCallFloat32VmNode.Create(AFunc));
end;

function TExprVirtMachNodeFactory.CallFloat64Func(AFunc: TFloat64Func): TExprNode;
begin
  Result := AddNode(TExprCallFloat64VmNode.Create(AFunc));
end;

function TExprVirtMachNodeFactory.CallFloat80Func(AFunc: TFloat80Func): TExprNode;
begin
  Result := AddNode(TExprCallFloat80VmNode.Create(AFunc));
end;

function TExprVirtMachNodeFactory.CallUnaryFunc(AFunc: TUnaryFunc; X: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallUnaryVmNode.Create(AFunc, X));
end;

function TExprVirtMachNodeFactory.CallUnary32Func(AFunc: TUnary32Func; X: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallUnary32VmNode.Create(AFunc, X));
end;

function TExprVirtMachNodeFactory.CallUnary64Func(AFunc: TUnary64Func; X: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallUnary64VmNode.Create(AFunc, X));
end;

function TExprVirtMachNodeFactory.CallUnary80Func(AFunc: TUnary80Func; X: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallUnary80VmNode.Create(AFunc, X));
end;

function TExprVirtMachNodeFactory.CallBinaryFunc(AFunc: TBinaryFunc; X, Y: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallBinaryVmNode.Create(AFunc, X, Y));
end;

function TExprVirtMachNodeFactory.CallBinary32Func(AFunc: TBinary32Func; X, Y: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallBinary32VmNode.Create(AFunc, X, Y));
end;

function TExprVirtMachNodeFactory.CallBinary64Func(AFunc: TBinary64Func; X, Y: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallBinary64VmNode.Create(AFunc, X, Y));
end;

function TExprVirtMachNodeFactory.CallBinary80Func(AFunc: TBinary80Func; X, Y: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallBinary80VmNode.Create(AFunc, X, Y));
end;

function TExprVirtMachNodeFactory.CallTernaryFunc(AFunc: TTernaryFunc; X, Y, Z: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallTernaryVmNode.Create(AFunc, X, Y, Z));
end;

function TExprVirtMachNodeFactory.CallTernary32Func(AFunc: TTernary32Func; X, Y, Z: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallTernary32VmNode.Create(AFunc, X, Y, Z));
end;

function TExprVirtMachNodeFactory.CallTernary64Func(AFunc: TTernary64Func; X, Y, Z: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallTernary64VmNode.Create(AFunc, X, Y, Z));
end;

function TExprVirtMachNodeFactory.CallTernary80Func(AFunc: TTernary80Func; X, Y, Z: TExprNode): TExprNode;
begin
  Result := AddNode(TExprCallTernary80VmNode.Create(AFunc, X, Y, Z));
end;

function TExprVirtMachNodeFactory.Compare(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprCompareVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.CompareEqual(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprEqualVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.CompareNotEqual(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprNotEqualVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.CompareLess(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprLessVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.CompareLessEqual(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprLessEqualVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.CompareGreater(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprGreaterVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.CompareGreaterEqual(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprGreaterEqualVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.LogicalAnd(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprLogicalAndVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.LogicalOr(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprLogicalOrVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.LogicalXor(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprLogicalXorVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.LogicalNot(AValue: TExprNode): TExprNode;
begin
  Result := AddNode(TExprUnaryVmNode.Create(TExprLogicalNotVmOp, [AValue]));
end;

function TExprVirtMachNodeFactory.BitwiseAnd(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprBitwiseAndVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.BitwiseOr(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprBitwiseOrVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.BitwiseXor(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprBitwiseXorVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.BitwiseNot(AValue: TExprNode): TExprNode;
begin
  Result := AddNode(TExprUnaryVmNode.Create(TExprBitwiseNotVmOp, [AValue]));
end;

function TExprVirtMachNodeFactory.ShiftLeft(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprShiftLeftVmOp, [ALeft, ARight]));
end;

function TExprVirtMachNodeFactory.ShiftRight(ALeft, ARight: TExprNode): TExprNode;
begin
  Result := AddNode(TExprBinaryVmNode.Create(TExprShiftRightVmOp, [ALeft, ARight]));
end;

//=== { TCompiledEvaluator } =================================================

constructor TCompiledEvaluator.Create;
begin
  inherited Create;
  FVirtMach := TExprVirtMach.Create;
end;

destructor TCompiledEvaluator.Destroy;
begin
  FVirtMach.Free;
  inherited Destroy;
end;

procedure TCompiledEvaluator.Compile(const AExpr: string);
var
  Lex: TExprSimpleLexer;
  Parse: TExprCompileParser;
  NodeFactory: TExprVirtMachNodeFactory;
begin
  if AExpr <> FExpr then
  begin
    FExpr := AExpr;
    FVirtMach.Clear;

    Parse := nil;
    NodeFactory := nil;
    Lex := TExprSimpleLexer.Create(FExpr);
    try
      NodeFactory := TExprVirtMachNodeFactory.Create;
      Parse := TExprCompileParser.Create(Lex, NodeFactory);
      Parse.Context := InternalContextSet;
      Parse.Compile;
      NodeFactory.GenCode(FVirtMach);
    finally
      Parse.Free;
      NodeFactory.Free;
      Lex.Free;
    end;
  end;
end;

function TCompiledEvaluator.Evaluate: TFloat;
begin
  Result := FVirtMach.Execute;
end;

//=== { TExprVar32Sym } ======================================================

constructor TExprVar32Sym.Create(const AIdent: string; ALoc: PFloat32);
begin
  Assert(ALoc <> nil);
  FLoc := ALoc;
  inherited Create(AIdent);
end;

function TExprVar32Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadVar32(FLoc);
end;

function TExprVar32Sym.Evaluate: TFloat;
begin
  Result := FLoc^;
end;

//=== { TExprVar64Sym } ======================================================

constructor TExprVar64Sym.Create(const AIdent: string; ALoc: PFloat64);
begin
  Assert(ALoc <> nil);
  FLoc := ALoc;
  inherited Create(AIdent);
end;

function TExprVar64Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadVar64(FLoc);
end;

function TExprVar64Sym.Evaluate: TFloat;
begin
  Result := FLoc^;
end;

//=== { TExprVar80Sym } ======================================================

constructor TExprVar80Sym.Create(const AIdent: string; ALoc: PFloat80);
begin
  Assert(ALoc <> nil);
  FLoc := ALoc;
  inherited Create(AIdent);
end;

function TExprVar80Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadVar80(FLoc);
end;

function TExprVar80Sym.Evaluate: TFloat;
begin
  Result := FLoc^;
end;

//=== { TExprCallFloatVmNode } ===============================================

constructor TExprCallFloatVmNode.Create(AFunc: TFloatFunc);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

procedure TExprCallFloatVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallFloatVmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallFloat32VmNode } =============================================

constructor TExprCallFloat32VmNode.Create(AFunc: TFloat32Func);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

procedure TExprCallFloat32VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallFloat32VmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallFloat64VmNode } =============================================

constructor TExprCallFloat64VmNode.Create(AFunc: TFloat64Func);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

procedure TExprCallFloat64VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallFloat64VmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallFloat80VmNode } =============================================

constructor TExprCallFloat80VmNode.Create(AFunc: TFloat80Func);
begin
  FFunc := AFunc;
  inherited Create([]);
end;

procedure TExprCallFloat80VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallFloat80VmOp.Create(FFunc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallUnaryVmNode } ===============================================

constructor TExprCallUnaryVmNode.Create(AFunc: TUnaryFunc; X: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X]);
end;

procedure TExprCallUnaryVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallUnaryVmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallUnary32VmNode } =============================================

constructor TExprCallUnary32VmNode.Create(AFunc: TUnary32Func; X: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X]);
end;

procedure TExprCallUnary32VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallUnary32VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallUnary64VmNode } =============================================

constructor TExprCallUnary64VmNode.Create(AFunc: TUnary64Func; X: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X]);
end;

procedure TExprCallUnary64VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallUnary64VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallUnary80VmNode } =============================================

constructor TExprCallUnary80VmNode.Create(AFunc: TUnary80Func; X: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X]);
end;

procedure TExprCallUnary80VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallUnary80VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallBinaryVmNode } ==============================================

constructor TExprCallBinaryVmNode.Create(AFunc: TBinaryFunc; X, Y: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y]);
end;

procedure TExprCallBinaryVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallBinaryVmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallBinary32VmNode } ============================================

constructor TExprCallBinary32VmNode.Create(AFunc: TBinary32Func; X, Y: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y]);
end;

procedure TExprCallBinary32VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallBinary32VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallBinary64VmNode } ============================================

constructor TExprCallBinary64VmNode.Create(AFunc: TBinary64Func; X, Y: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y]);
end;

procedure TExprCallBinary64VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallBinary64VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallBinary80VmNode } ============================================

constructor TExprCallBinary80VmNode.Create(AFunc: TBinary80Func; X, Y: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y]);
end;

procedure TExprCallBinary80VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallBinary80VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallTernaryVmNode } =============================================

constructor TExprCallTernaryVmNode.Create(AFunc: TTernaryFunc; X, Y, Z: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y, Z]);
end;

procedure TExprCallTernaryVmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallTernaryVmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallTernary32VmNode } ===========================================

constructor TExprCallTernary32VmNode.Create(AFunc: TTernary32Func; X, Y, Z: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y, Z]);
end;

procedure TExprCallTernary32VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallTernary32VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallTernary64VmNode } ===========================================

constructor TExprCallTernary64VmNode.Create(AFunc: TTernary64Func; X, Y, Z: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y, Z]);
end;

procedure TExprCallTernary64VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallTernary64VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprCallTernary80VmNode } ===========================================

constructor TExprCallTernary80VmNode.Create(AFunc: TTernary80Func; X, Y, Z: TExprNode);
begin
  FFunc := AFunc;
  inherited Create([X, Y, Z]);
end;

procedure TExprCallTernary80VmNode.GenCode(AVirtMach: TExprVirtMach);
begin
  FExprVmCode := TExprCallTernary80VmOp.Create(
    FFunc,
    VmDeps[0].ExprVmCode.OutputLoc,
    VmDeps[1].ExprVmCode.OutputLoc,
    VmDeps[2].ExprVmCode.OutputLoc);
  AVirtMach.Add(FExprVmCode);
end;

//=== { TExprAbstractFuncSym } ===============================================

function TExprAbstractFuncSym.CompileFirstArg: TExprNode;
begin
  if Lexer.CurrTok <> etLParen then
    raise EJclExprEvalError.CreateRes(@RsExprEvalFirstArg);
  Result := CompileParser.CompileExprLevel0(True);
end;

function TExprAbstractFuncSym.CompileNextArg: TExprNode;
begin
  if Lexer.CurrTok <> etComma then
    raise EJclExprEvalError.CreateRes(@RsExprEvalNextArg);
  Result := CompileParser.CompileExprLevel0(True);
end;

function TExprAbstractFuncSym.EvalFirstArg: TFloat;
begin
  if Lexer.CurrTok <> etLParen then
    raise EJclExprEvalError.CreateRes(@RsExprEvalFirstArg);
  Result := EvalParser.EvalExprLevel0(True);
end;

function TExprAbstractFuncSym.EvalNextArg: TFloat;
begin
  if Lexer.CurrTok <> etComma then
    raise EJclExprEvalError.CreateRes(@RsExprEvalNextArg);
  Result := EvalParser.EvalExprLevel0(True);
end;

procedure TExprAbstractFuncSym.EndArgs;
begin
  if Lexer.CurrTok <> etRParen then
    raise EJclExprEvalError.CreateRes(@RsExprEvalEndArgs);
  Lexer.NextTok;
end;

//=== { TExprFuncSym } =======================================================

constructor TExprFuncSym.Create(const AIdent: string; AFunc: TFloatFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprFuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallFloatFunc(FFunc);
end;

function TExprFuncSym.Evaluate: TFloat;
begin
  Result := FFunc;
end;

//=== { TExprFloat32FuncSym } ================================================

constructor TExprFloat32FuncSym.Create(const AIdent: string; AFunc: TFloat32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprFloat32FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallFloat32Func(FFunc);
end;

function TExprFloat32FuncSym.Evaluate: TFloat;
begin
  Result := FFunc;
end;

//=== { TExprFloat64FuncSym } ================================================

constructor TExprFloat64FuncSym.Create(const AIdent: string; AFunc: TFloat64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprFloat64FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallFloat64Func(FFunc);
end;

function TExprFloat64FuncSym.Evaluate: TFloat;
begin
  Result := FFunc;
end;

//=== { TExprFloat80FuncSym } ================================================

constructor TExprFloat80FuncSym.Create(const AIdent: string; AFunc: TFloat80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprFloat80FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallFloat80Func(FFunc);
end;

function TExprFloat80FuncSym.Evaluate: TFloat;
begin
  Result := FFunc;
end;

//=== { TExprUnaryFuncSym } ==================================================

constructor TExprUnaryFuncSym.Create(const AIdent: string; AFunc: TUnaryFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprUnaryFuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallUnaryFunc(FFunc, CompileFirstArg);
  EndArgs;
end;

function TExprUnaryFuncSym.Evaluate: TFloat;
begin
  Result := FFunc(EvalFirstArg);
  EndArgs;
end;

//=== { TExprUnary32FuncSym } ================================================

constructor TExprUnary32FuncSym.Create(const AIdent: string; AFunc: TUnary32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprUnary32FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallUnary32Func(FFunc, CompileFirstArg);
  EndArgs;
end;

function TExprUnary32FuncSym.Evaluate: TFloat;
begin
  Result := FFunc(EvalFirstArg);
  EndArgs;
end;

//=== { TExprUnary64FuncSym } ================================================

constructor TExprUnary64FuncSym.Create(const AIdent: string; AFunc: TUnary64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprUnary64FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallUnary64Func(FFunc, CompileFirstArg);
  EndArgs;
end;

function TExprUnary64FuncSym.Evaluate: TFloat;
begin
  Result := FFunc(EvalFirstArg);
  EndArgs;
end;

//=== { TExprUnary80FuncSym } ================================================

constructor TExprUnary80FuncSym.Create(const AIdent: string; AFunc: TUnary80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprUnary80FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallUnary80Func(FFunc, CompileFirstArg);
  EndArgs;
end;

function TExprUnary80FuncSym.Evaluate: TFloat;
begin
  Result := FFunc(EvalFirstArg);
  EndArgs;
end;

//=== { TExprBinaryFuncSym } =================================================

constructor TExprBinaryFuncSym.Create(const AIdent: string; AFunc: TBinaryFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprBinaryFuncSym.Compile: TExprNode;
var
  X, Y: TExprNode;
begin
  // must be called this way, because evaluation order of function
  // parameters is not defined; we need CompileFirstArg to be called
  // first.
  X := CompileFirstArg;
  Y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinaryFunc(FFunc, X, Y);
end;

function TExprBinaryFuncSym.Evaluate: TFloat;
var
  X, Y: TFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  Result := FFunc(X, Y);
  EndArgs;
end;

//=== { TExprBinary32FuncSym } ===============================================

constructor TExprBinary32FuncSym.Create(const AIdent: string; AFunc: TBinary32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprBinary32FuncSym.Compile: TExprNode;
var
  X, Y: TExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinary32Func(FFunc, X, Y);
end;

function TExprBinary32FuncSym.Evaluate: TFloat;
var
  X, Y: TFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y);
end;

//=== { TExprBinary64FuncSym } ===============================================

constructor TExprBinary64FuncSym.Create(const AIdent: string; AFunc: TBinary64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprBinary64FuncSym.Compile: TExprNode;
var
  X, Y: TExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinary64Func(FFunc, X, Y);
end;

function TExprBinary64FuncSym.Evaluate: TFloat;
var
  X, Y: TFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y);
end;

//=== { TExprBinary80FuncSym } ===============================================

constructor TExprBinary80FuncSym.Create(const AIdent: string; AFunc: TBinary80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprBinary80FuncSym.Compile: TExprNode;
var
  X, Y: TExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallBinary80Func(FFunc, X, Y);
end;

function TExprBinary80FuncSym.Evaluate: TFloat;
var
  X, Y: TFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y);
end;

//=== { TExprTernaryFuncSym } ================================================

constructor TExprTernaryFuncSym.Create(const AIdent: string; AFunc: TTernaryFunc);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprTernaryFuncSym.Compile: TExprNode;
var
  X, Y, Z: TExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  Z := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallTernaryFunc(FFunc, X, Y, Z);
end;

function TExprTernaryFuncSym.Evaluate: TFloat;
var
  X, Y, Z: TFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  Z := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y, Z);
end;

//=== { TExprTernary32FuncSym } ==============================================

constructor TExprTernary32FuncSym.Create(const AIdent: string; AFunc: TTernary32Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprTernary32FuncSym.Compile: TExprNode;
var
  X, Y, Z: TExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  Z := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallTernary32Func(FFunc, X, Y, Z);
end;

function TExprTernary32FuncSym.Evaluate: TFloat;
var
  X, Y, Z: TFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  Z := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y, Z);
end;

//=== { TExprTernary64FuncSym } ==============================================

constructor TExprTernary64FuncSym.Create(const AIdent: string; AFunc: TTernary64Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprTernary64FuncSym.Compile: TExprNode;
var
  X, Y, Z: TExprNode;
begin
  X := CompileFirstArg;
  Y := CompileNextArg;
  Z := CompileNextArg;
  EndArgs;
  Result := NodeFactory.CallTernary64Func(FFunc, X, Y, Z);
end;

function TExprTernary64FuncSym.Evaluate: TFloat;
var
  X, Y, Z: TFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  Z := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y, Z);
end;

//=== { TExprTernary80FuncSym } ==============================================

constructor TExprTernary80FuncSym.Create(const AIdent: string; AFunc: TTernary80Func);
begin
  Assert(Assigned(AFunc));
  inherited Create(AIdent);
  FFunc := AFunc;
end;

function TExprTernary80FuncSym.Compile: TExprNode;
begin
  Result := NodeFactory.CallTernary80Func(FFunc, CompileFirstArg,
    CompileNextArg, CompileNextArg);
  EndArgs;
end;

function TExprTernary80FuncSym.Evaluate: TFloat;
var
  X, Y, Z: TFloat;
begin
  X := EvalFirstArg;
  Y := EvalNextArg;
  Z := EvalNextArg;
  EndArgs;
  Result := FFunc(X, Y, Z);
end;

//=== { TExprConstSym } ======================================================

constructor TExprConstSym.Create(const AIdent: string; AValue: TFloat);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

function TExprConstSym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

function TExprConstSym.Evaluate: TFloat;
begin
  Result := FValue;
end;

//=== { TExprConst32Sym } ====================================================

constructor TExprConst32Sym.Create(const AIdent: string; AValue: TFloat32);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

function TExprConst32Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

function TExprConst32Sym.Evaluate: TFloat;
begin
  Result := FValue;
end;

//=== { TExprConst64Sym } ====================================================

constructor TExprConst64Sym.Create(const AIdent: string; AValue: TFloat64);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

function TExprConst64Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

function TExprConst64Sym.Evaluate: TFloat;
begin
  Result := FValue;
end;

//=== { TExprConst80Sym } ====================================================

constructor TExprConst80Sym.Create(const AIdent: string; AValue: TFloat80);
begin
  inherited Create(AIdent);
  FValue := AValue;
end;

function TExprConst80Sym.Compile: TExprNode;
begin
  Result := NodeFactory.LoadConst(FValue);
end;

function TExprConst80Sym.Evaluate: TFloat;
begin
  Result := FValue;
end;

//=== { TEasyEvaluator } =====================================================

constructor TEasyEvaluator.Create;
begin
  inherited Create;
  FOwnContext := TExprHashContext.Create(False, cExprEvalHashSize);
  FExtContextSet := TExprSetContext.Create(False);
  FInternalContextSet := TExprSetContext.Create(False);

  // user added names get precedence over external context's names
  FInternalContextSet.Add(FExtContextSet);
  FInternalContextSet.Add(FOwnContext);
end;

destructor TEasyEvaluator.Destroy;
begin
  FInternalContextSet.Free;
  FOwnContext.Free;
  FExtContextSet.Free;
  inherited Destroy;
end;

procedure TEasyEvaluator.AddConst(const AName: string; AConst: TFloat80);
begin
  FOwnContext.Add(TExprConst80Sym.Create(AName, AConst));
end;

procedure TEasyEvaluator.AddConst(const AName: string; AConst: TFloat64);
begin
  FOwnContext.Add(TExprConst64Sym.Create(AName, AConst));
end;

procedure TEasyEvaluator.AddConst(const AName: string; AConst: TFloat32);
begin
  FOwnContext.Add(TExprConst32Sym.Create(AName, AConst));
end;

procedure TEasyEvaluator.AddVar(const AName: string; var AVar: TFloat32);
begin
  FOwnContext.Add(TExprVar32Sym.Create(AName, @AVar));
end;

procedure TEasyEvaluator.AddVar(const AName: string; var AVar: TFloat64);
begin
  FOwnContext.Add(TExprVar64Sym.Create(AName, @AVar));
end;

procedure TEasyEvaluator.AddVar(const AName: string; var AVar: TFloat80);
begin
  FOwnContext.Add(TExprVar80Sym.Create(AName, @AVar));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TFloat32Func);
begin
  FOwnContext.Add(TExprFloat32FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TFloat64Func);
begin
  FOwnContext.Add(TExprFloat64FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TFloat80Func);
begin
  FOwnContext.Add(TExprFloat80FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TUnary32Func);
begin
  FOwnContext.Add(TExprUnary32FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TUnary64Func);
begin
  FOwnContext.Add(TExprUnary64FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TUnary80Func);
begin
  FOwnContext.Add(TExprUnary80FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TBinary32Func);
begin
  FOwnContext.Add(TExprBinary32FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TBinary64Func);
begin
  FOwnContext.Add(TExprBinary64FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TBinary80Func);
begin
  FOwnContext.Add(TExprBinary80FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TTernary32Func);
begin
  FOwnContext.Add(TExprTernary32FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TTernary64Func);
begin
  FOwnContext.Add(TExprTernary64FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.AddFunc(const AName: string; AFunc: TTernary80Func);
begin
  FOwnContext.Add(TExprTernary80FuncSym.Create(AName, AFunc));
end;

procedure TEasyEvaluator.Clear;
begin
  FOwnContext.FHashMap.Iterate(nil, Iterate_FreeObjects);
  FOwnContext.FHashMap.Clear;
end;

procedure TEasyEvaluator.Remove(const AName: string);
begin
  FOwnContext.Remove(AName);
end;

//=== { TInternalCompiledExpression } ========================================

type
  TInternalCompiledExpression = class(TObject)
  private
    FVirtMach: TExprVirtMach;
    FRefCount: Integer;
  public
    constructor Create(AVirtMach: TExprVirtMach);
    destructor Destroy; override;
    property VirtMach: TExprVirtMach read FVirtMach;
    property RefCount: Integer read FRefCount write FRefCount;
  end;

constructor TInternalCompiledExpression.Create(AVirtMach: TExprVirtMach);
begin
  inherited Create;
  FVirtMach := AVirtMach;
end;

destructor TInternalCompiledExpression.Destroy;
begin
  FVirtMach.Free;
  inherited Destroy;
end;

//=== { TExpressionCompiler } ================================================

constructor TExpressionCompiler.Create;
begin
  FExprHash := TStringHashMap.Create(CaseInsensitiveTraits,
    cExprEvalHashSize);
  inherited Create;
end;

destructor TExpressionCompiler.Destroy;
begin
  FExprHash.Iterate(nil, Iterate_FreeObjects);
  FExprHash.Free;
  inherited Destroy;
end;

function TExpressionCompiler.Compile(const AExpr: string): TCompiledExpression;
var
  Ice: TInternalCompiledExpression;
  Vm: TExprVirtMach;
  Parser: TExprCompileParser;
  Lexer: TExprSimpleLexer;
  NodeFactory: TExprVirtMachNodeFactory;
begin
  if FExprHash.Find(AExpr, Ice) then
  begin
    // expression already exists, add reference
    Result := Ice.VirtMach.Execute;
    Ice.RefCount := Ice.RefCount + 1;
  end
  else
  begin
    // compile fresh expression
    Parser := nil;
    NodeFactory := nil;
    Lexer := TExprSimpleLexer.Create(AExpr);
    try
      NodeFactory := TExprVirtMachNodeFactory.Create;
      Parser := TExprCompileParser.Create(Lexer, NodeFactory);
      Parser.Context := InternalContextSet;
      Parser.Compile;

      Ice := nil;
      Vm := TExprVirtMach.Create;
      try
        NodeFactory.GenCode(Vm);
        Ice := TInternalCompiledExpression.Create(Vm);
        Ice.RefCount := 1;
        FExprHash.Add(AExpr, Ice);
      except
        Ice.Free;
        Vm.Free;
        raise;
      end;
    finally
      NodeFactory.Free;
      Parser.Free;
      Lexer.Free;
    end;

    Result := Ice.VirtMach.Execute;
  end;
end;

type
  PIceFindResult = ^TIceFindResult;
  TIceFindResult = record
    Found: Boolean;
    Ce: TCompiledExpression;
    Ice: TInternalCompiledExpression;
    Expr: string;
  end;

function IterateFindIce(AUserData: Pointer; const AStr: string; var APtr: Pointer): Boolean;
var
  PIfr: PIceFindResult;
  Ice: TInternalCompiledExpression;
  Ce: TCompiledExpression;
begin
  PIfr := AUserData;
  Ice := APtr;
  Ce := Ice.VirtMach.Execute;

  if (TMethod(PIfr^.Ce).Code = TMethod(Ce).Code) and
    (TMethod(PIfr^.Ce).Data = TMethod(Ce).Data) then
  begin
    PIfr^.Found := True;
    PIfr^.Ice := Ice;
    PIfr^.Expr := AStr;
    Result := False;
  end else
    Result := True;
end;

procedure TExpressionCompiler.Delete(ACompiledExpression: TCompiledExpression);
var
  Ifr: TIceFindResult;
begin
  with Ifr do
  begin
    Found := False;
    Ce := ACompiledExpression;
    Ice := nil;
    Expr := '';
    FExprHash.Iterate(@Ifr, IterateFindIce);
    if not Found then
      raise EJclExprEvalError.CreateRes(@RsExprEvalExprPtrNotFound);
    Remove(Expr);
  end;
end;

procedure TExpressionCompiler.Remove(const AExpr: string);
var
  Ice: TInternalCompiledExpression;
begin
  if not FExprHash.Find(AExpr, Ice) then
    raise EJclExprEvalError.CreateResFmt(@RsExprEvalExprNotFound, [AExpr]);

  Ice.RefCount := Ice.RefCount - 1;
  Assert(Ice.RefCount >= 0, LoadResString(@RsExprEvalExprRefCountAssertion));
  if Ice.RefCount = 0 then
  begin
    Ice.Free;
    FExprHash.Remove(AExpr);
  end;
end;

procedure TExpressionCompiler.Clear;
begin
  FExprHash.Iterate(nil, Iterate_FreeObjects);
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
