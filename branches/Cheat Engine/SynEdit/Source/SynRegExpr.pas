{$IFNDEF QSYNREGEXPR}
unit SynRegExpr;
{$ENDIF}

{
     TRegExpr class library
     Delphi Regular Expressions

 Copyright (c) 1999-2004 Andrey V. Sorokin, St.Petersburg, Russia

 You may use this software in any kind of development,
 including comercial, redistribute, and modify it freely,
 under the following restrictions :
 1. This software is provided as it is, without any kind of
    warranty given. Use it at Your own risk.The author is not
    responsible for any consequences of use of this software.
 2. The origin of this software may not be mispresented, You
    must not claim that You wrote the original software. If
    You use this software in any kind of product, it would be
    appreciated that there in a information box, or in the
    documentation would be an acknowledgement like

     Partial Copyright (c) 2004 Andrey V. Sorokin
                                http://RegExpStudio.com
                                mailto:anso@mail.ru

 3. You may not have any income from distributing this source
    (or altered version of it) to other developers. When You
    use this product in a comercial package, the source may
    not be charged seperatly.
 4. Altered versions must be plainly marked as such, and must
    not be misrepresented as being the original software.
 5. RegExp Studio application and all the visual components as 
    well as documentation is not part of the TRegExpr library 
    and is not free for usage.

                                    mailto:anso@mail.ru
                                    http://RegExpStudio.com
                                    http://anso.da.ru/
}

interface

{$INCLUDE SynEdit.inc}

// ======== Determine compiler
{$IFDEF VER80} Sorry, TRegExpr is for 32-bits Delphi only. Delphi 1 is not supported (and whos really care today?!). {$ENDIF}
{$IFDEF VER90} {$DEFINE D2} {$ENDIF} // D2
{$IFDEF VER93} {$DEFINE D2} {$ENDIF} // CPPB 1
{$IFDEF VER100} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D3
{$IFDEF VER110} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // CPPB 3
{$IFDEF VER120} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D4
{$IFDEF VER130} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D5
{$IFDEF VER140} {$DEFINE D6} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D6
{$IFDEF VER150} {$DEFINE D7} {$DEFINE D6} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D7
{$IFDEF VER170} {$DEFINE D7} {$DEFINE D6} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D2005
{$IFDEF VER180} {$DEFINE D7} {$DEFINE D6} {$DEFINE D5} {$DEFINE D4} {$DEFINE D3} {$DEFINE D2} {$ENDIF} // D2006

// ======== Define base compiler options
{$BOOLEVAL OFF}
{$EXTENDEDSYNTAX ON}
{$LONGSTRINGS ON}
{$OPTIMIZATION ON}
{$IFDEF D6}
  {$WARN SYMBOL_PLATFORM OFF} // Suppress .Net warnings
{$ENDIF}
{$IFDEF D7}
  {$WARN UNSAFE_CAST OFF} // Suppress .Net warnings
  {$WARN UNSAFE_TYPE OFF} // Suppress .Net warnings
  {$WARN UNSAFE_CODE OFF} // Suppress .Net warnings
{$ENDIF}
{$IFDEF FPC}
 {$MODE DELPHI} // Delphi-compatible mode in FreePascal
{$ENDIF}

// ======== Define options for TRegExpr engine
{.$DEFINE UniCode} // Unicode support
{$DEFINE RegExpPCodeDump} // p-code dumping (see Dump method)
{$IFNDEF FPC} // the option is not supported in FreePascal
 {$DEFINE reRealExceptionAddr} // exceptions will point to appropriate source line, not to Error procedure
{$ENDIF}
{$DEFINE ComplexBraces} // support braces in complex cases
{$IFNDEF UniCode} // the option applicable only for non-UniCode mode
 {$DEFINE UseSetOfChar} // Significant optimization by using set of char
{$ENDIF}
{$IFDEF UseSetOfChar}
 {$DEFINE UseFirstCharSet} // Fast skip between matches for r.e. that starts with determined set of chars
{$ENDIF}

// ======== Define Pascal-language options
// Define 'UseAsserts' option (do not edit this definitions).
// Asserts used to catch 'strange bugs' in TRegExpr implementation (when something goes
// completely wrong). You can swith asserts on/off with help of {$C+}/{$C-} compiler options.
{$IFDEF D3} {$DEFINE UseAsserts} {$ENDIF}
{$IFDEF FPC} {$DEFINE UseAsserts} {$ENDIF}

// Define 'use subroutine parameters default values' option (do not edit this definition).
{$IFDEF D4} {$DEFINE DefParam} {$ENDIF}

// Define 'OverMeth' options, to use method overloading (do not edit this definitions).
{$IFDEF D5} {$DEFINE OverMeth} {$ENDIF}
{$IFDEF FPC} {$DEFINE OverMeth} {$ENDIF}

uses
 Classes,  // TStrings in Split method
 SysUtils; // Exception

type
 {$IFDEF UniCode}
 PRegExprChar = PWideChar;
 RegExprString = WideString;
 REChar = WideChar;
 {$ELSE}
 PRegExprChar = PChar;
 RegExprString = AnsiString; //###0.952 was string
 REChar = Char;
 {$ENDIF}
 TREOp = REChar; // internal p-code type //###0.933
 PREOp = ^TREOp;
 TRENextOff = integer; // internal Next "pointer" (offset to current p-code) //###0.933
 PRENextOff = ^TRENextOff; // used for extracting Next "pointers" from compiled r.e. //###0.933
 TREBracesArg = integer; // type of {m,n} arguments
 PREBracesArg = ^TREBracesArg;

const
 REOpSz = SizeOf (TREOp) div SizeOf (REChar); // size of p-code in RegExprString units
 RENextOffSz = SizeOf (TRENextOff) div SizeOf (REChar); // size of Next 'pointer' -"-
 REBracesArgSz = SizeOf (TREBracesArg) div SizeOf (REChar); // size of BRACES arguments -"-

type
 TRegExprInvertCaseFunction = function (const Ch : REChar) : REChar
                               of object;

const
  EscChar = '\'; // 'Escape'-char ('\' in common r.e.) used for escaping metachars (\w, \d etc).
  RegExprModifierI : boolean = False;    // default value for ModifierI
  RegExprModifierR : boolean = True;     // default value for ModifierR
  RegExprModifierS : boolean = True;     // default value for ModifierS
  RegExprModifierG : boolean = True;     // default value for ModifierG
  RegExprModifierM : boolean = False;    // default value for ModifierM
  RegExprModifierX : boolean = False;    // default value for ModifierX
  RegExprSpaceChars : RegExprString =    // default value for SpaceChars
  ' '#$9#$A#$D#$C;
  RegExprWordChars : RegExprString =     // default value for WordChars
    '0123456789' //###0.940
  + 'abcdefghijklmnopqrstuvwxyz'
  + 'ABCDEFGHIJKLMNOPQRSTUVWXYZ_';
  RegExprLineSeparators : RegExprString =// default value for LineSeparators
   #$d#$a{$IFDEF UniCode}+#$b#$c#$2028#$2029#$85{$ENDIF}; //###0.947
  RegExprLinePairedSeparator : RegExprString =// default value for LinePairedSeparator
   #$d#$a;
  { if You need Unix-styled line separators (only \n), then use:
  RegExprLineSeparators = #$a;
  RegExprLinePairedSeparator = '';
  }


const
 NSUBEXP = 15; // max number of subexpression //###0.929
 // Cannot be more than NSUBEXPMAX
 // Be carefull - don't use values which overflow CLOSE opcode
 // (in this case you'll get compiler erorr).
 // Big NSUBEXP will cause more slow work and more stack required
 NSUBEXPMAX = 255; // Max possible value for NSUBEXP. //###0.945
 // Don't change it! It's defined by internal TRegExpr design.

 MaxBracesArg = $7FFFFFFF - 1; // max value for {n,m} arguments //###0.933

 {$IFDEF ComplexBraces}
 LoopStackMax = 10; // max depth of loops stack //###0.925
 {$ENDIF}

 TinySetLen = 3;
 // if range includes more then TinySetLen chars, //###0.934
 // then use full (32 bytes) ANYOFFULL instead of ANYOF[BUT]TINYSET
 // !!! Attension ! If you change TinySetLen, you must
 // change code marked as "//!!!TinySet"


type

{$IFDEF UseSetOfChar}
 PSetOfREChar = ^TSetOfREChar;
 TSetOfREChar = set of REChar;
{$ENDIF}

 TRegExpr = class;

 TRegExprReplaceFunction = function (ARegExpr : TRegExpr): string
                               of object;

 TRegExpr = class
   private
    startp : array [0 .. NSUBEXP - 1] of PRegExprChar; // founded expr starting points
    endp : array [0 .. NSUBEXP - 1] of PRegExprChar; // founded expr end points

    {$IFDEF ComplexBraces}
    LoopStack : array [1 .. LoopStackMax] of integer; // state before entering loop
    LoopStackIdx : integer; // 0 - out of all loops
    {$ENDIF}

    // The "internal use only" fields to pass info from compile
    // to execute that permits the execute phase to run lots faster on
    // simple cases.
    regstart : REChar; // char that must begin a match; '\0' if none obvious
    reganch : REChar; // is the match anchored (at beginning-of-line only)?
    regmust : PRegExprChar; // string (pointer into program) that match must include, or nil
    regmlen : integer; // length of regmust string
    // Regstart and reganch permit very fast decisions on suitable starting points
    // for a match, cutting down the work a lot.  Regmust permits fast rejection
    // of lines that cannot possibly match.  The regmust tests are costly enough
    // that regcomp() supplies a regmust only if the r.e. contains something
    // potentially expensive (at present, the only such thing detected is * or +
    // at the start of the r.e., which can involve a lot of backup).  Regmlen is
    // supplied because the test in regexec() needs it and regcomp() is computing
    // it anyway.
    {$IFDEF UseFirstCharSet} //###0.929
    FirstCharSet : TSetOfREChar;
    {$ENDIF}

    // work variables for Exec's routins - save stack in recursion}
    reginput : PRegExprChar; // String-input pointer.
    fInputStart : PRegExprChar; // Pointer to first char of input string.
    fInputEnd : PRegExprChar; // Pointer to char AFTER last char of input string

    // work variables for compiler's routines
    regparse : PRegExprChar;  // Input-scan pointer.
    regnpar : integer; // count.
    regdummy : char;
    regcode : PRegExprChar;   // Code-emit pointer; @regdummy = don't.
    regsize : integer; // Code size.

    regexpbeg : PRegExprChar; // only for error handling. Contains
    // pointer to beginning of r.e. while compiling
    fExprIsCompiled : boolean; // true if r.e. successfully compiled

    // programm is essentially a linear encoding
    // of a nondeterministic finite-state machine (aka syntax charts or
    // "railroad normal form" in parsing technology).  Each node is an opcode
    // plus a "next" pointer, possibly plus an operand.  "Next" pointers of
    // all nodes except BRANCH implement concatenation; a "next" pointer with
    // a BRANCH on both ends of it is connecting two alternatives.  (Here we
    // have one of the subtle syntax dependencies:  an individual BRANCH (as
    // opposed to a collection of them) is never concatenated with anything
    // because of operator precedence.)  The operand of some types of node is
    // a literal string; for others, it is a node leading into a sub-FSM.  In
    // particular, the operand of a BRANCH node is the first node of the branch.
    // (NB this is *not* a tree structure:  the tail of the branch connects
    // to the thing following the set of BRANCHes.)  The opcodes are:
    programm : PRegExprChar; // Unwarranted chumminess with compiler.

    fExpression : PRegExprChar; // source of compiled r.e.
    fInputString : PRegExprChar; // input string

    fLastError : integer; // see Error, LastError

    fModifiers : integer; // modifiers
    fCompModifiers : integer; // compiler's copy of modifiers
    fProgModifiers : integer; // modifiers values from last programm compilation

    fSpaceChars : RegExprString; //###0.927
    fWordChars : RegExprString; //###0.929
    fInvertCase : TRegExprInvertCaseFunction; //###0.927

    fLineSeparators : RegExprString; //###0.941
    fLinePairedSeparatorAssigned : boolean;
    fLinePairedSeparatorHead,
    fLinePairedSeparatorTail : REChar;
    {$IFNDEF UniCode}
    fLineSeparatorsSet : set of REChar;
    {$ENDIF}

    procedure InvalidateProgramm;
    // Mark programm as have to be [re]compiled

    function IsProgrammOk : boolean; //###0.941
    // Check if we can use precompiled r.e. or
    // [re]compile it if something changed

    function GetExpression : RegExprString;
    procedure SetExpression (const s : RegExprString);

    function GetModifierStr : RegExprString;
    class function ParseModifiersStr (const AModifiers : RegExprString;
      var AModifiersInt : integer) : boolean; //###0.941 class function now
    // Parse AModifiers string and return true and set AModifiersInt
    // if it's in format 'ismxrg-ismxrg'.
    procedure SetModifierStr (const AModifiers : RegExprString);

    function GetModifier (AIndex : integer) : boolean;
    procedure SetModifier (AIndex : integer; ASet : boolean);

    procedure Error (AErrorID : integer); virtual; // error handler.
    // Default handler raise exception ERegExpr with
    // Message = ErrorMsg (AErrorID), ErrorCode = AErrorID
    // and CompilerErrorPos = value of property CompilerErrorPos.


    {==================== Compiler section ===================}
    function CompileRegExpr (exp : PRegExprChar) : boolean;
    // compile a regular expression into internal code

    procedure Tail (p : PRegExprChar; val : PRegExprChar);
    // set the next-pointer at the end of a node chain

    procedure OpTail (p : PRegExprChar; val : PRegExprChar);
    // regoptail - regtail on operand of first argument; nop if operandless

    function EmitNode (op : TREOp) : PRegExprChar;
    // regnode - emit a node, return location

    procedure EmitC (b : REChar);
    // emit (if appropriate) a byte of code

    procedure InsertOperator (op : TREOp; opnd : PRegExprChar; sz : integer); //###0.90
    // insert an operator in front of already-emitted operand
    // Means relocating the operand.

    function ParseReg (paren : integer; var flagp : integer) : PRegExprChar;
    // regular expression, i.e. main body or parenthesized thing

    function ParseBranch (var flagp : integer) : PRegExprChar;
    // one alternative of an | operator

    function ParsePiece (var flagp : integer) : PRegExprChar;
    // something followed by possible [*+?]

    function ParseAtom (var flagp : integer) : PRegExprChar;
    // the lowest level

    function GetCompilerErrorPos : integer;
    // current pos in r.e. - for error hanling

    {$IFDEF UseFirstCharSet} //###0.929
    procedure FillFirstCharSet (prog : PRegExprChar);
    {$ENDIF}

    {===================== Mathing section ===================}
    function regrepeat (p : PRegExprChar; AMax : integer) : integer;
    // repeatedly match something simple, report how many

    function regnext (p : PRegExprChar) : PRegExprChar;
    // dig the "next" pointer out of a node

    function MatchPrim (prog : PRegExprChar) : boolean;
    // recursively matching routine

    function ExecPrim (AOffset: integer) : boolean;
    // Exec for stored InputString

    {$IFDEF RegExpPCodeDump}
    function DumpOp (op : REChar) : RegExprString;
    {$ENDIF}

    function GetSubExprMatchCount : integer;
    function GetMatchPos (Idx : integer) : integer;
    function GetMatchLen (Idx : integer) : integer;
    function GetMatch (Idx : integer) : RegExprString;

    function GetInputString : RegExprString;
    procedure SetInputString (const AInputString : RegExprString);

    {$IFNDEF UseSetOfChar}
    function StrScanCI (s : PRegExprChar; ch : REChar) : PRegExprChar; //###0.928
    {$ENDIF}

    procedure SetLineSeparators (const AStr : RegExprString);
    procedure SetLinePairedSeparator (const AStr : RegExprString);
    function GetLinePairedSeparator : RegExprString;

   public
    constructor Create;
    destructor Destroy; override;

    class function VersionMajor : integer; //###0.944
    class function VersionMinor : integer; //###0.944

    property Expression : RegExprString read GetExpression write SetExpression;
    // Regular expression.
    // For optimization, TRegExpr will automatically compiles it into 'P-code'
    // (You can see it with help of Dump method) and stores in internal
    // structures. Real [re]compilation occures only when it really needed -
    // while calling Exec[Next], Substitute, Dump, etc
    // and only if Expression or other P-code affected properties was changed
    // after last [re]compilation.
    // If any errors while [re]compilation occures, Error method is called
    // (by default Error raises exception - see below)

    property ModifierStr : RegExprString read GetModifierStr write SetModifierStr;
    // Set/get default values of r.e.syntax modifiers. Modifiers in
    // r.e. (?ismx-ismx) will replace this default values.
    // If you try to set unsupported modifier, Error will be called
    // (by defaul Error raises exception ERegExpr).

    property ModifierI : boolean index 1 read GetModifier write SetModifier;
    // Modifier /i - caseinsensitive, initialized from RegExprModifierI

    property ModifierR : boolean index 2 read GetModifier write SetModifier;
    // Modifier /r - use r.e.syntax extended for russian,
    // (was property ExtSyntaxEnabled in previous versions)
    // If true, then à-ÿ  additional include russian letter '¸',
    // À-ß  additional include '¨', and à-ß include all russian symbols.
    // You have to turn it off if it may interfere with you national alphabet.
    // , initialized from RegExprModifierR

    property ModifierS : boolean index 3 read GetModifier write SetModifier;
    // Modifier /s - '.' works as any char (else as [^\n]),
    // , initialized from RegExprModifierS

    property ModifierG : boolean index 4 read GetModifier write SetModifier;
    // Switching off modifier /g switchs all operators in
    // non-greedy style, so if ModifierG = False, then
    // all '*' works as '*?', all '+' as '+?' and so on.
    // , initialized from RegExprModifierG

    property ModifierM : boolean index 5 read GetModifier write SetModifier;
    // Treat string as multiple lines. That is, change `^' and `$' from
    // matching at only the very start or end of the string to the start
    // or end of any line anywhere within the string.
    // , initialized from RegExprModifierM

    property ModifierX : boolean index 6 read GetModifier write SetModifier;
    // Modifier /x - eXtended syntax, allow r.e. text formatting,
    // see description in the help. Initialized from RegExprModifierX

    function Exec (const AInputString : RegExprString) : boolean; {$IFDEF OverMeth} overload;
    {$IFNDEF FPC} // I do not know why FreePascal cannot overload methods with empty param list
    function Exec : boolean; overload; //###0.949
    {$ENDIF}
    function Exec (AOffset: integer) : boolean; overload; //###0.949
    {$ENDIF}
    // match a programm against a string AInputString
    // !!! Exec store AInputString into InputString property
    // For Delphi 5 and higher available overloaded versions - first without
    // parameter (uses already assigned to InputString property value)
    // and second that has integer parameter and is same as ExecPos

    function ExecNext : boolean;
    // find next match:
    //    ExecNext;
    // works same as
    //    if MatchLen [0] = 0 then ExecPos (MatchPos [0] + 1)
    //     else ExecPos (MatchPos [0] + MatchLen [0]);
    // but it's more simpler !
    // Raises exception if used without preceeding SUCCESSFUL call to
    // Exec* (Exec, ExecPos, ExecNext). So You always must use something like
    // if Exec (InputString) then repeat { proceed results} until not ExecNext;

    function ExecPos (AOffset: integer {$IFDEF DefParam}= 1{$ENDIF}) : boolean;
    // find match for InputString starting from AOffset position
    // (AOffset=1 - first char of InputString)

    property InputString : RegExprString read GetInputString write SetInputString;
    // returns current input string (from last Exec call or last assign
    // to this property).
    // Any assignment to this property clear Match* properties !

    function Substitute (const ATemplate : RegExprString) : RegExprString;
    // Returns ATemplate with '$&' or '$0' replaced by whole r.e.
    // occurence and '$n' replaced by occurence of subexpression #n.
    // Since v.0.929 '$' used instead of '\' (for future extensions
    // and for more Perl-compatibility) and accept more then one digit.
    // If you want place into template raw '$' or '\', use prefix '\'
    // Example: '1\$ is $2\\rub\\' -> '1$ is <Match[2]>\rub\'
    // If you want to place raw digit after '$n' you must delimit
    // n with curly braces '{}'.
    // Example: 'a$12bc' -> 'a<Match[12]>bc'
    // 'a${1}2bc' -> 'a<Match[1]>2bc'.

    procedure Split (AInputStr : RegExprString; APieces : TStrings);
    // Split AInputStr into APieces by r.e. occurencies
    // Internally calls Exec[Next]

    function Replace (AInputStr : RegExprString;
      const AReplaceStr : RegExprString;
      AUseSubstitution : boolean{$IFDEF DefParam}= False{$ENDIF}) //###0.946
     : RegExprString; {$IFDEF OverMeth} overload;
    function Replace (AInputStr : RegExprString;
      AReplaceFunc : TRegExprReplaceFunction)
     : RegExprString; overload;
    {$ENDIF}
    function ReplaceEx (AInputStr : RegExprString;
      AReplaceFunc : TRegExprReplaceFunction)
     : RegExprString;
    // Returns AInputStr with r.e. occurencies replaced by AReplaceStr
    // If AUseSubstitution is true, then AReplaceStr will be used
    // as template for Substitution methods.
    // For example:
    //  Expression := '({-i}block|var)\s*\(\s*([^ ]*)\s*\)\s*';
    //  Replace ('BLOCK( test1)', 'def "$1" value "$2"', True);
    //   will return:  def 'BLOCK' value 'test1'
    //  Replace ('BLOCK( test1)', 'def "$1" value "$2"')
    //   will return:  def "$1" value "$2"
    // Internally calls Exec[Next]
    // Overloaded version and ReplaceEx operate with call-back function,
    // so You can implement really complex functionality.

    property SubExprMatchCount : integer read GetSubExprMatchCount;
    // Number of subexpressions has been found in last Exec* call.
    // If there are no subexpr. but whole expr was found (Exec* returned True),
    // then SubExprMatchCount=0, if no subexpressions nor whole
    // r.e. found (Exec* returned false) then SubExprMatchCount=-1.
    // Note, that some subexpr. may be not found and for such
    // subexpr. MathPos=MatchLen=-1 and Match=''.
    // For example: Expression := '(1)?2(3)?';
    //  Exec ('123'): SubExprMatchCount=2, Match[0]='123', [1]='1', [2]='3'
    //  Exec ('12'): SubExprMatchCount=1, Match[0]='12', [1]='1'
    //  Exec ('23'): SubExprMatchCount=2, Match[0]='23', [1]='', [2]='3'
    //  Exec ('2'): SubExprMatchCount=0, Match[0]='2'
    //  Exec ('7') - return False: SubExprMatchCount=-1

    property MatchPos [Idx : integer] : integer read GetMatchPos;
    // pos of entrance subexpr. #Idx into tested in last Exec*
    // string. First subexpr. have Idx=1, last - MatchCount,
    // whole r.e. have Idx=0.
    // Returns -1 if in r.e. no such subexpr. or this subexpr.
    // not found in input string.

    property MatchLen [Idx : integer] : integer read GetMatchLen;
    // len of entrance subexpr. #Idx r.e. into tested in last Exec*
    // string. First subexpr. have Idx=1, last - MatchCount,
    // whole r.e. have Idx=0.
    // Returns -1 if in r.e. no such subexpr. or this subexpr.
    // not found in input string.
    // Remember - MatchLen may be 0 (if r.e. match empty string) !

    property Match [Idx : integer] : RegExprString read GetMatch;
    // == copy (InputString, MatchPos [Idx], MatchLen [Idx])
    // Returns '' if in r.e. no such subexpr. or this subexpr.
    // not found in input string.

    function LastError : integer;
    // Returns ID of last error, 0 if no errors (unusable if
    // Error method raises exception) and clear internal status
    // into 0 (no errors).

    function ErrorMsg (AErrorID : integer) : RegExprString; virtual;
    // Returns Error message for error with ID = AErrorID.

    property CompilerErrorPos : integer read GetCompilerErrorPos;
    // Returns pos in r.e. there compiler stopped.
    // Usefull for error diagnostics

    property SpaceChars : RegExprString read fSpaceChars write fSpaceChars; //###0.927
    // Contains chars, treated as /s (initially filled with RegExprSpaceChars
    // global constant)

    property WordChars : RegExprString read fWordChars write fWordChars; //###0.929
    // Contains chars, treated as /w (initially filled with RegExprWordChars
    // global constant)

    property LineSeparators : RegExprString read fLineSeparators write SetLineSeparators; //###0.941
    // line separators (like \n in Unix)

    property LinePairedSeparator : RegExprString read GetLinePairedSeparator write SetLinePairedSeparator; //###0.941
    // paired line separator (like \r\n in DOS and Windows).
    // must contain exactly two chars or no chars at all

    class function InvertCaseFunction  (const Ch : REChar) : REChar;
    // Converts Ch into upper case if it in lower case or in lower
    // if it in upper (uses current system local setings)

    property InvertCase : TRegExprInvertCaseFunction read fInvertCase write fInvertCase; //##0.935
    // Set this property if you want to override case-insensitive functionality.
    // Create set it to RegExprInvertCaseFunction (InvertCaseFunction by default)

    procedure Compile; //###0.941
    // [Re]compile r.e. Usefull for example for GUI r.e. editors (to check
    // all properties validity).

    {$IFDEF RegExpPCodeDump}
    function Dump : RegExprString;
    // dump a compiled regexp in vaguely comprehensible form
    {$ENDIF}
  end;

 ERegExpr = class (Exception)
   public
    ErrorCode : integer;
    CompilerErrorPos : integer;
  end;

const
  RegExprInvertCaseFunction : TRegExprInvertCaseFunction = {$IFDEF FPC} nil {$ELSE} TRegExpr.InvertCaseFunction{$ENDIF};
  // defaul for InvertCase property

function ExecRegExpr (const ARegExpr, AInputStr : RegExprString) : boolean;
// true if string AInputString match regular expression ARegExpr
// ! will raise exeption if syntax errors in ARegExpr

procedure SplitRegExpr (const ARegExpr, AInputStr : RegExprString; APieces : TStrings);
// Split AInputStr into APieces by r.e. ARegExpr occurencies

function ReplaceRegExpr (const ARegExpr, AInputStr, AReplaceStr : RegExprString;
      AUseSubstitution : boolean{$IFDEF DefParam}= False{$ENDIF}) : RegExprString; //###0.947
// Returns AInputStr with r.e. occurencies replaced by AReplaceStr
// If AUseSubstitution is true, then AReplaceStr will be used
// as template for Substitution methods.
// For example:
//  ReplaceRegExpr ('({-i}block|var)\s*\(\s*([^ ]*)\s*\)\s*',
//   'BLOCK( test1)', 'def "$1" value "$2"', True)
//  will return:  def 'BLOCK' value 'test1'
//  ReplaceRegExpr ('({-i}block|var)\s*\(\s*([^ ]*)\s*\)\s*',
//   'BLOCK( test1)', 'def "$1" value "$2"')
//   will return:  def "$1" value "$2"

function QuoteRegExprMetaChars (const AStr : RegExprString) : RegExprString;
// Replace all metachars with its safe representation,
// for example 'abc$cd.(' converts into 'abc\$cd\.\('
// This function usefull for r.e. autogeneration from
// user input

function RegExprSubExpressions (const ARegExpr : string;
 ASubExprs : TStrings; AExtendedSyntax : boolean{$IFDEF DefParam}= False{$ENDIF}) : integer;
// Makes list of subexpressions found in ARegExpr r.e.
// In ASubExps every item represent subexpression,
// from first to last, in format:
//  String - subexpression text (without '()')
//  low word of Object - starting position in ARegExpr, including '('
//   if exists! (first position is 1)
//  high word of Object - length, including starting '(' and ending ')'
//   if exist!
// AExtendedSyntax - must be True if modifier /m will be On while
// using the r.e.
// Usefull for GUI editors of r.e. etc (You can find example of using
// in TestRExp.dpr project)
// Returns
//  0      Success. No unbalanced brackets was found;
//  -1     There are not enough closing brackets ')';
//  -(n+1) At position n was found opening '[' without  //###0.942
//         corresponding closing ']';
//  n      At position n was found closing bracket ')' without
//         corresponding opening '('.
// If Result <> 0, then ASubExpr can contain empty items or illegal ones


implementation

uses
{$IFDEF SYN_WIN32}
 Windows; // CharUpper/Lower
{$ELSE}
  Libc; //Qt.pas from Borland does not expose char handling functions
{$ENDIF}

const
 TRegExprVersionMajor : integer = 0;
 TRegExprVersionMinor : integer = 952;
 // TRegExpr.VersionMajor/Minor return values of this constants

 MaskModI = 1;  // modifier /i bit in fModifiers
 MaskModR = 2;  // -"- /r
 MaskModS = 4;  // -"- /s
 MaskModG = 8;  // -"- /g
 MaskModM = 16; // -"- /m
 MaskModX = 32; // -"- /x

 {$IFDEF UniCode}
 XIgnoredChars = ' '#9#$d#$a;
 {$ELSE}
 XIgnoredChars = [' ', #9, #$d, #$a];
 {$ENDIF}

{=============================================================}
{=================== WideString functions ====================}
{=============================================================}

{$IFDEF UniCode}

function StrPCopy (Dest: PRegExprChar; const Source: RegExprString): PRegExprChar;
 var
  i, Len : Integer;
 begin
  Len := length (Source); //###0.932
  for i := 1 to Len do
   Dest [i - 1] := Source [i];
  Dest [Len] := #0;
  Result := Dest;
 end; { of function StrPCopy
--------------------------------------------------------------}

function StrLCopy (Dest, Source: PRegExprChar; MaxLen: Cardinal): PRegExprChar;
 var i: Integer;
 begin
  for i := 0 to MaxLen - 1 do
   Dest [i] := Source [i];
  Result := Dest;
 end; { of function StrLCopy
--------------------------------------------------------------}

function StrLen (Str: PRegExprChar): Cardinal;
 begin
  Result:=0;
  while Str [result] <> #0
   do Inc (Result);
 end; { of function StrLen
--------------------------------------------------------------}

function StrPos (Str1, Str2: PRegExprChar): PRegExprChar;
 var n: Integer;
 begin
  Result := nil;
  n := Pos (RegExprString (Str2), RegExprString (Str1));
  if n = 0
   then EXIT;
  Result := Str1 + n - 1;
 end; { of function StrPos
--------------------------------------------------------------}

function StrLComp (Str1, Str2: PRegExprChar; MaxLen: Cardinal): Integer;
 var S1, S2: RegExprString;
 begin
  S1 := Str1;
  S2 := Str2;
  if Copy (S1, 1, MaxLen) > Copy (S2, 1, MaxLen)
   then Result := 1
   else
    if Copy (S1, 1, MaxLen) < Copy (S2, 1, MaxLen)
     then Result := -1
     else Result := 0;
 end; { function StrLComp
--------------------------------------------------------------}

function StrScan (Str: PRegExprChar; Chr: WideChar): PRegExprChar;
 begin
  Result := nil;
  while (Str^ <> #0) and (Str^ <> Chr)
   do Inc (Str);
  if (Str^ <> #0)
   then Result := Str;
 end; { of function StrScan
--------------------------------------------------------------}

{$ENDIF}


{=============================================================}
{===================== Global functions ======================}
{=============================================================}

function ExecRegExpr (const ARegExpr, AInputStr : RegExprString) : boolean;
 var r : TRegExpr;
 begin
  r := TRegExpr.Create;
  try
    r.Expression := ARegExpr;
    Result := r.Exec (AInputStr);
    finally r.Free;
   end;
 end; { of function ExecRegExpr
--------------------------------------------------------------}

procedure SplitRegExpr (const ARegExpr, AInputStr : RegExprString; APieces : TStrings);
 var r : TRegExpr;
 begin
  APieces.Clear;
  r := TRegExpr.Create;
  try
    r.Expression := ARegExpr;
    r.Split (AInputStr, APieces);
    finally r.Free;
   end;
 end; { of procedure SplitRegExpr
--------------------------------------------------------------}

function ReplaceRegExpr (const ARegExpr, AInputStr, AReplaceStr : RegExprString;
      AUseSubstitution : boolean{$IFDEF DefParam}= False{$ENDIF}) : RegExprString;
 begin
  with TRegExpr.Create do try
    Expression := ARegExpr;
    Result := Replace (AInputStr, AReplaceStr, AUseSubstitution);
    finally Free;
   end;
 end; { of function ReplaceRegExpr
--------------------------------------------------------------}

function QuoteRegExprMetaChars (const AStr : RegExprString) : RegExprString;
 const
  RegExprMetaSet : RegExprString = '^$.[()|?+*'+EscChar+'{'
  + ']}'; // - this last are additional to META.
  // Very similar to META array, but slighly changed.
  // !Any changes in META array must be synchronized with this set.
 var
  i, i0, Len : integer;
 begin
  Result := '';
  Len := length (AStr);
  i := 1;
  i0 := i;
  while i <= Len do begin
    if Pos (AStr [i], RegExprMetaSet) > 0 then begin
      Result := Result + System.Copy (AStr, i0, i - i0)
                 + EscChar + AStr [i];
      i0 := i + 1;
     end;
    inc (i);
   end;
  Result := Result + System.Copy (AStr, i0, MaxInt); // Tail
 end; { of function QuoteRegExprMetaChars
--------------------------------------------------------------}

function RegExprSubExpressions (const ARegExpr : string;
 ASubExprs : TStrings; AExtendedSyntax : boolean{$IFDEF DefParam}= False{$ENDIF}) : integer;
 type
  TStackItemRec =  record //###0.945
    SubExprIdx : integer;
    StartPos : integer;
   end;
  TStackArray = packed array [0 .. NSUBEXPMAX - 1] of TStackItemRec;
 var
  Len, SubExprLen : integer;
  i, i0 : integer;
  Modif : integer;
  Stack : ^TStackArray; //###0.945
  StackIdx, StackSz : integer;
 begin
  Result := 0; // no unbalanced brackets found at this very moment

  ASubExprs.Clear; // I don't think that adding to non empty list
  // can be usefull, so I simplified algorithm to work only with empty list

  Len := length (ARegExpr); // some optimization tricks

  // first we have to calculate number of subexpression to reserve
  // space in Stack array (may be we'll reserve more then need, but
  // it's faster then memory reallocation during parsing)
  StackSz := 1; // add 1 for entire r.e.
  for i := 1 to Len do
   if ARegExpr [i] = '('
    then inc (StackSz);
//  SetLength (Stack, StackSz); //###0.945
  GetMem (Stack, SizeOf (TStackItemRec) * StackSz);
  try

  StackIdx := 0;
  i := 1;
  while (i <= Len) do begin
    case ARegExpr [i] of
      '(': begin
        if (i < Len) and (ARegExpr [i + 1] = '?') then begin
           // this is not subexpression, but comment or other
           // Perl extension. We must check is it (?ismxrg-ismxrg)
           // and change AExtendedSyntax if /x is changed.
           inc (i, 2); // skip '(?'
           i0 := i;
           while (i <= Len) and (ARegExpr [i] <> ')')
            do inc (i);
           if i > Len
            then Result := -1 // unbalansed '('
            else
             if TRegExpr.ParseModifiersStr (System.Copy (ARegExpr, i, i - i0), Modif)
              then AExtendedSyntax := (Modif and MaskModX) <> 0;
          end
         else begin // subexpression starts
           ASubExprs.Add (''); // just reserve space
           with Stack [StackIdx] do begin
             SubExprIdx := ASubExprs.Count - 1;
             StartPos := i;
            end;
           inc (StackIdx);
          end;
       end;
      ')': begin
        if StackIdx = 0
         then Result := i // unbalanced ')'
         else begin
           dec (StackIdx);
           with Stack [StackIdx] do begin
             SubExprLen := i - StartPos + 1;
             ASubExprs.Objects [SubExprIdx] :=
              TObject (StartPos or (SubExprLen ShL 16));
             ASubExprs [SubExprIdx] := System.Copy (
              ARegExpr, StartPos + 1, SubExprLen - 2); // add without brackets
            end;
          end;
       end;
      EscChar: inc (i); // skip quoted symbol
      '[': begin
        // we have to skip character ranges at once, because they can
        // contain '#', and '#' in it must NOT be recognized as eXtended
        // comment beginning!
        i0 := i;
        inc (i);
        if ARegExpr [i] = ']' // cannot be 'emty' ranges - this interpretes
         then inc (i);        // as ']' by itself
        while (i <= Len) and (ARegExpr [i] <> ']') do
         if ARegExpr [i] = EscChar //###0.942
          then inc (i, 2) // skip 'escaped' char to prevent stopping at '\]'
          else inc (i);
        if (i > Len) or (ARegExpr [i] <> ']') //###0.942
         then Result := - (i0 + 1); // unbalansed '[' //###0.942
       end;
      '#': if AExtendedSyntax then begin
        // skip eXtended comments
        while (i <= Len) and (ARegExpr [i] <> #$d) and (ARegExpr [i] <> #$a)
         // do not use [#$d, #$a] due to UniCode compatibility
         do inc (i);
        while (i + 1 <= Len) and ((ARegExpr [i + 1] = #$d) or (ARegExpr [i + 1] = #$a))
         do inc (i); // attempt to work with different kinds of line separators
        // now we are at the line separator that must be skipped.
       end;
      // here is no 'else' clause - we simply skip ordinary chars
     end; // of case
    inc (i); // skip scanned char
    // ! can move after Len due to skipping quoted symbol
   end;

  // check brackets balance
  if StackIdx <> 0
   then Result := -1; // unbalansed '('

  // check if entire r.e. added
  if (ASubExprs.Count = 0)
   or ((integer (ASubExprs.Objects [0]) and $FFFF) <> 1)
   or (((integer (ASubExprs.Objects [0]) ShR 16) and $FFFF) <> Len)
    // whole r.e. wasn't added because it isn't bracketed
    // well, we add it now:
    then ASubExprs.InsertObject (0, ARegExpr, TObject ((Len ShL 16) or 1));

  finally FreeMem (Stack);
  end;
 end; { of function RegExprSubExpressions
--------------------------------------------------------------}



const
 MAGIC       = TREOp (216);// programm signature

// name            opcode    opnd? meaning
 EEND        = TREOp (0);  // -    End of program
 BOL         = TREOp (1);  // -    Match "" at beginning of line
 EOL         = TREOp (2);  // -    Match "" at end of line
 ANY         = TREOp (3);  // -    Match any one character
 ANYOF       = TREOp (4);  // Str  Match any character in string Str
 ANYBUT      = TREOp (5);  // Str  Match any char. not in string Str
 BRANCH      = TREOp (6);  // Node Match this alternative, or the next
 BACK        = TREOp (7);  // -    Jump backward (Next < 0)
 EXACTLY     = TREOp (8);  // Str  Match string Str
 NOTHING     = TREOp (9);  // -    Match empty string
 STAR        = TREOp (10); // Node Match this (simple) thing 0 or more times
 PLUS        = TREOp (11); // Node Match this (simple) thing 1 or more times
 ANYDIGIT    = TREOp (12); // -    Match any digit (equiv [0-9])
 NOTDIGIT    = TREOp (13); // -    Match not digit (equiv [0-9])
 ANYLETTER   = TREOp (14); // -    Match any letter from property WordChars
 NOTLETTER   = TREOp (15); // -    Match not letter from property WordChars
 ANYSPACE    = TREOp (16); // -    Match any space char (see property SpaceChars)
 NOTSPACE    = TREOp (17); // -    Match not space char (see property SpaceChars)
 BRACES      = TREOp (18); // Node,Min,Max Match this (simple) thing from Min to Max times.
                           //      Min and Max are TREBracesArg
 COMMENT     = TREOp (19); // -    Comment ;)
 EXACTLYCI   = TREOp (20); // Str  Match string Str case insensitive
 ANYOFCI     = TREOp (21); // Str  Match any character in string Str, case insensitive
 ANYBUTCI    = TREOp (22); // Str  Match any char. not in string Str, case insensitive
 LOOPENTRY   = TREOp (23); // Node Start of loop (Node - LOOP for this loop)
 LOOP        = TREOp (24); // Node,Min,Max,LoopEntryJmp - back jump for LOOPENTRY.
                           //      Min and Max are TREBracesArg
                           //      Node - next node in sequence,
                           //      LoopEntryJmp - associated LOOPENTRY node addr
 ANYOFTINYSET= TREOp (25); // Chrs Match any one char from Chrs (exactly TinySetLen chars)
 ANYBUTTINYSET=TREOp (26); // Chrs Match any one char not in Chrs (exactly TinySetLen chars)
 ANYOFFULLSET= TREOp (27); // Set  Match any one char from set of char
                           // - very fast (one CPU instruction !) but takes 32 bytes of p-code
 BSUBEXP     = TREOp (28); // Idx  Match previously matched subexpression #Idx (stored as REChar) //###0.936
 BSUBEXPCI   = TREOp (29); // Idx  -"- in case-insensitive mode

 // Non-Greedy Style Ops //###0.940
 STARNG      = TREOp (30); // Same as START but in non-greedy mode
 PLUSNG      = TREOp (31); // Same as PLUS but in non-greedy mode
 BRACESNG    = TREOp (32); // Same as BRACES but in non-greedy mode
 LOOPNG      = TREOp (33); // Same as LOOP but in non-greedy mode

 // Multiline mode \m
 BOLML       = TREOp (34);  // -    Match "" at beginning of line
 EOLML       = TREOp (35);  // -    Match "" at end of line
 ANYML       = TREOp (36);  // -    Match any one character

 // Word boundary
 BOUND       = TREOp (37);  // Match "" between words //###0.943
 NOTBOUND    = TREOp (38);  // Match "" not between words //###0.943

 // !!! Change OPEN value if you add new opcodes !!!

 OPEN        = TREOp (39); // -    Mark this point in input as start of \n
                           //      OPEN + 1 is \1, etc.
 CLOSE       = TREOp (ord (OPEN) + NSUBEXP);
                           // -    Analogous to OPEN.

 // !!! Don't add new OpCodes after CLOSE !!!

// We work with p-code thru pointers, compatible with PRegExprChar.
// Note: all code components (TRENextOff, TREOp, TREBracesArg, etc)
// must have lengths that can be divided by SizeOf (REChar) !
// A node is TREOp of opcode followed Next "pointer" of TRENextOff type.
// The Next is a offset from the opcode of the node containing it.
// An operand, if any, simply follows the node. (Note that much of
// the code generation knows about this implicit relationship!)
// Using TRENextOff=integer speed up p-code processing.

// Opcodes description:
//
// BRANCH The set of branches constituting a single choice are hooked
//      together with their "next" pointers, since precedence prevents
//      anything being concatenated to any individual branch.  The
//      "next" pointer of the last BRANCH in a choice points to the
//      thing following the whole choice.  This is also where the
//      final "next" pointer of each individual branch points; each
//      branch starts with the operand node of a BRANCH node.
// BACK Normal "next" pointers all implicitly point forward; BACK
//      exists to make loop structures possible.
// STAR,PLUS,BRACES '?', and complex '*' and '+', are implemented as
//      circular BRANCH structures using BACK. Complex '{min,max}'
//      - as pair LOOPENTRY-LOOP (see below). Simple cases (one
//      character per match) are implemented with STAR, PLUS and
//      BRACES for speed and to minimize recursive plunges.
// LOOPENTRY,LOOP {min,max} are implemented as special pair
//      LOOPENTRY-LOOP. Each LOOPENTRY initialize loopstack for
//      current level.
// OPEN,CLOSE are numbered at compile time.


{=============================================================}
{================== Error handling section ===================}
{=============================================================}

const
 reeOk = 0;
 reeCompNullArgument = 100;
 reeCompRegexpTooBig = 101;
 reeCompParseRegTooManyBrackets = 102;
 reeCompParseRegUnmatchedBrackets = 103;
 reeCompParseRegUnmatchedBrackets2 = 104;
 reeCompParseRegJunkOnEnd = 105;
 reePlusStarOperandCouldBeEmpty = 106;
 reeNestedSQP = 107;
 reeBadHexDigit = 108;
 reeInvalidRange = 109;
 reeParseAtomTrailingBackSlash = 110;
 reeNoHexCodeAfterBSlashX = 111;
 reeHexCodeAfterBSlashXTooBig = 112;
 reeUnmatchedSqBrackets = 113;
 reeInternalUrp = 114;
 reeQPSBFollowsNothing = 115;
 reeTrailingBackSlash = 116;
 reeRarseAtomInternalDisaster = 119;
 reeBRACESArgTooBig = 122;
 reeBracesMinParamGreaterMax = 124;
 reeUnclosedComment = 125;
 reeComplexBracesNotImplemented = 126;
 reeUrecognizedModifier = 127;
 reeBadLinePairedSeparator = 128;
 reeRegRepeatCalledInappropriately = 1000;
 reeMatchPrimMemoryCorruption = 1001;
 reeMatchPrimCorruptedPointers = 1002;
 reeNoExpression = 1003;
 reeCorruptedProgram = 1004;
 reeNoInpitStringSpecified = 1005;
 reeOffsetMustBeGreaterThen0 = 1006;
 reeExecNextWithoutExec = 1007;
 reeGetInputStringWithoutInputString = 1008;
 reeDumpCorruptedOpcode = 1011;
 reeModifierUnsupported = 1013;
 reeLoopStackExceeded = 1014;
 reeLoopWithoutEntry = 1015;
 reeBadPCodeImported = 2000;

function TRegExpr.ErrorMsg (AErrorID : integer) : RegExprString;
 begin
  case AErrorID of
    reeOk: Result := 'No errors';
    reeCompNullArgument: Result := 'TRegExpr(comp): Null Argument';
    reeCompRegexpTooBig: Result := 'TRegExpr(comp): Regexp Too Big';
    reeCompParseRegTooManyBrackets: Result := 'TRegExpr(comp): ParseReg Too Many ()';
    reeCompParseRegUnmatchedBrackets: Result := 'TRegExpr(comp): ParseReg Unmatched ()';
    reeCompParseRegUnmatchedBrackets2: Result := 'TRegExpr(comp): ParseReg Unmatched ()';
    reeCompParseRegJunkOnEnd: Result := 'TRegExpr(comp): ParseReg Junk On End';
    reePlusStarOperandCouldBeEmpty: Result := 'TRegExpr(comp): *+ Operand Could Be Empty';
    reeNestedSQP: Result := 'TRegExpr(comp): Nested *?+';
    reeBadHexDigit: Result := 'TRegExpr(comp): Bad Hex Digit';
    reeInvalidRange: Result := 'TRegExpr(comp): Invalid [] Range';
    reeParseAtomTrailingBackSlash: Result := 'TRegExpr(comp): Parse Atom Trailing \';
    reeNoHexCodeAfterBSlashX: Result := 'TRegExpr(comp): No Hex Code After \x';
    reeHexCodeAfterBSlashXTooBig: Result := 'TRegExpr(comp): Hex Code After \x Is Too Big';
    reeUnmatchedSqBrackets: Result := 'TRegExpr(comp): Unmatched []';
    reeInternalUrp: Result := 'TRegExpr(comp): Internal Urp';
    reeQPSBFollowsNothing: Result := 'TRegExpr(comp): ?+*{ Follows Nothing';
    reeTrailingBackSlash: Result := 'TRegExpr(comp): Trailing \';
    reeRarseAtomInternalDisaster: Result := 'TRegExpr(comp): RarseAtom Internal Disaster';
    reeBRACESArgTooBig: Result := 'TRegExpr(comp): BRACES Argument Too Big';
    reeBracesMinParamGreaterMax: Result := 'TRegExpr(comp): BRACE Min Param Greater then Max';
    reeUnclosedComment: Result := 'TRegExpr(comp): Unclosed (?#Comment)';
    reeComplexBracesNotImplemented: Result := 'TRegExpr(comp): If you want take part in beta-testing BRACES ''{min,max}'' and non-greedy ops ''*?'', ''+?'', ''??'' for complex cases - remove ''.'' from {.$DEFINE ComplexBraces}';
    reeUrecognizedModifier: Result := 'TRegExpr(comp): Urecognized Modifier';
    reeBadLinePairedSeparator: Result := 'TRegExpr(comp): LinePairedSeparator must countain two different chars or no chars at all';

    reeRegRepeatCalledInappropriately: Result := 'TRegExpr(exec): RegRepeat Called Inappropriately';
    reeMatchPrimMemoryCorruption: Result := 'TRegExpr(exec): MatchPrim Memory Corruption';
    reeMatchPrimCorruptedPointers: Result := 'TRegExpr(exec): MatchPrim Corrupted Pointers';
    reeNoExpression: Result := 'TRegExpr(exec): Not Assigned Expression Property';
    reeCorruptedProgram: Result := 'TRegExpr(exec): Corrupted Program';
    reeNoInpitStringSpecified: Result := 'TRegExpr(exec): No Input String Specified';
    reeOffsetMustBeGreaterThen0: Result := 'TRegExpr(exec): Offset Must Be Greater Then 0';
    reeExecNextWithoutExec: Result := 'TRegExpr(exec): ExecNext Without Exec[Pos]';
    reeGetInputStringWithoutInputString: Result := 'TRegExpr(exec): GetInputString Without InputString';
    reeDumpCorruptedOpcode: Result := 'TRegExpr(dump): Corrupted Opcode';
    reeLoopStackExceeded: Result := 'TRegExpr(exec): Loop Stack Exceeded';
    reeLoopWithoutEntry: Result := 'TRegExpr(exec): Loop Without LoopEntry !';

    reeBadPCodeImported: Result := 'TRegExpr(misc): Bad p-code imported';
    else Result := 'Unknown error';
   end;
 end; { of procedure TRegExpr.Error
--------------------------------------------------------------}

function TRegExpr.LastError : integer;
 begin
  Result := fLastError;
  fLastError := reeOk;
 end; { of function TRegExpr.LastError
--------------------------------------------------------------}


{=============================================================}
{===================== Common section ========================}
{=============================================================}

class function TRegExpr.VersionMajor : integer; //###0.944
 begin
  Result := TRegExprVersionMajor;
 end; { of class function TRegExpr.VersionMajor
--------------------------------------------------------------}

class function TRegExpr.VersionMinor : integer; //###0.944
 begin
  Result := TRegExprVersionMinor;
 end; { of class function TRegExpr.VersionMinor
--------------------------------------------------------------}

constructor TRegExpr.Create;
 begin
  inherited;
  programm := nil;
  fExpression := nil;
  fInputString := nil;

  regexpbeg := nil;
  fExprIsCompiled := false;

  ModifierI := RegExprModifierI;
  ModifierR := RegExprModifierR;
  ModifierS := RegExprModifierS;
  ModifierG := RegExprModifierG;
  ModifierM := RegExprModifierM; //###0.940

  SpaceChars := RegExprSpaceChars; //###0.927
  WordChars := RegExprWordChars; //###0.929
  fInvertCase := RegExprInvertCaseFunction; //###0.927

  fLineSeparators := RegExprLineSeparators; //###0.941
  LinePairedSeparator := RegExprLinePairedSeparator; //###0.941
 end; { of constructor TRegExpr.Create
--------------------------------------------------------------}

destructor TRegExpr.Destroy;
 begin
  if programm <> nil
   then FreeMem (programm);
  if fExpression <> nil
   then FreeMem (fExpression);
  if fInputString <> nil
   then FreeMem (fInputString);
 end; { of destructor TRegExpr.Destroy
--------------------------------------------------------------}

class function TRegExpr.InvertCaseFunction (const Ch : REChar) : REChar;
 begin
  {$IFDEF UniCode}
  if Ch >= #128
   then Result := Ch
  else
  {$ENDIF}
   begin
    Result := {$IFDEF FPC}AnsiUpperCase (Ch) [1]{$ELSE} {$IFDEF SYN_WIN32}REChar (CharUpper (PChar (Ch))){$ELSE}REChar (toupper (integer (Ch))){$ENDIF} {$ENDIF};
    if Result = Ch
     then Result := {$IFDEF FPC}AnsiLowerCase (Ch) [1]{$ELSE} {$IFDEF SYN_WIN32}REChar (CharLower (PChar (Ch))){$ELSE}REChar(tolower (integer (Ch))){$ENDIF} {$ENDIF};
   end;
 end; { of function TRegExpr.InvertCaseFunction
--------------------------------------------------------------}

function TRegExpr.GetExpression : RegExprString;
 begin
  if fExpression <> nil
   then Result := fExpression
   else Result := '';
 end; { of function TRegExpr.GetExpression
--------------------------------------------------------------}

procedure TRegExpr.SetExpression (const s : RegExprString);
 var
  Len : integer; //###0.950
 begin
  if (s <> fExpression) or not fExprIsCompiled then begin
    fExprIsCompiled := false;
    if fExpression <> nil then begin
      FreeMem (fExpression);
      fExpression := nil;
     end;
    if s <> '' then begin
      Len := length (s); //###0.950
      GetMem (fExpression, (Len + 1) * SizeOf (REChar));
//      StrPCopy (fExpression, s); //###0.950 replaced due to StrPCopy limitation of 255 chars
      {$IFDEF UniCode}
      StrPCopy (fExpression, Copy (s, 1, Len)); //###0.950
      {$ELSE}
      StrLCopy (fExpression, PRegExprChar (s), Len); //###0.950
      {$ENDIF UniCode}

      InvalidateProgramm; //###0.941
     end;
   end;
 end; { of procedure TRegExpr.SetExpression
--------------------------------------------------------------}

function TRegExpr.GetSubExprMatchCount : integer;
 begin
  if Assigned (fInputString) then begin
     Result := NSUBEXP - 1;
     while (Result > 0) and ((startp [Result] = nil)
                             or (endp [Result] = nil))
      do dec (Result);
    end
   else Result := -1;
 end; { of function TRegExpr.GetSubExprMatchCount
--------------------------------------------------------------}

function TRegExpr.GetMatchPos (Idx : integer) : integer;
 begin
  if (Idx >= 0) and (Idx < NSUBEXP) and Assigned (fInputString)
     and Assigned (startp [Idx]) and Assigned (endp [Idx]) then begin
     Result := (startp [Idx] - fInputString) + 1;
    end
   else Result := -1;
 end; { of function TRegExpr.GetMatchPos
--------------------------------------------------------------}

function TRegExpr.GetMatchLen (Idx : integer) : integer;
 begin
  if (Idx >= 0) and (Idx < NSUBEXP) and Assigned (fInputString)
     and Assigned (startp [Idx]) and Assigned (endp [Idx]) then begin
     Result := endp [Idx] - startp [Idx];
    end
   else Result := -1;
 end; { of function TRegExpr.GetMatchLen
--------------------------------------------------------------}

function TRegExpr.GetMatch (Idx : integer) : RegExprString;
 begin
  if (Idx >= 0) and (Idx < NSUBEXP) and Assigned (fInputString)
     and Assigned (startp [Idx]) and Assigned (endp [Idx])
   //then Result := copy (fInputString, MatchPos [Idx], MatchLen [Idx]) //###0.929
   then SetString (Result, startp [idx], endp [idx] - startp [idx])
   else Result := '';
 end; { of function TRegExpr.GetMatch
--------------------------------------------------------------}

function TRegExpr.GetModifierStr : RegExprString;
 begin
  Result := '-';

  if ModifierI
   then Result := 'i' + Result
   else Result := Result + 'i';
  if ModifierR
   then Result := 'r' + Result
   else Result := Result + 'r';
  if ModifierS
   then Result := 's' + Result
   else Result := Result + 's';
  if ModifierG
   then Result := 'g' + Result
   else Result := Result + 'g';
  if ModifierM
   then Result := 'm' + Result
   else Result := Result + 'm';
  if ModifierX
   then Result := 'x' + Result
   else Result := Result + 'x';

  if Result [length (Result)] = '-' // remove '-' if all modifiers are 'On'
   then System.Delete (Result, length (Result), 1);
 end; { of function TRegExpr.GetModifierStr
--------------------------------------------------------------}

class function TRegExpr.ParseModifiersStr (const AModifiers : RegExprString;
var AModifiersInt : integer) : boolean;
// !!! Be carefull - this is class function and must not use object instance fields
 var
  i : integer;
  IsOn : boolean;
  Mask : integer;
 begin
  Result := true;
  IsOn := true;
  Mask := 0; // prevent compiler warning
  for i := 1 to length (AModifiers) do
   if AModifiers [i] = '-'
    then IsOn := false
    else begin
      if Pos (AModifiers [i], 'iI') > 0
       then Mask := MaskModI
      else if Pos (AModifiers [i], 'rR') > 0
       then Mask := MaskModR
      else if Pos (AModifiers [i], 'sS') > 0
       then Mask := MaskModS
      else if Pos (AModifiers [i], 'gG') > 0
       then Mask := MaskModG
      else if Pos (AModifiers [i], 'mM') > 0
       then Mask := MaskModM
      else if Pos (AModifiers [i], 'xX') > 0
       then Mask := MaskModX
      else begin
        Result := false;
        EXIT;
       end;
      if IsOn
       then AModifiersInt := AModifiersInt or Mask
       else AModifiersInt := AModifiersInt and not Mask;
     end;
 end; { of function TRegExpr.ParseModifiersStr
--------------------------------------------------------------}

procedure TRegExpr.SetModifierStr (const AModifiers : RegExprString);
 begin
  if not ParseModifiersStr (AModifiers, fModifiers)
   then Error (reeModifierUnsupported);
 end; { of procedure TRegExpr.SetModifierStr
--------------------------------------------------------------}

function TRegExpr.GetModifier (AIndex : integer) : boolean;
 var
  Mask : integer;
 begin
  Result := false;
  case AIndex of
    1: Mask := MaskModI;
    2: Mask := MaskModR;
    3: Mask := MaskModS;
    4: Mask := MaskModG;
    5: Mask := MaskModM;
    6: Mask := MaskModX;
    else begin
      Error (reeModifierUnsupported);
      EXIT;
     end;
   end;
  Result := (fModifiers and Mask) <> 0;
 end; { of function TRegExpr.GetModifier
--------------------------------------------------------------}

procedure TRegExpr.SetModifier (AIndex : integer; ASet : boolean);
 var
  Mask : integer;
 begin
  case AIndex of
    1: Mask := MaskModI;
    2: Mask := MaskModR;
    3: Mask := MaskModS;
    4: Mask := MaskModG;
    5: Mask := MaskModM;
    6: Mask := MaskModX;
    else begin
      Error (reeModifierUnsupported);
      EXIT;
     end;
   end;
  if ASet
   then fModifiers := fModifiers or Mask
   else fModifiers := fModifiers and not Mask;
 end; { of procedure TRegExpr.SetModifier
--------------------------------------------------------------}


{=============================================================}
{==================== Compiler section =======================}
{=============================================================}

procedure TRegExpr.InvalidateProgramm;
 begin
  if programm <> nil then begin
    FreeMem (programm);
    programm := nil;
   end;
 end; { of procedure TRegExpr.InvalidateProgramm
--------------------------------------------------------------}

procedure TRegExpr.Compile; //###0.941
 begin
  if fExpression = nil then begin // No Expression assigned
    Error (reeNoExpression);
    EXIT;
   end;
  CompileRegExpr (fExpression);
 end; { of procedure TRegExpr.Compile
--------------------------------------------------------------}

function TRegExpr.IsProgrammOk : boolean;
 {$IFNDEF UniCode}
 var
  i : integer;
 {$ENDIF}
 begin
  Result := false;

  // check modifiers
  if fModifiers <> fProgModifiers //###0.941
   then InvalidateProgramm;

  // can we optimize line separators by using sets?
  {$IFNDEF UniCode}
  fLineSeparatorsSet := [];
  for i := 1 to length (fLineSeparators)
   do System.Include (fLineSeparatorsSet, fLineSeparators [i]);
  {$ENDIF}

  // [Re]compile if needed
  if programm = nil
   then Compile; //###0.941

  // check [re]compiled programm
  if programm = nil
   then EXIT // error was set/raised by Compile (was reeExecAfterCompErr)
  else if programm [0] <> MAGIC // Program corrupted.
   then Error (reeCorruptedProgram)
  else Result := true;
 end; { of function TRegExpr.IsProgrammOk
--------------------------------------------------------------}

procedure TRegExpr.Tail (p : PRegExprChar; val : PRegExprChar);
// set the next-pointer at the end of a node chain
 var
  scan : PRegExprChar;
  temp : PRegExprChar;
//  i : int64;
 begin
  if p = @regdummy
   then EXIT;
  // Find last node.
  scan := p;
  REPEAT
   temp := regnext (scan);
   if temp = nil
    then BREAK;
   scan := temp;
  UNTIL false;
  // Set Next 'pointer'
  if val < scan
   then PRENextOff (scan + REOpSz)^ := - (scan - val) //###0.948
   // work around PWideChar subtraction bug (Delphi uses
   // shr after subtraction to calculate widechar distance %-( )
   // so, if difference is negative we have .. the "feature" :(
   // I could wrap it in $IFDEF UniCode, but I didn't because
   // "P – Q computes the difference between the address given
   // by P (the higher address) and the address given by Q (the
   // lower address)" - Delphi help quotation.
   else PRENextOff (scan + REOpSz)^ := val - scan; //###0.933
 end; { of procedure TRegExpr.Tail
--------------------------------------------------------------}

procedure TRegExpr.OpTail (p : PRegExprChar; val : PRegExprChar);
// regtail on operand of first argument; nop if operandless
 begin
  // "Operandless" and "op != BRANCH" are synonymous in practice.
  if (p = nil) or (p = @regdummy) or (PREOp (p)^ <> BRANCH)
   then EXIT;
  Tail (p + REOpSz + RENextOffSz, val); //###0.933
 end; { of procedure TRegExpr.OpTail
--------------------------------------------------------------}

function TRegExpr.EmitNode (op : TREOp) : PRegExprChar; //###0.933
// emit a node, return location
 begin
  Result := regcode;
  if Result <> @regdummy then begin
     PREOp (regcode)^ := op;
     inc (regcode, REOpSz);
     PRENextOff (regcode)^ := 0; // Next "pointer" := nil
     inc (regcode, RENextOffSz);
    end
   else inc (regsize, REOpSz + RENextOffSz); // compute code size without code generation
 end; { of function TRegExpr.EmitNode
--------------------------------------------------------------}

procedure TRegExpr.EmitC (b : REChar);
// emit a byte to code
 begin
  if regcode <> @regdummy then begin
     regcode^ := b;
     inc (regcode);
    end
   else inc (regsize); // Type of p-code pointer always is ^REChar
 end; { of procedure TRegExpr.EmitC
--------------------------------------------------------------}

procedure TRegExpr.InsertOperator (op : TREOp; opnd : PRegExprChar; sz : integer);
// insert an operator in front of already-emitted operand
// Means relocating the operand.
 var
  src, dst, place : PRegExprChar;
  i : integer;
 begin
  if regcode = @regdummy then begin
    inc (regsize, sz);
    EXIT;
   end;
  src := regcode;
  inc (regcode, sz);
  dst := regcode;
  while src > opnd do begin
    dec (dst);
    dec (src);
    dst^ := src^;
   end;
  place := opnd; // Op node, where operand used to be.
  PREOp (place)^ := op;
  inc (place, REOpSz);
  for i := 1 + REOpSz to sz do begin
    place^ := #0;
    inc (place);
   end;
 end; { of procedure TRegExpr.InsertOperator
--------------------------------------------------------------}

function strcspn (s1 : PRegExprChar; s2 : PRegExprChar) : integer;
// find length of initial segment of s1 consisting
// entirely of characters not from s2
 var scan1, scan2 : PRegExprChar;
 begin
  Result := 0;
  scan1 := s1;
  while scan1^ <> #0 do begin
    scan2 := s2;
    while scan2^ <> #0 do
     if scan1^ = scan2^
      then EXIT
      else inc (scan2);
    inc (Result);
    inc (scan1)
   end;
 end; { of function strcspn
--------------------------------------------------------------}

const
// Flags to be passed up and down.
 HASWIDTH =   01; // Known never to match nil string.
 SIMPLE   =   02; // Simple enough to be STAR/PLUS/BRACES operand.
 SPSTART  =   04; // Starts with * or +.
 WORST    =   0;  // Worst case.
 META : array [0 .. 12] of REChar = (
  '^', '$', '.', '[', '(', ')', '|', '?', '+', '*', EscChar, '{', #0);
 // Any modification must be synchronized with QuoteRegExprMetaChars !!!

{$IFDEF UniCode}
 RusRangeLo : array [0 .. 33] of REChar =
  (#$430,#$431,#$432,#$433,#$434,#$435,#$451,#$436,#$437,
   #$438,#$439,#$43A,#$43B,#$43C,#$43D,#$43E,#$43F,
   #$440,#$441,#$442,#$443,#$444,#$445,#$446,#$447,
   #$448,#$449,#$44A,#$44B,#$44C,#$44D,#$44E,#$44F,#0);
 RusRangeHi : array [0 .. 33] of REChar =
  (#$410,#$411,#$412,#$413,#$414,#$415,#$401,#$416,#$417,
   #$418,#$419,#$41A,#$41B,#$41C,#$41D,#$41E,#$41F,
   #$420,#$421,#$422,#$423,#$424,#$425,#$426,#$427,
   #$428,#$429,#$42A,#$42B,#$42C,#$42D,#$42E,#$42F,#0);
 RusRangeLoLow = #$430{'à'};
 RusRangeLoHigh = #$44F{'ÿ'};
 RusRangeHiLow = #$410{'À'};
 RusRangeHiHigh = #$42F{'ß'};
{$ELSE}
 RusRangeLo = 'àáâãäå¸æçèéêëìíîïðñòóôõö÷øùúûüýþÿ';
 RusRangeHi = 'ÀÁÂÃÄÅ¨ÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ×ØÙÚÛÜÝÞß';
 RusRangeLoLow = 'à';
 RusRangeLoHigh = 'ÿ';
 RusRangeHiLow = 'À';
 RusRangeHiHigh = 'ß';
{$ENDIF}

function TRegExpr.CompileRegExpr (exp : PRegExprChar) : boolean;
// compile a regular expression into internal code
// We can't allocate space until we know how big the compiled form will be,
// but we can't compile it (and thus know how big it is) until we've got a
// place to put the code.  So we cheat:  we compile it twice, once with code
// generation turned off and size counting turned on, and once "for real".
// This also means that we don't allocate space until we are sure that the
// thing really will compile successfully, and we never have to move the
// code and thus invalidate pointers into it.  (Note that it has to be in
// one piece because free() must be able to free it all.)
// Beware that the optimization-preparation code in here knows about some
// of the structure of the compiled regexp.
 var
  scan, longest : PRegExprChar;
  len : cardinal;
  flags : integer;
 begin
  Result := false; // life too dark

  regparse := nil; // for correct error handling
  regexpbeg := exp;
  try

  if programm <> nil then begin
    FreeMem (programm);
    programm := nil;
   end;

  if exp = nil then begin
    Error (reeCompNullArgument);
    EXIT;
   end;

  fProgModifiers := fModifiers;
  // well, may it's paranoia. I'll check it later... !!!!!!!!

  // First pass: determine size, legality.
  fCompModifiers := fModifiers;
  regparse := exp;
  regnpar := 1;
  regsize := 0;
  regcode := @regdummy;
  EmitC (MAGIC);
  if ParseReg (0, flags) = nil
   then EXIT;

  // Small enough for 2-bytes programm pointers ?
  // ###0.933 no real p-code length limits now :)))
//  if regsize >= 64 * 1024 then begin
//    Error (reeCompRegexpTooBig);
//    EXIT;
//   end;

  // Allocate space.
  GetMem (programm, regsize * SizeOf (REChar));

  // Second pass: emit code.
  fCompModifiers := fModifiers;
  regparse := exp;
  regnpar := 1;
  regcode := programm;
  EmitC (MAGIC);
  if ParseReg (0, flags) = nil
   then EXIT;

  // Dig out information for optimizations.
  {$IFDEF UseFirstCharSet} //###0.929
  FirstCharSet := [];
  FillFirstCharSet (programm + REOpSz);
  {$ENDIF}
  regstart := #0; // Worst-case defaults.
  reganch := #0;
  regmust := nil;
  regmlen := 0;
  scan := programm + REOpSz; // First BRANCH.
  if PREOp (regnext (scan))^ = EEND then begin // Only one top-level choice.
    scan := scan + REOpSz + RENextOffSz;

    // Starting-point info.
    if PREOp (scan)^ = EXACTLY
     then regstart := (scan + REOpSz + RENextOffSz)^
     else if PREOp (scan)^ = BOL
           then inc (reganch);

    // If there's something expensive in the r.e., find the longest
    // literal string that must appear and make it the regmust.  Resolve
    // ties in favor of later strings, since the regstart check works
    // with the beginning of the r.e. and avoiding duplication
    // strengthens checking.  Not a strong reason, but sufficient in the
    // absence of others.
    if (flags and SPSTART) <> 0 then begin
        longest := nil;
        len := 0;
        while scan <> nil do begin
          if (PREOp (scan)^ = EXACTLY)
             and (strlen (scan + REOpSz + RENextOffSz) >= len) then begin
              longest := scan + REOpSz + RENextOffSz;
              len := strlen (longest);
           end;
          scan := regnext (scan);
         end;
        regmust := longest;
        regmlen := len;
     end;
   end;

  Result := true;

  finally begin
    if not Result
     then InvalidateProgramm;
    regexpbeg := nil;
    fExprIsCompiled := Result; //###0.944
   end;
  end;

 end; { of function TRegExpr.CompileRegExpr
--------------------------------------------------------------}

function TRegExpr.ParseReg (paren : integer; var flagp : integer) : PRegExprChar;
// regular expression, i.e. main body or parenthesized thing
// Caller must absorb opening parenthesis.
// Combining parenthesis handling with the base level of regular expression
// is a trifle forced, but the need to tie the tails of the branches to what
// follows makes it hard to avoid.
 var
  ret, br, ender : PRegExprChar;
  parno : integer;
  flags : integer;
  SavedModifiers : integer;
 begin
  Result := nil;
  flagp := HASWIDTH; // Tentatively.
  parno := 0; // eliminate compiler stupid warning
  SavedModifiers := fCompModifiers;

  // Make an OPEN node, if parenthesized.
  if paren <> 0 then begin
      if regnpar >= NSUBEXP then begin
        Error (reeCompParseRegTooManyBrackets);
        EXIT;
       end;
      parno := regnpar;
      inc (regnpar);
      ret := EmitNode (TREOp (ord (OPEN) + parno));
    end
   else ret := nil;

  // Pick up the branches, linking them together.
  br := ParseBranch (flags);
  if br = nil then begin
    Result := nil;
    EXIT;
   end;
  if ret <> nil
   then Tail (ret, br) // OPEN -> first.
   else ret := br;
  if (flags and HASWIDTH) = 0
   then flagp := flagp and not HASWIDTH;
  flagp := flagp or flags and SPSTART;
  while (regparse^ = '|') do begin
    inc (regparse);
    br := ParseBranch (flags);
    if br = nil then begin
       Result := nil;
       EXIT;
      end;
    Tail (ret, br); // BRANCH -> BRANCH.
    if (flags and HASWIDTH) = 0
     then flagp := flagp and not HASWIDTH;
    flagp := flagp or flags and SPSTART;
   end;

  // Make a closing node, and hook it on the end.
  if paren <> 0
   then ender := EmitNode (TREOp (ord (CLOSE) + parno))
   else ender := EmitNode (EEND);
  Tail (ret, ender);

  // Hook the tails of the branches to the closing node.
  br := ret;
  while br <> nil do begin
    OpTail (br, ender);
    br := regnext (br);
   end;

  // Check for proper termination.
  if paren <> 0 then
   if regparse^ <> ')' then begin
      Error (reeCompParseRegUnmatchedBrackets);
      EXIT;
     end
    else inc (regparse); // skip trailing ')'
  if (paren = 0) and (regparse^ <> #0) then begin
      if regparse^ = ')'
       then Error (reeCompParseRegUnmatchedBrackets2)
       else Error (reeCompParseRegJunkOnEnd);
      EXIT;
    end;
  fCompModifiers := SavedModifiers; // restore modifiers of parent
  Result := ret;
 end; { of function TRegExpr.ParseReg
--------------------------------------------------------------}

function TRegExpr.ParseBranch (var flagp : integer) : PRegExprChar;
// one alternative of an | operator
// Implements the concatenation operator.
 var
  ret, chain, latest : PRegExprChar;
  flags : integer;
 begin
  flagp := WORST; // Tentatively.

  ret := EmitNode (BRANCH);
  chain := nil;
  while (regparse^ <> #0) and (regparse^ <> '|')
        and (regparse^ <> ')') do begin
    latest := ParsePiece (flags);
    if latest = nil then begin
      Result := nil;
      EXIT;
     end;
    flagp := flagp or flags and HASWIDTH;
    if chain = nil // First piece.
     then flagp := flagp or flags and SPSTART
     else Tail (chain, latest);
    chain := latest;
   end;
  if chain = nil // Loop ran zero times.
   then EmitNode (NOTHING);
  Result := ret;
 end; { of function TRegExpr.ParseBranch
--------------------------------------------------------------}

function TRegExpr.ParsePiece (var flagp : integer) : PRegExprChar;
// something followed by possible [*+?{]
// Note that the branching code sequences used for ? and the general cases
// of * and + and { are somewhat optimized:  they use the same NOTHING node as
// both the endmarker for their branch list and the body of the last branch.
// It might seem that this node could be dispensed with entirely, but the
// endmarker role is not redundant.
 function parsenum (AStart, AEnd : PRegExprChar) : TREBracesArg;
  begin
   Result := 0;
   if AEnd - AStart + 1 > 8 then begin // prevent stupid scanning
     Error (reeBRACESArgTooBig);
     EXIT;
    end;
   while AStart <= AEnd do begin
       Result := Result * 10 + (ord (AStart^) - ord ('0'));
       inc (AStart);
      end;
   if (Result > MaxBracesArg) or (Result < 0) then begin
     Error (reeBRACESArgTooBig);
     EXIT;
    end;
  end;

 var
  op : REChar;
  NonGreedyOp, NonGreedyCh : boolean; //###0.940
  TheOp : TREOp; //###0.940
  NextNode : PRegExprChar;
  flags : integer;
  BracesMin, Bracesmax : TREBracesArg;
  p, savedparse : PRegExprChar;

 procedure EmitComplexBraces (ABracesMin, ABracesMax : TREBracesArg;
   ANonGreedyOp : boolean); //###0.940
  {$IFDEF ComplexBraces}
  var
   off : integer;
  {$ENDIF}
   begin
   {$IFNDEF ComplexBraces}
   Error (reeComplexBracesNotImplemented);
   {$ELSE}
   if ANonGreedyOp
    then TheOp := LOOPNG
    else TheOp := LOOP;
   InsertOperator (LOOPENTRY, Result, REOpSz + RENextOffSz);
   NextNode := EmitNode (TheOp);
   if regcode <> @regdummy then begin
      off := (Result + REOpSz + RENextOffSz)
       - (regcode - REOpSz - RENextOffSz); // back to Atom after LOOPENTRY
      PREBracesArg (regcode)^ := ABracesMin;
      inc (regcode, REBracesArgSz);
      PREBracesArg (regcode)^ := ABracesMax;
      inc (regcode, REBracesArgSz);
      PRENextOff (regcode)^ := off;
      inc (regcode, RENextOffSz);
     end
    else inc (regsize, REBracesArgSz * 2 + RENextOffSz);
   Tail (Result, NextNode); // LOOPENTRY -> LOOP
   if regcode <> @regdummy then
    Tail (Result + REOpSz + RENextOffSz, NextNode); // Atom -> LOOP
   {$ENDIF}
  end;

 procedure EmitSimpleBraces (ABracesMin, ABracesMax : TREBracesArg;
   ANonGreedyOp : boolean); //###0.940
  begin
   if ANonGreedyOp //###0.940
    then TheOp := BRACESNG
    else TheOp := BRACES;
   InsertOperator (TheOp, Result, REOpSz + RENextOffSz + REBracesArgSz * 2);
   if regcode <> @regdummy then begin
     PREBracesArg (Result + REOpSz + RENextOffSz)^ := ABracesMin;
     PREBracesArg (Result + REOpSz + RENextOffSz + REBracesArgSz)^ := ABracesMax;
    end;
  end;

 begin
  Result := ParseAtom (flags);
  if Result = nil
   then EXIT;

  op := regparse^;
  if not ((op = '*') or (op = '+') or (op = '?') or (op = '{')) then begin
    flagp := flags;
    EXIT;
   end;
  if ((flags and HASWIDTH) = 0) and (op <> '?') then begin
    Error (reePlusStarOperandCouldBeEmpty);
    EXIT;
   end;

  case op of
    '*': begin
      flagp := WORST or SPSTART;
      NonGreedyCh := (regparse + 1)^ = '?'; //###0.940
      NonGreedyOp := NonGreedyCh or ((fCompModifiers and MaskModG) = 0); //###0.940
      if (flags and SIMPLE) = 0 then begin
         if NonGreedyOp //###0.940
          then EmitComplexBraces (0, MaxBracesArg, NonGreedyOp)
          else begin // Emit x* as (x&|), where & means "self".
            InsertOperator (BRANCH, Result, REOpSz + RENextOffSz); // Either x
            OpTail (Result, EmitNode (BACK)); // and loop
            OpTail (Result, Result); // back
            Tail (Result, EmitNode (BRANCH)); // or
            Tail (Result, EmitNode (NOTHING)); // nil.
           end
        end
       else begin // Simple
         if NonGreedyOp //###0.940
          then TheOp := STARNG
          else TheOp := STAR;
         InsertOperator (TheOp, Result, REOpSz + RENextOffSz);
        end;
      if NonGreedyCh //###0.940
       then inc (regparse); // Skip extra char ('?')
     end; { of case '*'}
    '+': begin
      flagp := WORST or SPSTART or HASWIDTH;
      NonGreedyCh := (regparse + 1)^ = '?'; //###0.940
      NonGreedyOp := NonGreedyCh or ((fCompModifiers and MaskModG) = 0); //###0.940
      if (flags and SIMPLE) = 0 then begin
         if NonGreedyOp //###0.940
          then EmitComplexBraces (1, MaxBracesArg, NonGreedyOp)
          else begin // Emit x+ as x(&|), where & means "self".
            NextNode := EmitNode (BRANCH); // Either
            Tail (Result, NextNode);
            Tail (EmitNode (BACK), Result);    // loop back
            Tail (NextNode, EmitNode (BRANCH)); // or
            Tail (Result, EmitNode (NOTHING)); // nil.
           end
        end
       else begin // Simple
         if NonGreedyOp //###0.940
          then TheOp := PLUSNG
          else TheOp := PLUS;
         InsertOperator (TheOp, Result, REOpSz + RENextOffSz);
        end;
      if NonGreedyCh //###0.940
       then inc (regparse); // Skip extra char ('?')
     end; { of case '+'}
    '?': begin
      flagp := WORST;
      NonGreedyCh := (regparse + 1)^ = '?'; //###0.940
      NonGreedyOp := NonGreedyCh or ((fCompModifiers and MaskModG) = 0); //###0.940
      if NonGreedyOp then begin //###0.940  // We emit x?? as x{0,1}?
         if (flags and SIMPLE) = 0
          then EmitComplexBraces (0, 1, NonGreedyOp)
          else EmitSimpleBraces (0, 1, NonGreedyOp);
        end
       else begin // greedy '?'
         InsertOperator (BRANCH, Result, REOpSz + RENextOffSz); // Either x
         Tail (Result, EmitNode (BRANCH));  // or
         NextNode := EmitNode (NOTHING); // nil.
         Tail (Result, NextNode);
         OpTail (Result, NextNode);
        end;
      if NonGreedyCh //###0.940
       then inc (regparse); // Skip extra char ('?')
     end; { of case '?'}
   '{': begin
      savedparse := regparse;
      // !!!!!!!!!!!!
      // Filip Jirsak's note - what will happen, when we are at the end of regparse?
      inc (regparse);
      p := regparse;
      while Pos (regparse^, '0123456789') > 0  // <min> MUST appear
       do inc (regparse);
      if (regparse^ <> '}') and (regparse^ <> ',') or (p = regparse) then begin
        regparse := savedparse;
        flagp := flags;
        EXIT;
       end;
      BracesMin := parsenum (p, regparse - 1);
      if regparse^ = ',' then begin
         inc (regparse);
         p := regparse;
         while Pos (regparse^, '0123456789') > 0
          do inc (regparse);
         if regparse^ <> '}' then begin
           regparse := savedparse;
           EXIT;
          end;
         if p = regparse
          then BracesMax := MaxBracesArg
          else BracesMax := parsenum (p, regparse - 1);
        end
       else BracesMax := BracesMin; // {n} == {n,n}
      if BracesMin > BracesMax then begin
        Error (reeBracesMinParamGreaterMax);
        EXIT;
       end;
      if BracesMin > 0
       then flagp := WORST;
      if BracesMax > 0
       then flagp := flagp or HASWIDTH or SPSTART;

      NonGreedyCh := (regparse + 1)^ = '?'; //###0.940
      NonGreedyOp := NonGreedyCh or ((fCompModifiers and MaskModG) = 0); //###0.940
      if (flags and SIMPLE) <> 0
       then EmitSimpleBraces (BracesMin, BracesMax, NonGreedyOp)
       else EmitComplexBraces (BracesMin, BracesMax, NonGreedyOp);
      if NonGreedyCh //###0.940
       then inc (regparse); // Skip extra char '?'
     end; { of case '{'}
//    else // here we can't be
   end; { of case op}

  inc (regparse);
  if (regparse^ = '*') or (regparse^ = '+') or (regparse^ = '?') or (regparse^ = '{') then begin
    Error (reeNestedSQP);
    EXIT;
   end;
 end; { of function TRegExpr.ParsePiece
--------------------------------------------------------------}

function TRegExpr.ParseAtom (var flagp : integer) : PRegExprChar;
// the lowest level
// Optimization:  gobbles an entire sequence of ordinary characters so that
// it can turn them into a single node, which is smaller to store and
// faster to run.  Backslashed characters are exceptions, each becoming a
// separate node; the code is simpler that way and it's not worth fixing.
 var
  ret : PRegExprChar;
  flags : integer;
  RangeBeg, RangeEnd : REChar;
  CanBeRange : boolean;
  len : integer;
  ender : REChar;
  begmodfs : PRegExprChar;

  {$IFDEF UseSetOfChar} //###0.930
  RangePCodeBeg : PRegExprChar;
  RangePCodeIdx : integer;
  RangeIsCI : boolean;
  RangeSet : TSetOfREChar;
  RangeLen : integer;
  RangeChMin, RangeChMax : REChar;
  {$ENDIF}

 procedure EmitExactly (ch : REChar);
  begin
   if (fCompModifiers and MaskModI) <> 0
    then ret := EmitNode (EXACTLYCI)
    else ret := EmitNode (EXACTLY);
   EmitC (ch);
   EmitC (#0);
   flagp := flagp or HASWIDTH or SIMPLE;
  end;

 procedure EmitStr (const s : RegExprString);
  var i : integer;
  begin
   for i := 1 to length (s)
    do EmitC (s [i]);
  end;

 function HexDig (ch : REChar) : integer;
  begin
   Result := 0;
   if (ch >= 'a') and (ch <= 'f')
    then ch := REChar (ord (ch) - (ord ('a') - ord ('A')));
   if (ch < '0') or (ch > 'F') or ((ch > '9') and (ch < 'A')) then begin
     Error (reeBadHexDigit);
     EXIT;
    end;
   Result := ord (ch) - ord ('0');
   if ch >= 'A'
    then Result := Result - (ord ('A') - ord ('9') - 1);
  end;

 function EmitRange (AOpCode : REChar) : PRegExprChar;
  begin
   {$IFDEF UseSetOfChar}
   case AOpCode of
     ANYBUTCI, ANYBUT:
       Result := EmitNode (ANYBUTTINYSET);
     else // ANYOFCI, ANYOF
       Result := EmitNode (ANYOFTINYSET);
    end;
   case AOpCode of
     ANYBUTCI, ANYOFCI:
       RangeIsCI := True;
     else // ANYBUT, ANYOF
       RangeIsCI := False;
    end;
   RangePCodeBeg := regcode;
   RangePCodeIdx := regsize;
   RangeLen := 0;
   RangeSet := [];
   RangeChMin := #255;
   RangeChMax := #0;
   {$ELSE}
   Result := EmitNode (AOpCode);
   // ToDo:
   // !!!!!!!!!!!!! Implement ANYOF[BUT]TINYSET generation for UniCode !!!!!!!!!!
   {$ENDIF}
  end;

{$IFDEF UseSetOfChar}
 procedure EmitRangeCPrim (b : REChar); //###0.930
  begin
   if b in RangeSet
    then EXIT;
   inc (RangeLen);
   if b < RangeChMin
    then RangeChMin := b;
   if b > RangeChMax
    then RangeChMax := b;
   Include (RangeSet, b);
  end;
 {$ENDIF}

 procedure EmitRangeC (b : REChar);
  {$IFDEF UseSetOfChar}
  var
   Ch : REChar;
  {$ENDIF}
  begin
   CanBeRange := false;
   {$IFDEF UseSetOfChar}
    if b <> #0 then begin
       EmitRangeCPrim (b); //###0.930
       if RangeIsCI
        then EmitRangeCPrim (InvertCase (b)); //###0.930
      end
     else begin
       {$IFDEF UseAsserts}
       Assert (RangeLen > 0, 'TRegExpr.ParseAtom(subroutine EmitRangeC): empty range'); // impossible, but who knows..
       Assert (RangeChMin <= RangeChMax, 'TRegExpr.ParseAtom(subroutine EmitRangeC): RangeChMin > RangeChMax'); // impossible, but who knows..
       {$ENDIF}
       if RangeLen <= TinySetLen then begin // emit "tiny set"
          if regcode = @regdummy then begin
            regsize := RangePCodeIdx + TinySetLen; // RangeChMin/Max !!!
            EXIT;
           end;
          regcode := RangePCodeBeg;
          for Ch := RangeChMin to RangeChMax do //###0.930
           if Ch in RangeSet then begin
             regcode^ := Ch;
             inc (regcode);
            end;
          // fill rest:
          while regcode < RangePCodeBeg + TinySetLen do begin
            regcode^ := RangeChMax;
            inc (regcode);
           end;
         end
        else begin
          if regcode = @regdummy then begin
            regsize := RangePCodeIdx + SizeOf (TSetOfREChar);
            EXIT;
           end;
          if (RangePCodeBeg - REOpSz - RENextOffSz)^ = ANYBUTTINYSET
           then RangeSet := [#0 .. #255] - RangeSet;
          PREOp (RangePCodeBeg - REOpSz - RENextOffSz)^ := ANYOFFULLSET;
          regcode := RangePCodeBeg;
          Move (RangeSet, regcode^, SizeOf (TSetOfREChar));
          inc (regcode, SizeOf (TSetOfREChar));
         end;
      end;
   {$ELSE}
   EmitC (b);
   {$ENDIF}
  end;

 procedure EmitSimpleRangeC (b : REChar);
  begin
   RangeBeg := b;
   EmitRangeC (b);
   CanBeRange := true;
  end;

 procedure EmitRangeStr (const s : RegExprString);
  var i : integer;
  begin
   for i := 1 to length (s)
    do EmitRangeC (s [i]);
  end;

 function UnQuoteChar (var APtr : PRegExprChar) : REChar; //###0.934
  begin
   case APtr^ of
     't': Result := #$9;  // tab (HT/TAB)
     'n': Result := #$a;  // newline (NL)
     'r': Result := #$d;  // car.return (CR)
     'f': Result := #$c;  // form feed (FF)
     'a': Result := #$7;  // alarm (bell) (BEL)
     'e': Result := #$1b; // escape (ESC)
     'x': begin // hex char
       Result := #0;
       inc (APtr);
       if APtr^ = #0 then begin
         Error (reeNoHexCodeAfterBSlashX);
         EXIT;
        end;
       if APtr^ = '{' then begin // \x{nnnn} //###0.936
          REPEAT
           inc (APtr);
           if APtr^ = #0 then begin
             Error (reeNoHexCodeAfterBSlashX);
             EXIT;
            end;
           if APtr^ <> '}' then begin
              if (Ord (Result)
                  ShR (SizeOf (REChar) * 8 - 4)) and $F <> 0 then begin
                Error (reeHexCodeAfterBSlashXTooBig);
                EXIT;
               end;
              Result := REChar ((Ord (Result) ShL 4) or HexDig (APtr^));
              // HexDig will cause Error if bad hex digit found
             end
            else BREAK;
          UNTIL False;
         end
        else begin
          Result := REChar (HexDig (APtr^));
          // HexDig will cause Error if bad hex digit found
          inc (APtr);
          if APtr^ = #0 then begin
            Error (reeNoHexCodeAfterBSlashX);
            EXIT;
           end;
          Result := REChar ((Ord (Result) ShL 4) or HexDig (APtr^));
          // HexDig will cause Error if bad hex digit found
         end;
      end;
     else Result := APtr^;
    end;
  end;

 begin
  Result := nil;
  flagp := WORST; // Tentatively.

  inc (regparse);
  case (regparse - 1)^ of
    '^': if ((fCompModifiers and MaskModM) = 0)
           or ((fLineSeparators = '') and not fLinePairedSeparatorAssigned)
          then ret := EmitNode (BOL)
          else ret := EmitNode (BOLML);
    '$': if ((fCompModifiers and MaskModM) = 0)
           or ((fLineSeparators = '') and not fLinePairedSeparatorAssigned)
          then ret := EmitNode (EOL)
          else ret := EmitNode (EOLML);
    '.':
       if (fCompModifiers and MaskModS) <> 0 then begin
          ret := EmitNode (ANY);
          flagp := flagp or HASWIDTH or SIMPLE;
         end
        else begin // not /s, so emit [^:LineSeparators:]
          ret := EmitNode (ANYML);
          flagp := flagp or HASWIDTH; // not so simple ;)
//          ret := EmitRange (ANYBUT);
//          EmitRangeStr (LineSeparators); //###0.941
//          EmitRangeStr (LinePairedSeparator); // !!! isn't correct if have to accept only paired
//          EmitRangeC (#0);
//          flagp := flagp or HASWIDTH or SIMPLE;
         end;
    '[': begin
        if regparse^ = '^' then begin // Complement of range.
           if (fCompModifiers and MaskModI) <> 0
            then ret := EmitRange (ANYBUTCI)
            else ret := EmitRange (ANYBUT);
           inc (regparse);
          end
         else
          if (fCompModifiers and MaskModI) <> 0
           then ret := EmitRange (ANYOFCI)
           else ret := EmitRange (ANYOF);

        CanBeRange := false;

        if (regparse^ = ']') then begin
          EmitSimpleRangeC (regparse^); // []-a] -> ']' .. 'a'
          inc (regparse);
         end;

        while (regparse^ <> #0) and (regparse^ <> ']') do begin
          if (regparse^ = '-')
              and ((regparse + 1)^ <> #0) and ((regparse + 1)^ <> ']')
              and CanBeRange then begin
             inc (regparse);
             RangeEnd := regparse^;
             if RangeEnd = EscChar then begin
               {$IFDEF UniCode} //###0.935
               if (ord ((regparse + 1)^) < 256)
                  and (char ((regparse + 1)^)
                        in ['d', 'D', 's', 'S', 'w', 'W']) then begin
               {$ELSE}
               if (regparse + 1)^ in ['d', 'D', 's', 'S', 'w', 'W'] then begin
               {$ENDIF}
                 EmitRangeC ('-'); // or treat as error ?!!
                 CONTINUE;
                end;
               inc (regparse);
               RangeEnd := UnQuoteChar (regparse);
              end;

             // r.e.ranges extension for russian
             if ((fCompModifiers and MaskModR) <> 0)
                and (RangeBeg = RusRangeLoLow) and (RangeEnd = RusRangeLoHigh) then begin
               EmitRangeStr (RusRangeLo);
              end
             else if ((fCompModifiers and MaskModR) <> 0)
                 and (RangeBeg = RusRangeHiLow) and (RangeEnd = RusRangeHiHigh) then begin
               EmitRangeStr (RusRangeHi);
              end
             else if ((fCompModifiers and MaskModR) <> 0)
                  and (RangeBeg = RusRangeLoLow) and (RangeEnd = RusRangeHiHigh) then begin
               EmitRangeStr (RusRangeLo);
               EmitRangeStr (RusRangeHi);
              end
             else begin // standard r.e. handling
               if RangeBeg > RangeEnd then begin
                 Error (reeInvalidRange);
                 EXIT;
                end;
               inc (RangeBeg);
               EmitRangeC (RangeEnd); // prevent infinite loop if RangeEnd=$ff
               while RangeBeg < RangeEnd do begin //###0.929
                 EmitRangeC (RangeBeg);
                 inc (RangeBeg);
                end;
              end;
             inc (regparse);
            end
           else begin
             if regparse^ = EscChar then begin
                inc (regparse);
                if regparse^ = #0 then begin
                  Error (reeParseAtomTrailingBackSlash);
                  EXIT;
                 end;
                case regparse^ of // r.e.extensions
                  'd': EmitRangeStr ('0123456789');
                  'w': EmitRangeStr (WordChars);
                  's': EmitRangeStr (SpaceChars);
                  else EmitSimpleRangeC (UnQuoteChar (regparse));
                 end; { of case}
               end
              else EmitSimpleRangeC (regparse^);
             inc (regparse);
            end;
         end; { of while}
        EmitRangeC (#0);
        if regparse^ <> ']' then begin
          Error (reeUnmatchedSqBrackets);
          EXIT;
         end;
        inc (regparse);
        flagp := flagp or HASWIDTH or SIMPLE;
      end;
    '(': begin
        if regparse^ = '?' then begin
           // check for extended Perl syntax : (?..)
           if (regparse + 1)^ = '#' then begin // (?#comment)
              inc (regparse, 2); // find closing ')'
              while (regparse^ <> #0) and (regparse^ <> ')')
               do inc (regparse);
              if regparse^ <> ')' then begin
                Error (reeUnclosedComment);
                EXIT;
               end;
              inc (regparse); // skip ')'
              ret := EmitNode (COMMENT); // comment
             end
           else begin // modifiers ?
             inc (regparse); // skip '?'
             begmodfs := regparse;
             while (regparse^ <> #0) and (regparse^ <> ')')
              do inc (regparse);
             if (regparse^ <> ')')
                or not ParseModifiersStr (copy (begmodfs, 1, (regparse - begmodfs)), fCompModifiers) then begin
               Error (reeUrecognizedModifier);
               EXIT;
              end;
             inc (regparse); // skip ')'
             ret := EmitNode (COMMENT); // comment
//             Error (reeQPSBFollowsNothing);
//             EXIT;
            end;
          end
         else begin
           ret := ParseReg (1, flags);
           if ret = nil then begin
             Result := nil;
             EXIT;
            end;
           flagp := flagp or flags and (HASWIDTH or SPSTART);
          end;
      end;
    #0, '|', ')': begin // Supposed to be caught earlier.
       Error (reeInternalUrp);
       EXIT;
      end;
    '?', '+', '*': begin
       Error (reeQPSBFollowsNothing);
       EXIT;
      end;
    EscChar: begin
        if regparse^ = #0 then begin
          Error (reeTrailingBackSlash);
          EXIT;
         end;
        case regparse^ of // r.e.extensions
          'b': ret := EmitNode (BOUND); //###0.943
          'B': ret := EmitNode (NOTBOUND); //###0.943
          'A': ret := EmitNode (BOL); //###0.941
          'Z': ret := EmitNode (EOL); //###0.941
          'd': begin // r.e.extension - any digit ('0' .. '9')
             ret := EmitNode (ANYDIGIT);
             flagp := flagp or HASWIDTH or SIMPLE;
            end;
          'D': begin // r.e.extension - not digit ('0' .. '9')
             ret := EmitNode (NOTDIGIT);
             flagp := flagp or HASWIDTH or SIMPLE;
            end;
          's': begin // r.e.extension - any space char
             {$IFDEF UseSetOfChar}
             ret := EmitRange (ANYOF);
             EmitRangeStr (SpaceChars);
             EmitRangeC (#0);
             {$ELSE}
             ret := EmitNode (ANYSPACE);
             {$ENDIF}
             flagp := flagp or HASWIDTH or SIMPLE;
            end;
          'S': begin // r.e.extension - not space char
             {$IFDEF UseSetOfChar}
             ret := EmitRange (ANYBUT);
             EmitRangeStr (SpaceChars);
             EmitRangeC (#0);
             {$ELSE}
             ret := EmitNode (NOTSPACE);
             {$ENDIF}
             flagp := flagp or HASWIDTH or SIMPLE;
            end;
          'w': begin // r.e.extension - any english char / digit / '_'
             {$IFDEF UseSetOfChar}
             ret := EmitRange (ANYOF);
             EmitRangeStr (WordChars);
             EmitRangeC (#0);
             {$ELSE}
             ret := EmitNode (ANYLETTER);
             {$ENDIF}
             flagp := flagp or HASWIDTH or SIMPLE;
            end;
          'W': begin // r.e.extension - not english char / digit / '_'
             {$IFDEF UseSetOfChar}
             ret := EmitRange (ANYBUT);
             EmitRangeStr (WordChars);
             EmitRangeC (#0);
             {$ELSE}
             ret := EmitNode (NOTLETTER);
             {$ENDIF}
             flagp := flagp or HASWIDTH or SIMPLE;
            end;
           '1' .. '9': begin //###0.936
             if (fCompModifiers and MaskModI) <> 0
              then ret := EmitNode (BSUBEXPCI)
              else ret := EmitNode (BSUBEXP);
             EmitC (REChar (ord (regparse^) - ord ('0')));
             flagp := flagp or HASWIDTH or SIMPLE;
            end;
          else EmitExactly (UnQuoteChar (regparse));
         end; { of case}
        inc (regparse);
      end;
    else begin
      dec (regparse);
      if ((fCompModifiers and MaskModX) <> 0) and // check for eXtended syntax
          ((regparse^ = '#')
           or ({$IFDEF UniCode}StrScan (XIgnoredChars, regparse^) <> nil //###0.947
               {$ELSE}regparse^ in XIgnoredChars{$ENDIF})) then begin //###0.941 \x
         if regparse^ = '#' then begin // Skip eXtended comment
            // find comment terminator (group of \n and/or \r)
            while (regparse^ <> #0) and (regparse^ <> #$d) and (regparse^ <> #$a)
             do inc (regparse);
            while (regparse^ = #$d) or (regparse^ = #$a) // skip comment terminator
             do inc (regparse); // attempt to support different type of line separators
           end
          else begin // Skip the blanks!
            while {$IFDEF UniCode}StrScan (XIgnoredChars, regparse^) <> nil //###0.947
                  {$ELSE}regparse^ in XIgnoredChars{$ENDIF}
             do inc (regparse);
           end;
         ret := EmitNode (COMMENT); // comment
        end
       else begin
         len := strcspn (regparse, META);
         if len <= 0 then
          if regparse^ <> '{' then begin
             Error (reeRarseAtomInternalDisaster);
             EXIT;
            end
           else len := strcspn (regparse + 1, META) + 1; // bad {n,m} - compile as EXATLY
         ender := (regparse + len)^;
         if (len > 1)
            and ((ender = '*') or (ender = '+') or (ender = '?') or (ender = '{'))
          then dec (len); // Back off clear of ?+*{ operand.
         flagp := flagp or HASWIDTH;
         if len = 1
         then flagp := flagp or SIMPLE;
         if (fCompModifiers and MaskModI) <> 0
          then ret := EmitNode (EXACTLYCI)
          else ret := EmitNode (EXACTLY);
         while (len > 0)
          and (((fCompModifiers and MaskModX) = 0) or (regparse^ <> '#')) do begin
           if ((fCompModifiers and MaskModX) = 0) or not ( //###0.941
              {$IFDEF UniCode}StrScan (XIgnoredChars, regparse^) <> nil //###0.947
              {$ELSE}regparse^ in XIgnoredChars{$ENDIF} )
            then EmitC (regparse^);
           inc (regparse);
           dec (len);
          end;
         EmitC (#0);
        end; { of if not comment}
     end; { of case else}
   end; { of case}

  Result := ret;
 end; { of function TRegExpr.ParseAtom
--------------------------------------------------------------}

function TRegExpr.GetCompilerErrorPos : integer;
 begin
  Result := 0;
  if (regexpbeg = nil) or (regparse = nil)
   then EXIT; // not in compiling mode ?
  Result := regparse - regexpbeg;
 end; { of function TRegExpr.GetCompilerErrorPos
--------------------------------------------------------------}


{=============================================================}
{===================== Matching section ======================}
{=============================================================}

{$IFNDEF UseSetOfChar}
function TRegExpr.StrScanCI (s : PRegExprChar; ch : REChar) : PRegExprChar; //###0.928 - now method of TRegExpr
 begin
  while (s^ <> #0) and (s^ <> ch) and (s^ <> InvertCase (ch))
   do inc (s);
  if s^ <> #0
   then Result := s
   else Result := nil;
 end; { of function TRegExpr.StrScanCI
--------------------------------------------------------------}
{$ENDIF}

function TRegExpr.regrepeat (p : PRegExprChar; AMax : integer) : integer;
// repeatedly match something simple, report how many
 var
  scan : PRegExprChar;
  opnd : PRegExprChar;
  TheMax : integer;
  {Ch,} InvCh : REChar; //###0.931
  sestart, seend : PRegExprChar; //###0.936
 begin
  Result := 0;
  scan := reginput;
  opnd := p + REOpSz + RENextOffSz; //OPERAND
  TheMax := fInputEnd - scan;
  if TheMax > AMax
   then TheMax := AMax;
  case PREOp (p)^ of
    ANY: begin
    // note - ANYML cannot be proceeded in regrepeat because can skip
    // more than one char at once
      Result := TheMax;
      inc (scan, Result);
     end;
    EXACTLY: begin // in opnd can be only ONE char !!!
//      Ch := opnd^; // store in register //###0.931
      while (Result < TheMax) and (opnd^ = scan^) do begin
        inc (Result);
        inc (scan);
       end;
     end;
    EXACTLYCI: begin // in opnd can be only ONE char !!!
//      Ch := opnd^; // store in register //###0.931
      while (Result < TheMax) and (opnd^ = scan^) do begin // prevent unneeded InvertCase //###0.931
        inc (Result);
        inc (scan);
       end;
      if Result < TheMax then begin //###0.931
        InvCh := InvertCase (opnd^); // store in register
        while (Result < TheMax) and
              ((opnd^ = scan^) or (InvCh = scan^)) do begin
          inc (Result);
          inc (scan);
         end;
       end;
     end;
    BSUBEXP: begin //###0.936
      sestart := startp [ord (opnd^)];
      if sestart = nil
       then EXIT;
      seend := endp [ord (opnd^)];
      if seend = nil
       then EXIT;
      REPEAT
        opnd := sestart;
        while opnd < seend do begin
          if (scan >= fInputEnd) or (scan^ <> opnd^)
           then EXIT;
          inc (scan);
          inc (opnd);
         end;
        inc (Result);
        reginput := scan;
      UNTIL Result >= AMax;
     end;
    BSUBEXPCI: begin //###0.936
      sestart := startp [ord (opnd^)];
      if sestart = nil
       then EXIT;
      seend := endp [ord (opnd^)];
      if seend = nil
       then EXIT;
      REPEAT
        opnd := sestart;
        while opnd < seend do begin
          if (scan >= fInputEnd) or
             ((scan^ <> opnd^) and (scan^ <> InvertCase (opnd^)))
           then EXIT;
          inc (scan);
          inc (opnd);
         end;
        inc (Result);
        reginput := scan;
      UNTIL Result >= AMax;
     end;
    ANYDIGIT:
      while (Result < TheMax) and
         (scan^ >= '0') and (scan^ <= '9') do begin
        inc (Result);
        inc (scan);
       end;
    NOTDIGIT:
      while (Result < TheMax) and
         ((scan^ < '0') or (scan^ > '9')) do begin
        inc (Result);
        inc (scan);
       end;
    {$IFNDEF UseSetOfChar} //###0.929
    ANYLETTER:
      while (Result < TheMax) and
       (Pos (scan^, fWordChars) > 0) //###0.940
     {  ((scan^ >= 'a') and (scan^ <= 'z') !! I've forgotten (>='0') and (<='9')
       or (scan^ >= 'A') and (scan^ <= 'Z') or (scan^ = '_'))} do begin
        inc (Result);
        inc (scan);
       end;
    NOTLETTER:
      while (Result < TheMax) and
       (Pos (scan^, fWordChars) <= 0)  //###0.940
     {   not ((scan^ >= 'a') and (scan^ <= 'z') !! I've forgotten (>='0') and (<='9')
         or (scan^ >= 'A') and (scan^ <= 'Z')
         or (scan^ = '_'))} do begin
        inc (Result);
        inc (scan);
       end;
    ANYSPACE:
      while (Result < TheMax) and
         (Pos (scan^, fSpaceChars) > 0) do begin
        inc (Result);
        inc (scan);
       end;
    NOTSPACE:
      while (Result < TheMax) and
         (Pos (scan^, fSpaceChars) <= 0) do begin
        inc (Result);
        inc (scan);
       end;
    {$ENDIF}
    ANYOFTINYSET: begin
      while (Result < TheMax) and //!!!TinySet
       ((scan^ = opnd^) or (scan^ = (opnd + 1)^)
        or (scan^ = (opnd + 2)^)) do begin
        inc (Result);
        inc (scan);
       end;
     end;
    ANYBUTTINYSET: begin
      while (Result < TheMax) and //!!!TinySet
       (scan^ <> opnd^) and (scan^ <> (opnd + 1)^)
        and (scan^ <> (opnd + 2)^) do begin
        inc (Result);
        inc (scan);
       end;
     end;
    {$IFDEF UseSetOfChar} //###0.929
    ANYOFFULLSET: begin
      while (Result < TheMax) and
       (scan^ in PSetOfREChar (opnd)^) do begin
        inc (Result);
        inc (scan);
       end;
     end;
    {$ELSE}
    ANYOF:
      while (Result < TheMax) and
         (StrScan (opnd, scan^) <> nil) do begin
        inc (Result);
        inc (scan);
       end;
    ANYBUT:
      while (Result < TheMax) and
         (StrScan (opnd, scan^) = nil) do begin
        inc (Result);
        inc (scan);
       end;
    ANYOFCI:
      while (Result < TheMax) and (StrScanCI (opnd, scan^) <> nil) do begin
        inc (Result);
        inc (scan);
       end;
    ANYBUTCI:
      while (Result < TheMax) and (StrScanCI (opnd, scan^) = nil) do begin
        inc (Result);
        inc (scan);
       end;
    {$ENDIF}
    else begin // Oh dear. Called inappropriately.
      Result := 0; // Best compromise.
      Error (reeRegRepeatCalledInappropriately);
      EXIT;
     end;
   end; { of case}
  reginput := scan;
 end; { of function TRegExpr.regrepeat
--------------------------------------------------------------}

function TRegExpr.regnext (p : PRegExprChar) : PRegExprChar;
// dig the "next" pointer out of a node
 var offset : TRENextOff;
 begin
  if p = @regdummy then begin
    Result := nil;
    EXIT;
   end;
  offset := PRENextOff (p + REOpSz)^; //###0.933 inlined NEXT
  if offset = 0
   then Result := nil
   else Result := p + offset;
 end; { of function TRegExpr.regnext
--------------------------------------------------------------}

function TRegExpr.MatchPrim (prog : PRegExprChar) : boolean;
// recursively matching routine
// Conceptually the strategy is simple:  check to see whether the current
// node matches, call self recursively to see whether the rest matches,
// and then act accordingly.  In practice we make some effort to avoid
// recursion, in particular by going through "ordinary" nodes (that don't
// need to know whether the rest of the match failed) by a loop instead of
// by recursion.
 var
  scan : PRegExprChar; // Current node.
  next : PRegExprChar; // Next node.
  len : integer;
  opnd : PRegExprChar;
  no : integer;
  save : PRegExprChar;
  nextch : REChar;
  BracesMin, BracesMax : integer; // we use integer instead of TREBracesArg for better support */+
  {$IFDEF ComplexBraces}
  SavedLoopStack : array [1 .. LoopStackMax] of integer; // :(( very bad for recursion
  SavedLoopStackIdx : integer; //###0.925
  {$ENDIF}
 begin
  Result := false;
  scan := prog;

  while scan <> nil do begin
     len := PRENextOff (scan + 1)^; //###0.932 inlined regnext
     if len = 0
      then next := nil
      else next := scan + len;

     case scan^ of
         NOTBOUND, //###0.943 //!!! think about UseSetOfChar !!!
         BOUND:
         if (scan^ = BOUND)
          xor (
          ((reginput = fInputStart) or (Pos ((reginput - 1)^, fWordChars) <= 0))
            and (reginput^ <> #0) and (Pos (reginput^, fWordChars) > 0)
           or
            (reginput <> fInputStart) and (Pos ((reginput - 1)^, fWordChars) > 0)
            and ((reginput^ = #0) or (Pos (reginput^, fWordChars) <= 0)))
          then EXIT;

         BOL: if reginput <> fInputStart
               then EXIT;
         EOL: if reginput^ <> #0
               then EXIT;
         BOLML: if reginput > fInputStart then begin
            nextch := (reginput - 1)^;
            if (nextch <> fLinePairedSeparatorTail)
               or ((reginput - 1) <= fInputStart)
               or ((reginput - 2)^ <> fLinePairedSeparatorHead)
              then begin
               if (nextch = fLinePairedSeparatorHead)
                 and (reginput^ = fLinePairedSeparatorTail)
                then EXIT; // don't stop between paired separator
               if
                 {$IFNDEF UniCode}
                 not (nextch in fLineSeparatorsSet)
                 {$ELSE}
                 (pos (nextch, fLineSeparators) <= 0)
                 {$ENDIF}
                then EXIT;
              end;
           end;
         EOLML: if reginput^ <> #0 then begin
            nextch := reginput^;
            if (nextch <> fLinePairedSeparatorHead)
               or ((reginput + 1)^ <> fLinePairedSeparatorTail)
             then begin
               if (nextch = fLinePairedSeparatorTail)
                 and (reginput > fInputStart)
                 and ((reginput - 1)^ = fLinePairedSeparatorHead)
                then EXIT; // don't stop between paired separator
               if
                 {$IFNDEF UniCode}
                 not (nextch in fLineSeparatorsSet)
                 {$ELSE}
                 (pos (nextch, fLineSeparators) <= 0)
                 {$ENDIF}
                then EXIT;
              end;
           end;
         ANY: begin
            if reginput^ = #0
             then EXIT;
            inc (reginput);
           end;
         ANYML: begin //###0.941
            if (reginput^ = #0)
             or ((reginput^ = fLinePairedSeparatorHead)
                 and ((reginput + 1)^ = fLinePairedSeparatorTail))
             or {$IFNDEF UniCode} (reginput^ in fLineSeparatorsSet)
                {$ELSE} (pos (reginput^, fLineSeparators) > 0) {$ENDIF}
             then EXIT;
            inc (reginput);
           end;
         ANYDIGIT: begin
            if (reginput^ = #0) or (reginput^ < '0') or (reginput^ > '9')
             then EXIT;
            inc (reginput);
           end;
         NOTDIGIT: begin
            if (reginput^ = #0) or ((reginput^ >= '0') and (reginput^ <= '9'))
             then EXIT;
            inc (reginput);
           end;
         {$IFNDEF UseSetOfChar} //###0.929
         ANYLETTER: begin
            if (reginput^ = #0) or (Pos (reginput^, fWordChars) <= 0) //###0.943
             then EXIT;
            inc (reginput);
           end;
         NOTLETTER: begin
            if (reginput^ = #0) or (Pos (reginput^, fWordChars) > 0) //###0.943
             then EXIT;
            inc (reginput);
           end;
         ANYSPACE: begin
            if (reginput^ = #0) or not (Pos (reginput^, fSpaceChars) > 0) //###0.943
             then EXIT;
            inc (reginput);
           end;
         NOTSPACE: begin
            if (reginput^ = #0) or (Pos (reginput^, fSpaceChars) > 0) //###0.943
             then EXIT;
            inc (reginput);
           end;
         {$ENDIF}
         EXACTLYCI: begin
            opnd := scan + REOpSz + RENextOffSz; // OPERAND
            // Inline the first character, for speed.
            if (opnd^ <> reginput^)
               and (InvertCase (opnd^) <> reginput^)
             then EXIT;
            len := strlen (opnd);
            //###0.929 begin
            no := len;
            save := reginput;
            while no > 1 do begin
              inc (save);
              inc (opnd);
              if (opnd^ <> save^)
                 and (InvertCase (opnd^) <> save^)
               then EXIT;
              dec (no);
             end;
            //###0.929 end
            inc (reginput, len);
           end;
         EXACTLY: begin
            opnd := scan + REOpSz + RENextOffSz; // OPERAND
            // Inline the first character, for speed.
            if opnd^ <> reginput^
             then EXIT;
            len := strlen (opnd);
            //###0.929 begin
            no := len;
            save := reginput;
            while no > 1 do begin
              inc (save);
              inc (opnd);
              if opnd^ <> save^
               then EXIT;
              dec (no);
             end;
            //###0.929 end
            inc (reginput, len);
           end;
         BSUBEXP: begin //###0.936
           no := ord ((scan + REOpSz + RENextOffSz)^);
           if startp [no] = nil
            then EXIT;
           if endp [no] = nil
            then EXIT;
           save := reginput;
           opnd := startp [no];
           while opnd < endp [no] do begin
             if (save >= fInputEnd) or (save^ <> opnd^)
              then EXIT;
             inc (save);
             inc (opnd);
            end;
           reginput := save;
          end;
         BSUBEXPCI: begin //###0.936
           no := ord ((scan + REOpSz + RENextOffSz)^);
           if startp [no] = nil
            then EXIT;
           if endp [no] = nil
            then EXIT;
           save := reginput;
           opnd := startp [no];
           while opnd < endp [no] do begin
             if (save >= fInputEnd) or
                ((save^ <> opnd^) and (save^ <> InvertCase (opnd^)))
              then EXIT;
             inc (save);
             inc (opnd);
            end;
           reginput := save;
          end;
         ANYOFTINYSET: begin
           if (reginput^ = #0) or //!!!TinySet
             ((reginput^ <> (scan + REOpSz + RENextOffSz)^)
             and (reginput^ <> (scan + REOpSz + RENextOffSz + 1)^)
             and (reginput^ <> (scan + REOpSz + RENextOffSz + 2)^))
            then EXIT;
           inc (reginput);
          end;
         ANYBUTTINYSET: begin
           if (reginput^ = #0) or //!!!TinySet
             (reginput^ = (scan + REOpSz + RENextOffSz)^)
             or (reginput^ = (scan + REOpSz + RENextOffSz + 1)^)
             or (reginput^ = (scan + REOpSz + RENextOffSz + 2)^)
            then EXIT;
           inc (reginput);
          end;
         {$IFDEF UseSetOfChar} //###0.929
         ANYOFFULLSET: begin
           if (reginput^ = #0)
              or not (reginput^ in PSetOfREChar (scan + REOpSz + RENextOffSz)^)
            then EXIT;
           inc (reginput);
          end;
         {$ELSE}
         ANYOF: begin
            if (reginput^ = #0) or (StrScan (scan + REOpSz + RENextOffSz, reginput^) = nil)
             then EXIT;
            inc (reginput);
           end;
         ANYBUT: begin
            if (reginput^ = #0) or (StrScan (scan + REOpSz + RENextOffSz, reginput^) <> nil)
             then EXIT;
            inc (reginput);
           end;
         ANYOFCI: begin
            if (reginput^ = #0) or (StrScanCI (scan + REOpSz + RENextOffSz, reginput^) = nil)
             then EXIT;
            inc (reginput);
           end;
         ANYBUTCI: begin
            if (reginput^ = #0) or (StrScanCI (scan + REOpSz + RENextOffSz, reginput^) <> nil)
             then EXIT;
            inc (reginput);
           end;
         {$ENDIF}
         NOTHING: ;
         COMMENT: ;
         BACK: ;
         Succ (OPEN) .. TREOp (Ord (OPEN) + NSUBEXP - 1) : begin //###0.929
            no := ord (scan^) - ord (OPEN);
//            save := reginput;
            save := startp [no]; //###0.936
            startp [no] := reginput; //###0.936
            Result := MatchPrim (next);
            if not Result //###0.936
             then startp [no] := save;
//            if Result and (startp [no] = nil)
//             then startp [no] := save;
             // Don't set startp if some later invocation of the same
             // parentheses already has.
            EXIT;
           end;
         Succ (CLOSE) .. TREOp (Ord (CLOSE) + NSUBEXP - 1): begin //###0.929
            no := ord (scan^) - ord (CLOSE);
//            save := reginput;
            save := endp [no]; //###0.936
            endp [no] := reginput; //###0.936
            Result := MatchPrim (next);
            if not Result //###0.936
             then endp [no] := save;
//            if Result and (endp [no] = nil)
//             then endp [no] := save;
             // Don't set endp if some later invocation of the same
             // parentheses already has.
            EXIT;
           end;
         BRANCH: begin
            if (next^ <> BRANCH) // No choice.
             then next := scan + REOpSz + RENextOffSz // Avoid recursion
             else begin
               REPEAT
                save := reginput;
                Result := MatchPrim (scan + REOpSz + RENextOffSz);
                if Result
                 then EXIT;
                reginput := save;
                scan := regnext (scan);
               UNTIL (scan = nil) or (scan^ <> BRANCH);
               EXIT;
              end;
           end;
         {$IFDEF ComplexBraces}
         LOOPENTRY: begin //###0.925
           no := LoopStackIdx;
           inc (LoopStackIdx);
           if LoopStackIdx > LoopStackMax then begin
             Error (reeLoopStackExceeded);
             EXIT;
            end;
           save := reginput;
           LoopStack [LoopStackIdx] := 0; // init loop counter
           Result := MatchPrim (next); // execute LOOP
           LoopStackIdx := no; // cleanup
           if Result
            then EXIT;
           reginput := save;
           EXIT;
          end;
         LOOP, LOOPNG: begin //###0.940
           if LoopStackIdx <= 0 then begin
             Error (reeLoopWithoutEntry);
             EXIT;
            end;
           opnd := scan + PRENextOff (scan + REOpSz + RENextOffSz + 2 * REBracesArgSz)^;
           BracesMin := PREBracesArg (scan + REOpSz + RENextOffSz)^;
           BracesMax := PREBracesArg (scan + REOpSz + RENextOffSz + REBracesArgSz)^;
           save := reginput;
           if LoopStack [LoopStackIdx] >= BracesMin then begin // Min alredy matched - we can work
              if scan^ = LOOP then begin
                 // greedy way - first try to max deep of greed ;)
                 if LoopStack [LoopStackIdx] < BracesMax then begin
                   inc (LoopStack [LoopStackIdx]);
                   no := LoopStackIdx;
                   Result := MatchPrim (opnd);
                   LoopStackIdx := no;
                   if Result
                    then EXIT;
                   reginput := save;
                  end;
                 dec (LoopStackIdx); // Fail. May be we are too greedy? ;)
                 Result := MatchPrim (next);
                 if not Result
                  then reginput := save;
                 EXIT;
                end
               else begin
                 // non-greedy - try just now
                 Result := MatchPrim (next);
                 if Result
                  then EXIT
                  else reginput := save; // failed - move next and try again
                 if LoopStack [LoopStackIdx] < BracesMax then begin
                   inc (LoopStack [LoopStackIdx]);
                   no := LoopStackIdx;
                   Result := MatchPrim (opnd);
                   LoopStackIdx := no;
                   if Result
                    then EXIT;
                   reginput := save;
                  end;
                 dec (LoopStackIdx); // Failed - back up
                 EXIT;
                end
             end
            else begin // first match a min_cnt times
              inc (LoopStack [LoopStackIdx]);
              no := LoopStackIdx;
              Result := MatchPrim (opnd);
              LoopStackIdx := no;
              if Result
               then EXIT;
              dec (LoopStack [LoopStackIdx]);
              reginput := save;
              EXIT;
             end;
          end;
         {$ENDIF}
         STAR, PLUS, BRACES, STARNG, PLUSNG, BRACESNG: begin
           // Lookahead to avoid useless match attempts when we know
           // what character comes next.
           nextch := #0;
           if next^ = EXACTLY
            then nextch := (next + REOpSz + RENextOffSz)^;
           BracesMax := MaxInt; // infinite loop for * and + //###0.92
           if (scan^ = STAR) or (scan^ = STARNG)
            then BracesMin := 0  // STAR
            else if (scan^ = PLUS) or (scan^ = PLUSNG)
             then BracesMin := 1 // PLUS
             else begin // BRACES
               BracesMin := PREBracesArg (scan + REOpSz + RENextOffSz)^;
               BracesMax := PREBracesArg (scan + REOpSz + RENextOffSz + REBracesArgSz)^;
              end;
           save := reginput;
           opnd := scan + REOpSz + RENextOffSz;
           if (scan^ = BRACES) or (scan^ = BRACESNG)
            then inc (opnd, 2 * REBracesArgSz);

           if (scan^ = PLUSNG) or (scan^ = STARNG) or (scan^ = BRACESNG) then begin
             // non-greedy mode
              BracesMax := regrepeat (opnd, BracesMax); // don't repeat more than BracesMax
              // Now we know real Max limit to move forward (for recursion 'back up')
              // In some cases it can be faster to check only Min positions first,
              // but after that we have to check every position separtely instead
              // of fast scannig in loop.
              no := BracesMin;
              while no <= BracesMax do begin
                reginput := save + no;
                // If it could work, try it.
                if (nextch = #0) or (reginput^ = nextch) then begin
                  {$IFDEF ComplexBraces}
                  System.Move (LoopStack, SavedLoopStack, SizeOf (LoopStack)); //###0.925
                  SavedLoopStackIdx := LoopStackIdx;
                  {$ENDIF}
                  if MatchPrim (next) then begin
                    Result := true;
                    EXIT;
                   end;
                  {$IFDEF ComplexBraces}
                  System.Move (SavedLoopStack, LoopStack, SizeOf (LoopStack));
                  LoopStackIdx := SavedLoopStackIdx;
                  {$ENDIF}
                 end;
                inc (no); // Couldn't or didn't - move forward.
               end; { of while}
              EXIT;
             end
            else begin // greedy mode
              no := regrepeat (opnd, BracesMax); // don't repeat more than max_cnt
              while no >= BracesMin do begin
                // If it could work, try it.
                if (nextch = #0) or (reginput^ = nextch) then begin
                  {$IFDEF ComplexBraces}
                  System.Move (LoopStack, SavedLoopStack, SizeOf (LoopStack)); //###0.925
                  SavedLoopStackIdx := LoopStackIdx;
                  {$ENDIF}
                  if MatchPrim (next) then begin
                    Result := true;
                    EXIT;
                   end;
                  {$IFDEF ComplexBraces}
                  System.Move (SavedLoopStack, LoopStack, SizeOf (LoopStack));
                  LoopStackIdx := SavedLoopStackIdx;
                  {$ENDIF}
                 end;
                dec (no); // Couldn't or didn't - back up.
                reginput := save + no;
               end; { of while}
              EXIT;
             end;
          end;
         EEND: begin
           Result := true;  // Success!
           EXIT;
          end;
        else begin
            Error (reeMatchPrimMemoryCorruption);
            EXIT;
          end;
        end; { of case scan^}
        scan := next;
    end; { of while scan <> nil}

  // We get here only if there's trouble -- normally "case EEND" is the
  // terminating point.
  Error (reeMatchPrimCorruptedPointers);
 end; { of function TRegExpr.MatchPrim
--------------------------------------------------------------}

{$IFDEF UseFirstCharSet} //###0.929
procedure TRegExpr.FillFirstCharSet (prog : PRegExprChar);
 var
  scan : PRegExprChar; // Current node.
  next : PRegExprChar; // Next node.
  opnd : PRegExprChar;
  min_cnt : integer;
 begin
  scan := prog;
  while scan <> nil do begin
     next := regnext (scan);
     case PREOp (scan)^ of
         BSUBEXP, BSUBEXPCI: begin //###0.938
           FirstCharSet := [#0 .. #255]; // :((( we cannot
           // optimize r.e. if it starts with back reference
           EXIT;
          end;
         BOL, BOLML: ; // EXIT; //###0.937
         EOL, EOLML: begin //###0.948 was empty in 0.947, was EXIT in 0.937
           Include (FirstCharSet, #0);
           if ModifierM
            then begin
              opnd := PRegExprChar (LineSeparators);
              while opnd^ <> #0 do begin
                Include (FirstCharSet, opnd^);
                inc (opnd);
              end;
            end;
           EXIT;
         end;
         BOUND, NOTBOUND: ; //###0.943 ?!!
         ANY, ANYML: begin // we can better define ANYML !!!
           FirstCharSet := [#0 .. #255]; //###0.930
           EXIT;
          end;
         ANYDIGIT: begin
           FirstCharSet := FirstCharSet + ['0' .. '9'];
           EXIT;
          end;
         NOTDIGIT: begin
           FirstCharSet := FirstCharSet + ([#0 .. #255] - ['0' .. '9']); //###0.948 FirstCharSet was forgotten
           EXIT;
          end;
         EXACTLYCI: begin
           Include (FirstCharSet, (scan + REOpSz + RENextOffSz)^);
           Include (FirstCharSet, InvertCase ((scan + REOpSz + RENextOffSz)^));
           EXIT;
          end;
         EXACTLY: begin
           Include (FirstCharSet, (scan + REOpSz + RENextOffSz)^);
           EXIT;
          end;
         ANYOFFULLSET: begin
           FirstCharSet := FirstCharSet + PSetOfREChar (scan + REOpSz + RENextOffSz)^;
           EXIT;
          end;
         ANYOFTINYSET: begin
           //!!!TinySet
           Include (FirstCharSet, (scan + REOpSz + RENextOffSz)^);
           Include (FirstCharSet, (scan + REOpSz + RENextOffSz + 1)^);
           Include (FirstCharSet, (scan + REOpSz + RENextOffSz + 2)^);
           // ...                                                      // up to TinySetLen
           EXIT;
          end;
         ANYBUTTINYSET: begin
           //!!!TinySet
           FirstCharSet := FirstCharSet + ([#0 .. #255] - [ //###0.948 FirstCharSet was forgotten
            (scan + REOpSz + RENextOffSz)^,
            (scan + REOpSz + RENextOffSz + 1)^,
            (scan + REOpSz + RENextOffSz + 2)^]);
           // ...                                                      // up to TinySetLen
           EXIT;
          end;
         NOTHING: ;
         COMMENT: ;
         BACK: ;
         Succ (OPEN) .. TREOp (Ord (OPEN) + NSUBEXP - 1) : begin //###0.929
            FillFirstCharSet (next);
            EXIT;
           end;
         Succ (CLOSE) .. TREOp (Ord (CLOSE) + NSUBEXP - 1): begin //###0.929
            FillFirstCharSet (next);
            EXIT;
           end;
         BRANCH: begin
            if (PREOp (next)^ <> BRANCH) // No choice.
             then next := scan + REOpSz + RENextOffSz // Avoid recursion.
             else begin
               REPEAT
                FillFirstCharSet (scan + REOpSz + RENextOffSz);
                scan := regnext (scan);
               UNTIL (scan = nil) or (PREOp (scan)^ <> BRANCH);
               EXIT;
              end;
           end;
         {$IFDEF ComplexBraces}
         LOOPENTRY: begin //###0.925
//           LoopStack [LoopStackIdx] := 0; //###0.940 line removed
           FillFirstCharSet (next); // execute LOOP
           EXIT;
          end;
         LOOP, LOOPNG: begin //###0.940
           opnd := scan + PRENextOff (scan + REOpSz + RENextOffSz + REBracesArgSz * 2)^;
           min_cnt := PREBracesArg (scan + REOpSz + RENextOffSz)^;
           FillFirstCharSet (opnd);
           if min_cnt = 0
            then FillFirstCharSet (next);
           EXIT;
          end;
         {$ENDIF}
         STAR, STARNG: //###0.940
           FillFirstCharSet (scan + REOpSz + RENextOffSz);
         PLUS, PLUSNG: begin //###0.940
           FillFirstCharSet (scan + REOpSz + RENextOffSz);
           EXIT;
          end;
         BRACES, BRACESNG: begin //###0.940
           opnd := scan + REOpSz + RENextOffSz + REBracesArgSz * 2;
           min_cnt := PREBracesArg (scan + REOpSz + RENextOffSz)^; // BRACES
           FillFirstCharSet (opnd);
           if min_cnt > 0
            then EXIT;
          end;
         EEND: begin
            FirstCharSet := [#0 .. #255]; //###0.948
            EXIT;
           end;
        else begin
            Error (reeMatchPrimMemoryCorruption);
            EXIT;
          end;
        end; { of case scan^}
        scan := next;
    end; { of while scan <> nil}
 end; { of procedure FillFirstCharSet
--------------------------------------------------------------}
{$ENDIF}

function TRegExpr.Exec (const AInputString : RegExprString) : boolean;
 begin
  InputString := AInputString;
  Result := ExecPrim (1);
 end; { of function TRegExpr.Exec
--------------------------------------------------------------}

{$IFDEF OverMeth}
{$IFNDEF FPC}
function TRegExpr.Exec : boolean;
 begin
  Result := ExecPrim (1);
 end; { of function TRegExpr.Exec
--------------------------------------------------------------}
{$ENDIF}
function TRegExpr.Exec (AOffset: integer) : boolean;
 begin
  Result := ExecPrim (AOffset);
 end; { of function TRegExpr.Exec
--------------------------------------------------------------}
{$ENDIF}

function TRegExpr.ExecPos (AOffset: integer {$IFDEF DefParam}= 1{$ENDIF}) : boolean;
 begin
  Result := ExecPrim (AOffset);
 end; { of function TRegExpr.ExecPos
--------------------------------------------------------------}

function TRegExpr.ExecPrim (AOffset: integer) : boolean;
 procedure ClearMatchs;
  // Clears matchs array
  var i : integer;
  begin
   for i := 0 to NSUBEXP - 1 do begin
     startp [i] := nil;
     endp [i] := nil;
    end;
  end; { of procedure ClearMatchs;
..............................................................}
 function RegMatch (str : PRegExprChar) : boolean;
  // try match at specific point
  begin
   //###0.949 removed clearing of start\endp
   reginput := str;
   Result := MatchPrim (programm + REOpSz);
   if Result then begin
     startp [0] := str;
     endp [0] := reginput;
    end;
  end; { of function RegMatch
..............................................................}
 var
  s : PRegExprChar;
  StartPtr: PRegExprChar;
  InputLen : integer;
 begin
  Result := false; // Be paranoid...

  ClearMatchs; //###0.949
  // ensure that Match cleared either if optimization tricks or some error
  // will lead to leaving ExecPrim without actual search. That is
  // importent for ExecNext logic and so on.

  if not IsProgrammOk //###0.929
   then EXIT;

  // Check InputString presence
  if not Assigned (fInputString) then begin
    Error (reeNoInpitStringSpecified);
    EXIT;
   end;

  InputLen := length (fInputString);

  //Check that the start position is not negative
  if AOffset < 1 then begin
    Error (reeOffsetMustBeGreaterThen0);
    EXIT;
   end;
  // Check that the start position is not longer than the line
  // If so then exit with nothing found
  if AOffset > (InputLen + 1) // for matching empty string after last char.
   then EXIT;

  StartPtr := fInputString + AOffset - 1;

  // If there is a "must appear" string, look for it.
  if regmust <> nil then begin
    s := StartPtr;
    REPEAT
     s := StrScan (s, regmust [0]);
     if s <> nil then begin
       if StrLComp (s, regmust, regmlen) = 0
        then BREAK; // Found it.
       inc (s);
      end;
    UNTIL s = nil;
    if s = nil // Not present.
     then EXIT;
   end;

  // Mark beginning of line for ^ .
  fInputStart := fInputString;

  // Pointer to end of input stream - for
  // pascal-style string processing (may include #0)
  fInputEnd := fInputString + InputLen;

  {$IFDEF ComplexBraces}
  // no loops started
  LoopStackIdx := 0; //###0.925
  {$ENDIF}

  // Simplest case:  anchored match need be tried only once.
  if reganch <> #0 then begin
    Result := RegMatch (StartPtr);
    EXIT;
   end;

  // Messy cases:  unanchored match.
  s := StartPtr;
  if regstart <> #0 then // We know what char it must start with.
    REPEAT
     s := StrScan (s, regstart);
     if s <> nil then begin
       Result := RegMatch (s);
       if Result
        then EXIT
        else ClearMatchs; //###0.949
       inc (s);
      end;
    UNTIL s = nil
   else begin // We don't - general case.
     repeat //###0.948
       {$IFDEF UseFirstCharSet}
       if s^ in FirstCharSet
        then Result := RegMatch (s);
       {$ELSE}
       Result := RegMatch (s);
       {$ENDIF}
       if Result or (s^ = #0) // Exit on a match or after testing the end-of-string.
        then EXIT
        else ClearMatchs; //###0.949
       inc (s);
     until false;
(*  optimized and fixed by Martin Fuller - empty strings
    were not allowed to pass thru in UseFirstCharSet mode
     {$IFDEF UseFirstCharSet} //###0.929
     while s^ <> #0 do begin
       if s^ in FirstCharSet
        then Result := RegMatch (s);
       if Result
        then EXIT;
       inc (s);
      end;
     {$ELSE}
     REPEAT
      Result := RegMatch (s);
      if Result
       then EXIT;
      inc (s);
     UNTIL s^ = #0;
     {$ENDIF}
*)
    end;
  // Failure
 end; { of function TRegExpr.ExecPrim
--------------------------------------------------------------}

function TRegExpr.ExecNext : boolean;
 var offset : integer;
 begin
  Result := false;
  if not Assigned (startp[0]) or not Assigned (endp[0]) then begin
    Error (reeExecNextWithoutExec);
    EXIT;
   end;
//  Offset := MatchPos [0] + MatchLen [0];
//  if MatchLen [0] = 0
  Offset := endp [0] - fInputString + 1; //###0.929
  if endp [0] = startp [0] //###0.929
   then inc (Offset); // prevent infinite looping if empty string match r.e.
  Result := ExecPrim (Offset);
 end; { of function TRegExpr.ExecNext
--------------------------------------------------------------}

function TRegExpr.GetInputString : RegExprString;
 begin
  if not Assigned (fInputString) then begin
    Error (reeGetInputStringWithoutInputString);
    EXIT;
   end;
  Result := fInputString;
 end; { of function TRegExpr.GetInputString
--------------------------------------------------------------}

procedure TRegExpr.SetInputString (const AInputString : RegExprString);
 var
  Len : integer;
  i : integer;
 begin
  // clear Match* - before next Exec* call it's undefined
  for i := 0 to NSUBEXP - 1 do begin
    startp [i] := nil;
    endp [i] := nil;
   end;

  // need reallocation of input string buffer ?
  Len := length (AInputString);
  if Assigned (fInputString) and (Length (fInputString) <> Len) then begin
    FreeMem (fInputString);
    fInputString := nil;
   end;
  // buffer [re]allocation
  if not Assigned (fInputString)
   then GetMem (fInputString, (Len + 1) * SizeOf (REChar));

  // copy input string into buffer
  {$IFDEF UniCode}
  StrPCopy (fInputString, Copy (AInputString, 1, Len)); //###0.927
  {$ELSE}
  StrLCopy (fInputString, PRegExprChar (AInputString), Len);
  {$ENDIF}

  {
  fInputString : string;
  fInputStart, fInputEnd : PRegExprChar;

  SetInputString:
  fInputString := AInputString;
  UniqueString (fInputString);
  fInputStart := PChar (fInputString);
  Len := length (fInputString);
  fInputEnd := PRegExprChar (integer (fInputStart) + Len); ??
  !! startp/endp âñå ðàâíî áóäåò îïàñíî èñïîëüçîâàòü ?
  }
 end; { of procedure TRegExpr.SetInputString
--------------------------------------------------------------}

procedure TRegExpr.SetLineSeparators (const AStr : RegExprString);
 begin
  if AStr <> fLineSeparators then begin
    fLineSeparators := AStr;
    InvalidateProgramm;
   end;
 end; { of procedure TRegExpr.SetLineSeparators
--------------------------------------------------------------}

procedure TRegExpr.SetLinePairedSeparator (const AStr : RegExprString);
 begin
  if length (AStr) = 2 then begin
     if AStr [1] = AStr [2] then begin
      // it's impossible for our 'one-point' checking to support
      // two chars separator for identical chars
       Error (reeBadLinePairedSeparator);
       EXIT;
      end;
     if not fLinePairedSeparatorAssigned
      or (AStr [1] <> fLinePairedSeparatorHead)
      or (AStr [2] <> fLinePairedSeparatorTail) then begin
       fLinePairedSeparatorAssigned := true;
       fLinePairedSeparatorHead := AStr [1];
       fLinePairedSeparatorTail := AStr [2];
       InvalidateProgramm;
      end;
    end
   else if length (AStr) = 0 then begin
     if fLinePairedSeparatorAssigned then begin
       fLinePairedSeparatorAssigned := false;
       InvalidateProgramm;
      end;
    end
   else Error (reeBadLinePairedSeparator);
 end; { of procedure TRegExpr.SetLinePairedSeparator
--------------------------------------------------------------}

function TRegExpr.GetLinePairedSeparator : RegExprString;
 begin
  if fLinePairedSeparatorAssigned then begin
     {$IFDEF UniCode}
     // Here is some UniCode 'magic'
     // If You do know better decision to concatenate
     // two WideChars, please, let me know!
     Result := fLinePairedSeparatorHead; //###0.947
     Result := Result + fLinePairedSeparatorTail;
     {$ELSE}
     Result := fLinePairedSeparatorHead + fLinePairedSeparatorTail;
     {$ENDIF}
    end
   else Result := '';
 end; { of function TRegExpr.GetLinePairedSeparator
--------------------------------------------------------------}

function TRegExpr.Substitute (const ATemplate : RegExprString) : RegExprString;
// perform substitutions after a regexp match
// completely rewritten in 0.929
 var
  TemplateLen : integer;
  TemplateBeg, TemplateEnd : PRegExprChar;
  p, p0, ResultPtr : PRegExprChar;
  ResultLen : integer;
  n : integer;
  Ch : REChar;
 function ParseVarName (var APtr : PRegExprChar) : integer;
  // extract name of variable (digits, may be enclosed with
  // curly braces) from APtr^, uses TemplateEnd !!!
  const
   Digits = ['0' .. '9'];
  var
   p : PRegExprChar;
   Delimited : boolean;
  begin
   Result := 0;
   p := APtr;
   Delimited := (p < TemplateEnd) and (p^ = '{');
   if Delimited
    then inc (p); // skip left curly brace
   if (p < TemplateEnd) and (p^ = '&')
    then inc (p) // this is '$&' or '${&}'
    else
     while (p < TemplateEnd) and
      {$IFDEF UniCode} //###0.935
      (ord (p^) < 256) and (char (p^) in Digits)
      {$ELSE}
      (p^ in Digits)
      {$ENDIF}
       do begin
       Result := Result * 10 + (ord (p^) - ord ('0')); //###0.939
       inc (p);
      end;
   if Delimited then
    if (p < TemplateEnd) and (p^ = '}')
     then inc (p) // skip right curly brace
     else p := APtr; // isn't properly terminated
   if p = APtr
    then Result := -1; // no valid digits found or no right curly brace
   APtr := p;
  end;
 begin
  // Check programm and input string
  if not IsProgrammOk
   then EXIT;
  if not Assigned (fInputString) then begin
    Error (reeNoInpitStringSpecified);
    EXIT;
   end;
  // Prepare for working
  TemplateLen := length (ATemplate);
  if TemplateLen = 0 then begin // prevent nil pointers
    Result := '';
    EXIT;
   end;
  TemplateBeg := pointer (ATemplate);
  TemplateEnd := TemplateBeg + TemplateLen;
  // Count result length for speed optimization.
  ResultLen := 0;
  p := TemplateBeg;
  while p < TemplateEnd do begin
    Ch := p^;
    inc (p);
    if Ch = '$'
     then n := ParseVarName (p)
     else n := -1;
    if n >= 0 then begin
       if (n < NSUBEXP) and Assigned (startp [n]) and Assigned (endp [n])
        then inc (ResultLen, endp [n] - startp [n]);
      end
     else begin
       if (Ch = EscChar) and (p < TemplateEnd)
        then inc (p); // quoted or special char followed
       inc (ResultLen);
      end;
   end;
  // Get memory. We do it once and it significant speed up work !
  if ResultLen = 0 then begin
    Result := '';
    EXIT;
   end;
  SetString (Result, nil, ResultLen);
  // Fill Result
  ResultPtr := pointer (Result);
  p := TemplateBeg;
  while p < TemplateEnd do begin
    Ch := p^;
    inc (p);
    if Ch = '$'
     then n := ParseVarName (p)
     else n := -1;
    if n >= 0 then begin
       p0 := startp [n];
       if (n < NSUBEXP) and Assigned (p0) and Assigned (endp [n]) then
        while p0 < endp [n] do begin
          ResultPtr^ := p0^;
          inc (ResultPtr);
          inc (p0);
         end;
      end
     else begin
       if (Ch = EscChar) and (p < TemplateEnd) then begin // quoted or special char followed
         Ch := p^;
         inc (p);
        end;
       ResultPtr^ := Ch;
       inc (ResultPtr);
      end;
   end;
 end; { of function TRegExpr.Substitute
--------------------------------------------------------------}

procedure TRegExpr.Split (AInputStr : RegExprString; APieces : TStrings);
 var PrevPos : integer;
 begin
  PrevPos := 1;
  if Exec (AInputStr) then
   REPEAT
    APieces.Add (System.Copy (AInputStr, PrevPos, MatchPos [0] - PrevPos));
    PrevPos := MatchPos [0] + MatchLen [0];
   UNTIL not ExecNext;
  APieces.Add (System.Copy (AInputStr, PrevPos, MaxInt)); // Tail
 end; { of procedure TRegExpr.Split
--------------------------------------------------------------}

function TRegExpr.Replace (AInputStr : RegExprString; const AReplaceStr : RegExprString;
      AUseSubstitution : boolean{$IFDEF DefParam}= False{$ENDIF}) : RegExprString;
 var
  PrevPos : integer;
 begin
  Result := '';
  PrevPos := 1;
  if Exec (AInputStr) then
   REPEAT
    Result := Result + System.Copy (AInputStr, PrevPos,
      MatchPos [0] - PrevPos);
    if AUseSubstitution //###0.946
    then Result := Result + Substitute (AReplaceStr)
    else Result := Result + AReplaceStr;
    PrevPos := MatchPos [0] + MatchLen [0];
   UNTIL not ExecNext;
  Result := Result + System.Copy (AInputStr, PrevPos, MaxInt); // Tail
 end; { of function TRegExpr.Replace
--------------------------------------------------------------}

function TRegExpr.ReplaceEx (AInputStr : RegExprString;
      AReplaceFunc : TRegExprReplaceFunction)
     : RegExprString;
 var
  PrevPos : integer;
 begin
  Result := '';
  PrevPos := 1;
  if Exec (AInputStr) then
   REPEAT
    Result := Result + System.Copy (AInputStr, PrevPos,
      MatchPos [0] - PrevPos)
     + AReplaceFunc (Self);
    PrevPos := MatchPos [0] + MatchLen [0];
   UNTIL not ExecNext;
  Result := Result + System.Copy (AInputStr, PrevPos, MaxInt); // Tail
 end; { of function TRegExpr.ReplaceEx
--------------------------------------------------------------}


{$IFDEF OverMeth}
function TRegExpr.Replace (AInputStr : RegExprString;
      AReplaceFunc : TRegExprReplaceFunction)
     : RegExprString;
 begin
  ReplaceEx (AInputStr, AReplaceFunc);
 end; { of function TRegExpr.Replace
--------------------------------------------------------------}
{$ENDIF}

{=============================================================}
{====================== Debug section ========================}
{=============================================================}

{$IFDEF RegExpPCodeDump}
function TRegExpr.DumpOp (op : TREOp) : RegExprString;
// printable representation of opcode
 begin
  case op of
    BOL:          Result := 'BOL';
    EOL:          Result := 'EOL';
    BOLML:        Result := 'BOLML';
    EOLML:        Result := 'EOLML';
    BOUND:        Result := 'BOUND'; //###0.943
    NOTBOUND:     Result := 'NOTBOUND'; //###0.943
    ANY:          Result := 'ANY';
    ANYML:        Result := 'ANYML'; //###0.941
    ANYLETTER:    Result := 'ANYLETTER';
    NOTLETTER:    Result := 'NOTLETTER';
    ANYDIGIT:     Result := 'ANYDIGIT';
    NOTDIGIT:     Result := 'NOTDIGIT';
    ANYSPACE:     Result := 'ANYSPACE';
    NOTSPACE:     Result := 'NOTSPACE';
    ANYOF:        Result := 'ANYOF';
    ANYBUT:       Result := 'ANYBUT';
    ANYOFCI:      Result := 'ANYOF/CI';
    ANYBUTCI:     Result := 'ANYBUT/CI';
    BRANCH:       Result := 'BRANCH';
    EXACTLY:      Result := 'EXACTLY';
    EXACTLYCI:    Result := 'EXACTLY/CI';
    NOTHING:      Result := 'NOTHING';
    COMMENT:      Result := 'COMMENT';
    BACK:         Result := 'BACK';
    EEND:         Result := 'END';
    BSUBEXP:      Result := 'BSUBEXP';
    BSUBEXPCI:    Result := 'BSUBEXP/CI';
    Succ (OPEN) .. TREOp (Ord (OPEN) + NSUBEXP - 1): //###0.929
                  Result := Format ('OPEN[%d]', [ord (op) - ord (OPEN)]);
    Succ (CLOSE) .. TREOp (Ord (CLOSE) + NSUBEXP - 1): //###0.929
                  Result := Format ('CLOSE[%d]', [ord (op) - ord (CLOSE)]);
    STAR:         Result := 'STAR';
    PLUS:         Result := 'PLUS';
    BRACES:       Result := 'BRACES';
    {$IFDEF ComplexBraces}
    LOOPENTRY:    Result := 'LOOPENTRY'; //###0.925
    LOOP:         Result := 'LOOP'; //###0.925
    LOOPNG:       Result := 'LOOPNG'; //###0.940
    {$ENDIF}
    ANYOFTINYSET: Result:= 'ANYOFTINYSET';
    ANYBUTTINYSET:Result:= 'ANYBUTTINYSET';
    {$IFDEF UseSetOfChar} //###0.929
    ANYOFFULLSET: Result:= 'ANYOFFULLSET';
    {$ENDIF}
    STARNG:       Result := 'STARNG'; //###0.940
    PLUSNG:       Result := 'PLUSNG'; //###0.940
    BRACESNG:     Result := 'BRACESNG'; //###0.940
    else Error (reeDumpCorruptedOpcode);
   end; {of case op}
  Result := ':' + Result;
 end; { of function TRegExpr.DumpOp
--------------------------------------------------------------}

function TRegExpr.Dump : RegExprString;
// dump a regexp in vaguely comprehensible form
 var
  s : PRegExprChar;
  op : TREOp; // Arbitrary non-END op.
  next : PRegExprChar;
  i : integer;
  Diff : integer;
{$IFDEF UseSetOfChar} //###0.929
  Ch : REChar;
{$ENDIF}
 begin
  if not IsProgrammOk //###0.929
   then EXIT;

  op := EXACTLY;
  Result := '';
  s := programm + REOpSz;
  while op <> EEND do begin // While that wasn't END last time...
     op := s^;
     Result := Result + Format ('%2d%s', [s - programm, DumpOp (s^)]); // Where, what.
     next := regnext (s);
     if next = nil // Next ptr.
      then Result := Result + ' (0)'
      else begin
        if next > s //###0.948 PWideChar subtraction workaround (see comments in Tail method for details)
         then Diff := next - s
         else Diff := - (s - next);
        Result := Result + Format (' (%d) ', [(s - programm) + Diff]);
       end;
     inc (s, REOpSz + RENextOffSz);
     if (op = ANYOF) or (op = ANYOFCI) or (op = ANYBUT) or (op = ANYBUTCI)
        or (op = EXACTLY) or (op = EXACTLYCI) then begin
         // Literal string, where present.
         while s^ <> #0 do begin
           Result := Result + s^;
           inc (s);
          end;
         inc (s);
      end;
     if (op = ANYOFTINYSET) or (op = ANYBUTTINYSET) then begin
       for i := 1 to TinySetLen do begin
         Result := Result + s^;
         inc (s);
        end;
      end;
     if (op = BSUBEXP) or (op = BSUBEXPCI) then begin
       Result := Result + ' \' + IntToStr (Ord (s^));
       inc (s);
      end;
     {$IFDEF UseSetOfChar} //###0.929
     if op = ANYOFFULLSET then begin
       for Ch := #0 to #255 do
        if Ch in PSetOfREChar (s)^ then
         if Ch < ' '
          then Result := Result + '#' + IntToStr (Ord (Ch)) //###0.936
          else Result := Result + Ch;
       inc (s, SizeOf (TSetOfREChar));
      end;
     {$ENDIF}
     if (op = BRACES) or (op = BRACESNG) then begin //###0.941
       // show min/max argument of BRACES operator
       Result := Result + Format ('{%d,%d}', [PREBracesArg (s)^, PREBracesArg (s + REBracesArgSz)^]);
       inc (s, REBracesArgSz * 2);
      end;
     {$IFDEF ComplexBraces}
     if (op = LOOP) or (op = LOOPNG) then begin //###0.940
       Result := Result + Format (' -> (%d) {%d,%d}', [
        (s - programm - (REOpSz + RENextOffSz)) + PRENextOff (s + 2 * REBracesArgSz)^,
        PREBracesArg (s)^, PREBracesArg (s + REBracesArgSz)^]);
       inc (s, 2 * REBracesArgSz + RENextOffSz);
      end;
     {$ENDIF}
     Result := Result + #$d#$a;
   end; { of while}

  // Header fields of interest.

  if regstart <> #0
   then Result := Result + 'start ' + regstart;
  if reganch <> #0
   then Result := Result + 'anchored ';
  if regmust <> nil
   then Result := Result + 'must have ' + regmust;
  {$IFDEF UseFirstCharSet} //###0.929
  Result := Result + #$d#$a'FirstCharSet:';
  for Ch := #0 to #255 do
   if Ch in FirstCharSet
    then begin
      if Ch < ' '
       then Result := Result + '#' + IntToStr(Ord(Ch)) //###0.948
       else Result := Result + Ch;
    end;
  {$ENDIF}
  Result := Result + #$d#$a;
 end; { of function TRegExpr.Dump
--------------------------------------------------------------}
{$ENDIF}

{$IFDEF reRealExceptionAddr}
{$OPTIMIZATION ON}
// ReturnAddr works correctly only if compiler optimization is ON
// I placed this method at very end of unit because there are no
// way to restore compiler optimization flag ...
{$ENDIF}
procedure TRegExpr.Error (AErrorID : integer);
{$IFDEF reRealExceptionAddr}
 function ReturnAddr : pointer; //###0.938
  asm
   mov  eax,[ebp+4]
  end;
{$ENDIF}
 var
  e : ERegExpr;
 begin
  fLastError := AErrorID; // dummy stub - useless because will raise exception
  if AErrorID < 1000 // compilation error ?
   then e := ERegExpr.Create (ErrorMsg (AErrorID) // yes - show error pos
             + ' (pos ' + IntToStr (CompilerErrorPos) + ')')
   else e := ERegExpr.Create (ErrorMsg (AErrorID));
  e.ErrorCode := AErrorID;
  e.CompilerErrorPos := CompilerErrorPos;
  raise e
   {$IFDEF reRealExceptionAddr}
   At ReturnAddr; //###0.938
   {$ENDIF}
 end; { of procedure TRegExpr.Error
--------------------------------------------------------------}

(*
  PCode persistence:
   FirstCharSet
   programm, regsize
   regstart // -> programm
   reganch // -> programm
   regmust, regmlen // -> programm
   fExprIsCompiled
*)

// be carefull - placed here code will be always compiled with
// compiler optimization flag

{$IFDEF FPC}
initialization
 RegExprInvertCaseFunction := TRegExpr.InvertCaseFunction;

{$ENDIF}
end.

