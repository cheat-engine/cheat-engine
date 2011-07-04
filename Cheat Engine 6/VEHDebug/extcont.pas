unit extcont;

{$mode delphi}

interface

uses
  Classes, SysUtils, jwawindows, windows;

  //credits to jedi code library for filling in the "extended registers"
const CONTEXT_EXTENDED=$20;

type
  TJclMMContentType = (mt8Bytes, mt4Words, mt2DWords, mt1QWord, mt2Singles, mt1Double);

  TJclMMRegister = packed record
    case TJclMMContentType of
      mt8Bytes:
        (Bytes: array [0..7] of Byte;);
      mt4Words:
        (Words: array [0..3] of Word;);
      mt2DWords:
        (DWords: array [0..1] of Cardinal;);
      mt1QWord:
        (QWords: Int64;);
      mt2Singles:
        (Singles: array [0..1] of Single;);
      mt1Double:
        (Doubles: double;);
  end;

  TJclFPUContentType = (ftExtended, ftMM);

  TJclFPUData = packed record
    case TJclFPUContentType of
      ftExtended:
        (FloatValue: Extended;);
      ftMM:
        (MMRegister: TJclMMRegister;
         Reserved: Word;);
  end;

  TJclFPURegister = packed record
    Data: TJclFPUData;
    Reserved: array [0..5] of Byte;
  end;

  TJclFPURegisters = array [0..7] of TJclFPURegister;

  TJclXMMContentType = (xt16Bytes, xt8Words, xt4DWords, xt2QWords, xt4Singles, xt2Doubles);

  TJclXMMRegister = packed record
    case TJclXMMContentType of
      xt16Bytes:
        (Bytes: array [0..15] of Byte;);
      xt8Words:
        (Words: array [0..7] of Word;);
      xt4DWords:
        (DWords: array [0..3] of Cardinal;);
      xt2QWords:
        (QWords: array [0..1] of Int64;);
      xt4Singles:
        (Singles: array [0..3] of Single;);
      xt2Doubles:
        (Doubles: array [0..1] of Double;);
  end;

  TJclProcessorSize = (ps32Bits, ps64Bits);

  TJclXMMRegisters = packed record
    case TJclProcessorSize of
      ps32Bits:
        (LegacyXMM: array [0..7] of TJclXMMRegister;
         LegacyReserved: array [0..127] of Byte;);
      ps64Bits:
        (LongXMM: array [0..15] of TJclXMMRegister;);
  end;

  TextendedRegisters = packed record    //fxsave
    //extended registers
    FCW: Word;                           // bytes from 0   to 1
    FSW: Word;                           // bytes from 2   to 3
    FTW: Byte;                           // byte 4
    Reserved1: Byte;                     // byte 5
    FOP: Word;                           // bytes from 6   to 7
    FpuIp: Cardinal;                     // bytes from 8   to 11
    CS: Word;                            // bytes from 12  to 13
    Reserved2: Word;                     // bytes from 14  to 15
    FpuDp: Cardinal;                     // bytes from 16  to 19
    DS: Word;                            // bytes from 20  to 21
    Reserved3: Word;                     // bytes from 22  to 23
    MXCSR: Cardinal;                     // bytes from 24  to 27
    MXCSRMask: Cardinal;                 // bytes from 28  to 31
    FPURegisters: TJclFPURegisters;      // bytes from 32  to 159
    XMMRegisters: TJclXMMRegisters;      // bytes from 160 to 415
    Reserved4: array [416..511] of Byte; // bytes from 416 to 511
  end;


type
  _ECONTEXT = packed record
    ContextFlags: DWORD;
    Dr0: DWORD;
    Dr1: DWORD;
    Dr2: DWORD;
    Dr3: DWORD;
    Dr6: DWORD;
    Dr7: DWORD;

    FloatSave: TFloatingSaveArea;

    SegGs: DWORD;
    SegFs: DWORD;
    SegEs: DWORD;
    SegDs: DWORD;

    Edi: DWORD;
    Esi: DWORD;
    Ebx: DWORD;
    Edx: DWORD;
    Ecx: DWORD;
    Eax: DWORD;

    Ebp: DWORD;
    Eip: DWORD;
    SegCs: DWORD;
    EFlags: DWORD;
    Esp: DWORD;
    SegSs: DWORD;

    ext: TExtendedRegisters;
  end;
  ECONTEXT=_ECONTEXT;
  TEContext=ECONTEXT;
  PEContext = ^TEContext;

implementation

end.

