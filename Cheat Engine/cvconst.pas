unit cvconst;

{$mode delphi}

interface

uses
  Classes, SysUtils;



type
  TBasicType=(
     btNoType = 0,
     btVoid = 1,
     btChar = 2,
     btWChar = 3,
     btInt = 6,
     btUInt = 7,
     btFloat = 8,
     btBCD = 9,
     btBool = 10,
     btLong = 13,
     btULong = 14,
     btInt2=16,
     btCurrency = 25,
     btDate = 26,
     btVariant = 27,
     btComplex = 28,
     btBit = 29,
     btBSTR = 30,
     btHresult = 31);

  TSymTagEnum=(
     SymTagNull=0,
     SymTagExe,
     SymTagCompiland,
     SymTagCompilandDetails,
     SymTagCompilandEnv,
     SymTagFunction,
     SymTagBlock,
     SymTagData,
     SymTagAnnotation,
     SymTagLabel,
     SymTagPublicSymbol,
     SymTagUDT,
     SymTagEnum,
     SymTagFunctionType,
     SymTagPointerType,
     SymTagArrayType,
     SymTagBaseType,
     SymTagTypedef,
     SymTagBaseClass,
     SymTagFriend,
     SymTagFunctionArgType,
     SymTagFuncDebugStart,
     SymTagFuncDebugEnd,
     SymTagUsingNamespace,
     SymTagVTableShape,
     SymTagVTable,
     SymTagCustom,
     SymTagThunk,
     SymTagCustomType,
     SymTagManagedType,
     SymTagDimension);



//enum CV_HREG_e
// Register subset shared by all processor types,
// must not overlap with any of the ranges below, hence the high values


const
    CV_ALLREG_ERR   =   30000;
    CV_ALLREG_TEB   =   30001;
    CV_ALLREG_TIMER =   30002;
    CV_ALLREG_EFAD1 =   30003;
    CV_ALLREG_EFAD2 =   30004;
    CV_ALLREG_EFAD3 =   30005;
    CV_ALLREG_VFRAME=   30006;
    CV_ALLREG_HANDLE=   30007;
    CV_ALLREG_PARAMS=   30008;
    CV_ALLREG_LOCALS=   30009;
    CV_ALLREG_TID   =   30010;
    CV_ALLREG_ENV   =   30011;
    CV_ALLREG_CMDLN =   30012;


    //  Register set for the Intel 80x86 and ix86 processor series
    //  (plus PCODE registers)

    CV_REG_NONE     =   0;
    CV_REG_AL       =   1;
    CV_REG_CL       =   2;
    CV_REG_DL       =   3;
    CV_REG_BL       =   4;
    CV_REG_AH       =   5;
    CV_REG_CH       =   6;
    CV_REG_DH       =   7;
    CV_REG_BH       =   8;
    CV_REG_AX       =   9;
    CV_REG_CX       =  10;
    CV_REG_DX       =  11;
    CV_REG_BX       =  12;
    CV_REG_SP       =  13;
    CV_REG_BP       =  14;
    CV_REG_SI       =  15;
    CV_REG_DI       =  16;
    CV_REG_EAX      =  17;
    CV_REG_ECX      =  18;
    CV_REG_EDX      =  19;
    CV_REG_EBX      =  20;
    CV_REG_ESP      =  21;
    CV_REG_EBP      =  22;
    CV_REG_ESI      =  23;
    CV_REG_EDI      =  24;
    CV_REG_ES       =  25;
    CV_REG_CS       =  26;
    CV_REG_SS       =  27;
    CV_REG_DS       =  28;
    CV_REG_FS       =  29;
    CV_REG_GS       =  30;
    CV_REG_IP       =  31;
    CV_REG_FLAGS    =  32;
    CV_REG_EIP      =  33;
    CV_REG_EFLAGS   =  34;
    CV_REG_TEMP     =  40;          // PCODE Temp
    CV_REG_TEMPH    =  41;          // PCODE TempH
    CV_REG_QUOTE    =  42;          // PCODE Quote
    CV_REG_PCDR3    =  43;          // PCODE reserved
    CV_REG_PCDR4    =  44;          // PCODE reserved
    CV_REG_PCDR5    =  45;          // PCODE reserved
    CV_REG_PCDR6    =  46;          // PCODE reserved
    CV_REG_PCDR7    =  47;          // PCODE reserved
    CV_REG_CR0      =  80;          // CR0 -- control registers
    CV_REG_CR1      =  81;
    CV_REG_CR2      =  82;
    CV_REG_CR3      =  83;
    CV_REG_CR4      =  84;          // Pentium
    CV_REG_DR0      =  90;          // Debug register
    CV_REG_DR1      =  91;
    CV_REG_DR2      =  92;
    CV_REG_DR3      =  93;
    CV_REG_DR4      =  94;
    CV_REG_DR5      =  95;
    CV_REG_DR6      =  96;
    CV_REG_DR7      =  97;
    CV_REG_GDTR     =  110;
    CV_REG_GDTL     =  111;
    CV_REG_IDTR     =  112;
    CV_REG_IDTL     =  113;
    CV_REG_LDTR     =  114;
    CV_REG_TR       =  115;

    CV_REG_PSEUDO1  =  116;
    CV_REG_PSEUDO2  =  117;
    CV_REG_PSEUDO3  =  118;
    CV_REG_PSEUDO4  =  119;
    CV_REG_PSEUDO5  =  120;
    CV_REG_PSEUDO6  =  121;
    CV_REG_PSEUDO7  =  122;
    CV_REG_PSEUDO8  =  123;
    CV_REG_PSEUDO9  =  124;

    CV_REG_ST0      =  128;
    CV_REG_ST1      =  129;
    CV_REG_ST2      =  130;
    CV_REG_ST3      =  131;
    CV_REG_ST4      =  132;
    CV_REG_ST5      =  133;
    CV_REG_ST6      =  134;
    CV_REG_ST7      =  135;
    CV_REG_CTRL     =  136;
    CV_REG_STAT     =  137;
    CV_REG_TAG      =  138;
    CV_REG_FPIP     =  139;
    CV_REG_FPCS     =  140;
    CV_REG_FPDO     =  141;
    CV_REG_FPDS     =  142;
    CV_REG_ISEM     =  143;
    CV_REG_FPEIP    =  144;
    CV_REG_FPEDO    =  145;

    CV_REG_MM0      =  146;
    CV_REG_MM1      =  147;
    CV_REG_MM2      =  148;
    CV_REG_MM3      =  149;
    CV_REG_MM4      =  150;
    CV_REG_MM5      =  151;
    CV_REG_MM6      =  152;
    CV_REG_MM7      =  153;

    CV_REG_XMM0     =  154; // KATMAI registers
    CV_REG_XMM1     =  155;
    CV_REG_XMM2     =  156;
    CV_REG_XMM3     =  157;
    CV_REG_XMM4     =  158;
    CV_REG_XMM5     =  159;
    CV_REG_XMM6     =  160;
    CV_REG_XMM7     =  161;

    CV_REG_XMM00    =  162; // KATMAI sub-registers
    CV_REG_XMM01    =  163;
    CV_REG_XMM02    =  164;
    CV_REG_XMM03    =  165;
    CV_REG_XMM10    =  166;
    CV_REG_XMM11    =  167;
    CV_REG_XMM12    =  168;
    CV_REG_XMM13    =  169;
    CV_REG_XMM20    =  170;
    CV_REG_XMM21    =  171;
    CV_REG_XMM22    =  172;
    CV_REG_XMM23    =  173;
    CV_REG_XMM30    =  174;
    CV_REG_XMM31    =  175;
    CV_REG_XMM32    =  176;
    CV_REG_XMM33    =  177;
    CV_REG_XMM40    =  178;
    CV_REG_XMM41    =  179;
    CV_REG_XMM42    =  180;
    CV_REG_XMM43    =  181;
    CV_REG_XMM50    =  182;
    CV_REG_XMM51    =  183;
    CV_REG_XMM52    =  184;
    CV_REG_XMM53    =  185;
    CV_REG_XMM60    =  186;
    CV_REG_XMM61    =  187;
    CV_REG_XMM62    =  188;
    CV_REG_XMM63    =  189;
    CV_REG_XMM70    =  190;
    CV_REG_XMM71    =  191;
    CV_REG_XMM72    =  192;
    CV_REG_XMM73    =  193;

    CV_REG_XMM0L    =  194;
    CV_REG_XMM1L    =  195;
    CV_REG_XMM2L    =  196;
    CV_REG_XMM3L    =  197;
    CV_REG_XMM4L    =  198;
    CV_REG_XMM5L    =  199;
    CV_REG_XMM6L    =  200;
    CV_REG_XMM7L    =  201;

    CV_REG_XMM0H    =  202;
    CV_REG_XMM1H    =  203;
    CV_REG_XMM2H    =  204;
    CV_REG_XMM3H    =  205;
    CV_REG_XMM4H    =  206;
    CV_REG_XMM5H    =  207;
    CV_REG_XMM6H    =  208;
    CV_REG_XMM7H    =  209;

    CV_REG_MXCSR    =  211; // XMM status register

    CV_REG_EDXEAX   =  212; // EDX:EAX pair

    CV_REG_EMM0L    =  220; // XMM sub-registers (WNI integer)
    CV_REG_EMM1L    =  221;
    CV_REG_EMM2L    =  222;
    CV_REG_EMM3L    =  223;
    CV_REG_EMM4L    =  224;
    CV_REG_EMM5L    =  225;
    CV_REG_EMM6L    =  226;
    CV_REG_EMM7L    =  227;

    CV_REG_EMM0H    =  228;
    CV_REG_EMM1H    =  229;
    CV_REG_EMM2H    =  230;
    CV_REG_EMM3H    =  231;
    CV_REG_EMM4H    =  232;
    CV_REG_EMM5H    =  233;
    CV_REG_EMM6H    =  234;
    CV_REG_EMM7H    =  235;

    // do not change the order of these regs; first one must be even too
    CV_REG_MM00     =  236;
    CV_REG_MM01     =  237;
    CV_REG_MM10     =  238;
    CV_REG_MM11     =  239;
    CV_REG_MM20     =  240;
    CV_REG_MM21     =  241;
    CV_REG_MM30     =  242;
    CV_REG_MM31     =  243;
    CV_REG_MM40     =  244;
    CV_REG_MM41     =  245;
    CV_REG_MM50     =  246;
    CV_REG_MM51     =  247;
    CV_REG_MM60     =  248;
    CV_REG_MM61     =  249;
    CV_REG_MM70     =  250;
    CV_REG_MM71     =  251;

    //
    // AMD64 registers
    //

    CV_AMD64_AL       =   1;
    CV_AMD64_CL       =   2;
    CV_AMD64_DL       =   3;
    CV_AMD64_BL       =   4;
    CV_AMD64_AH       =   5;
    CV_AMD64_CH       =   6;
    CV_AMD64_DH       =   7;
    CV_AMD64_BH       =   8;
    CV_AMD64_AX       =   9;
    CV_AMD64_CX       =  10;
    CV_AMD64_DX       =  11;
    CV_AMD64_BX       =  12;
    CV_AMD64_SP       =  13;
    CV_AMD64_BP       =  14;
    CV_AMD64_SI       =  15;
    CV_AMD64_DI       =  16;
    CV_AMD64_EAX      =  17;
    CV_AMD64_ECX      =  18;
    CV_AMD64_EDX      =  19;
    CV_AMD64_EBX      =  20;
    CV_AMD64_ESP      =  21;
    CV_AMD64_EBP      =  22;
    CV_AMD64_ESI      =  23;
    CV_AMD64_EDI      =  24;
    CV_AMD64_ES       =  25;
    CV_AMD64_CS       =  26;
    CV_AMD64_SS       =  27;
    CV_AMD64_DS       =  28;
    CV_AMD64_FS       =  29;
    CV_AMD64_GS       =  30;
    CV_AMD64_FLAGS    =  32;
    CV_AMD64_RIP      =  33;
    CV_AMD64_EFLAGS   =  34;

    // Control registers
    CV_AMD64_CR0      =  80;
    CV_AMD64_CR1      =  81;
    CV_AMD64_CR2      =  82;
    CV_AMD64_CR3      =  83;
    CV_AMD64_CR4      =  84;
    CV_AMD64_CR8      =  88;

    // Debug registers
    CV_AMD64_DR0      =  90;
    CV_AMD64_DR1      =  91;
    CV_AMD64_DR2      =  92;
    CV_AMD64_DR3      =  93;
    CV_AMD64_DR4      =  94;
    CV_AMD64_DR5      =  95;
    CV_AMD64_DR6      =  96;
    CV_AMD64_DR7      =  97;
    CV_AMD64_DR8      =  98;
    CV_AMD64_DR9      =  99;
    CV_AMD64_DR10     =  100;
    CV_AMD64_DR11     =  101;
    CV_AMD64_DR12     =  102;
    CV_AMD64_DR13     =  103;
    CV_AMD64_DR14     =  104;
    CV_AMD64_DR15     =  105;

    CV_AMD64_GDTR     =  110;
    CV_AMD64_GDTL     =  111;
    CV_AMD64_IDTR     =  112;
    CV_AMD64_IDTL     =  113;
    CV_AMD64_LDTR     =  114;
    CV_AMD64_TR       =  115;

    CV_AMD64_ST0      =  128;
    CV_AMD64_ST1      =  129;
    CV_AMD64_ST2      =  130;
    CV_AMD64_ST3      =  131;
    CV_AMD64_ST4      =  132;
    CV_AMD64_ST5      =  133;
    CV_AMD64_ST6      =  134;
    CV_AMD64_ST7      =  135;
    CV_AMD64_CTRL     =  136;
    CV_AMD64_STAT     =  137;
    CV_AMD64_TAG      =  138;
    CV_AMD64_FPIP     =  139;
    CV_AMD64_FPCS     =  140;
    CV_AMD64_FPDO     =  141;
    CV_AMD64_FPDS     =  142;
    CV_AMD64_ISEM     =  143;
    CV_AMD64_FPEIP    =  144;
    CV_AMD64_FPEDO    =  145;

    CV_AMD64_MM0      =  146;
    CV_AMD64_MM1      =  147;
    CV_AMD64_MM2      =  148;
    CV_AMD64_MM3      =  149;
    CV_AMD64_MM4      =  150;
    CV_AMD64_MM5      =  151;
    CV_AMD64_MM6      =  152;
    CV_AMD64_MM7      =  153;

    CV_AMD64_XMM0     =  154;   // KATMAI registers
    CV_AMD64_XMM1     =  155;
    CV_AMD64_XMM2     =  156;
    CV_AMD64_XMM3     =  157;
    CV_AMD64_XMM4     =  158;
    CV_AMD64_XMM5     =  159;
    CV_AMD64_XMM6     =  160;
    CV_AMD64_XMM7     =  161;

    CV_AMD64_XMM0_0   =  162;   // KATMAI sub-registers
    CV_AMD64_XMM0_1   =  163;
    CV_AMD64_XMM0_2   =  164;
    CV_AMD64_XMM0_3   =  165;
    CV_AMD64_XMM1_0   =  166;
    CV_AMD64_XMM1_1   =  167;
    CV_AMD64_XMM1_2   =  168;
    CV_AMD64_XMM1_3   =  169;
    CV_AMD64_XMM2_0   =  170;
    CV_AMD64_XMM2_1   =  171;
    CV_AMD64_XMM2_2   =  172;
    CV_AMD64_XMM2_3   =  173;
    CV_AMD64_XMM3_0   =  174;
    CV_AMD64_XMM3_1   =  175;
    CV_AMD64_XMM3_2   =  176;
    CV_AMD64_XMM3_3   =  177;
    CV_AMD64_XMM4_0   =  178;
    CV_AMD64_XMM4_1   =  179;
    CV_AMD64_XMM4_2   =  180;
    CV_AMD64_XMM4_3   =  181;
    CV_AMD64_XMM5_0   =  182;
    CV_AMD64_XMM5_1   =  183;
    CV_AMD64_XMM5_2   =  184;
    CV_AMD64_XMM5_3   =  185;
    CV_AMD64_XMM6_0   =  186;
    CV_AMD64_XMM6_1   =  187;
    CV_AMD64_XMM6_2   =  188;
    CV_AMD64_XMM6_3   =  189;
    CV_AMD64_XMM7_0   =  190;
    CV_AMD64_XMM7_1   =  191;
    CV_AMD64_XMM7_2   =  192;
    CV_AMD64_XMM7_3   =  193;

    CV_AMD64_XMM0L    =  194;
    CV_AMD64_XMM1L    =  195;
    CV_AMD64_XMM2L    =  196;
    CV_AMD64_XMM3L    =  197;
    CV_AMD64_XMM4L    =  198;
    CV_AMD64_XMM5L    =  199;
    CV_AMD64_XMM6L    =  200;
    CV_AMD64_XMM7L    =  201;

    CV_AMD64_XMM0H    =  202;
    CV_AMD64_XMM1H    =  203;
    CV_AMD64_XMM2H    =  204;
    CV_AMD64_XMM3H    =  205;
    CV_AMD64_XMM4H    =  206;
    CV_AMD64_XMM5H    =  207;
    CV_AMD64_XMM6H    =  208;
    CV_AMD64_XMM7H    =  209;

    CV_AMD64_MXCSR    =  211;   // XMM status register

    CV_AMD64_EMM0L    =  220;   // XMM sub-registers (WNI integer)
    CV_AMD64_EMM1L    =  221;
    CV_AMD64_EMM2L    =  222;
    CV_AMD64_EMM3L    =  223;
    CV_AMD64_EMM4L    =  224;
    CV_AMD64_EMM5L    =  225;
    CV_AMD64_EMM6L    =  226;
    CV_AMD64_EMM7L    =  227;

    CV_AMD64_EMM0H    =  228;
    CV_AMD64_EMM1H    =  229;
    CV_AMD64_EMM2H    =  230;
    CV_AMD64_EMM3H    =  231;
    CV_AMD64_EMM4H    =  232;
    CV_AMD64_EMM5H    =  233;
    CV_AMD64_EMM6H    =  234;
    CV_AMD64_EMM7H    =  235;

    // do not change the order of these regs; first one must be even too
    CV_AMD64_MM00     =  236;
    CV_AMD64_MM01     =  237;
    CV_AMD64_MM10     =  238;
    CV_AMD64_MM11     =  239;
    CV_AMD64_MM20     =  240;
    CV_AMD64_MM21     =  241;
    CV_AMD64_MM30     =  242;
    CV_AMD64_MM31     =  243;
    CV_AMD64_MM40     =  244;
    CV_AMD64_MM41     =  245;
    CV_AMD64_MM50     =  246;
    CV_AMD64_MM51     =  247;
    CV_AMD64_MM60     =  248;
    CV_AMD64_MM61     =  249;
    CV_AMD64_MM70     =  250;
    CV_AMD64_MM71     =  251;

    // Extended KATMAI registers
    CV_AMD64_XMM8     =  252;   // KATMAI registers
    CV_AMD64_XMM9     =  253;
    CV_AMD64_XMM10    =  254;
    CV_AMD64_XMM11    =  255;
    CV_AMD64_XMM12    =  256;
    CV_AMD64_XMM13    =  257;
    CV_AMD64_XMM14    =  258;
    CV_AMD64_XMM15    =  259;

    CV_AMD64_XMM8_0   =  260;   // KATMAI sub-registers
    CV_AMD64_XMM8_1   =  261;
    CV_AMD64_XMM8_2   =  262;
    CV_AMD64_XMM8_3   =  263;
    CV_AMD64_XMM9_0   =  264;
    CV_AMD64_XMM9_1   =  265;
    CV_AMD64_XMM9_2   =  266;
    CV_AMD64_XMM9_3   =  267;
    CV_AMD64_XMM10_0  =  268;
    CV_AMD64_XMM10_1  =  269;
    CV_AMD64_XMM10_2  =  270;
    CV_AMD64_XMM10_3  =  271;
    CV_AMD64_XMM11_0  =  272;
    CV_AMD64_XMM11_1  =  273;
    CV_AMD64_XMM11_2  =  274;
    CV_AMD64_XMM11_3  =  275;
    CV_AMD64_XMM12_0  =  276;
    CV_AMD64_XMM12_1  =  277;
    CV_AMD64_XMM12_2  =  278;
    CV_AMD64_XMM12_3  =  279;
    CV_AMD64_XMM13_0  =  280;
    CV_AMD64_XMM13_1  =  281;
    CV_AMD64_XMM13_2  =  282;
    CV_AMD64_XMM13_3  =  283;
    CV_AMD64_XMM14_0  =  284;
    CV_AMD64_XMM14_1  =  285;
    CV_AMD64_XMM14_2  =  286;
    CV_AMD64_XMM14_3  =  287;
    CV_AMD64_XMM15_0  =  288;
    CV_AMD64_XMM15_1  =  289;
    CV_AMD64_XMM15_2  =  290;
    CV_AMD64_XMM15_3  =  291;

    CV_AMD64_XMM8L    =  292;
    CV_AMD64_XMM9L    =  293;
    CV_AMD64_XMM10L   =  294;
    CV_AMD64_XMM11L   =  295;
    CV_AMD64_XMM12L   =  296;
    CV_AMD64_XMM13L   =  297;
    CV_AMD64_XMM14L   =  298;
    CV_AMD64_XMM15L   =  299;

    CV_AMD64_XMM8H    =  300;
    CV_AMD64_XMM9H    =  301;
    CV_AMD64_XMM10H   =  302;
    CV_AMD64_XMM11H   =  303;
    CV_AMD64_XMM12H   =  304;
    CV_AMD64_XMM13H   =  305;
    CV_AMD64_XMM14H   =  306;
    CV_AMD64_XMM15H   =  307;

    CV_AMD64_EMM8L    =  308;   // XMM sub-registers (WNI integer)
    CV_AMD64_EMM9L    =  309;
    CV_AMD64_EMM10L   =  310;
    CV_AMD64_EMM11L   =  311;
    CV_AMD64_EMM12L   =  312;
    CV_AMD64_EMM13L   =  313;
    CV_AMD64_EMM14L   =  314;
    CV_AMD64_EMM15L   =  315;

    CV_AMD64_EMM8H    =  316;
    CV_AMD64_EMM9H    =  317;
    CV_AMD64_EMM10H   =  318;
    CV_AMD64_EMM11H   =  319;
    CV_AMD64_EMM12H   =  320;
    CV_AMD64_EMM13H   =  321;
    CV_AMD64_EMM14H   =  322;
    CV_AMD64_EMM15H   =  323;

    // Low byte forms of some standard registers
    CV_AMD64_SIL      =  324;
    CV_AMD64_DIL      =  325;
    CV_AMD64_BPL      =  326;
    CV_AMD64_SPL      =  327;

    // 64-bit regular registers
    CV_AMD64_RAX      =  328;
    CV_AMD64_RBX      =  329;
    CV_AMD64_RCX      =  330;
    CV_AMD64_RDX      =  331;
    CV_AMD64_RSI      =  332;
    CV_AMD64_RDI      =  333;
    CV_AMD64_RBP      =  334;
    CV_AMD64_RSP      =  335;

    // 64-bit integer registers with 8-; 16-; and 32-bit forms (B; W; and D)
    CV_AMD64_R8       =  336;
    CV_AMD64_R9       =  337;
    CV_AMD64_R10      =  338;
    CV_AMD64_R11      =  339;
    CV_AMD64_R12      =  340;
    CV_AMD64_R13      =  341;
    CV_AMD64_R14      =  342;
    CV_AMD64_R15      =  343;

    CV_AMD64_R8B      =  344;
    CV_AMD64_R9B      =  345;
    CV_AMD64_R10B     =  346;
    CV_AMD64_R11B     =  347;
    CV_AMD64_R12B     =  348;
    CV_AMD64_R13B     =  349;
    CV_AMD64_R14B     =  350;
    CV_AMD64_R15B     =  351;

    CV_AMD64_R8W      =  352;
    CV_AMD64_R9W      =  353;
    CV_AMD64_R10W     =  354;
    CV_AMD64_R11W     =  355;
    CV_AMD64_R12W     =  356;
    CV_AMD64_R13W     =  357;
    CV_AMD64_R14W     =  358;
    CV_AMD64_R15W     =  359;

    CV_AMD64_R8D      =  360;
    CV_AMD64_R9D      =  361;
    CV_AMD64_R10D     =  362;
    CV_AMD64_R11D     =  363;
    CV_AMD64_R12D     =  364;
    CV_AMD64_R13D     =  365;
    CV_AMD64_R14D     =  366;
    CV_AMD64_R15D     =  367;


implementation

end.

