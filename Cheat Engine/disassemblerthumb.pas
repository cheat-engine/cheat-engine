unit DisassemblerThumb;

{$mode objfpc}{$H+}

{
This disassembler is build based on the information in chapter F of the ARM DDI 0487A.e ID121714 Documentation


}

interface

uses
  Classes, SysUtils, LastDisassembleData, strutils;

type
  TThumbDisassembler=object
  private
    a: ptruint;
    opcode: uint16;
    opcode2: uint16; //for 32-bit thumb2

    procedure ADC_R_T1;
    procedure AND_R_T1;
    procedure EOR_R_T1;
    procedure SBC_R_T1;
    procedure TST_R_T1;
    procedure RSB_I_T1;
    procedure CMP_R_T1;
    procedure CMP_R_T2;
    procedure CMN_R_T1;
    procedure ORR_R_T1;
    procedure MUL_R_T1;
    procedure BIC_R_T1;
    procedure MVN_R_T1;

    procedure MOV_R_T1;
    procedure MOV_R_T2;
    procedure MOV_RSR_T1;

    procedure ADD_R_T2;
    procedure ADD_SP_R_T1;
    procedure ADD_SP_R_T2;

    procedure ADD_R_T1;
    procedure SUB_R_T1;

    procedure ADD_I_T1;
    procedure ADD_I_T2;
    procedure ADD_I_T4;

    procedure ADR_T2;
    procedure ADR_T3;

    procedure SUB_I_T1;
    procedure SUB_I_T2;
    procedure SUB_I_T4;
    procedure SUB_I_T5;


    procedure SSAT_T1;
    procedure SSAT16_T1;

    procedure USAT_T1;
    procedure USAT16_T1;

    procedure SBFX_T1;
    procedure UBFX_T1;
    procedure BFI_T1;
    procedure BFC_T1;

    procedure MOV_I_T1;
    procedure MOV_I_T3;
    procedure MOVT_T1;

    procedure CMP_I_T1;

    procedure B_T1;
    procedure B_T2;
    procedure B_T3;
    procedure B_T4;

    procedure BL_X_T1;
    procedure BL_X_T2;

    procedure BX_T1;
    procedure BLX_R_T1;
    procedure BXJ_T1;


    procedure LDR_L_T1;

    procedure STR_R_T1;
    procedure STRH_R_T1;
    procedure STRB_R_T1;
    procedure LDRSB_R_T1;
    procedure LDR_R_T1;

    procedure LDRH_R_T1;
    procedure LDRB_R_T1;
    procedure LDRSH_R_T1;

    procedure STR_I_T1;
    procedure LDR_I_T1;
    procedure STR_I_T2;
    procedure LDR_I_T2;
    procedure STRB_I_T1;
    procedure LDRB_I_T1;
    procedure STRH_I_T1;
    procedure LDRH_I_T1;

    procedure ADR_T1;
    procedure ADD_SP_I_T1;
    procedure ADD_SP_I_T2;
    procedure SUB_SP_I_T1;
    procedure CBZ_CBNZ;
    procedure SXTH_T1;
    procedure SXTB_T1;
    procedure UXTH_T1;
    procedure UXTB_T1;
    procedure PUSH_T1;
    procedure POP_T1;

    procedure SETEND_T1;
    procedure CPS_T1;
    procedure CPS_T2;
    procedure REV_T1;
    procedure REV16_T1;
    procedure REVSH_T1;
    procedure DBG_T1;
    procedure HLT_T1;
    procedure BKPT_T1;
    procedure UDF_T1;
    procedure SVC_T1;
    procedure IT_T1;

    procedure STM_T1;
    procedure LDM_T1;

    procedure MSR_BR_T1;
    procedure MSR_R_T1;
    procedure MRS_BR_T1;
    procedure MRS_R_T1;


    //T32:
    procedure T32;
    procedure T32_DataProcessing_Reg;
    procedure T32_DataProcessing_Shifted_Reg;
    procedure T32_DataProcessing_Modified_Immediate;
    procedure T32_DataProcessing_Plain_Binary_Immediate;
    procedure T32_Branches_And_Miscellaneous_Control;
    procedure T32_LoadStore_Multiple;
    procedure T32_STR_LDR_STL;
    procedure T32_LoadWord;
    procedure T32_LoadHalfWord_MemoryHints;
    procedure T32_LoadByte_MemoryHints;

    procedure T32_StoreSingleDataItem;
    procedure T32_Multiply_MultiplyAccumulate_AbsoluteDiff;
    procedure T32_LongMultiply_LongMultiplyAccumulate_Divide;





  public
    LastDisassembleData: TLastDisassembleData;
    function disassemble(var address: ptrUint): string;
  end;

implementation

uses
  DisassemblerArm
{$ifndef armdev}
,ProcessHandlerUnit, NewKernelHandler
{$endif}
  ;



procedure TThumbDisassembler.T32_LongMultiply_LongMultiplyAccumulate_Divide;
var
  op1, op2: byte;
  x: byte;
begin
  op1:=(opcode shr 4) and 7;
  op2:=(opcode2 shr 4) and $f;
  x:=(op1 shl 4) or op2;

  case x of
    0: ;//SMULL
    $1f: ;//SDIV
    $20: ;//UMUL
    $3f: ;//UDIV
    $40: ;//SMLAL
    $48..$4b: ;//SMLALBB
    $4c..$4d: ;//SMLALD
    $5c..$5d: ;//SMLSLD
    $60: ;//UMLAL
    $66: ;//UMAAL


  end;

end;

procedure TThumbDisassembler.T32_Multiply_MultiplyAccumulate_AbsoluteDiff;
var op1, Ra, op2: byte;
  x: byte;
begin
  op1:=(opcode shr 4) and $7;
  Ra:=(opcode2 shr 12) and $f;
  op2:=(opcode2 shr 4) and 3;

  x:=(op1 shl 2) or op2;

  case op1 of
    0: ;//if ra=15 then MUL else MLA
    1: ;//MLS
    $4..$7: ;// if ra=15 then SMULLBB else SMLABB
    $8..$9: ;//if ra=15 then SMUAD else SMLAD
    $C..$d: ;//if ra=15 then SMULWB else SMLAWB
    $10..$11: ;//if ra=15 then SMUSD else SMLDS
    $14..$15: ;//if ra=15 then SMMUL else SMMLA
    $18..$19: ;//SMMLS
    $1c: ;//if ra=15 then USAD8 else USADA8
  end;

end;

procedure TThumbDisassembler.T32_DataProcessing_Reg;
var op1, rn, op2,x: byte;
begin
  op1:=(opcode shr 4) and $f;
  rn:=opcode and $f;
  op2:=(opcode shr 4) and $f;

  if (op2=0) and (op1 shr 1=1) then
  begin
    //movs_reg_shifted
  end
  else
  if (op1 shr 3=1) then
  begin
    case op2 shr 2 of
      0:
      begin
        //Parallel Addition and subtraction, signed
        op1:=(opcode shr 4) and 7;
        op2:=(opcode shr 4) and 3;

        x:=(op2 shl 3) or op1;

        case x of
          0: ; //SADD8
          1: ; //SADD16
          2: ; //SASX
          4: ; //SSUB8
          5: ; //SSUB16
          6: ; //SSAX

          $8+0: ; //QADD8
          $8+1: ; //QADD16
          $8+2: ; //QASX
          $8+4: ; //QSUB8
          $8+5: ; //QSUB16
          $8+6: ; //QSAX

          $10+0: ; //SHADD8
          $10+1: ; //SHADD16
          $10+2: ; //SHASX
          $10+4: ; //SHSUB8
          $10+5: ; //SHSUB16
          $10+6: ; //SHSAX
        end;


      end;
      1:
      begin
        //Parallel addition and subtraction, unsigned
        op1:=(opcode shr 4) and 7;
        op2:=(opcode shr 4) and 3;

        x:=(op2 shl 3) or op1;

        case x of
          0: ; //UADD8
          1: ; //UADD16
          2: ; //UASX
          4: ; //USUB8
          5: ; //USUB16
          6: ; //USAX

          $8+0: ; //UQADD8
          $8+1: ; //UQADD16
          $8+2: ; //UQASX
          $8+4: ; //UQSUB8
          $8+5: ; //UQSUB16
          $8+6: ; //UQSAX

          $10+0: ; //UHADD8
          $10+1: ; //UHADD16
          $10+2: ; //UHASX
          $10+4: ; //UHSUB8
          $10+5: ; //UHSUB16
          $10+6: ; //UHSAX
        end;
      end;
      2:
      begin
        //miscelanious operations
        op1:=(opcode shr 4) and 7;
        op2:=(opcode shr 4) and 3;

        x:=(op1 shl 2) or op2;

        case x of
          0: ; //QADD
          1: ; //QDADD
          2: ; //QSUB
          3: ; //QDSUB

          $4+0: ; //REV
          $4+1: ; //REV16
          $4+2: ; //RBIT
          $4+3: ; //REVSH


          $8: ; //SEL
          $c: ; //CLZ
          $10..$13: ; //CRC32
          $14..$17: ; //CRC32C
        end;
      end;
    end;
  end
  else
  begin
    if op2 shr 3=1 then
    begin
      case op1 of
        0: ;//if rn=15 then SXTH else SXTAH ;
        1: ;//if rn=15 then UXTH else UXTAH;
        2: ;//if rn=15 then SXTB16 else SXTAB16;
        3: ;//if rn=15 then UXTB16 else UXTAB16
        4: ;//if rn=15 then SXTB else SXTAB16
        5: ;//if rn=15 then UXTB else UXTAB;
      end;
    end;
  end;

end;

procedure TThumbDisassembler.T32_DataProcessing_Shifted_Reg;
var op, s, rn, rd: byte;
  rds: byte;
begin
  op:=(opcode shr 5) and $f;
  s:=(opcode shr 4) and 1;
  rn:=opcode and $f;

  rd:=(opcode2 shr 8) and $f;
  rds:=(rd shl 1) or s;

  case op of
    0:
    begin
//      if rds=31 then and_r else tst_r;
    end;

    1:
    begin
      //BIC_r;

    end;

    2:
    begin
//      if rn=15 then mov_r else orr_r
    end;

    3:
    begin
//      if rn=15 then mvn_r else orn_r
    end;

    4:
    begin
     // if rds=31 then TEQ_r else EOR_R
    end;

    6:
    begin
      // PKHBT

    end;

    8:
    begin
      //if rds=31 then CMN_R else ADD_R

    end;

    10:
    begin
      //ADC_R

    end;

    11:
    begin
     // SBC_R
    end;

    13:
    begin
//      if RDS =31 then CMP_R else SUB_R

    end;

    14:
    begin
//      RSB_R;
    end;

  end;

end;

procedure TThumbDisassembler.T32_StoreSingleDataItem;
var op1, op2: byte;
begin
  op1:=(opcode shr 5) and 7;
  op2:=(opcode shr 6) and $3f;

  case op1 of
    4: ; //STRB_I
    5: ; //STRH_I
    6: ; //STR_I
    else
    begin
      if (op2=0) then
      begin
        case op1 of
          0: ; //STRB_R;
          1: ; //STRH_R
          2: ; //STR_R;
        end;
      end
      else
      if (op2 and $24=$24) or ((op2 shr 2)=12) then
      begin
        case op1 of
          0: ; //STRB_I;
          1: ; //STRH_I;
          2: ; //STR_I;
        end;
      end
      else
      if (op2 shr 2)=13 then
      begin
        case op1 of
          0: ; //STRBT;
          1: ; //STRHT;
          2: ; //STRT;
        end;
      end;
    end;
  end;


end;

procedure TThumbDisassembler.T32_LoadByte_MemoryHints;
var op1, rn, rt, op2: byte;
begin
  op1:=(opcode shr 7) and 3;
  rn:=opcode and $f;
  rt:=(opcode2 shr 12) and $f;
  op2:=(opcode shr 6) and $3f;

  if (rn=15) then
    begin
      if (op1 shl 1)=1 then
      begin
        if rt<>15 then
        begin
          //LDRSB_LIT
        end
        else
        begin
          //PLI_R
        end;
      end
      else
      begin
        if rt<>15 then
        begin
          //LDRB_IMM
        end
        else
        begin
          //PLD_LIT
        end;
      end;
    end
    else //rn<>15
    case op1 of
      0:
      begin
        if (op2 and $24=$24) or ((op2 shr 2 = 12) and (rt<>15) ) then
        begin
          //LDRB_IMM;
        end
        else
        begin
          if op2=0 then
          begin
            if rt=15 then
            begin
              //PLD
            end
            else
            begin
              //LDRB_R
            end;
          end
          else
          begin
            //op2<>0
            case op2 shr 2 of
              12:
              begin
                if rt=15 then
                begin
                  //PLD
                end;
              end;

              14:
              begin
                //LDRBT
              end;
            end;
          end;
        end;
      end;

      1:
      begin
        if rt=15 then
        begin
          //PLI
        end
        else
        begin
          //LDRB_IMM
        end;
      end;

      2:
      begin
        if (op2 and $24=$24) or ((op2 shr 2 = 12) and (rt<>15) ) then
        begin
          //LDRSB_IMM;
        end
        else
        begin
          if op2=0 then
          begin
            if rt=15 then
            begin
              //PLI
            end
            else
            begin
              //LDRSB_R
            end;
          end
          else
          begin
            //op2<>0
            case op2 shr 2 of
              12:
              begin
                if rt=15 then
                begin
                  //PLI
                end;
              end;

              14:
              begin
                //LDRSBT
              end;
            end;
          end;
        end;
      end;

      3:
      begin
        if (rt=15) then
        begin
          //PLI
        end
        else
        begin
          //LDRSB
        end;
      end;
    end;
end;

procedure TThumbDisassembler.T32_LoadHalfWord_MemoryHints;
var op1, rn, rt, op2: byte;
begin
  op1:=(opcode shr 7) and 3;
  rn:=opcode and $f;
  rt:=(opcode2 shr 12) and $f;
  op2:=(opcode shr 6) and $3f;

  if (rn=15) then
  begin
    if (op1 shl 1)=1 then
    begin
      if rt<>15 then
      begin
        //LDRSH_LIT
      end
      else
      begin
        //NOP
      end;
    end
    else
    begin
      if rt<>15 then
      begin
        //LDRH_LIT
      end
      else
      begin
        //PLD_LIT
      end;
    end;
  end
  else //rn<>15
  case op1 of
    0:
    begin
      if (op2 and $24=$24) or ((op2 shr 2 = 12) and (rt<>15) ) then
      begin
        //LDRH_IMM;
      end
      else
      begin
        if op2=0 then
        begin
          if rt=15 then
          begin
            //PLD
          end
          else
          begin
            //LDRH_R
          end;
        end
        else
        begin
          //op2<>0
          case op2 shr 2 of
            12:
            begin
              if rt=15 then
              begin
                //PLD
              end;
            end;

            14:
            begin
              //LDRHT
            end;
          end;
        end;
      end;
    end;

    1:
    begin
      if rt=15 then
      begin
        //PLD_R
      end
      else
      begin
        //LDRH_IMM
      end;
    end;

    2:
    begin
      if (op2 and $24=$24) or ((op2 shr 2 = 12) and (rt<>15) ) then
      begin
        //LDRSH_IMM;
      end
      else
      begin
        if op2=0 then
        begin
          if rt=15 then
          begin
            //NOP
          end
          else
          begin
            //LDRSH_R
          end;
        end
        else
        begin
          //op2<>0
          case op2 shr 2 of
            12:
            begin
              if rt=15 then
              begin
                //NOP
              end;
            end;

            14:
            begin
              //LDRSHT
            end;
          end;
        end;
      end;
    end;

    3:
    begin
      if (rt=15) then ; //NOP
    end;
  end;
end;

procedure TThumbDisassembler.T32_LoadWord;
var op1, Rn, op2: byte;
begin
  op1:=(opcode shr 7) and 3;
  Rn:=opcode and $f;
  op2:=(opcode shr 6) and $3f;

  if (rn=15) then
  begin
    if (op1<=1) then ; //LDR_Literal;
  end
  else
  begin
    if (op1=1) then
    begin
      //LDR_Immediatye
    end
    else
    begin
      if (op2=0) then
      begin
        // LDR_R;
      end
      else
      begin
        if ((op2 shr 2)=12) or ((op2 and $24)=$24 ) then
        begin
          //LDR_IMM
        end;
      end;

    end;
  end;

end;

procedure TThumbDisassembler.T32_STR_LDR_STL;
var
  op1, op2, Rn, op3: byte;
begin
  op1:=(opcode shr 7) and 3;
  op2:=(opcode shr 4) and 3;
  Rn:=opcode and $f;
  op3:=(opcode2 shr 4) and $f;

  case (op1 shl 2) or op2 of
    0: ; //STREX
    1: ; //LDREX
    2,6,8,10,12,14: ; //STRD_I_
    3, 7,9,11,13,15: ;// if rn=15 then LDRD_Literal else LDRD_Immediate;
    4:
    begin
      case op3 of
        4: ; //STREB
        5: ; //STREXH
        7: ; //STREXD
        8: ; //STLB
        9: ; //STLH
        10: ; //STL
        12: ; //STLEXB
        13: ; //STLEXH
        14: ; //STLEX
        15: ; //STLEXD
      end;
    end;

    5:
    begin
      case op3 of
        0: ; //TBB_TBH
        1: ; //TBH
        4: ; //LDREXB
        5: ; //LDREXH
        7: ; //LDREXD
        8: ; //LDAB
        9: ; //LDAH
        10: ; //LDA
        12: ; //LDAEXB
        13: ; //LDAEXH
        14: ; //LDAEX
        15: ; //LDAEXD
      end;
    end;



  end;

end;

procedure TThumbDisassembler.T32_LoadStore_Multiple;
var
  op, W,L,Rn: byte;

  WRn: byte;
begin
  op:=(opcode shr 7) and 3;
  w:=(opcode shr 5) and 1;
  l:=(opcode shr 4) and 1;
  Rn:=opcode and $f;

  WRn:=(W shl 4) or Rn;

  case op of
    0:
    begin
      case L of
        0: ; //SRS
        1: ; //RFE
      end;
    end;

    1:
    begin
      case L of
        0: ;//STM
        1: ;//LDM ////not W_Rn: $1D
      end;
    end;

    2:
    begin
      case L of
        0: ;//STMDB //not W_Rn: $1D
        1: ;//LDMDB
      end;
    end;

    3:
    begin
      case L of
        0: ;//SRS
        1: ;//RFE
      end;
    end;
  end;

end;

procedure TThumbDisassembler.T32_Branches_And_Miscellaneous_Control;
var op, op1, op2, op3, imm8: byte;
begin
  op:=(opcode shr 4) and $7f;
  op3:=opcode and $f;
  op1:=(opcode2 shr 12) and 7;
  op2:=(opcode2 shr 8) and $f;
  imm8:=opcode2 and $ff;

  case op1 of
    0,2:
    begin
      if (((op shr 3) and 7)<>7) then
      begin
        B_T3;
      end
      else
      begin
        case op of
          $38,$39:
          begin
            if (imm8 shr 5) and 1=1 then
            begin
              MSR_BR_T1;
            end
            else
            begin
              MSR_R_T1;
            end;

          end;

          $3a:
          begin
            //Change PE_State_and_Hints
            if ((opcode2 shl 8) and 7)<>0 then
            begin
              CPS_T2;
            end
            else
            begin
              case (opcode2 and $ff) of
                0: LastDisassembleData.opcode:='NOP';
                1: LastDisassembleData.opcode:='YIELD';
                2: LastDisassembleData.opcode:='WFE';
                3: LastDisassembleData.opcode:='WFI';
                4: LastDisassembleData.opcode:='SEV';
                5: LastDisassembleData.opcode:='SEVL';
                $f0..$ff: DBG_T1;
              end;
            end;

          end;
          $3b:
          begin
            //Misc_Control_instructions
            case ((opcode2 shr 4) and $f) of
              2: LastDisassembleData.opcode:='CLREX';
              4:
              begin
                LastDisassembleData.opcode:='DSB';
                LastDisassembleData.parameters:=inttohex(opcode2 and $f,1);
              end;

              5:
              begin
                LastDisassembleData.opcode:='DMB';
                LastDisassembleData.parameters:=inttohex(opcode2 and $f,1);
              end;

              6:
              begin
                LastDisassembleData.opcode:='ISB';
                LastDisassembleData.parameters:=inttohex(opcode2 and $f,1);
              end;
            end;
          end;
          $3c: BXJ_T1; //BXJ
          $3d:
          begin
            if imm8=0 then
            begin
              LastDisassembleData.opcode:='ERET';
            end
            else
            begin
              SUB_I_T5;
            end;
          end;

          $3e, $3f:
          begin
            if (imm8 shr 5) and 1=1 then
            begin
              MRS_BR_T1;
            end
            else
            begin
              MRS_R_T1;
            end;
          end;

          $78:
          begin
            if (op1=0) and (op2=0) and (op3=15) and ((imm8 shr 2)=0) then
              LastDisassembleData.opcode:='DCPS'+inttostr(imm8 and 3);
          end;

          $7e:
          begin
            if (op1=0) and ((imm8 shr 2)=0) then
            begin
              LastDisassembleData.opcode:='HVC';
              LastDisassembleData.parameters:=inttohex(((opcode and $f) shl 12) or (opcode2 and $fff),1);
            end;
          end;

          $7f:
          begin
            if (op1=0) and ((imm8 shr 2)=0) then
            begin
              LastDisassembleData.opcode:='SMC';
              LastDisassembleData.parameters:=inttohex(opcode and $f,1);
            end;

            if (op1=2) then
            begin
              LastDisassembleData.opcode:='UDF';
              LastDisassembleData.parameters:=inttohex(((opcode and $f) shl 12) or (opcode2 and $fff),1);
            end;
          end;
        end;
      end;
    end;

    1,3:
    begin
      //B
      B_T4;
    end;

    4,6: BL_X_T1;
    5,7: BL_X_T2;
  end;

end;

procedure TThumbDisassembler.T32_DataProcessing_Plain_Binary_Immediate;
var
  op, Rn: byte;
begin
  op:=(opcode shr 4) and $1f;
  Rn:=opcode and $f;

  case op of
    0: if rn<>15 then ADD_I_T4 else ADR_T3;
    4: MOV_I_T3;
    10: if rn<>15 then SUB_I_T4 else ADR_T2;
    14: MOVT_T1;
    16: SSAT_T1;
    18: if (((opcode2 shr 6) and 3)=0) and (((opcode2 shr 12) and 7)=0) then SSAT16_T1 else SSAT_T1;
    20: SBFX_T1;
    22: if rn<>15 then BFI_T1 else BFC_T1;
    24: USAT_T1;
    26: if (((opcode2 shr 6) and 3)=0) and (((opcode2 shr 12) and 7)=0) then USAT16_T1 else USAT_T1;
    28: UBFX_T1;

  end;

end;

procedure TThumbDisassembler.T32_DataProcessing_Modified_Immediate;
var
  op: byte;
  S: byte;
  Rn: byte;
  Rd: byte;
  a1: boolean;

  i: Byte;
  imm3: byte;
  v: byte;

  imm12: word;

  imm32: dword;
begin
  op:=(opcode shr 5) and $f;
  s:=(opcode shr 4) and 1;
  Rn:=opcode and $f;
  Rd:=(opcode2 shr 8) and $f;

  i:=(opcode shr 10) and 1;
  imm3:=(opcode2 shr 12) and 7;
  v:=opcode2 and $ff;

  a1:=(S=1) and (Rd=$F);

  imm12:=(i shl 12) or (imm3 shl 8) or v;

  if (imm12 shr 10)=0 then
  begin
    case (imm12 shr 8) and 3 of
      0: imm32:=v;
      1: imm32:=(v shl 16) or v;
      2: imm32:=(v shl 24) or (v shl 8);
      3: imm32:=(v shl 24) or (v shl 16) or (v shl 8) or v;
    end;
  end
  else
  begin
    //rotate (explained on page 2530/2531)
    imm32:=(1 shl 7) or (imm12 and $3f); //no, not a mistake, the 8th bit gets replaced by a 1
    imm32:=RorDWord(imm32, imm12 shr 7);
  end;

  case op of
    0: if a1 then
       begin
         LastDisassembleData.opcode:='TST';
         LastDisassembleData.parameters:=ArmRegisters[Rn]+','+inttohex(imm32,1);
       end
       else
       begin
         if (s=1) and (rd<>15) then
           LastDisassembleData.opcode:='ANDS'
         else
           LastDisassembleData.opcode:='AND';

         LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[Rn]+','+inttohex(imm32,1);
       end;

    1:
    begin
      if (s=1) and (rd<>15) then
        LastDisassembleData.opcode:='BIC'
      else
        LastDisassembleData.opcode:='BICS';

      LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[Rn]+','+inttohex(imm32,1);
    end;

    2: if rn=15 then
       begin
         if (s=1) and (rd<>15) then
           LastDisassembleData.opcode:='MOV'
         else
           LastDisassembleData.opcode:='MOVS';

         LastDisassembleData.parameters:=ArmRegisters[rd]+','+inttohex(imm32,1);
       end
       else
       begin
         if (s=1) and (rd<>15) then
           LastDisassembleData.opcode:='ORR'
         else
           LastDisassembleData.opcode:='ORRS';

         LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[Rn]+','+inttohex(imm32,1);
       end;
    3: if rn=15 then
       begin
         if (s=1) then
           LastDisassembleData.opcode:='MVN'
         else
           LastDisassembleData.opcode:='MVNS';

         LastDisassembleData.parameters:=ArmRegisters[rd]+','+inttohex(imm32,1);
       end
       else
       begin
         if (s=1) then
           LastDisassembleData.opcode:='ORN'
         else
           LastDisassembleData.opcode:='ORNS';

         LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[Rn]+','+inttohex(imm32,1);
       end;
    4: if a1 then
       begin
         LastDisassembleData.opcode:='TEQ';
         LastDisassembleData.parameters:=ArmRegisters[Rn]+','+inttohex(imm32,1);
       end
       else
       begin
         if (s=1) and (rd<>15) then
           LastDisassembleData.opcode:='EOR'
         else
           LastDisassembleData.opcode:='EORS';

         LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[Rn]+','+inttohex(imm32,1);
       end;
    8: if a1 then
       begin
         LastDisassembleData.opcode:='CMN';
         LastDisassembleData.parameters:=ArmRegisters[Rn]+','+inttohex(imm32,1);
       end
       else
       begin
         if (s=1) and (rd<>15) then
           LastDisassembleData.opcode:='ADDS'
         else
           LastDisassembleData.opcode:='ADD';

         LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[Rn]+','+inttohex(imm32,1);
       end;
    10:
    begin
      if (s=1) then
        LastDisassembleData.opcode:='ADCS'
      else
        LastDisassembleData.opcode:='ADC';

      LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[Rn]+','+inttohex(imm32,1);
    end;
    11:
    begin
      if (s=1) then
        LastDisassembleData.opcode:='SBC'
      else
        LastDisassembleData.opcode:='SBCS';

      LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[Rn]+','+inttohex(imm32,1);
    end;

    13: if a1 then
        begin
          LastDisassembleData.opcode:='CMP';
          LastDisassembleData.parameters:=ArmRegisters[Rn]+','+inttohex(imm32,1);
        end
        else
        begin
          if (s=1) and (rd<>15) then
            LastDisassembleData.opcode:='SUBS'
          else
            LastDisassembleData.opcode:='SUB';

          LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[Rn]+','+inttohex(imm32,1);
        end;
    14:
    begin
      if (s=1) then
        LastDisassembleData.opcode:='RSB'
      else
        LastDisassembleData.opcode:='RSBS';

      LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[Rn]+','+inttohex(imm32,1);
    end;
  end;


end;

procedure TThumbDisassembler.t32;
var x: ptruint;
  op1, op2, op: byte;
begin
  //read opcode2
  {$ifdef armdev}
  opcode2:=pword(a+2)^;
  setlength(LastDisassembleData.Bytes,4);
  pword(@LastDisassembleData.Bytes[2])^:=opcode2;
  {$else}
  if readprocessmemory(processhandle, pointer(ptruint(a)-1+2), @opcode2, sizeof(opcode2), x) then
  begin
    setlength(LastDisassembleData.Bytes,4);
    puint16(@LastDisassembleData.Bytes[2])^:=opcode2;
  end;
  {$endif}

  op1:=(opcode shr 11) and 3;
  op2:=(opcode shr 4) and $7f;
  op:=(opcode2 shr 15) and 1;

  case op1 of
    1:
    begin
      if (op2 shr 5)=0 then
      begin
        if (op2 shr 2) and 1 = 0 then
        begin
          //LoadStore multiple
          T32_LoadStore_Multiple;
        end
        else
        begin
          //LoadStore-Exclusive, load acquire/store release table branch
          T32_STR_LDR_STL;
        end;
      end
      else
      if (op2 shr 5)=1 then
      begin
        //Data-processing (shifted reg)
        T32_DataProcessing_Shifted_Reg;
      end
      else
      begin
        //CoProcessor, Advanced SIMD, floating point instr...

        //todo: perhaps in the future
      end;
    end;

    2:
    begin
      if op=0 then
      begin
        if (op2 shr 5) and 1=0 then
        begin
          //Data processing (modified immediate)
          T32_DataProcessing_Modified_Immediate;

        end
        else
        begin
          //Data processing (plain binary immediate)
          T32_DataProcessing_Plain_Binary_Immediate;

        end;

      end
      else
      begin
        //Branches and miscellaneous
        T32_Branches_And_Miscellaneous_Control;

      end;

    end;

    3:
    begin
      if (op2 shr 4=0) and ((op2 and 1)=0) then
      begin
        //Store single data Item
        T32_StoreSingleDataItem;

      end
      else
      if (op2 shr 5=0) then //00xxxxx
      begin
        case (op2 and 7) of
          1: T32_LoadByte_MemoryHints; //LoadByte, memory hints
          3: T32_LoadHalfWord_MemoryHints;//Load halfword, memory hints
          5: T32_LoadWord;//Load word
          7: ;//undefined
          else
          begin
            if ((op2 shr 4) and 1=1) and ((op2 and 1)=0) then
            begin
              //Advanced SIMD element or structure load/store
            end;

          end;
        end;
      end
      else
      if (op2 shr 4)=2 then
      begin
        //data processing (register)
        T32_DataProcessing_Reg;
      end
      else
      if (op2 shr 3)=6 then
      begin
        //Multiply, multiple accumulate, and absolute difference
        T32_Multiply_MultiplyAccumulate_AbsoluteDiff;
      end
      else
      if (op2 shr 3)=7 then
      begin
        //Long multiple, long multiple accumulate, and divide
        T32_LongMultiply_LongMultiplyAccumulate_Divide;
      end
      else
      if (op2 shr 6)=1 then
      begin
        //CoProcessor, Advanced SIMD, and floating point instructions
      end;


    end;
  end;

  //todo: implement me
end;


procedure TThumbDisassembler.STM_T1;
var
  i: integer;
  rn: byte;
  reglist: word;
  rcount: integer;
  m: byte;
  _rlist: string;
begin
  reglist:=opcode and $ff;
  rn:=(opcode shr 8) and 7;

  _rlist:='{';
  rcount:=0;

  for i:=0 to 7 do
  begin
    if (reglist shr i) and 1=1 then
    begin
      if rcount=0 then
        _rlist:=_rlist+ArmRegisters[i]
      else
        _rlist:=_rlist+', '+ArmRegisters[i];

      inc(rcount);
    end;
  end;

  _rlist:=_rlist+'}';

  LastDisassembleData.opcode:='STM';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+_rlist;
end;

procedure TThumbDisassembler.LDM_T1;
var
  i: integer;
  rn: byte;
  reglist: word;
  rcount: integer;
  m: byte;
  _rlist: string;
begin
  reglist:=opcode and $ff;
  rn:=(opcode shr 8) and 7;

  _rlist:='{';
  rcount:=0;

  for i:=0 to 7 do
  begin
    if (reglist shr i) and 1=1 then
    begin
      if rcount=0 then
        _rlist:=_rlist+ArmRegisters[i]
      else
        _rlist:=_rlist+', '+ArmRegisters[i];

      inc(rcount);
    end;
  end;

  _rlist:=_rlist+'}';

  LastDisassembleData.opcode:='LDM';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+_rlist;
end;

procedure TThumbDisassembler.REVSH_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='REVSH';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;


procedure TThumbDisassembler.IT_T1;
var
  firstcond: byte;
  mask: byte;
  c: string;
  i: integer;

  m: string;
begin
  firstcond:=(opcode shr 4) and $f;
  mask:=opcode and $f;
  c:=ArmConditions[firstcond];

  case mask of
    1: m:='TTT';
    2: m:='TT';
    3: m:='TTE';
    4: m:='T';
    5: m:='TET';
    6: m:='TE';
    7: m:='TEE';
    8: m:='';
    9: m:='ETT';
    10: m:='ET';
    11: m:='ETE';
    12: m:='E';
    13: m:='EET';
    14: m:='EE';
    15: m:='EEE';
  end;

  LastDisassembleData.opcode:='IT'+m;
  LastDisassembleData.parameters:=c;
end;

procedure TThumbDisassembler.UDF_T1;
var imm8: byte;
begin
  imm8:=opcode and $ff;
  LastDisassembleData.opcode:='UDF';
  LastDisassembleData.parameters:=inttohex(imm8,1);
end;

procedure TThumbDisassembler.SVC_T1;
var imm8: byte;
begin
  imm8:=opcode and $ff;
  LastDisassembleData.opcode:='SVC';
  LastDisassembleData.parameters:=inttohex(imm8,1);
end;

procedure TThumbDisassembler.DBG_T1;
var opt: byte;
begin
  opt:=opcode and $f;
  LastDisassembleData.opcode:='DBG';
  LastDisassembleData.parameters:=inttohex(opt,1);
end;

procedure TThumbDisassembler.BKPT_T1;
var imm8: byte;
begin
  imm8:=opcode and $ff;
  LastDisassembleData.opcode:='BKPT';
  LastDisassembleData.parameters:=inttohex(imm8,1);
end;

procedure TThumbDisassembler.HLT_T1;
var imm6: byte;
begin
  imm6:=opcode and $3f;
  LastDisassembleData.opcode:='HLT';
  LastDisassembleData.parameters:=inttohex(imm6,1);
end;

procedure TThumbDisassembler.REV_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='REV';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;

procedure TThumbDisassembler.REV16_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='REV16';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;

procedure TThumbDisassembler.CPS_T1;
var _F,_I,_A, IM: byte;
  p: string;
begin
  _f:=opcode and 1;
  _i:=(opcode shr 1) and 1;
  _a:=(opcode shr 2) and 1;
  im:=(opcode shr 4) and 1;

  if im=1 then
    LastDisassembleData.opcode:='CPSID'
  else
    LastDisassembleData.opcode:='CPSIE';

  p:='';
  if _a<>0 then
    p:='a';

  if _i<>0 then
    p:=p+'i';

  if _f<>0 then
    p:=p+'f';

  LastDisassembleData.parameters:=p;
end;

procedure TThumbDisassembler.CPS_T2;
var _F,_I,_A, IM,M, mode: byte;
  p: string;
begin
  im:=(opcode2 shr 9) and 3;
  m:=(opcode2 shr 8) and 1;
  _a:=(opcode shr 7) and 1;
  _i:=(opcode shr 6) and 1;
  _f:=(opcode2 shr 5) and 1;
  mode:=opcode2 and $f;

  p:='';
  if _a<>0 then
    p:='a';

  if _i<>0 then
    p:=p+'i';

  if _f<>0 then
    p:=p+'f';

  case (im shl 1) or m of
    1:
    begin
      LastDisassembleData.opcode:='CPS';
      LastDisassembleData.parameters:=inttohex(mode,1);
    end;

    4:
    begin
      LastDisassembleData.opcode:='CPSIE';
      LastDisassembleData.parameters:=p;
    end;

    5:
    begin
      LastDisassembleData.opcode:='CPSIE';
      LastDisassembleData.parameters:=p+','+inttohex(mode,1);
    end;

    6:
    begin
      LastDisassembleData.opcode:='CPSID';
      LastDisassembleData.parameters:=p;
    end;

    7:
    begin
      LastDisassembleData.opcode:='CPSID';
      LastDisassembleData.parameters:=p+','+inttohex(mode,1);
    end;

  end;



end;



procedure TThumbDisassembler.SETEND_T1;
begin
  LastDisassembleData.opcode:='SETEND';

  if (opcode shr 3) and 1 =0 then
    LastDisassembleData.parameters:='LE'
  else
    LastDisassembleData.parameters:='BE';
end;

procedure TThumbDisassembler.PUSH_T1;
var
  i: integer;
  reglist: word;
  rcount: integer;
  m: byte;
  _rlist: string;
begin
  reglist:=opcode and $ff;
  m:=(opcode shr 8) and 1;


  if m=1 then
  begin
    _rlist:='{'+ArmRegisters[14];
    rcount:=1;
  end
  else
  begin
    _rlist:='{';
    rcount:=0;
  end;

  for i:=0 to 7 do
  begin
    if (reglist shr i) and 1=1 then
    begin
      if rcount=0 then
        _rlist:=_rlist+ArmRegisters[i]
      else
        _rlist:=_rlist+', '+ArmRegisters[i];

      inc(rcount);
    end;
  end;

  _rlist:=_rlist+'}';

  LastDisassembleData.opcode:='PUSH';
  LastDisassembleData.parameters:=_rlist;
end;

procedure TThumbDisassembler.POP_T1;
var
  i: integer;
  reglist: word;
  rcount: integer;
  p: byte;
  _rlist: string;
begin
  reglist:=opcode and $ff;
  p:=(opcode shr 8) and 1;

  if p=1 then
  begin
    _rlist:='{'+ArmRegisters[15];
    rcount:=1;
  end
  else
  begin
    _rlist:='{';
    rcount:=0;
  end;

  for i:=0 to 7 do
  begin
    if (reglist shr i) and 1=1 then
    begin
      if rcount=0 then
        _rlist:=_rlist+ArmRegisters[i]
      else
        _rlist:=_rlist+', '+ArmRegisters[i];

      inc(rcount);
    end;
  end;

  _rlist:=_rlist+'}';

  LastDisassembleData.opcode:='PUSH';
  LastDisassembleData.parameters:=_rlist;
end;

procedure TThumbDisassembler.SXTH_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='SXTH';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;

procedure TThumbDisassembler.SXTB_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='SXTB';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;

procedure TThumbDisassembler.UXTH_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='UXTH';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;

procedure TThumbDisassembler.UXTB_T1;
var rd, rs: byte;
begin
  rd:=opcode and 7;
  rs:=(opcode shr 3) and 7;

  LastDisassembleData.opcode:='UXTB';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rs];
end;


procedure TThumbDisassembler.CBZ_CBNZ;
var rs, imm5, i, op: byte;
  imm32: dword;
begin
  rs:=opcode and $7;
  imm5:=(opcode shr 3) and $1f;
  i:=(opcode shr 9) and 1;
  op:=(opcode shr 11) and 1;
  if (op=0) then
    LastDisassembleData.opcode:='CBZ'
  else
    LastDisassembleData.opcode:='CBNZ';

  imm32:=((i shl 6) or imm5) * 2;

  LastDisassembleData.parameters:=ArmRegisters[rs]+','+inttohex(a+2+imm32,1);
end;

procedure TThumbDisassembler.ADD_SP_I_T1;
var rd, imm8: byte;
begin
  rd:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='ADD';
  LastDisassembleData.parameters:=ArmRegisters[rd]+',SP,'+inttohex(imm8*4,1);
end;

procedure TThumbDisassembler.ADD_SP_I_T2;
var imm7: byte;
begin
  imm7:=opcode and $7f;

  LastDisassembleData.opcode:='ADD';
  LastDisassembleData.parameters:='SP,'+inttohex(imm7*4,1);
end;

procedure TThumbDisassembler.SUB_SP_I_T1;
var imm7: byte;
begin
  imm7:=opcode and $7f;

  LastDisassembleData.opcode:='SUB';
  LastDisassembleData.parameters:='SP,'+inttohex(imm7*4,1);
end;

procedure TThumbDisassembler.ADR_T1;
var rd, imm8: byte;
begin
  rd:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='ADR';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+inttohex(Align(a+2,4)+imm8*4,1);
end;

procedure TThumbDisassembler.STR_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='STR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;

procedure TThumbDisassembler.LDR_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='LDR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;


procedure TThumbDisassembler.STR_I_T2;
var rt, imm8: byte;
  _imm: string;
begin
  rt:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  if imm8<>0 then
    _imm:=','+inttohex(imm8*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='STR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',[SP'+_imm+']';
end;

procedure TThumbDisassembler.LDR_I_T2;
var rt, imm8: byte;
  _imm: string;
begin
  rt:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  if imm8<>0 then
    _imm:=','+inttohex(imm8*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='LDR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',[SP'+_imm+']';
end;


procedure TThumbDisassembler.STRB_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='STRB';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;

procedure TThumbDisassembler.LDRB_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='LDRB';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;

procedure TThumbDisassembler.STRH_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='STRH';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;

procedure TThumbDisassembler.LDRH_I_T1;
var rt, rb, imm5: byte;
  _imm: string;
begin
  rt:=opcode and 7;
  rb:=(opcode shr 3) and 7;
  imm5:=(opcode shr 6) and $1f;

  if imm5<>0 then
    _imm:=','+inttohex(imm5*4,1)
  else
    _imm:='';

  LastDisassembleData.opcode:='LDRH';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rb]+_imm+']';
end;

procedure TThumbDisassembler.STR_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='STR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;


procedure TThumbDisassembler.STRH_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='STRH';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.STRB_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='STRB';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.LDRSB_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='LDRSB';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.LDR_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='LDR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.LDRH_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='LDRH';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.LDRB_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='LDRB';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;

procedure TThumbDisassembler.LDRSH_R_T1;
var rt, rn, rm: byte;
begin
  rt:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='LDRSH';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+ArmRegisters[rn]+','+ArmRegisters[rm]+']';
end;




procedure TThumbDisassembler.LDR_L_T1;
var rt, imm8: byte;
begin
  rt:=(opcode shr 8) and $7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='LDR';
  LastDisassembleData.parameters:=ArmRegisters[rt]+',['+inttohex(align(a+2,4)+imm8*4,1)+']';
end;

procedure TThumbDisassembler.B_T1;
var cond, imm8: byte;
begin
  cond:=(opcode shr 8) and $7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='B'+ArmConditions[cond];
  LastDisassembleData.parameters:=inttohex(a+2+imm8*2,1);
end;

procedure TThumbDisassembler.B_T2;
var imm11: word;
begin
  imm11:=opcode and $7ff;
  LastDisassembleData.opcode:='B';
  LastDisassembleData.parameters:=inttohex(a+2+imm11*2,1);
end;

procedure TThumbDisassembler.B_T3;
var
  s, cond, imm6, j1, j2: byte;
  imm11: word;
  imm32: ptruint;
begin
  s:=(opcode shr 10) and 1;
  cond:=(opcode shr 6) and $f;
  imm6:=opcode and $7f;
  j1:=(opcode2 shr 13) and 1;
  j2:=(opcode2 shr 11) and 1;
  imm11:=opcode2 and $7ff;

  //S:J2:J1:imm6:imm11:'0'
  imm32:=(j2 shl 18) or (j1 shl 17) or (imm6 shl 11) or imm11;
  imm32:=imm32 shl 1;
  if s=1 then imm32:=-imm32;

  LastDisassembleData.opcode:='B'+ArmConditions[cond];
  LastDisassembleData.parameters:=inttohex(a+4+imm32,8);
end;

procedure TThumbDisassembler.B_T4;
var
  s, j1, j2: byte;
  imm10, imm11: word;
  imm32: ptruint;

  i1, i2: byte;
begin
  s:=(opcode shr 10) and 1;
  imm10:=opcode and $3ff;

  j1:=(opcode2 shr 13) and 1;
  j2:=(opcode2 shr 11) and 1;
  imm11:=opcode2 and $7ff;



  i1:=not (j1 xor s);
  i2:=not (j2 xor s);

  //S:J2:J1:imm10:imm11:'0'
  imm32:=(i1 shl 23) or (i2 shl 22) or (imm10 shl 11) or imm11;
  imm32:=imm32 shl 1;
  if s=1 then imm32:=-imm32;

  LastDisassembleData.opcode:='B';
  LastDisassembleData.parameters:=inttohex(a+4+imm32,8);
end;

procedure TThumbDisassembler.BL_X_T1;
var
  s, j1, j2: byte;
  imm10: word;
  imm11: word;

  i1, i2: byte;
  imm32: ptruint;
begin
  s:=(opcode shr 10) and 1;
  imm10:=opcode and $3ff;

  j1:=(opcode2 shr 13) and 1;
  j2:=(opcode2 shr 11) and 1;
  imm11:=opcode2 and $7ff;

  i1:=not (j1 xor s);
  i2:=not (j2 xor s);

    //S:J2:J1:imm10:imm11:'0'
  imm32:=(i1 shl 23) or (i2 shl 22) or (imm10 shl 11) or imm11;
  imm32:=imm32 shl 1;
  if s=1 then imm32:=-imm32;

  LastDisassembleData.opcode:='BL';
  LastDisassembleData.parameters:=inttohex(a+4+imm32,8);

end;

procedure TThumbDisassembler.BL_X_T2;
var
  s, j1, j2: byte;
  imm10h: word;
  imm10l: word;
  i1, i2: byte;
  imm32: ptruint;
begin
  s:=(opcode shr 10) and 1;
  imm10h:=opcode and $3ff;

  j1:=(opcode2 shr 13) and 1;
  j2:=(opcode2 shr 11) and 1;
  imm10l:=(opcode2 shr 1) and $3ff;

  i1:=not (j1 xor s);
  i2:=not (j2 xor s);

    //S:i1:i2:imm10h:imm10l:'00'
  imm32:=(i1 shl 22) or (i2 shl 21) or (imm10h shl 10) or imm10l;
  imm32:=imm32 shl 2;
  if s=1 then imm32:=-imm32;

  LastDisassembleData.opcode:='BLX';
  LastDisassembleData.parameters:=inttohex(a+4+imm32,8);

end;

procedure TThumbDisassembler.BX_T1;
var
  rm: byte;
begin
  rm:=(opcode shr 3) and $f;

  LastDisassembleData.opcode:='BX';
  LastDisassembleData.parameters:=ArmRegisters[rm];
end;

procedure TThumbDisassembler.BLX_R_T1;
var
  rm: byte;
begin
  rm:=(opcode shr 3) and $f;

  LastDisassembleData.opcode:='BLX';
  LastDisassembleData.parameters:=ArmRegisters[rm];
end;

procedure TThumbDisassembler.BXJ_T1;
var rn: byte;
begin
  rn:=opcode and $f;
  LastDisassembleData.opcode:='BXJ';
  LastDisassembleData.parameters:=ArmRegisters[rn];
end;

procedure TThumbDisassembler.ADD_I_T1;
var
  imm3: byte;
  rn: byte;
  rd: byte;
begin
  imm3:=(opcode shr 6) and 7;
  rn:=(opcode shr 3) and 7;
  rd:=opcode and 7;

  LastDisassembleData.opcode:='ADDS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+IntToHex(imm3,1);
end;

procedure TThumbDisassembler.ADD_I_T2;
var
  imm8: byte;
  rdn: byte;
begin
  imm8:=opcode and $ff;
  rdn:=(opcode shr 8) and 7;

  LastDisassembleData.opcode:='ADDS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+IntToHex(imm8,1);
end;

procedure TThumbDisassembler.ADD_I_T4;
var i, rn, imm3, rd, imm8: byte;
  imm12: word;
begin
  i:=(opcode shr 10) and 1;
  rn:=opcode and $f;
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 12) and $7;
  imm8:=opcode2 and $ff;

  imm12:=(i shr 11) or (imm3 shr 8) or (imm8);

  LastDisassembleData.opcode:='ADD';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+IntToHex(imm12,1);
end;

procedure TThumbDisassembler.ADR_T2;
var i, imm3, rd, imm8: byte;
  imm12: word;
begin
  i:=(opcode shr 10) and 1;
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 12) and $7;
  imm8:=opcode2 and $ff;

  imm12:=(i shr 11) or (imm3 shr 8) or (imm8);

  LastDisassembleData.opcode:='ADR';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(imm12,1);
end;

procedure TThumbDisassembler.ADR_T3;
var i, imm3, rd, imm8: byte;
  imm12: word;
begin
  i:=(opcode shr 10) and 1;
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 12) and $7;
  imm8:=opcode2 and $ff;

  imm12:=(i shr 11) or (imm3 shr 8) or (imm8);

  LastDisassembleData.opcode:='ADR';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(imm12,1);
end;


procedure TThumbDisassembler.SUB_I_T1;
var
  imm3: byte;
  rn: byte;
  rd: byte;
begin
  imm3:=(opcode shr 6) and 7;
  rn:=(opcode shr 3) and 7;
  rd:=opcode and 7;

  LastDisassembleData.opcode:='SUBS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+IntToHex(imm3,1);
end;

procedure TThumbDisassembler.SUB_I_T2;
var
  imm8: byte;
  rdn: byte;
begin
  imm8:=opcode and $ff;
  rdn:=(opcode shr 8) and 7;

  LastDisassembleData.opcode:='SUBS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+IntToHex(imm8,1);
end;


procedure TThumbDisassembler.SUB_I_T4;
var i, rn, imm3, rd, imm8: byte;
  imm12: word;
begin
  i:=(opcode shr 10) and 1;
  rn:=opcode and $f;
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 12) and $7;
  imm8:=opcode2 and $ff;

  imm12:=(i shr 11) or (imm3 shr 8) or (imm8);

  LastDisassembleData.opcode:='SUB';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+IntToHex(imm12,1);
end;

procedure TThumbDisassembler.SUB_I_T5;
var imm8: byte;
begin
  imm8:=opcode2 and $ff;
  LastDisassembleData.opcode:='SUBS';
  LastDisassembleData.parameters:='PC,LR,'+ IntToHex(imm8,1);
end;

procedure TThumbDisassembler.SSAT_T1;
var sh, rn, imm3, rd, imm2, sat_imm: byte;
begin
  sh:=(opcode shr 5) and 1;
  rn:=opcode and $f;
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 8) and $f;
  imm2:=(opcode2 shr 6) and $3;
  sat_imm:=opcode2 and $f;

  LastDisassembleData.opcode:='SSAT';
  if (sh=1) and not ((imm3=0) and (imm2=0)) then
    LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(sat_imm+1,1)+','+ArmRegisters[rn]+', ASR '+inttoHex((imm3 shl 2) or imm2,1)
  else
    LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(sat_imm+1,1)+','+ArmRegisters[rn]+', LSL '+inttoHex((imm3 shl 2) or imm2,1);
end;

procedure TThumbDisassembler.SSAT16_T1;
var rn, rd, sat_imm: byte;
begin
  rn:=opcode and $f;
  rd:=(opcode2 shr 8) and $f;
  sat_imm:=opcode2 and $f;

  LastDisassembleData.opcode:='SSAT16';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(sat_imm+1,1)+','+ArmRegisters[rn];
end;

procedure TThumbDisassembler.USAT_T1;
var sh, rn, imm3, rd, imm2, sat_imm: byte;
begin
  sh:=(opcode shr 5) and 1;
  rn:=opcode and $f;
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 8) and $f;
  imm2:=(opcode2 shr 6) and $3;
  sat_imm:=opcode2 and $f;

  LastDisassembleData.opcode:='USAT';
  if (sh=1) and not ((imm3=0) and (imm2=0)) then
    LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(sat_imm,1)+','+ArmRegisters[rn]+', ASR '+inttoHex((imm3 shl 2) or imm2,1)
  else
    LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(sat_imm,1)+','+ArmRegisters[rn]+', LSL '+inttoHex((imm3 shl 2) or imm2,1);
end;

procedure TThumbDisassembler.USAT16_T1;
var rn, rd, sat_imm: byte;
begin
  rn:=opcode and $f;
  rd:=(opcode2 shr 8) and $f;
  sat_imm:=opcode2 and $f;

  LastDisassembleData.opcode:='USAT16';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(sat_imm,1)+','+ArmRegisters[rn];
end;



procedure TThumbDisassembler.SBFX_T1;
var rn, imm3, rd, imm2, widthm1: byte;
begin
  rn:=opcode and $f;
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 8) and $f;
  imm2:=(opcode2 shr 6) and $3;
  widthm1:=opcode2 and $f;

  LastDisassembleData.opcode:='SBFX';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+inttohex((imm3 shl 2) or imm2, 1)+','+inttohex(widthm1+1,1);
end;

procedure TThumbDisassembler.UBFX_T1;
var rn, imm3, rd, imm2, widthm1: byte;
begin
  rn:=opcode and $f;
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 8) and $f;
  imm2:=(opcode2 shr 6) and $3;
  widthm1:=opcode2 and $f;

  LastDisassembleData.opcode:='UBFX';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+inttohex((imm3 shl 2) or imm2, 1)+','+inttohex(widthm1+1,1);
end;


procedure TThumbDisassembler.BFI_T1;
var rn, imm3, rd, imm2, msb: byte;
  lsb: byte;
begin
  rn:=opcode and $f;
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 8) and $f;
  imm2:=(opcode2 shr 6) and $3;
  msb:=opcode2 and $f;

  lsb:=(imm3 shl 2) or imm2;

  LastDisassembleData.opcode:='BFI';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+inttohex(lsb, 1)+','+inttohex(msb+1-lsb,1);
end;

procedure TThumbDisassembler.BFC_T1;
var rn, imm3, rd, imm2, msb: byte;
  lsb: byte;
begin
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 8) and $f;
  imm2:=(opcode2 shr 6) and $3;
  msb:=opcode2 and $f;

  lsb:=(imm3 shl 2) or imm2;

  LastDisassembleData.opcode:='BFI';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+inttohex(lsb, 1)+','+inttohex(msb+1-lsb,1);
end;


procedure TThumbDisassembler.MOV_I_T1;
var imm8, rd: byte;
begin
  rd:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='MOVS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(imm8,1);
end;

procedure TThumbDisassembler.MOV_I_T3;
var i, imm4, imm3, rd, imm8, imm16: byte;
begin
  i:=(opcode shr 10) and 1;
  imm4:=opcode and $f;
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 12) and $7;
  imm8:=opcode2 and $ff;

  imm16:=(imm4 shr 12) or (i shr 11) or (imm3 shr 8) or (imm8);

  LastDisassembleData.opcode:='MOV';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(imm16,1);
end;

procedure TThumbDisassembler.MOVT_T1;
var i, imm4, imm3, rd, imm8, imm16: byte;
begin
  i:=(opcode shr 10) and 1;
  imm4:=opcode and $f;
  imm3:=(opcode2 shr 12) and $7;
  rd:=(opcode2 shr 12) and $7;
  imm8:=opcode2 and $ff;

  imm16:=(imm4 shr 12) or (i shr 11) or (imm3 shr 8) or (imm8);

  LastDisassembleData.opcode:='MOVT';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+IntToHex(imm16,1);
end;

procedure TThumbDisassembler.CMP_I_T1;
var imm8, rn: byte;
begin
  rn:=(opcode shr 8) and 7;
  imm8:=opcode and $ff;

  LastDisassembleData.opcode:='CMP';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+IntToHex(imm8,1);
end;


procedure TThumbDisassembler.ADD_SP_R_T1;
var
  dm: byte;
  rdm: byte;
begin
  dm:=(opcode shr 7) and 1;
  rdm:=opcode and 7;
  rdm:=(dm shl 4) or rdm;


  LastDisassembleData.opcode:='ADD';
  LastDisassembleData.parameters:='SP,'+ArmRegisters[rdm];
end;

procedure TThumbDisassembler.ADD_SP_R_T2;
var
  rm: byte;
begin
  rm:=(opcode shr 3) and $f;

  LastDisassembleData.opcode:='ADD';
  LastDisassembleData.parameters:='SP,'+ArmRegisters[rm];
end;


procedure TThumbDisassembler.ADD_R_T1;
var rd, rn, rm: byte;
begin
  rd:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='ADDS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.ADD_R_T2;
var rdn, rm,dn: byte;
begin
  rdn:=opcode and 7;
  rm:=(opcode shr 3) and $f;
  dn:=(opcode shr 7) and 1;

  rdn:=(dn shl 4) or rdn;

  LastDisassembleData.opcode:='ADD';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.SUB_R_T1;
var rd, rn, rm: byte;
begin
  rd:=opcode and 7;
  rn:=(opcode shr 3) and 7;
  rm:=(opcode shr 6) and 7;

  LastDisassembleData.opcode:='SUBS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+','+ArmRegisters[rm];
end;


procedure TThumbDisassembler.ADC_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='ADCS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.AND_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='ANDS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.EOR_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='EORS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.SBC_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='SBCS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.TST_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='TST';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.RSB_I_T1;
var
  rd,rn: byte;
begin
  rn:=(opcode shr 3) and 7;
  rd:=opcode and 7;
  LastDisassembleData.opcode:='RSBS';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+',0';
end;

procedure TThumbDisassembler.CMP_R_T1;
var
  rn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rn:=opcode and 7;
  LastDisassembleData.opcode:='CMP';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.CMP_R_T2;
var
  rn,rm,N: byte;
begin
  rm:=(opcode shr 3) and $f;
  rn:=opcode and 7;
  n:=(opcode shr 7) and 1;

  rn:=(n shl 4) or rn;

  LastDisassembleData.opcode:='CMP';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+ArmRegisters[rm];
end;


procedure TThumbDisassembler.CMN_R_T1;
var
  rn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rn:=opcode and 7;
  LastDisassembleData.opcode:='CMN';
  LastDisassembleData.parameters:=ArmRegisters[rn]+','+ArmRegisters[rm];
end;


procedure TThumbDisassembler.ORR_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='ORRS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;


procedure TThumbDisassembler.MUL_R_T1;
var
  rdm,rn: byte;
begin
  rn:=(opcode shr 3) and 7;
  rdm:=opcode and 7;
  LastDisassembleData.opcode:='MULS';
  LastDisassembleData.parameters:=ArmRegisters[rdm]+','+ArmRegisters[rn];
end;

procedure TThumbDisassembler.BIC_R_T1;
var
  rdn,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rdn:=opcode and 7;
  LastDisassembleData.opcode:='BICS';
  LastDisassembleData.parameters:=ArmRegisters[rdn]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.MVN_R_T1;
var
  rd,rm: byte;
begin
  rm:=(opcode shr 3) and 7;
  rd:=opcode and 7;
  LastDisassembleData.opcode:='MVN';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.MOV_R_T1;
var
  rd,rm,D: byte;
begin
  rd:=(opcode and 7);
  rm:=(opcode shr 3) and $f;
  d:=(opcode shr 7) and 1;

  rd:=(d shl 4) or rd;

  LastDisassembleData.opcode:='MOV';
  LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rm];
end;

procedure TThumbDisassembler.MOV_RSR_T1;
var
  op: byte;
  rs,rdm: byte;
  shiftname: string;
begin
  op:=(opcode shr 6) and 3;
  rs:=(opcode shr 3) and 7;
  rdm:=opcode and 7;

  case op of
    2: shiftname:='LSL';
    3: shiftname:='LSR';
    4: shiftname:='ASR';
    7: shiftname:='ROR';
  end;


  LastDisassembleData.opcode:='MOVS';
  LastDisassembleData.parameters:=ArmRegisters[rdm]+','+ArmRegisters[rdm]+','+shiftname+' '+ArmRegisters[rs];
end;

procedure TThumbDisassembler.MOV_R_T2;
var
  op: byte;
  rn,rd: byte;
  imm5: byte;
  shiftname: string;
begin
  if ((opcode shr 13)=0) and (((opcode shr 11) and 3)<>3) then
  begin
    op:=(opcode shr 11) and 3;
    imm5:=(opcode shr 6) and $1f;
    rn:=(opcode shr 3) and 7;
    rd:=opcode and 7;

    if imm5>0 then
    begin
      case op of
        0: shiftname:='LSL';
        1: shiftname:='LSR';
        2: shiftname:='ASR';
        3: shiftname:='ROR';
      end;
      shiftname:=','+shiftname+' '+inttohex(imm5,1);
    end
    else
      shiftname:='';

    LastDisassembleData.opcode:='MOVS';
    LastDisassembleData.parameters:=ArmRegisters[rd]+','+ArmRegisters[rn]+shiftname;
  end;
end;


procedure TThumbDisassembler.MSR_BR_T1;
var R, Rn, M1, m: byte;
  bankedreg: string;
begin
  r:=(opcode shr 4) and 1;
  rn:=opcode and $f;
  m1:=(opcode2 shr 8) and $f;
  m:=(opcode2 shr 4) and 1;

  LastDisassembleData.opcode:='MSR';

  case r of
    0:
    begin
      case m of
        0:
        case m1 of
          0: bankedreg:='R8_usr';
          1: bankedreg:='R9_usr';
          2: bankedreg:='R10_usr';
          3: bankedreg:='R11_usr';
          4: bankedreg:='R12_usr';
          5: bankedreg:='SP_usr';
          6: bankedreg:='LR_usr';
          8: bankedreg:='R8_fiq';
          9: bankedreg:='R9_fiq';
         10: bankedreg:='R10_fiq';
         11: bankedreg:='R11_fiq';
         12: bankedreg:='R12_fiq';
         13: bankedreg:='SP_fiq';
         14: bankedreg:='LR_fiq';
        end;

        1:
        case m1 of
          0: bankedreg:='LR_irq';
          1: bankedreg:='SP_irq';
          2: bankedreg:='LR_svc';
          3: bankedreg:='SP_svc';
          4: bankedreg:='LR_abt';
          5: bankedreg:='SP_abt';
          6: bankedreg:='LR_und';
          7: bankedreg:='SP_und';
         12: bankedreg:='LR_mon';
         13: bankedreg:='SP_mon';
         14: bankedreg:='ELR_hyp';
         15: bankedreg:='SP_hyp';
        end;
      end;

    end;

    1:
    begin
      case m of
        0:
        case m1 of
          14: bankedreg:='SPSR_fiq';
        end;

        1:
        case m1 of
          0: bankedreg:='SPSR_irq';
          2: bankedreg:='SPSR_svc';
          4: bankedreg:='SPSR_abt';
          6: bankedreg:='SPSR_und';
         12: bankedreg:='SPSR_mon';
         14: bankedreg:='SPSR_hyp';
        end;
      end;

    end;

  end;

  LastDisassembleData.parameters:=bankedreg+','+ArmRegisters[rn];
end;

procedure TThumbDisassembler.MSR_R_T1;
var
  r, rn, mask: byte;
  spec_reg: string;
begin
  r:=(opcode shr 4) and 1;
  rn:=opcode and $f;
  mask:=(opcode shr 8) and $f;


  LastDisassembleData.opcode:='MSR';
  if R=0 then
    LastDisassembleData.parameters:='CPSR('+intToBin(mask,4)+'),'+ArmRegisters[rn]
  else
    LastDisassembleData.parameters:='SPSR('+intToBin(mask,4)+'),'+ArmRegisters[rn];
end;

procedure TThumbDisassembler.MRS_BR_T1;
var R, M1, Rd, m: byte;
  bankedreg: string;
begin
  r:=(opcode shr 4) and 1;
  m1:=(opcode2 shr 8) and $f;
  rd:=(opcode shr 8) and $f;
  m:=(opcode2 shr 4) and 1;

  LastDisassembleData.opcode:='MRS';

  case r of
    0:
    begin
      case m of
        0:
        case m1 of
          0: bankedreg:='R8_usr';
          1: bankedreg:='R9_usr';
          2: bankedreg:='R10_usr';
          3: bankedreg:='R11_usr';
          4: bankedreg:='R12_usr';
          5: bankedreg:='SP_usr';
          6: bankedreg:='LR_usr';
          8: bankedreg:='R8_fiq';
          9: bankedreg:='R9_fiq';
         10: bankedreg:='R10_fiq';
         11: bankedreg:='R11_fiq';
         12: bankedreg:='R12_fiq';
         13: bankedreg:='SP_fiq';
         14: bankedreg:='LR_fiq';
        end;

        1:
        case m1 of
          0: bankedreg:='LR_irq';
          1: bankedreg:='SP_irq';
          2: bankedreg:='LR_svc';
          3: bankedreg:='SP_svc';
          4: bankedreg:='LR_abt';
          5: bankedreg:='SP_abt';
          6: bankedreg:='LR_und';
          7: bankedreg:='SP_und';
         12: bankedreg:='LR_mon';
         13: bankedreg:='SP_mon';
         14: bankedreg:='ELR_hyp';
         15: bankedreg:='SP_hyp';
        end;
      end;

    end;

    1:
    begin
      case m of
        0:
        case m1 of
          14: bankedreg:='SPSR_fiq';
        end;

        1:
        case m1 of
          0: bankedreg:='SPSR_irq';
          2: bankedreg:='SPSR_svc';
          4: bankedreg:='SPSR_abt';
          6: bankedreg:='SPSR_und';
         12: bankedreg:='SPSR_mon';
         14: bankedreg:='SPSR_hyp';
        end;
      end;

    end;

  end;

  LastDisassembleData.parameters:=ArmRegisters[rd]+','+bankedreg;
end;

procedure TThumbDisassembler.MRS_R_T1;
var
  r, rd: byte;
begin
  //todo: someday...
  r:=(opcode shr 4) and 1;
  rd:=(opcode shr 8) and $f;

  LastDisassembleData.opcode:='MRS';
  if R=0 then
    LastDisassembleData.parameters:=ArmRegisters[rd]+',CPSR'
  else
    LastDisassembleData.parameters:=ArmRegisters[rd]+',APSR'

end;


function TThumbDisassembler.disassemble(var address: ptruint): string;
var x: ptruint;
  i: integer;
begin

  a:=address;
  {$ifdef armdev}
  opcode:=pword(address)^;
  setlength(LastDisassembleData.Bytes,2);
  pword(@LastDisassembleData.Bytes[0])^:=opcode;
  {$else}
  x:=0;
  if readprocessmemory(processhandle, pointer(address-1), @opcode, sizeof(opcode), x) then
  begin
    setlength(LastDisassembleData.Bytes,2);
    puint16(@LastDisassembleData.Bytes[0])^:=opcode;
  end;
  {$endif}

  LastDisassembleData.address:=a;

  case (opcode shr 10) of
    $00..$0f:
    begin
      //Shift, Add, Subtract, Move and Compare
      case (opcode shr 9) and $1f of
        $0..$b: MOV_R_T2;     //000 0x xx -  000 10 xx
        $c: ADD_R_T1;
        $d: SUB_R_T1;
        $e: ADD_I_T1;
        $f: SUB_I_T1;
        $10..$13: MOV_I_T1;
        $14..$17: CMP_I_T1;
        $18..$1b: ADD_I_T2;
        $1c..$1f: SUB_I_T2;
      end;
    end;


    $10:
    begin
      //data Processing 010000
      case (opcode shr 6) and $f of //4 bits (bit 6-9)
        0: AND_R_T1; //0100000000******
        1: EOR_R_T1;
        2,3,4,7: MOV_RSR_T1; //010000001x - 0100000100 - 0100000111
        5: ADC_R_T1;
        6: SBC_R_T1;
        8: TST_R_T1;
        9: RSB_I_T1;
        10: CMP_R_T1;
        11: CMN_R_T1;
        12: ORR_R_T1;
        13: MUL_R_T1;
        14: BIC_R_T1;
        15: MVN_R_T1;
      end;
    end;

    $11:
    begin
      //special Data Instructions And Branch And Exchange;
      case (opcode shr 6) and $f of
        0..3:
        begin
          case (opcode shr 3) and $f of
            $D: ADD_SP_R_T1;
            0,1,2,3,4,6,7:
            begin
              if ((opcode shr 7) and 1)=0 then
                ADD_R_T2
              else
              begin
                if (opcode and 7)=5 then
                  ADD_SP_R_T2
                else
                  ADD_R_T2;
              end;
            end;

            8,9,$a,$b,$c,$e,$f:
            begin
              if ((opcode shr 7) and 1)=0 then
                ADD_R_T2
              else
              begin
                if (opcode and 7)=5 then
                  ADD_SP_R_T2
                else
                  ADD_R_T2;
              end;

            end;
          end;
        end;

        4..7: CMP_R_T2;
        8..11: MOV_R_T1;
        12..13: BX_T1;
        14..15: BLX_R_T1;
      end;

    end;

    $12..$13: LDR_L_T1;

    $14..$27:
    begin
      //Load Store Single Data Item;
      case (opcode shr 9) and $7f of
        $28: STR_R_T1;
        $29: STRH_R_T1;
        $2a: STRB_R_T1;
        $2b: LDRSB_R_T1;
        $2c: LDR_R_T1;
        $2d: LDRH_R_T1;
        $2e: LDRB_R_T1;
        $2f: LDRSH_R_T1;
        $30..$33: STR_I_T1;
        $34..$37: LDR_I_T1;
        $38..$3b: STRB_I_T1;
        $3c..$3f: LDRB_I_T1;
        $40..$43: STRH_I_T1;
        $44..$47: LDRH_I_T1;
        $48..$4b: STR_I_T2;
        $4c..$4f: LDR_I_T2;
      end;
    end;
    $28..$29: ADR_T1;
    $2a..$2b: ADD_SP_I_T1;
    $2c..$2f:
    begin
      //misc16
      case (opcode shr 5) and $7f of
        $0..$3: ADD_SP_I_T2;
        $4..$7: SUB_SP_I_T1;
        $8..$f: CBZ_CBNZ;
        $10..$11: SXTH_T1;
        $12..$13: SXTB_T1;
        $14..$15: UXTH_T1;
        $16..$17: UXTB_T1;
        $18..$1f: CBZ_CBNZ;
        $20..$2f: PUSH_T1;
        $32: SETEND_T1;
        $33: CPS_T1;
        $48..$4f: CBZ_CBNZ;
        $50..$51: REV_T1;
        $52..$53: REV16_T1;

        $54..$55: HLT_T1;
        $56..$57: REVSH_T1;
        $58..$5f: CBZ_CBNZ;
        $60..$6f: POP_T1;
        $70..$77: BKPT_T1;
        $78..$7f:
        begin
          if (opcode and $f)<>0 then IT_T1
          else
          case ((opcode shr 4) and $7) of
            0: LastDisassembleData.opcode:='NOP';
            1: LastDisassembleData.opcode:='YIELD';
            2: LastDisassembleData.opcode:='WFE';
            3: LastDisassembleData.opcode:='WFI';
            4: LastDisassembleData.opcode:='SEV';
            5: LastDisassembleData.opcode:='SEVL';
          end;

        end;
      end;

    end;
    $30..$31: STM_T1;
    $32..$33: LDM_T1;
    $34..$37:
    begin
      case (opcode shr 8) and $7 of
        $e: UDF_T1;
        $f: SVC_T1;
        else
          B_T1;
      end;
    end;
    $38..$39: B_T2;
    $3a..$3f: T32;
  end;

  result:=inttohex(LastDisassembleData.address,8);
  result:=result+' - ';
  for i:=0 to length(LastDisassembleData.bytes)-1 do
    result:=result+inttohex(LastDisassembleData.Bytes[i],2)+' ';
  result:=result+' - ';
  result:=result+LastDisassembleData.opcode;
  result:=result+' ';
  result:=result+LastDisassembleData.parameters;

  inc(address, length(LastDisassembleData.Bytes));
end;

end.

