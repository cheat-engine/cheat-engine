/*
 * vmeventhandler_amd.h
 *
 *  Created on: May 22, 2013
 *      Author: eric
 */

#ifndef VMEVENTHANDLER_AMD_H_
#define VMEVENTHANDLER_AMD_H_

#include "vmmhelper.h"

//define Exit codes:
#define VMEXIT_CR0_READ  0x00
#define VMEXIT_CR1_READ  0x01
#define VMEXIT_CR2_READ  0x02
#define VMEXIT_CR3_READ  0x03
#define VMEXIT_CR4_READ  0x04
#define VMEXIT_CR5_READ  0x05
#define VMEXIT_CR6_READ  0x06
#define VMEXIT_CR7_READ  0x07
#define VMEXIT_CR8_READ  0x08
#define VMEXIT_CR9_READ  0x09
#define VMEXIT_CR10_READ 0x0a
#define VMEXIT_CR11_READ 0x0b
#define VMEXIT_CR12_READ 0x0c
#define VMEXIT_CR13_READ 0x0d
#define VMEXIT_CR14_READ 0x0e
#define VMEXIT_CR15_READ 0x0f

#define VMEXIT_CR0_WRITE  0x10
#define VMEXIT_CR1_WRITE  0x11
#define VMEXIT_CR2_WRITE  0x12
#define VMEXIT_CR3_WRITE  0x13
#define VMEXIT_CR4_WRITE  0x14
#define VMEXIT_CR5_WRITE  0x15
#define VMEXIT_CR6_WRITE  0x16
#define VMEXIT_CR7_WRITE  0x17
#define VMEXIT_CR8_WRITE  0x18
#define VMEXIT_CR9_WRITE  0x19
#define VMEXIT_CR10_WRITE 0x1a
#define VMEXIT_CR11_WRITE 0x1b
#define VMEXIT_CR12_WRITE 0x1c
#define VMEXIT_CR13_WRITE 0x1d
#define VMEXIT_CR14_WRITE 0x1e
#define VMEXIT_CR15_WRITE 0x1f

#define VMEXIT_DR0_READ  0x20
#define VMEXIT_DR1_READ  0x21
#define VMEXIT_DR2_READ  0x22
#define VMEXIT_DR3_READ  0x23
#define VMEXIT_DR4_READ  0x24
#define VMEXIT_DR5_READ  0x25
#define VMEXIT_DR6_READ  0x26
#define VMEXIT_DR7_READ  0x27
#define VMEXIT_DR8_READ  0x28
#define VMEXIT_DR9_READ  0x29
#define VMEXIT_DR10_READ 0x2a
#define VMEXIT_DR11_READ 0x2b
#define VMEXIT_DR12_READ 0x2c
#define VMEXIT_DR13_READ 0x2d
#define VMEXIT_DR14_READ 0x2e
#define VMEXIT_DR15_READ 0x2f

#define VMEXIT_DR0_WRITE  0x30
#define VMEXIT_DR1_WRITE  0x31
#define VMEXIT_DR2_WRITE  0x32
#define VMEXIT_DR3_WRITE  0x33
#define VMEXIT_DR4_WRITE  0x34
#define VMEXIT_DR5_WRITE  0x35
#define VMEXIT_DR6_WRITE  0x36
#define VMEXIT_DR7_WRITE  0x37
#define VMEXIT_DR8_WRITE  0x38
#define VMEXIT_DR9_WRITE  0x39
#define VMEXIT_DR10_WRITE 0x3a
#define VMEXIT_DR11_WRITE 0x3b
#define VMEXIT_DR12_WRITE 0x3c
#define VMEXIT_DR13_WRITE 0x3d
#define VMEXIT_DR14_WRITE 0x3e
#define VMEXIT_DR15_WRITE 0x3f
#define VMEXIT_EXCP0      0x40
#define VMEXIT_EXCP1      0x41
#define VMEXIT_EXCP2      0x42
#define VMEXIT_EXCP3      0x43
#define VMEXIT_EXCP4      0x44
#define VMEXIT_EXCP5      0x45
#define VMEXIT_EXCP6      0x46
#define VMEXIT_EXCP7      0x47
#define VMEXIT_EXCP8      0x48
#define VMEXIT_EXCP9      0x49
#define VMEXIT_EXCP10      0x4a
#define VMEXIT_EXCP11      0x4b
#define VMEXIT_EXCP12      0x4c
#define VMEXIT_EXCP13      0x4d
#define VMEXIT_EXCP14      0x4e
#define VMEXIT_EXCP15      0x4f
#define VMEXIT_EXCP16      0x50
#define VMEXIT_EXCP17      0x51
#define VMEXIT_EXCP18      0x52
#define VMEXIT_EXCP19      0x53
#define VMEXIT_EXCP20      0x54
#define VMEXIT_EXCP21      0x55
#define VMEXIT_EXCP22      0x56
#define VMEXIT_EXCP23      0x57
#define VMEXIT_EXCP24      0x58
#define VMEXIT_EXCP25      0x59
#define VMEXIT_EXCP26      0x5a
#define VMEXIT_EXCP27      0x5b
#define VMEXIT_EXCP28      0x5c
#define VMEXIT_EXCP29      0x5d
#define VMEXIT_EXCP30      0x5e
#define VMEXIT_EXCP31      0x5f
#define VMEXIT_INTR        0x60
#define VMEXIT_NMI         0x61
#define VMEXIT_SMI         0x62
#define VMEXIT_INIT        0x63
#define VMEXIT_VINTR       0x64
#define VMEXIT_CR0_SEL_WRITE 0x65
#define VMEXIT_IDTR_READ   0x66
#define VMEXIT_GDTR_READ   0x67
#define VMEXIT_LDTR_READ   0x68
#define VMEXIT_TR_READ     0x69
#define VMEXIT_IDTR_WRITE  0x6a
#define VMEXIT_GDTR_WRITE  0x6b
#define VMEXIT_LDTR_WRITE  0x6c
#define VMEXIT_TR_WRITE    0x6d
#define VMEXIT_RDTSC       0x6e
#define VMEXIT_RDPMC       0x6f
#define VMEXIT_PUSHF       0x70
#define VMEXIT_POPF        0x71
#define VMEXIT_CPUID       0x72
#define VMEXIT_RSM         0x73
#define VMEXIT_IRET        0x74
#define VMEXIT_SWINT       0x75
#define VMEXIT_INVD        0x76
#define VMEXIT_PAUSE       0x77
#define VMEXIT_HLT         0x78

#define VMEXIT_INVLPG      0x79
#define VMEXIT_INVLPGA     0x7a
#define VMEXIT_IOIO        0x7b
#define VMEXIT_MSR         0x7c
#define VMEXIT_TASK_SWITCH 0x7d
#define VMEXIT_FERR_FREEZE 0x7e
#define VMEXIT_SHUTDOWN    0x7f
#define VMEXIT_VMRUN       0x80
#define VMEXIT_VMMCALL     0x81
#define VMEXIT_VMLOAD      0x82
#define VMEXIT_VMSAVE      0x83
#define VMEXIT_STGI        0x84
#define VMEXIT_CLGI        0x85
#define VMEXIT_SKINIT      0x86
#define VMEXIT_RDTSCP      0x87
#define VMEXIT_ICEBP       0x88
#define VMEXIT_WBINVD      0x89
#define VMEXIT_MONITOR     0x8a
#define VMEXIT_MWAIT       0x8b
#define VMEXIT_MWAIT_CONDITIONAL 0x8c
#define VMEXIT_XSETBV      0x8d
#define VMEXIT_NPF         0x400 //nested paging pagefault. (Note for future: Look up possible (ab)use for stealthedit)
#define AVIC_INCOMPLETE_IPI 0x401
#define AVIC_NOACCEL       0x402
#define VMEXIT_INVALID     -1


int handleVMEvent_amd(pcpuinfo currentcpuinfo, VMRegisters *vmregisters, FXSAVE64 *fxsave);


#endif /* VMEVENTHANDLER_AMD_H_ */
