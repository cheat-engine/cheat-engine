/*
 * msrnames.h
 *
 *  Created on: Sep 7, 2009
 *      Author: erich
 */

#ifndef MSRNAMES_H_
#define MSRNAMES_H_

#define IA32_TIME_STAMP_COUNTER   0x10
#define IA32_APICBASE_MSR         0x1b
#define IA32_FEATURE_CONTROL_MSR  0x3a
#define IA32_TSC_ADJUST           0x3b
#define IA32_MTRRCAP_MSR          0xfe
#define IA32_SYSENTER_CS_MSR      0x174
#define IA32_SYSENTER_ESP_MSR     0x175
#define IA32_SYSENTER_EIP_MSR     0x176

#define IA32_MISC_ENABLE          0x1a0
#define IA32_DEBUGCTL_MSR         0x1d9

#define IA32_MTRR_PHYSBASE0       0x200 //access using this+x*0x2
#define IA32_MTRR_PHYSMASK0       0x201
#define IA32_MTRR_PHYSBASE1       0x202
#define IA32_MTRR_PHYSMASK1       0x203
#define IA32_MTRR_PHYSBASE2       0x204
#define IA32_MTRR_PHYSMASK2       0x205
#define IA32_MTRR_PHYSBASE3       0x206
#define IA32_MTRR_PHYSMASK3       0x207
#define IA32_MTRR_PHYSBASE4       0x208
#define IA32_MTRR_PHYSMASK4       0x209
#define IA32_MTRR_PHYSBASE5       0x20a
#define IA32_MTRR_PHYSMASK5       0x20b
#define IA32_MTRR_PHYSBASE6       0x20c
#define IA32_MTRR_PHYSMASK6       0x20d
#define IA32_MTRR_PHYSBASE7       0x20e
#define IA32_MTRR_PHYSMASK7       0x20f
#define IA32_MTRR_PHYSBASE8       0x210
#define IA32_MTRR_PHYSMASK8       0x211
#define IA32_MTRR_PHYSBASE9       0x212
#define IA32_MTRR_PHYSMASK9       0x213


#define IA32_MTRR_FIX64K_00000    0x250  //8*64k blocks  (00000000->0007ffff)
#define IA32_MTRR_FIX16K_80000    0x258  //8*16k blocks  (00080000->0009ffff)
#define IA32_MTRR_FIX16K_A0000    0x259  //..            (000a0000->000bffff)
#define IA32_MTRR_FIX4K_C0000     0x268  //8*4k blocks)  (000c0000->000c7fff)
#define IA32_MTRR_FIX4K_C8000     0x269  //              (000c8000->000cffff)
#define IA32_MTRR_FIX4K_D0000     0x26a  //              (000d0000->000d7fff)
#define IA32_MTRR_FIX4K_D8000     0x26b  //              (000d8000->000dffff)
#define IA32_MTRR_FIX4K_E0000     0x26c  //              (000e0000->000e7fff)
#define IA32_MTRR_FIX4K_E8000     0x26d  //              (000e8000->000effff)
#define IA32_MTRR_FIX4K_F0000     0x26e  //              (000f0000->000f7fff)
#define IA32_MTRR_FIX4K_F8000     0x26f  //              (000f8000->000fffff)




#define IA32_MTRR_DEF_TYPE_MSR    0x2ff



#define IA32_VMX_BASIC_MSR          0x480
#define IA32_VMX_PINBASED_CTLS_MSR  0x481
#define IA32_VMX_PROCBASED_CTLS_MSR 0x482
#define IA32_VMX_EXIT_CTLS_MSR      0x483
#define IA32_VMX_ENTRY_CTLS_MSR     0x484
#define IA32_VMX_MISC_CTLS_MSR      0x485

#define IA32_VMX_CR0_FIXED0_MSR   0x486
#define IA32_VMX_CR0_FIXED1_MSR   0x487
#define IA32_VMX_CR4_FIXED0_MSR   0x488
#define IA32_VMX_CR4_FIXED1_MSR   0x489

#define IA32_VMX_PROCBASED_CTLS2_MSR 0x48b
#define IA32_VMX_EPT_VPID_CAP_MSR    0x48c

#define IA32_VMX_TRUE_PINBASED_CTLS_MSR 0x48d
#define IA32_VMX_TRUE_PROCBASED_CTLS_MSR 0x48e

#define IA32_RTIT_CTL_MSR         0x570
#define IA32_DS_AREA              0x600

#define IA32_X2APIC_LVT_PMI_MSR   0x834



#define EFER_MSR				          0xc0000080
#define IA32_LSTAR                0xc0000082
#define IA32_FMASK_MSR            0xc0000084
#define IA32_FS_BASE_MSR          0xc0000100
#define IA32_GS_BASE_MSR          0xc0000101
#define IA32_GS_BASE_KERNEL_MSR   0xc0000102

#define IA32_TSC_MSR              0x10
#define IA32_TSC_AUX_MSR          0xc0000103

#define VM_HSAVE_PA_MSR           0xc0010117 //AMD

#endif /* MSRNAMES_H_ */
