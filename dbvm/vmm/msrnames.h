/*
 * msrnames.h
 *
 *  Created on: Sep 7, 2009
 *      Author: erich
 */

#ifndef MSRNAMES_H_
#define MSRNAMES_H_

#define IA32_FEATURE_CONTROL_MSR  0x3a
#define IA32_SYSENTER_CS_MSR      0x174
#define IA32_SYSENTER_ESP_MSR     0x175
#define IA32_SYSENTER_EIP_MSR     0x176

#define IA32_MISC_ENABLE          0x1a0
#define IA32_DEBUGCTL_MSR         0x1d9

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

#define IA32_DS_AREA              0x600
#define EFER_MSR				  0xc0000080

#define IA32_FS_BASE_MSR          0xc0000100
#define IA32_GS_BASE_MSR          0xc0000101


#endif /* MSRNAMES_H_ */
