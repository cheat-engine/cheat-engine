#ifndef VMREADWRITE_H_
#define VMREADWRITE_H_

#define vm_vpid						0
#define vm_posted_interrupt_vector	2
#define vm_eptp_index				4

#define vm_guest_es                 0x800
#define vm_guest_cs                 0x802
#define vm_guest_ss                 0x804
#define vm_guest_ds                 0x806
#define vm_guest_fs                 0x808
#define vm_guest_gs                 0x80a
#define vm_guest_ldtr               0x80c
#define vm_guest_tr                 0x80e

#define vm_iobitmap_a				        0x2000
#define vm_iobitmap_b				        0x2002

#define vm_eptpointer               0x201a
#define vm_guest_physical_address   0x2400

#define vm_vmcs_link_pointer        0x2800
#define vm_guest_IA32_DEBUGCTL      0x2802
#define vm_guest_IA32_PAT           0x2804
#define vm_guest_IA32_EFER          0x2806
#define vm_guest_IA32_PERF_GLOBAL_CTRL 0x2808

#define vm_execution_controls_pin   0x4000
#define vm_execution_controls_cpu   0x4002
#define vm_exception_bitmap         0x4004
#define vm_execution_controls_cpu_secondary 0x401e

#define vm_exit_controls            0x400c
#define vm_entry_controls           0x4012
#define vm_entry_interruptioninfo   0x4016
#define vm_entry_exceptionerrorcode 0x4018
#define vm_entry_instructionlength  0x401a

#define vm_errorcode                0x4400
#define vm_exit_reason              0x4402
#define vm_exit_interruptioninfo    0x4404
#define vm_exit_interruptionerror   0x4406
#define vm_idtvector_information    0x4408
#define vm_idtvector_error          0x440a
#define vm_exit_instructionlength   0x440c
#define vm_instruction_information  0x440e

#define vm_guest_es_limit           0x4800
#define vm_guest_cs_limit           0x4802
#define vm_guest_ss_limit           0x4804
#define vm_guest_ds_limit           0x4806
#define vm_guest_fs_limit           0x4808
#define vm_guest_gs_limit           0x480a
#define vm_guest_ldtr_limit         0x480c
#define vm_guest_tr_limit           0x480e
#define vm_guest_gdt_limit          0x4810
#define vm_guest_idt_limit          0x4812

#define vm_guest_es_access_rights   0x4814
#define vm_guest_cs_access_rights   0x4816
#define vm_guest_ss_access_rights   0x4818
#define vm_guest_ds_access_rights   0x481a
#define vm_guest_fs_access_rights   0x481c
#define vm_guest_gs_access_rights   0x481e
#define vm_guest_ldtr_access_rights 0x4820
#define vm_guest_tr_access_rights   0x4822
#define vm_guest_interruptability_state 0x4824
#define vm_guest_activity_state     0x4826
#define vm_guest_IA32_SYSENTER_CS   0x482a
#define vm_preemption_timer_value   0x482e

#define vm_cr0_guest_host_mask      0x6000
#define vm_cr4_guest_host_mask      0x6002
#define vm_cr0_read_shadow          0x6004 //what the guest sees when he reads CR3
#define vm_cr4_read_shadow          0x6006
#define vm_cr3_targetvalue0			    0x6008



#define vm_exit_qualification       0x6400
#define vm_io_rcx                   0x6402
#define vm_io_rsi                   0x6404
#define vm_io_rdi                   0x6406
#define vm_io_rip                   0x6408
#define vm_guest_linear_address     0x640a


#define vm_guest_cr0                0x6800 //actual cr0
#define vm_guest_cr3                0x6802 //the actual CR3 value for the guest
#define vm_guest_cr4                0x6804
#define vm_guest_es_base            0x6806
#define vm_guest_cs_base            0x6808
#define vm_guest_ss_base            0x680a
#define vm_guest_ds_base            0x680c
#define vm_guest_fs_base            0x680e
#define vm_guest_gs_base            0x6810
#define vm_guest_ldtr_base          0x6812
#define vm_guest_tr_base            0x6814
#define vm_guest_gdtr_base          0x6816
#define vm_guest_idtr_base          0x6818
#define vm_guest_dr7                0x681a
#define vm_guest_rsp                0x681c
#define vm_guest_rip                0x681e
#define vm_guest_rflags             0x6820
#define vm_pending_debug_exceptions 0x6822
#define vm_guest_IA32_SYSENTER_ESP  0x6824
#define vm_guest_IA32_SYSENTER_EIP  0x6826

//host 0x*c**
#define vm_host_es                    0xc00
#define vm_host_cs                    0xc02
#define vm_host_ss                    0xc04
#define vm_host_ds                    0xc06
#define vm_host_fs                    0xc08
#define vm_host_gs                    0xc0a
#define vm_host_tr                    0xc0c

#define vm_host_IA32_PAT              0x2C00
#define vm_host_IA32_EFER             0x2C02
#define vm_host_IA32_PERF_GLOBAL_CTRL 0x2c04

#define vm_host_IA32_SYSENTER_CS      0x4c00

#define vm_host_cr0                   0x6c00
#define vm_host_cr3                   0x6c02
#define vm_host_cr4                   0x6c04
#define vm_host_fs_base               0x6c06
#define vm_host_gs_base               0x6c08
#define vm_host_tr_base               0x6c0a
#define vm_host_gdtr_base             0x6c0c
#define vm_host_idtr_base             0x6c0e
#define vm_host_IA32_SYSENTER_ESP     0x6c10
#define vm_host_IA32_SYSENTER_EIP     0x6c12
#define vm_host_rsp                   0x6c14
#define vm_host_rip                   0x6c16



#endif /*VMREADWRITE_H_*/
