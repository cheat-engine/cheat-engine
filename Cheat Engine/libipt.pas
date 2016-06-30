unit libipt;

{$mode DELPHI}

interface

uses
  Windows, Classes, SysUtils;

const
  pt_asid_no_cr3  = QWORD($ffffffffffffffff);
  pt_asid_no_vmcs = QWORD($ffffffffffffffff);
  pt_max_insn_size= 15;

type
  pt_error_code = (pte_ok,pte_internal,pte_invalid,pte_nosync,
        pte_bad_opc,pte_bad_packet,pte_bad_context,
        pte_eos,pte_bad_query,pte_nomem,pte_bad_config,
        pte_noip,pte_ip_suppressed,pte_nomap,
        pte_bad_insn,pte_no_time,pte_no_cbr,
        pte_bad_image,pte_bad_lock,pte_not_supported,
        pte_retstack_empty,pte_bad_retcomp,
        pte_bad_status_update,pte_no_enable,
        pte_event_ignored);

  pt_insn_class = (ptic_error,ptic_other,ptic_call,ptic_return,
          ptic_jump,ptic_cond_jump,ptic_far_call,
          ptic_far_return,ptic_far_jump);

  pt_exec_mode = (ptem_unknown,ptem_16bit,ptem_32bit,ptem_64bit);

  PPT_Image=pointer;
  Ppt_encoder = pointer;
  Ppt_packet_decoder = pointer;

  Ppt_query_decoder = pointer;

  Ppt_insn_decoder = pointer;

  Ppt_packet_unknown = pointer;


  TPT_ASID=record
    size: SIZE_T;
    cr3:  uint64;
    vmcs: uint64;
  end;
  PPT_ASID=^TPT_ASID;

  Ppt_insn = ^pt_insn;
  pt_insn = record
      ip : uint64;
      iclass : pt_insn_class;
      mode : pt_exec_mode;
      raw : array[0..(pt_max_insn_size)-1] of uint8;
      size : uint8;
      flag0 : word;
      {
      flag0 bit:
      0: speculative - the instruction was executed speculatively.
      1: aborted - speculative execution was aborted after this instruction
      2: committed - speculative execution was committed after this instruction
      3: disabled - tracing was disabled after this instruction.
      4: enabled - tracing was enabled at this instruction
      5: resumed - tracing was resumed at this instruction. In addition to tracing being enabled, it continues from the IP at which tracing had been disabled before.
      6: interrupted - normal execution flow was interrupted after this instruction.
      7: resynced - tracing resumed at this instruction after an overflow.
      8: stopped - tracing was stopped after this instruction
      }
    end;

  pt_cpu_vendor = (pcv_unknown,pcv_intel);

  Ppt_cpu = ^pt_cpu;
  pt_cpu = record
      vendor : pt_cpu_vendor;
      family : uint16;
      model : uint8;
      stepping : uint8;
    end;

  ppt_errata = ^pt_errata;
  pt_errata = record
      flag0 : word;
      reserved : array[0..14] of uint32;
    end;

  Ppt_config = ^pt_config;

  pt_config = record
      size : size_t;
      beginaddress : pointer;
      endaddress : pointer;
      decode : record
          callback : function (unknown:Ppt_packet_unknown; config:Ppt_config; pos:Puint8; context:pointer):integer;cdecl;
          context : pointer;
        end;
      cpu : pt_cpu;
      errata : pt_errata;
      cpuid_0x15_eax : uint32;
      cpuid_0x15_ebx : uint32;
      mtc_freq : uint8;
      nom_freq : uint8;
    end;


  Tread_memory_callback=function(buffer: PByteArray; size: SIZE_T; asid: PPT_ASID; ip: uint64; context: pointer): integer; cdecl;






var
  pt_image_alloc:function(name: pchar): PPT_Image;  cdecl;
  pt_image_free:procedure(img: PPT_Image); cdecl;
  pt_image_set_callback: function(img: PPT_Image; callback: Tread_memory_callback; context: pointer): integer; cdecl;

  pt_cpu_read:function (cpu: ppt_cpu): integer; cdecl;
  pt_cpu_errata: function(errata: ppt_errata; cpu: ppt_cpu): integer; cdecl;

  pt_insn_alloc_decoder:function(config: ppt_config): ppt_insn_decoder; cdecl;
  pt_insn_free_decoder:procedure(decoder: ppt_insn_decoder); cdecl;
  pt_insn_set_image:function(decoder: ppt_insn_decoder; image: PPT_Image): integer; cdecl;
  pt_insn_sync_forward:function(decoder: ppt_insn_decoder): integer; cdecl;
  pt_insn_next: function(decoder: ppt_insn_decoder; insn: Ppt_insn; size: size_t): integer; cdecl;
  pt_insn_get_offset:function(decoder: Ppt_insn_decoder; offset: PQWord): integer; cdecl;

  procedure pt_asid_init(asid: PPT_ASID); inline;
  procedure pt_config_init(config: ppt_config); inline;

  procedure libIptInit;

implementation

var hLibIPT: HModule=0;

procedure pt_asid_init(asid: PPT_ASID); inline;
begin
  asid^.size:=sizeof(TPT_ASID);
  asid^.cr3:=pt_asid_no_cr3;
  asid^.vmcs:=pt_asid_no_vmcs;
end;

procedure pt_config_init(config: ppt_config); inline;
begin
  zeromemory(config, sizeof(pt_config));
  config^.size:=sizeof(pt_config);
end;


procedure libIptInit;
begin
  if hLibIPT=0 then
  begin
    hLibIPT:=LoadLibrary('libipt-64.dll');
    if hLibIPT=0 then hLibIPT:=LoadLibrary('D:\svn\Cheat Engine\bin\libipt-64.dll'); //during debug

    if hLibIPT<>0 then
    begin
      pt_image_alloc:=GetProcAddress(hLibIPT, 'pt_image_alloc');
      pt_image_free:=GetProcAddress(hLibIPT, 'pt_image_free');
      pt_image_set_callback:=GetProcAddress(hLibIPT, 'pt_image_set_callback');


      pt_cpu_read:=GetProcAddress(hLibIPT, 'pt_cpu_read');
      pt_cpu_errata:=GetProcAddress(hLibIPT, 'pt_cpu_errata');

      pt_insn_alloc_decoder:=GetProcAddress(hLibIPT, 'pt_insn_alloc_decoder');
      pt_insn_free_decoder:=GetProcAddress(hLibIPT, 'pt_insn_free_decoder');
      pt_insn_set_image:=GetProcAddress(hLibIPT, 'pt_insn_set_image');

      pt_insn_sync_forward:=GetProcAddress(hLibIPT, 'pt_insn_sync_forward');
      pt_insn_next:=GetProcAddress(hLibIPT, 'pt_insn_next');
      pt_insn_get_offset:=GetProcAddress(hLibIPT, 'pt_insn_get_offset');

    end;
  end;
end;

end.

