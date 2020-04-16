/** @file
  When installed, the MP Services Protocol produces a collection of services
  that are needed for MP management.

  The MP Services Protocol provides a generalized way of performing following tasks:
    - Retrieving information of multi-processor environment and MP-related status of
      specific processors.
    - Dispatching user-provided function to APs.
    - Maintain MP-related processor status.

  The MP Services Protocol must be produced on any system with more than one logical
  processor.

  The Protocol is available only during boot time.

  MP Services Protocol is hardware-independent. Most of the logic of this protocol
  is architecturally neutral. It abstracts the multi-processor environment and
  status of processors, and provides interfaces to retrieve information, maintain,
  and dispatch.

  MP Services Protocol may be consumed by ACPI module. The ACPI module may use this
  protocol to retrieve data that are needed for an MP platform and report them to OS.
  MP Services Protocol may also be used to program and configure processors, such
  as MTRR synchronization for memory space attributes setting in DXE Services.
  MP Services Protocol may be used by non-CPU DXE drivers to speed up platform boot
  by taking advantage of the processing capabilities of the APs, for example, using
  APs to help test system memory in parallel with other device initialization.
  Diagnostics applications may also use this protocol for multi-processor.

Copyright (c) 2006 - 2017, Intel Corporation. All rights reserved.<BR>
SPDX-License-Identifier: BSD-2-Clause-Patent

  @par Revision Reference:
  This Protocol is defined in the UEFI Platform Initialization Specification 1.2,
  Volume 2:Driver Execution Environment Core Interface.

**/

#ifndef _MP_SERVICE_PROTOCOL_H_
#define _MP_SERVICE_PROTOCOL_H_


//#define EFI_AP_PROCEDURE void *
typedef VOID(EFIAPI *EFI_AP_PROCEDURE) (IN VOID *Buffer);


///
/// Global ID for the EFI_MP_SERVICES_PROTOCOL.
///
#define EFI_MP_SERVICES_PROTOCOL_GUID \
  { \
    0x3fdda605, 0xa76e, 0x4f46, {0xad, 0x29, 0x12, 0xf4, 0x53, 0x1b, 0x3d, 0x08} \
  }

///
/// Forward declaration for the EFI_MP_SERVICES_PROTOCOL.
///
typedef struct _EFI_MP_SERVICES_PROTOCOL EFI_MP_SERVICES_PROTOCOL;

///
/// Terminator for a list of failed CPUs returned by StartAllAPs().
///
#define END_OF_CPU_LIST    0xffffffff

///
/// This bit is used in the StatusFlag field of EFI_PROCESSOR_INFORMATION and
/// indicates whether the processor is playing the role of BSP. If the bit is 1,
/// then the processor is BSP. Otherwise, it is AP.
///
#define PROCESSOR_AS_BSP_BIT         0x00000001

///
/// This bit is used in the StatusFlag field of EFI_PROCESSOR_INFORMATION and
/// indicates whether the processor is enabled. If the bit is 1, then the
/// processor is enabled. Otherwise, it is disabled.
///
#define PROCESSOR_ENABLED_BIT        0x00000002

///
/// This bit is used in the StatusFlag field of EFI_PROCESSOR_INFORMATION and
/// indicates whether the processor is healthy. If the bit is 1, then the
/// processor is healthy. Otherwise, some fault has been detected for the processor.
///
#define PROCESSOR_HEALTH_STATUS_BIT  0x00000004

///
/// Structure that describes the pyhiscal location of a logical CPU.
///
typedef struct {
  ///
  /// Zero-based physical package number that identifies the cartridge of the processor.
  ///
  UINT32  Package;
  ///
  /// Zero-based physical core number within package of the processor.
  ///
  UINT32  Core;
  ///
  /// Zero-based logical thread number within core of the processor.
  ///
  UINT32  Thread;
} EFI_CPU_PHYSICAL_LOCATION;

///
/// Structure that describes information about a logical CPU.
///
typedef struct {
  ///
  /// The unique processor ID determined by system hardware.  For IA32 and X64,
  /// the processor ID is the same as the Local APIC ID. Only the lower 8 bits
  /// are used, and higher bits are reserved.  For IPF, the lower 16 bits contains
  /// id/eid, and higher bits are reserved.
  ///
  UINT64                     ProcessorId;
  ///
  /// Flags indicating if the processor is BSP or AP, if the processor is enabled
  /// or disabled, and if the processor is healthy. Bits 3..31 are reserved and
  /// must be 0.
  ///
  /// <pre>
  /// BSP  ENABLED  HEALTH  Description
  /// ===  =======  ======  ===================================================
  ///  0      0       0     Unhealthy Disabled AP.
  ///  0      0       1     Healthy Disabled AP.
  ///  0      1       0     Unhealthy Enabled AP.
  ///  0      1       1     Healthy Enabled AP.
  ///  1      0       0     Invalid. The BSP can never be in the disabled state.
  ///  1      0       1     Invalid. The BSP can never be in the disabled state.
  ///  1      1       0     Unhealthy Enabled BSP.
  ///  1      1       1     Healthy Enabled BSP.
  /// </pre>
  ///
  UINT32                     StatusFlag;
  ///
  /// The physical location of the processor, including the physical package number
  /// that identifies the cartridge, the physical core number within package, and
  /// logical thread number within core.
  ///
  EFI_CPU_PHYSICAL_LOCATION  Location;
} EFI_PROCESSOR_INFORMATION;

/**
  This service retrieves the number of logical processor in the platform
  and the number of those logical processors that are enabled on this boot.
  This service may only be called from the BSP.

  This function is used to retrieve the following information:
    - The number of logical processors that are present in the system.
    - The number of enabled logical processors in the system at the instant
      this call is made.

  Because MP Service Protocol provides services to enable and disable processors
  dynamically, the number of enabled logical processors may vary during the
  course of a boot session.

  If this service is called from an AP, then EFI_DEVICE_ERROR is returned.
  If NumberOfProcessors or NumberOfEnabledProcessors is NULL, then
  EFI_INVALID_PARAMETER is returned. Otherwise, the total number of processors
  is returned in NumberOfProcessors, the number of currently enabled processor
  is returned in NumberOfEnabledProcessors, and EFI_SUCCESS is returned.

  @param[in]  This                        A pointer to the EFI_MP_SERVICES_PROTOCOL
                                          instance.
  @param[out] NumberOfProcessors          Pointer to the total number of logical
                                          processors in the system, including the BSP
                                          and disabled APs.
  @param[out] NumberOfEnabledProcessors   Pointer to the number of enabled logical
                                          processors that exist in system, including
                                          the BSP.

  @retval EFI_SUCCESS             The number of logical processors and enabled
                                  logical processors was retrieved.
  @retval EFI_DEVICE_ERROR        The calling processor is an AP.
  @retval EFI_INVALID_PARAMETER   NumberOfProcessors is NULL.
  @retval EFI_INVALID_PARAMETER   NumberOfEnabledProcessors is NULL.

**/
typedef
EFI_STATUS
(EFIAPI *EFI_MP_SERVICES_GET_NUMBER_OF_PROCESSORS)(
  IN  EFI_MP_SERVICES_PROTOCOL  *This,
  OUT UINTN                     *NumberOfProcessors,
  OUT UINTN                     *NumberOfEnabledProcessors
  );

/**
  Gets detailed MP-related information on the requested processor at the
  instant this call is made. This service may only be called from the BSP.

  This service retrieves detailed MP-related information about any processor
  on the platform. Note the following:
    - The processor information may change during the course of a boot session.
    - The information presented here is entirely MP related.

  Information regarding the number of caches and their sizes, frequency of operation,
  slot numbers is all considered platform-related information and is not provided
  by this service.

  @param[in]  This                  A pointer to the EFI_MP_SERVICES_PROTOCOL
                                    instance.
  @param[in]  ProcessorNumber       The handle number of processor.
  @param[out] ProcessorInfoBuffer   A pointer to the buffer where information for
                                    the requested processor is deposited.

  @retval EFI_SUCCESS             Processor information was returned.
  @retval EFI_DEVICE_ERROR        The calling processor is an AP.
  @retval EFI_INVALID_PARAMETER   ProcessorInfoBuffer is NULL.
  @retval EFI_NOT_FOUND           The processor with the handle specified by
                                  ProcessorNumber does not exist in the platform.

**/
typedef
EFI_STATUS
(EFIAPI *EFI_MP_SERVICES_GET_PROCESSOR_INFO)(
  IN  EFI_MP_SERVICES_PROTOCOL   *This,
  IN  UINTN                      ProcessorNumber,
  OUT EFI_PROCESSOR_INFORMATION  *ProcessorInfoBuffer
  );

/**
  This service executes a caller provided function on all enabled APs. APs can
  run either simultaneously or one at a time in sequence. This service supports
  both blocking and non-blocking requests. The non-blocking requests use EFI
  events so the BSP can detect when the APs have finished. This service may only
  be called from the BSP.

  This function is used to dispatch all the enabled APs to the function specified
  by Procedure.  If any enabled AP is busy, then EFI_NOT_READY is returned
  immediately and Procedure is not started on any AP.

  If SingleThread is TRUE, all the enabled APs execute the function specified by
  Procedure one by one, in ascending order of processor handle number. Otherwise,
  all the enabled APs execute the function specified by Procedure simultaneously.

  If WaitEvent is NULL, execution is in blocking mode. The BSP waits until all
  APs finish or TimeoutInMicroSecs expires. Otherwise, execution is in non-blocking
  mode, and the BSP returns from this service without waiting for APs. If a
  non-blocking mode is requested after the UEFI Event EFI_EVENT_GROUP_READY_TO_BOOT
  is signaled, then EFI_UNSUPPORTED must be returned.

  If the timeout specified by TimeoutInMicroseconds expires before all APs return
  from Procedure, then Procedure on the failed APs is terminated. All enabled APs
  are always available for further calls to EFI_MP_SERVICES_PROTOCOL.StartupAllAPs()
  and EFI_MP_SERVICES_PROTOCOL.StartupThisAP(). If FailedCpuList is not NULL, its
  content points to the list of processor handle numbers in which Procedure was
  terminated.

  Note: It is the responsibility of the consumer of the EFI_MP_SERVICES_PROTOCOL.StartupAllAPs()
  to make sure that the nature of the code that is executed on the BSP and the
  dispatched APs is well controlled. The MP Services Protocol does not guarantee
  that the Procedure function is MP-safe. Hence, the tasks that can be run in
  parallel are limited to certain independent tasks and well-controlled exclusive
  code. EFI services and protocols may not be called by APs unless otherwise
  specified.

  In blocking execution mode, BSP waits until all APs finish or
  TimeoutInMicroSeconds expires.

  In non-blocking execution mode, BSP is freed to return to the caller and then
  proceed to the next task without having to wait for APs. The following
  sequence needs to occur in a non-blocking execution mode:

    -# The caller that intends to use this MP Services Protocol in non-blocking
       mode creates WaitEvent by calling the EFI CreateEvent() service.  The caller
       invokes EFI_MP_SERVICES_PROTOCOL.StartupAllAPs(). If the parameter WaitEvent
       is not NULL, then StartupAllAPs() executes in non-blocking mode. It requests
       the function specified by Procedure to be started on all the enabled APs,
       and releases the BSP to continue with other tasks.
    -# The caller can use the CheckEvent() and WaitForEvent() services to check
       the state of the WaitEvent created in step 1.
    -# When the APs complete their task or TimeoutInMicroSecondss expires, the MP
       Service signals WaitEvent by calling the EFI SignalEvent() function. If
       FailedCpuList is not NULL, its content is available when WaitEvent is
       signaled. If all APs returned from Procedure prior to the timeout, then
       FailedCpuList is set to NULL. If not all APs return from Procedure before
       the timeout, then FailedCpuList is filled in with the list of the failed
       APs. The buffer is allocated by MP Service Protocol using AllocatePool().
       It is the caller's responsibility to free the buffer with FreePool() service.
    -# This invocation of SignalEvent() function informs the caller that invoked
       EFI_MP_SERVICES_PROTOCOL.StartupAllAPs() that either all the APs completed
       the specified task or a timeout occurred. The contents of FailedCpuList
       can be examined to determine which APs did not complete the specified task
       prior to the timeout.

  @param[in]  This                    A pointer to the EFI_MP_SERVICES_PROTOCOL
                                      instance.
  @param[in]  Procedure               A pointer to the function to be run on
                                      enabled APs of the system. See type
                                      EFI_AP_PROCEDURE.
  @param[in]  SingleThread            If TRUE, then all the enabled APs execute
                                      the function specified by Procedure one by
                                      one, in ascending order of processor handle
                                      number.  If FALSE, then all the enabled APs
                                      execute the function specified by Procedure
                                      simultaneously.
  @param[in]  WaitEvent               The event created by the caller with CreateEvent()
                                      service.  If it is NULL, then execute in
                                      blocking mode. BSP waits until all APs finish
                                      or TimeoutInMicroSeconds expires.  If it's
                                      not NULL, then execute in non-blocking mode.
                                      BSP requests the function specified by
                                      Procedure to be started on all the enabled
                                      APs, and go on executing immediately. If
                                      all return from Procedure, or TimeoutInMicroSeconds
                                      expires, this event is signaled. The BSP
                                      can use the CheckEvent() or WaitForEvent()
                                      services to check the state of event.  Type
                                      EFI_EVENT is defined in CreateEvent() in
                                      the Unified Extensible Firmware Interface
                                      Specification.
  @param[in]  TimeoutInMicrosecsond   Indicates the time limit in microseconds for
                                      APs to return from Procedure, either for
                                      blocking or non-blocking mode. Zero means
                                      infinity.  If the timeout expires before
                                      all APs return from Procedure, then Procedure
                                      on the failed APs is terminated. All enabled
                                      APs are available for next function assigned
                                      by EFI_MP_SERVICES_PROTOCOL.StartupAllAPs()
                                      or EFI_MP_SERVICES_PROTOCOL.StartupThisAP().
                                      If the timeout expires in blocking mode,
                                      BSP returns EFI_TIMEOUT.  If the timeout
                                      expires in non-blocking mode, WaitEvent
                                      is signaled with SignalEvent().
  @param[in]  ProcedureArgument       The parameter passed into Procedure for
                                      all APs.
  @param[out] FailedCpuList           If NULL, this parameter is ignored. Otherwise,
                                      if all APs finish successfully, then its
                                      content is set to NULL. If not all APs
                                      finish before timeout expires, then its
                                      content is set to address of the buffer
                                      holding handle numbers of the failed APs.
                                      The buffer is allocated by MP Service Protocol,
                                      and it's the caller's responsibility to
                                      free the buffer with FreePool() service.
                                      In blocking mode, it is ready for consumption
                                      when the call returns. In non-blocking mode,
                                      it is ready when WaitEvent is signaled.  The
                                      list of failed CPU is terminated by
                                      END_OF_CPU_LIST.

  @retval EFI_SUCCESS             In blocking mode, all APs have finished before
                                  the timeout expired.
  @retval EFI_SUCCESS             In non-blocking mode, function has been dispatched
                                  to all enabled APs.
  @retval EFI_UNSUPPORTED         A non-blocking mode request was made after the
                                  UEFI event EFI_EVENT_GROUP_READY_TO_BOOT was
                                  signaled.
  @retval EFI_DEVICE_ERROR        Caller processor is AP.
  @retval EFI_NOT_STARTED         No enabled APs exist in the system.
  @retval EFI_NOT_READY           Any enabled APs are busy.
  @retval EFI_TIMEOUT             In blocking mode, the timeout expired before
                                  all enabled APs have finished.
  @retval EFI_INVALID_PARAMETER   Procedure is NULL.

**/
typedef
EFI_STATUS
(EFIAPI *EFI_MP_SERVICES_STARTUP_ALL_APS)(
  IN  EFI_MP_SERVICES_PROTOCOL  *This,
  IN  EFI_AP_PROCEDURE          Procedure,
  IN  BOOLEAN                   SingleThread,
  IN  EFI_EVENT                 WaitEvent               OPTIONAL,
  IN  UINTN                     TimeoutInMicroSeconds,
  IN  VOID                      *ProcedureArgument      OPTIONAL,
  OUT UINTN                     **FailedCpuList         OPTIONAL
  );

/**
  This service lets the caller get one enabled AP to execute a caller-provided
  function. The caller can request the BSP to either wait for the completion
  of the AP or just proceed with the next task by using the EFI event mechanism.
  See EFI_MP_SERVICES_PROTOCOL.StartupAllAPs() for more details on non-blocking
  execution support.  This service may only be called from the BSP.

  This function is used to dispatch one enabled AP to the function specified by
  Procedure passing in the argument specified by ProcedureArgument.  If WaitEvent
  is NULL, execution is in blocking mode. The BSP waits until the AP finishes or
  TimeoutInMicroSecondss expires. Otherwise, execution is in non-blocking mode.
  BSP proceeds to the next task without waiting for the AP. If a non-blocking mode
  is requested after the UEFI Event EFI_EVENT_GROUP_READY_TO_BOOT is signaled,
  then EFI_UNSUPPORTED must be returned.

  If the timeout specified by TimeoutInMicroseconds expires before the AP returns
  from Procedure, then execution of Procedure by the AP is terminated. The AP is
  available for subsequent calls to EFI_MP_SERVICES_PROTOCOL.StartupAllAPs() and
  EFI_MP_SERVICES_PROTOCOL.StartupThisAP().

  @param[in]  This                    A pointer to the EFI_MP_SERVICES_PROTOCOL
                                      instance.
  @param[in]  Procedure               A pointer to the function to be run on the
                                      designated AP of the system. See type
                                      EFI_AP_PROCEDURE.
  @param[in]  ProcessorNumber         The handle number of the AP. The range is
                                      from 0 to the total number of logical
                                      processors minus 1. The total number of
                                      logical processors can be retrieved by
                                      EFI_MP_SERVICES_PROTOCOL.GetNumberOfProcessors().
  @param[in]  WaitEvent               The event created by the caller with CreateEvent()
                                      service.  If it is NULL, then execute in
                                      blocking mode. BSP waits until this AP finish
                                      or TimeoutInMicroSeconds expires.  If it's
                                      not NULL, then execute in non-blocking mode.
                                      BSP requests the function specified by
                                      Procedure to be started on this AP,
                                      and go on executing immediately. If this AP
                                      return from Procedure or TimeoutInMicroSeconds
                                      expires, this event is signaled. The BSP
                                      can use the CheckEvent() or WaitForEvent()
                                      services to check the state of event.  Type
                                      EFI_EVENT is defined in CreateEvent() in
                                      the Unified Extensible Firmware Interface
                                      Specification.
  @param[in]  TimeoutInMicrosecsond   Indicates the time limit in microseconds for
                                      this AP to finish this Procedure, either for
                                      blocking or non-blocking mode. Zero means
                                      infinity.  If the timeout expires before
                                      this AP returns from Procedure, then Procedure
                                      on the AP is terminated. The
                                      AP is available for next function assigned
                                      by EFI_MP_SERVICES_PROTOCOL.StartupAllAPs()
                                      or EFI_MP_SERVICES_PROTOCOL.StartupThisAP().
                                      If the timeout expires in blocking mode,
                                      BSP returns EFI_TIMEOUT.  If the timeout
                                      expires in non-blocking mode, WaitEvent
                                      is signaled with SignalEvent().
  @param[in]  ProcedureArgument       The parameter passed into Procedure on the
                                      specified AP.
  @param[out] Finished                If NULL, this parameter is ignored.  In
                                      blocking mode, this parameter is ignored.
                                      In non-blocking mode, if AP returns from
                                      Procedure before the timeout expires, its
                                      content is set to TRUE. Otherwise, the
                                      value is set to FALSE. The caller can
                                      determine if the AP returned from Procedure
                                      by evaluating this value.

  @retval EFI_SUCCESS             In blocking mode, specified AP finished before
                                  the timeout expires.
  @retval EFI_SUCCESS             In non-blocking mode, the function has been
                                  dispatched to specified AP.
  @retval EFI_UNSUPPORTED         A non-blocking mode request was made after the
                                  UEFI event EFI_EVENT_GROUP_READY_TO_BOOT was
                                  signaled.
  @retval EFI_DEVICE_ERROR        The calling processor is an AP.
  @retval EFI_TIMEOUT             In blocking mode, the timeout expired before
                                  the specified AP has finished.
  @retval EFI_NOT_READY           The specified AP is busy.
  @retval EFI_NOT_FOUND           The processor with the handle specified by
                                  ProcessorNumber does not exist.
  @retval EFI_INVALID_PARAMETER   ProcessorNumber specifies the BSP or disabled AP.
  @retval EFI_INVALID_PARAMETER   Procedure is NULL.

**/
typedef
EFI_STATUS
(EFIAPI *EFI_MP_SERVICES_STARTUP_THIS_AP)(
  IN  EFI_MP_SERVICES_PROTOCOL  *This,
  IN  EFI_AP_PROCEDURE          Procedure,
  IN  UINTN                     ProcessorNumber,
  IN  EFI_EVENT                 WaitEvent               OPTIONAL,
  IN  UINTN                     TimeoutInMicroseconds,
  IN  VOID                      *ProcedureArgument      OPTIONAL,
  OUT BOOLEAN                   *Finished               OPTIONAL
  );

/**
  This service switches the requested AP to be the BSP from that point onward.
  This service changes the BSP for all purposes.   This call can only be performed
  by the current BSP.

  This service switches the requested AP to be the BSP from that point onward.
  This service changes the BSP for all purposes. The new BSP can take over the
  execution of the old BSP and continue seamlessly from where the old one left
  off. This service may not be supported after the UEFI Event EFI_EVENT_GROUP_READY_TO_BOOT
  is signaled.

  If the BSP cannot be switched prior to the return from this service, then
  EFI_UNSUPPORTED must be returned.

  @param[in] This              A pointer to the EFI_MP_SERVICES_PROTOCOL instance.
  @param[in] ProcessorNumber   The handle number of AP that is to become the new
                               BSP. The range is from 0 to the total number of
                               logical processors minus 1. The total number of
                               logical processors can be retrieved by
                               EFI_MP_SERVICES_PROTOCOL.GetNumberOfProcessors().
  @param[in] EnableOldBSP      If TRUE, then the old BSP will be listed as an
                               enabled AP. Otherwise, it will be disabled.

  @retval EFI_SUCCESS             BSP successfully switched.
  @retval EFI_UNSUPPORTED         Switching the BSP cannot be completed prior to
                                  this service returning.
  @retval EFI_UNSUPPORTED         Switching the BSP is not supported.
  @retval EFI_DEVICE_ERROR        The calling processor is an AP.
  @retval EFI_NOT_FOUND           The processor with the handle specified by
                                  ProcessorNumber does not exist.
  @retval EFI_INVALID_PARAMETER   ProcessorNumber specifies the current BSP or
                                  a disabled AP.
  @retval EFI_NOT_READY           The specified AP is busy.

**/
typedef
EFI_STATUS
(EFIAPI *EFI_MP_SERVICES_SWITCH_BSP)(
  IN EFI_MP_SERVICES_PROTOCOL  *This,
  IN  UINTN                    ProcessorNumber,
  IN  BOOLEAN                  EnableOldBSP
  );

/**
  This service lets the caller enable or disable an AP from this point onward.
  This service may only be called from the BSP.

  This service allows the caller enable or disable an AP from this point onward.
  The caller can optionally specify the health status of the AP by Health. If
  an AP is being disabled, then the state of the disabled AP is implementation
  dependent. If an AP is enabled, then the implementation must guarantee that a
  complete initialization sequence is performed on the AP, so the AP is in a state
  that is compatible with an MP operating system. This service may not be supported
  after the UEFI Event EFI_EVENT_GROUP_READY_TO_BOOT is signaled.

  If the enable or disable AP operation cannot be completed prior to the return
  from this service, then EFI_UNSUPPORTED must be returned.

  @param[in] This              A pointer to the EFI_MP_SERVICES_PROTOCOL instance.
  @param[in] ProcessorNumber   The handle number of AP.
                               The range is from 0 to the total number of
                               logical processors minus 1. The total number of
                               logical processors can be retrieved by
                               EFI_MP_SERVICES_PROTOCOL.GetNumberOfProcessors().
  @param[in] EnableAP          Specifies the new state for the processor for
                               enabled, FALSE for disabled.
  @param[in] HealthFlag        If not NULL, a pointer to a value that specifies
                               the new health status of the AP. This flag
                               corresponds to StatusFlag defined in
                               EFI_MP_SERVICES_PROTOCOL.GetProcessorInfo(). Only
                               the PROCESSOR_HEALTH_STATUS_BIT is used. All other
                               bits are ignored.  If it is NULL, this parameter
                               is ignored.

  @retval EFI_SUCCESS             The specified AP was enabled or disabled successfully.
  @retval EFI_UNSUPPORTED         Enabling or disabling an AP cannot be completed
                                  prior to this service returning.
  @retval EFI_UNSUPPORTED         Enabling or disabling an AP is not supported.
  @retval EFI_DEVICE_ERROR        The calling processor is an AP.
  @retval EFI_NOT_FOUND           Processor with the handle specified by ProcessorNumber
                                  does not exist.
  @retval EFI_INVALID_PARAMETER   ProcessorNumber specifies the BSP.

**/
typedef
EFI_STATUS
(EFIAPI *EFI_MP_SERVICES_ENABLEDISABLEAP)(
  IN  EFI_MP_SERVICES_PROTOCOL  *This,
  IN  UINTN                     ProcessorNumber,
  IN  BOOLEAN                   EnableAP,
  IN  UINT32                    *HealthFlag OPTIONAL
  );

/**
  This return the handle number for the calling processor.  This service may be
  called from the BSP and APs.

  This service returns the processor handle number for the calling processor.
  The returned value is in the range from 0 to the total number of logical
  processors minus 1. The total number of logical processors can be retrieved
  with EFI_MP_SERVICES_PROTOCOL.GetNumberOfProcessors(). This service may be
  called from the BSP and APs. If ProcessorNumber is NULL, then EFI_INVALID_PARAMETER
  is returned. Otherwise, the current processors handle number is returned in
  ProcessorNumber, and EFI_SUCCESS is returned.

  @param[in] This              A pointer to the EFI_MP_SERVICES_PROTOCOL instance.
  @param[in] ProcessorNumber   Pointer to the handle number of AP.
                               The range is from 0 to the total number of
                               logical processors minus 1. The total number of
                               logical processors can be retrieved by
                               EFI_MP_SERVICES_PROTOCOL.GetNumberOfProcessors().

  @retval EFI_SUCCESS             The current processor handle number was returned
                                  in ProcessorNumber.
  @retval EFI_INVALID_PARAMETER   ProcessorNumber is NULL.

**/
typedef
EFI_STATUS
(EFIAPI *EFI_MP_SERVICES_WHOAMI)(
  IN EFI_MP_SERVICES_PROTOCOL  *This,
  OUT UINTN                    *ProcessorNumber
  );

///
/// When installed, the MP Services Protocol produces a collection of services
/// that are needed for MP management.
///
/// Before the UEFI event EFI_EVENT_GROUP_READY_TO_BOOT is signaled, the module
/// that produces this protocol is required to place all APs into an idle state
/// whenever the APs are disabled or the APs are not executing code as requested
/// through the StartupAllAPs() or StartupThisAP() services. The idle state of
/// an AP before the UEFI event EFI_EVENT_GROUP_READY_TO_BOOT is signaled is
/// implementation dependent.
///
/// After the UEFI event EFI_EVENT_GROUP_READY_TO_BOOT is signaled, all the APs
/// must be placed in the OS compatible CPU state as defined by the UEFI
/// Specification. Implementations of this protocol may use the UEFI event
/// EFI_EVENT_GROUP_READY_TO_BOOT to force APs into the OS compatible state as
/// defined by the UEFI Specification. Modules that use this protocol must
/// guarantee that all non-blocking mode requests on all APs have been completed
/// before the UEFI event EFI_EVENT_GROUP_READY_TO_BOOT is signaled. Since the
/// order that event notification functions in the same event group are executed
/// is not deterministic, an event of type EFI_EVENT_GROUP_READY_TO_BOOT cannot
/// be used to guarantee that APs have completed their non-blocking mode requests.
///
/// When the UEFI event EFI_EVENT_GROUP_READY_TO_BOOT is signaled, the StartAllAPs()
/// and StartupThisAp() services must no longer support non-blocking mode requests.
/// The support for SwitchBSP() and EnableDisableAP() may no longer be supported
/// after this event is signaled. Since UEFI Applications and UEFI OS Loaders
/// execute after the UEFI event EFI_EVENT_GROUP_READY_TO_BOOT is signaled, these
/// UEFI images must be aware that the functionality of this protocol may be reduced.
///
struct _EFI_MP_SERVICES_PROTOCOL {
  EFI_MP_SERVICES_GET_NUMBER_OF_PROCESSORS  GetNumberOfProcessors;
  EFI_MP_SERVICES_GET_PROCESSOR_INFO        GetProcessorInfo;
  EFI_MP_SERVICES_STARTUP_ALL_APS           StartupAllAPs;
  EFI_MP_SERVICES_STARTUP_THIS_AP           StartupThisAP;
  EFI_MP_SERVICES_SWITCH_BSP                SwitchBSP;
  EFI_MP_SERVICES_ENABLEDISABLEAP           EnableDisableAP;
  EFI_MP_SERVICES_WHOAMI                    WhoAmI;
};

extern EFI_GUID gEfiMpServiceProtocolGuid;

#endif
