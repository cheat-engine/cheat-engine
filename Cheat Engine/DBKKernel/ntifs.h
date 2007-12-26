/*
    This is a free version of the file ntifs.h, release 46.
    The purpose of this include file is to build file system and
    file system filter drivers for Windows NT®, Windows® 2000 and
    Windows® XP.
    Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004 Bo Brantén.
    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.
    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

    The GNU General Public License is also available from:
    http://www.gnu.org/copyleft/gpl.html

    Windows and Windows NT are either registered trademarks or trademarks of
    Microsoft Corporation in the United States and/or other countries.

    DISCLAIMER: I do not encourage anyone to use this include file to build
    drivers used in production. The information in this include file is
    incomplete and intended only as an studying companion. The information
    has been found in books, magazines, on the Internet and received from
    contributors. Some of the information in this file may not be available
    in other publications intended for similar use, these should be used with
    extra care. Some of the information in this file may have different names
    than in other publications even though they describe the same thing.

    Please send comments, corrections and contributions to bosse@acc.umu.se

    The most recent version of this file is available from:
    http://www.acc.umu.se/~bosse/ntifs.h

    Thanks to:
      Andrey Shedel, Luigi Mori, Louis Joubert, Itai Shaham, David Welch,
      Emanuele Aliberti, Anton Altaparmakov, Dan Partelly, Mamaich,
      Yossi Yaffe, Gunnar André Dalsnes, Vadim V Vorobev, Ashot Oganesyan K,
      Oleg Nikityenko, Matt Wu, Tomas Olsson, Raaf, Anthony Choi,
      Alexey Logachyov, Marc-Antoine Ruel, Vyacheslav I. Levtchenko
      and Darja Isaksson.

    Revision history:

    46. 2004-06-08
        Added:
          Data types:
            TOKEN_OBJECT

    45. 2004-06-06
        Corrected:
          SERVICE_DESCRIPTOR_TABLE
        Added:
          Defines:
            TOKEN_SESSION_NOT_REFERENCED
            TOKEN_SANDBOX_INERT
            TOKEN_HAS_IMPERSONATE_PRIVILEGE
          Function prototypes:
            FsRtlDissectName
            RtlOemStringToCountedUnicodeSize
            RtlOemStringToUnicodeSize
            RtlOemStringToUnicodeString
            RtlUnicodeStringToOemSize
            RtlUnicodeStringToOemString
            RtlxOemStringToUnicodeSize
            RtlxUnicodeStringToOemSize

    44. 2003-05-06
        Added:
          Function prototypes:
            InbvAcquireDisplayOwnership
            InbvCheckDisplayOwnership
            InbvDisplayString
            InbvEnableBootDriver
            InbvEnableDisplayString
            InbvInstallDisplayStringFilter
            InbvIsBootDriverInstalled
            InbvNotifyDisplayOwnershipLost
            InbvResetDisplay
            InbvSetScrollRegion
            InbvSetTextColor
            InbvSolidColorFill

    43. 2003-04-07
        Added:
          Data types:
            MCB
          Function prototypes:
            FsRtlAddMcbEntry
            FsRtlInitializeMcb
            FsRtlLookupLastMcbEntry
            FsRtlLookupMcbEntry
            FsRtlNotifyFilterChangeDirectory
            FsRtlNotifyFilterReportChange
            FsRtlNumberOfRunsInMcb
            FsRtlRemoveMcbEntry
            FsRtlTruncateMcb
            FsRtlUninitializeMcb

    42. 2003-03-30
        Corrected:
          SYSTEM_CACHE_INFORMATION
          SYSTEM_INFORMATION_CLASS
        Added:
          Data types:
            SYSTEM_XXX_INFORMATION
            THREAD_STATE

    41. 2003-01-03
        Corrected:
          CcMapData
          PsDereferenceImpersonationToken
          PsDereferencePrimaryToken
          PsGetProcessExitTime
          PsReferencePrimaryToken
        Added:
          Defines:
            MAP_XXX
          Function prototypes:
            CcMdlWriteAbort
            PsAssignImpersonationToken
            PsChargeProcessNonPagedPoolQuota
            PsChargeProcessPagedPoolQuota
            PsChargeProcessPoolQuota
            PsDisableImpersonation
            PsImpersonateClient
            PsIsSystemThread
            PsRestoreImpersonation
            SeDeleteAccessState
            ZwOpenProcessTokenEx
            ZwOpenThreadTokenEx

    40. 2002-10-02
        Corrected:
          HANDLE_TABLE_ENTRY
        Added:
          Defines:
            FSRTL_FLAG_ADVANCED_HEADER
            FSRTL_FLAG2_SUPPORTS_FILTER_CONTEXTS
            FSRTL_FLAG2_PURGE_WHEN_MAPPED
          Data types:
            FILE_ID_BOTH_DIR_INFORMATION
            FILE_ID_FULL_DIR_INFORMATION

    39. 2002-08-04
        Added:
          Data types:
            LARGE_MCB
          Function prototypes:
            FsRtlAddLargeMcbEntry
            FsRtlGetNextLargeMcbEntry
            FsRtlInitializeLargeMcb
            FsRtlLookupLargeMcbEntry
            FsRtlLookupLastLargeMcbEntry
            FsRtlLookupLastLargeMcbEntryAndIndex
            FsRtlNumberOfRunsInLargeMcb
            FsRtlRemoveLargeMcbEntry
            FsRtlResetLargeMcb
            FsRtlSplitLargeMcb
            FsRtlTruncateLargeMcb
            FsRtlUninitializeLargeMcb

    38. 2002-06-30
        Added:
          Defines:
            FILE_READ_ONLY_VOLUME
          Function prototypes:
            FsRtlAllocateResource
            FsRtlIncrementCcFastReadNotPossible
            FsRtlIncrementCcFastReadNoWait
            FsRtlIncrementCcFastReadResourceMiss
            FsRtlIncrementCcFastReadWait
            KeIsAttachedProcess
            KeIsExecutingDpc
            KeRevertToUserAffinityThread
            KeUpdateSystemTime
            PsGetCurrentProcessSessionId
            PsGetCurrentThreadPreviousMode
            PsGetCurrentThreadStackBase
            PsGetCurrentThreadStackLimit
            RtlGetNtGlobalFlags

    37. 2002-05-18
        Uppdated for Windows XP:
          EPROCESS
          ETHREAD
          KPROCESS
          KTHREAD
          MMSUPPORT_FLAGS
          MMSUPPORT
          PRIVATE_CACHE_MAP_FLAGS
          PRIVATE_CACHE_MAP
          SHARED_CACHE_MAP
        Corrected:
          VACB
        Added:
          Data types:
            EPROCESS_QUOTA_ENTRY
            EPROCESS_QUOTA_BLOCK
            EX_FAST_REF
            EX_PUSH_LOCK
            EX_RUNDOWN_REF
            PAGEFAULT_HISTORY
            SE_AUDIT_PROCESS_CREATION_INFO
            SECTION_OBJECT
            TERMINATION_PORT

    36. 2002-05-14
        Corrected:
          FILE_FS_FULL_SIZE_INFORMATION

    35. 2002-03-23
        Added:
          Defines:
            COMPRESSION_XXX
          Data types:
            COMPRESSED_DATA_INFO
            OBJECT_HEADER
            VAD_HEADER
          Function prototypes:
            CcWaitForCurrentLazyWriterActivity
            FsRtlCheckOplock
            FsRtlCurrentBatchOplock
            FsRtlDeregisterUncProvider
            FsRtlInitializeOplock
            FsRtlOplockFsctrl
            FsRtlOplockIsFastIoPossible
            FsRtlRegisterUncProvider
            FsRtlUninitializeOplock
            RtlCompressBuffer
            RtlCompressChunks
            RtlDecompressBuffer
            RtlDecompressChunks
            RtlDecompressFragment
            RtlDescribeChunk
            RtlGetCompressionWorkSpaceSize
            RtlReserveChunk

    34. 2002-02-14
        Corrected:
          HARDWARE_PTE
          Changed the use of _WIN32_WINNT to VER_PRODUCTBUILD since _WIN32_WINNT
          is incorrectly defined in the Windows 2000 build environment included
          in the Windows XP DDK.

    33. 2002-01-20
        Added:
          Function prototypes:
            PsDereferenceImpersonationToken
            PsDereferencePrimaryToken

    32. 2002-01-18
        Corrected:
          ObReferenceObjectByName
          FILE_FS_OBJECT_ID_INFORMATION
          FILE_OBJECTID_INFORMATION
        Added:
          Externals:
            IoDriverObjectType
            SeExports
          Defines:
            FILE_ACTION_XXX
            FSCTL_XXX
            IO_FILE_OBJECT_XXX
            IRP_BEING_VERIFIED
            TOKEN_XXX
          Data types:
            DEVICE_MAP
            FILE_TRACKING_INFORMATION
            SE_EXPORTS
          Function prototypes:
            SeEnableAccessToExports

    31. 2001-12-23
        Corrected:
          QueryQuota in EXTENDED_IO_STACK_LOCATION
          FILE_LOCK
          CcPinMappedData
          CcPinRead
          CcPreparePinWrite
          FsRtlFastUnlockAll
          FsRtlFastUnlockAllByKey
          FsRtlFastUnlockSingle
          FsRtlInitializeFileLock
          FsRtlPrivateLock
          FsRtlProcessFileLock
          MmForceSectionClosed
          MmIsRecursiveIoFault
          SeImpersonateClient
          SeImpersonateClientEx
        Added:
          Defines:
            More FSRTL_FLAG_XXX
            PIN_XXX
            VACB_XXX
          Data types:
            REPARSE_DATA_BUFFER
          Function prototypes:
            CcCopyWriteWontFlush
            CcGetFileSizePointer
            CcGetFlushedValidData
            CcIsFileCached
            CcRemapBcb
            ExDisableResourceBoostLite
            ExQueryPoolBlockSize
            FsRtlAllocateFileLock
            FsRtlAreThereCurrentFileLocks
            FsRtlFastLock
            FsRtlFreeFileLock
            IoCheckDesiredAccess
            IoCheckEaBufferValidity
            IoCheckFunctionAccess
            IoCheckQuotaBufferValidity
            IoCreateStreamFileObjectLite
            IoFastQueryNetworkAttributes
            IoGetRequestorProcessId
            IoIsFileOpenedExclusively
            IoIsSystemThread
            IoIsValidNameGraftingBuffer
            IoSynchronousPageWrite
            IoThreadToProcess
            KeInitializeQueue
            KeInsertHeadQueue
            KeInsertQueue
            KeReadStateQueue
            KeRemoveQueue
            KeRundownQueue
            MmSetAddressRangeModified
            ObGetObjectPointerCount
            ObMakeTemporaryObject
            ObQueryObjectAuditingByHandle
            PsChargePoolQuota
            PsReturnPoolQuota
            SeAppendPrivileges
            SeAuditingFileEvents
            SeAuditingFileOrGlobalEvents
            SeCreateClientSecurity
            SeCreateClientSecurityFromSubjectContext
            SeDeleteClientSecurity
            SeDeleteObjectAuditAlarm
            SeFreePrivileges
            SeLockSubjectContext
            SeOpenObjectAuditAlarm
            SeOpenObjectForDeleteAuditAlarm
            SePrivilegeCheck
            SeQueryAuthenticationIdToken
            SeQuerySecurityDescriptorInfo
            SeQuerySessionIdToken
            SeSetAccessStateGenericMapping
            SeSetSecurityDescriptorInfo
            SeSetSecurityDescriptorInfoEx
            SeTokenIsAdmin
            SeTokenIsRestricted
            SeTokenType
            SeUnlockSubjectContext

    30. 2001-10-24
        Corrected:
          KINTERRUPT
          OBJECT_TYPE
        Added:
          Defines:
            More FSCTL_XXX
          Data types:
            BITMAP_RANGE
            CreateMailslot in EXTENDED_IO_STACK_LOCATION
            CreatePipe in EXTENDED_IO_STACK_LOCATION
            QueryQuota in EXTENDED_IO_STACK_LOCATION
            MAILSLOT_CREATE_PARAMETERS
            MBCB
            NAMED_PIPE_CREATE_PARAMETERS
            PRIVATE_CACHE_MAP_FLAGS
            PRIVATE_CACHE_MAP
            SECURITY_CLIENT_CONTEXT
            SHARED_CACHE_MAP
            VACB
          Function prototypes:
            HalQueryRealTimeClock
            HalSetRealTimeClock
            PsGetProcessExitTime
            PsIsThreadTerminating
            PsLookupProcessThreadByCid
            PsLookupThreadByThreadId
            SeQueryAuthenticationIdToken
          Externals:
            KeServiceDescriptorTable
            SePublicDefaultDacl
            SeSystemDefaultDacl

    29. 2001-10-06
        Added:
          Defines:
            FSRTL_VOLUME_XXX
          Function prototypes:
            FsRtlNotifyChangeDirectory
            FsRtlNotifyReportChange
            FsRtlNotifyVolumeEvent

    28. 2001-09-16
        Added:
          Function prototypes:
            FsRtlNotifyInitializeSync
            FsRtlNotifyUninitializeSync
            SeImpersonateClientEx
            SeReleaseSubjectContext

    27. 2001-08-25
        Corrected:
          KPROCESS
          FILE_LOCK_ANCHOR
          FsRtlNormalizeNtstatus
          RtlSecondsSince1970ToTime
          RtlTimeToSecondsSince1970
          SeQueryInformationToken
        Added:
          Defines:
            FS_LFN_APIS
          Data types:
            FILE_LOCK_ENTRY
            FILE_SHARED_LOCK_ENTRY
            FILE_EXCLUSIVE_LOCK_ENTRY
          Function prototypes:
            FsRtlCheckLockForReadAccess
            FsRtlCheckLockForWriteAccess
            FsRtlFastUnlockAll
            FsRtlFastUnlockAllByKey
            FsRtlFastUnlockSingle
            FsRtlGetFileSize
            FsRtlGetNextFileLock
            FsRtlInitializeFileLock
            FsRtlPrivateLock
            FsRtlProcessFileLock
            FsRtlUninitializeFileLock
            IoUnregisterFsRegistrationChange
            PsLookupProcessByProcessId
            SeQuerySubjectContextToken

    26. 2001-04-28
        Added:
          Defines:
            FSCTL_XXX
          Data types:
            RTL_SPLAY_LINKS
            TUNNEL
          Function prototypes:
            FsRtlAddToTunnelCache
            FsRtlDeleteKeyFromTunnelCache
            FsRtlDeleteTunnelCache
            FsRtlFindInTunnelCache
            FsRtlInitializeTunnelCache
            IoSetDeviceToVerify
            KeInitializeApc
            KeInsertQueueApc
            SeQueryInformationToken

    25. 2001-04-05
        Corrected:
          RtlImageNtHeader
          LPC_XXX
          OBJECT_BASIC_INFO
        Added:
          Defines:
            SID_REVISION
          Data types:
            DIRECTORY_BASIC_INFORMATION
            KINTERRUPT
            OBJECT_HANDLE_ATTRIBUTE_INFO
            PROCESS_PRIORITY_CLASS
            SECTION_BASIC_INFORMATION
            SECTION_IMAGE_INFORMATION
            SECTION_INFORMATION_CLASS
          Function prototypes:
            RtlSecondsSince1970ToTime
            RtlTimeToSecondsSince1970
            ZwAdjustPrivilegesToken
            ZwAlertThread
            ZwAccessCheckAndAuditAlarm
            ZwClearEvent
            ZwCloseObjectAuditAlarm
            ZwCreateSection
            ZwCreateSymbolicLinkObject
            ZwDuplicateToken
            ZwFlushInstructionCache
            ZwFlushVirtualMemory
            ZwInitiatePowerAction
            ZwLoadKey
            ZwNotifyChangeKey
            ZwOpenThread
            ZwPowerInformation
            ZwPulseEvent
            ZwQueryDefaultLocale
            ZwQueryDefaultUILanguage
            ZwQueryInformationProcess
            ZwQueryInstallUILanguage
            ZwQuerySection
            ZwReplaceKey
            ZwResetEvent
            ZwRestoreKey
            ZwSaveKey
            ZwSetDefaultLocale
            ZwSetDefaultUILanguage
            ZwSetEvent
            ZwSetInformationObject
            ZwSetInformationProcess
            ZwSetSecurityObject
            ZwSetSystemTime
            ZwTerminateProcess
            ZwUnloadKey
            ZwWaitForSingleObject
            ZwWaitForMultipleObjects
            ZwYieldExecution
        Removed functions that is not exported in kernel mode:
          CcZeroEndOfLastPage
          RtlAllocateAndInitializeSid
          ZwAcceptConnectPort
          ZwCompleteConnectPort
          ZwCreatePort
          ZwCreateProcess
          ZwCreateThread
          ZwFlushBuffersFile
          ZwGetContextThread
          ZwImpersonateClientOfPort
          ZwListenPort
          ZwLockFile
          ZwNotifyChangeDirectoryFile
          ZwQueryInformationPort
          ZwReadRequestData
          ZwReplyPort
          ZwReplyWaitReceivePort
          ZwReplyWaitReplyPort
          ZwRequestPort
          ZwUnlockFile
          ZwWriteRequestData

    24. 2001-03-08
        Corrected:
          EPROCESS
          ETHREAD
          FAST_IO_POSSIBLE
          QueryEa in EXTENDED_IO_STACK_LOCATION
        Added:
          Defines:
            Some more flags for FileSystemAttributes
          Data types:
            EXCEPTION_REGISTRATION_RECORD
            FILE_FS_FULL_SIZE_INFORMATION
            FILE_FS_OBJECT_ID_INFORMATION
            HANDLE_TABLE_ENTRY
            IO_CLIENT_EXTENSION
            PS_IMPERSONATION_INFORMATION
            SetEa and SetQuota in EXTENDED_IO_STACK_LOCATION
          Function prototypes:
            IoPageRead
            KeStackAttachProcess
            KeUnstackDetachProcess
            MmMapViewOfSection
            RtlSelfRelativeToAbsoluteSD
            SeCreateAccessState

    23. 2001-01-29
        Corrected:
          FSCTL_GET_VOLUME_INFORMATION
          FSCTL_READ_MFT_RECORD
          HARDWARE_PTE
          EPROCESS
          ETHREAD
          KAPC_STATE
          KPROCESS
          KTHREAD
          MMSUPPORT
        Added:
          Data types:
            KGDTENTRY
            KIDTENTRY
            MMSUPPORT_FLAGS

    22. 2000-12-23
        Corrected:
          EPROCESS
          KPROCESS
        Added:
          Data types:
            HARDWARE_PTE
            MMSUPPORT

    21. 2000-12-12
        Added:
          Defines:
            IO_TYPE_XXX
            OB_TYPE_XXX
            THREAD_STATE_XXX
          Data types:
            EPROCESS
            ETHREAD
            KAPC_STATE
            KEVENT_PAIR
            KPROCESS
            KTHREAD
            KQUEUE
            SERVICE_DESCRIPTOR_TABLE
            TEB

    20. 2000-12-03
        Added:
          Data types:
            OBJECT_TYPE
          Function prototypes:
            ObCreateObject
            ObInsertObject
            ObReferenceObjectByName

    19. 2000-11-25
        Removed a name from credits since the person want to be anonymous.

    18. 2000-10-13
        Corrected:
          PsReferenceImpersonationToken
        Added:
          Defines:
            FILE_PIPE_XXX
            LPC_XXX
            MAILSLOT_XXX
            PORT_XXX
            FSCTL_GET_VOLUME_INFORMATION
            FSCTL_READ_MFT_RECORD
            FSCTL_MAILSLOT_PEEK
            FSCTL_PIPE_XXX
          Data types:
            PORT_INFORMATION_CLASS
            BITMAP_DESCRIPTOR
            FILE_MAILSLOT_XXX
            FILE_PIPE_XXX
            MAPPING_PAIR
            GET_RETRIEVAL_DESCRIPTOR
            LPC_XXX
            MOVEFILE_DESCRIPTOR
          Function prototypes:
            InitializeMessageHeader
            MmForceSectionClosed
            ZwAcceptConnectPort
            ZwCompleteConnectPort
            ZwConnectPort
            ZwCreateEvent
            ZwCreatePort
            ZwImpersonateClientOfPort
            ZwListenPort
            ZwQueryInformationPort
            ZwReadRequestData
            ZwReplyPort
            ZwReplyWaitReceivePort
            ZwReplyWaitReplyPort
            ZwRequestPort
            ZwRequestWaitReplyPort
            ZwWriteRequestData

    17. 2000-05-21
        Added:
          Function prototypes:
            PsRevertToSelf
            SeCreateClientSecurity
            SeImpersonateClient
            ZwDuplicateObject

    16. 2000-03-28
        Added:
          Defines:
            FILE_STORAGE_TYPE_XXX
            FILE_VC_XXX
            IO_CHECK_CREATE_PARAMETERS
            IO_ATTACH_DEVICE
            IO_ATTACH_DEVICE_API
            IO_COMPLETION_XXX
          Data types:
            IO_COMPLETION_INFORMATION_CLASS
            OBJECT_INFO_CLASS
            SYSTEM_INFORMATION_CLASS
            FILE_LOCK_ANCHOR
            IO_COMPLETION_BASIC_INFORMATION
            OBJECT_BASIC_INFO
            OBJECT_NAME_INFO
            OBJECT_PROTECTION_INFO
            OBJECT_TYPE_INFO
            OBJECT_ALL_TYPES_INFO
            SYSTEM_CACHE_INFORMATION
          Function prototypes:
            FsRtlAllocatePool
            FsRtlAllocatePoolWithQuota
            FsRtlAllocatePoolWithQuotaTag
            FsRtlAllocatePoolWithTag
            FsRtlAreNamesEqual
            FsRtlFastCheckLockForRead
            FsRtlFastCheckLockForWrite
            FsRtlMdlReadComplete
            FsRtlMdlWriteComplete
            FsRtlNormalizeNtstatus
            RtlAllocateHeap
            RtlCreateHeap
            RtlDestroyHeap
            RtlFreeHeap
            RtlImageNtHeader
            ZwQueryObject
            ZwQuerySystemInformation
            ZwSetSystemInformation

    15. 2000-03-15
        Corrected:
          Renamed IoQueryFileVolumeInformation to IoQueryVolumeInformation
        Comment on:
          CcZeroEndOfLastPage

    14. 2000-03-12
        Corrected:
          IoCreateFile
        Added:
          #if (_WIN32_WINNT < 0x0500)/#endif around stuff that is included in
          the Windows 2000 DDK but is missing in the Windows NT 4.0 DDK.
          ZwOpenEvent

    13. 2000-02-08
        Corrected:
          PsReferenceImpersonationToken
        Comment on:
          RtlAllocateAndInitializeSid

    12. 1999-10-18
        Corrected:
          FILE_COMPRESSION_INFORMATION
        Added:
          Defines:
            ACCESS_ALLOWED_ACE_TYPE
            ACCESS_DENIED_ACE_TYPE
            SYSTEM_AUDIT_ACE_TYPE
            SYSTEM_ALARM_ACE_TYPE
            ANSI_DOS_STAR/QM/DOT
            DOS_STAR/QM/DOT
            FILE_EA_TYPE_XXX
            FILE_NEED_EA
            FILE_OPBATCH_BREAK_UNDERWAY
            SECURITY_WORLD_SID_AUTHORITY
            SECURITY_WORLD_RID
          Data types:
            POBJECT
            FILE_STORAGE_TYPE
            FILE_COMPLETION_INFORMATION
            FILE_COPY_ON_WRITE_INFORMATION
            FILE_FS_CONTROL_INFORMATION
            FILE_GET_EA_INFORMATION
            FILE_GET_QUOTA_INFORMATION
            FILE_OBJECTID_INFORMATION
            FILE_OLE_CLASSID_INFORMATION
            FILE_OLE_ALL_INFORMATION
            FILE_OLE_DIR_INFORMATION
            FILE_OLE_INFORMATION
            FILE_OLE_STATE_BITS_INFORMATION
            FILE_QUOTA_INFORMATION
          Function prototypes:
            HalDisplayString
            HalMakeBeep
            IoGetRequestorProcess
            ObQueryNameString
            ProbeForWrite
            RtlAbsoluteToSelfRelativeSD
            RtlGetDaclSecurityDescriptor
            RtlGetGroupSecurityDescriptor
            RtlGetOwnerSecurityDescriptor
            RtlInitializeSid
            RtlSetGroupSecurityDescriptor
            RtlSetOwnerSecurityDescriptor
            RtlSetSaclSecurityDescriptor
            ZwDeleteValueKey
            ZwDisplayString
            ZwQueryDirectoryObject

    11. 1999-10-13
        Corrected:
          ZwOpenProcessToken
          ZwOpenThreadToken
        Added:
          Function prototypes:
            RtlAllocateAndInitializeSid
            RtlCopySid
            RtlEqualSid
            RtlFillMemoryUlong
            RtlIsNameLegalDOS8Dot3
            RtlLengthRequiredSid
            RtlLengthSid
            RtlNtStatusToDosError
            RtlSubAuthorityCountSid
            RtlSubAuthoritySid
            RtlValidSid

    10. 1999-07-15
        Corrected:
          RtlConvertSidToUnicodeString
        Added:
          Externals:
            FsRtlLegalAnsiCharacterArray
            NtBuildNumber
          Defines:
            FSRTL_WILD_CHARACTER
            FlagOn
            FsRtlIsUnicodeCharacterWild
          Structures:
            FILE_ACCESS_INFORMATION
            FILE_MODE_INFORMATION
            GENERATE_NAME_CONTEXT
          Function prototypes:
            FsRtlDoesNameContainWildCards
            FsRtlIsNameInExpression
            IoSetInformation
            RtlGenerate8dot3Name
            ZwQuerySecurityObject

    9. 1999-07-12
       Corrected:
         EXTENDED_IO_STACK_LOCATION
         QueryDirectory in EXTENDED_IO_STACK_LOCATION
         ZwCreateThread
       Added:
         Structures:
           INITIAL_TEB
         Function prototypes:
           ZwQuerySymbolicLinkObject

    8. 1999-06-07
       Corrected:
         ZwOpenProcessToken
         ZwOpenThreadToken
       Added:
         Defines:
           FILE_OPLOCK_BROKEN_TO_LEVEL_2
           FILE_OPLOCK_BROKEN_TO_NONE
           FILE_CASE_SENSITIVE_SEARCH
           FILE_CASE_PRESERVED_NAMES
           FILE_UNICODE_ON_DISK
           FILE_PERSISTENT_ACLS
           FILE_FILE_COMPRESSION
           FILE_VOLUME_IS_COMPRESSED
           FSRTL_FLAG_ACQUIRE_MAIN_RSRC_EX
           FSRTL_FLAG_ACQUIRE_MAIN_RSRC_SH
           IOCTL_REDIR_QUERY_PATH
         Structures:
           FILE_FS_LABEL_INFORMATION
           PATHNAME_BUFFER
         In IO_STACK_LOCATION:
           FileSystemControl
           LockControl
           SetVolume
         Function prototypes:
           FsRtlCopyRead
           FsRtlCopyWrite
           IoVerifyVolume

    7. 1999-06-05
       Added:
         defines for TOKEN_XXX
         SID_NAME_USE
         TOKEN_INFORMATION_CLASS
         TOKEN_TYPE
         FILE_FS_ATTRIBUTE_INFORMATION
         FILE_FS_SIZE_INFORMATION
         SID_IDENTIFIER_AUTHORITY
         SID
         SID_AND_ATTRIBUTES
         TOKEN_CONTROL
         TOKEN_DEFAULT_DACL
         TOKEN_GROUPS
         TOKEN_OWNER
         TOKEN_PRIMARY_GROUP
         TOKEN_PRIVILEGES
         TOKEN_SOURCE
         TOKEN_STATISTICS
         TOKEN_USER
         IoCreateFile
         IoGetAttachedDevice
         IoGetBaseFileSystemDeviceObject
         PsReferenceImpersonationToken
         PsReferencePrimaryToken
         RtlConvertSidToUnicodeString
         SeCaptureSubjectContext
         SeMarkLogonSessionForTerminationNotification
         SeRegisterLogonSessionTerminatedRoutine
         SeUnregisterLogonSessionTerminatedRoutine
         ZwOpenProcessToken
         ZwOpenThreadToken
         ZwQueryInformationToken

    6. 1999-05-10
       Corrected declarations of Zw functions.
       Added:
         ZwCancelIoFile
         ZwDeleteFile
         ZwFlushBuffersFile
         ZwFsControlFile
         ZwLockFile
         ZwNotifyChangeDirectoryFile
         ZwOpenFile
         ZwQueryEaFile
         ZwSetEaFile
         ZwSetVolumeInformationFile
         ZwUnlockFile

    5. 1999-05-09
       Added:
         defines for FILE_ACTION_XXX and FILE_NOTIFY_XXX
         FILE_FS_VOLUME_INFORMATION
         RETRIEVAL_POINTERS_BUFFER
         STARTING_VCN_INPUT_BUFFER
         FsRtlNotifyFullReportChange

    4. 1999-04-11
       Corrected:
         ZwCreateThread
       Added:
         define _GNU_NTIFS_

    3. 1999-03-30
       Added:
         defines for MAP_XXX, MEM_XXX and SEC_XXX
         FILE_BOTH_DIR_INFORMATION
         FILE_DIRECTORY_INFORMATION
         FILE_FULL_DIR_INFORMATION
         FILE_NAMES_INFORMATION
         FILE_NOTIFY_INFORMATION
         FsRtlNotifyCleanup
         KeAttachProcess
         KeDetachProcess
         MmCreateSection
         ZwCreateProcess
         ZwCreateThread
         ZwDeviceIoControlFile
         ZwGetContextThread
         ZwLoadDriver
         ZwOpenDirectoryObject
         ZwOpenProcess
         ZwOpenSymbolicLinkObject
         ZwQueryDirectoryFile
         ZwUnloadDriver

    2. 1999-03-15
       Added:
         FILE_COMPRESSION_INFORMATION
         FILE_STREAM_INFORMATION
         FILE_LINK_INFORMATION
         FILE_RENAME_INFORMATION
         EXTENDED_IO_STACK_LOCATION
         IoQueryFileInformation
         IoQueryFileVolumeInformation
         ZwQueryVolumeInformationFile
       Moved include of ntddk.h to inside extern "C" block.

    1. 1999-03-11
       Initial release.
*/

#ifndef _NTIFS_
#define _NTIFS_
#define _GNU_NTIFS_

#ifdef __cplusplus
extern "C" {
#endif

#include <ntddk.h>
#include <ntverp.h>

typedef struct _SERVICE_DESCRIPTOR_TABLE    *PSERVICE_DESCRIPTOR_TABLE;
typedef struct _SE_EXPORTS                  *PSE_EXPORTS;

extern PUCHAR                       *FsRtlLegalAnsiCharacterArray;
extern POBJECT_TYPE                 *IoDriverObjectType;
extern PSERVICE_DESCRIPTOR_TABLE    KeServiceDescriptorTable;
extern PSHORT                       NtBuildNumber;
extern PSE_EXPORTS                  SeExports;
extern PACL                         SePublicDefaultDacl;
extern PACL                         SeSystemDefaultDacl;

#define ACCESS_ALLOWED_ACE_TYPE         (0x0)
#define ACCESS_DENIED_ACE_TYPE          (0x1)
#define SYSTEM_AUDIT_ACE_TYPE           (0x2)
#define SYSTEM_ALARM_ACE_TYPE           (0x3)

#define ANSI_DOS_STAR                   ('<')
#define ANSI_DOS_QM                     ('>')
#define ANSI_DOS_DOT                    ('"')

#define DOS_STAR                        (L'<')
#define DOS_QM                          (L'>')
#define DOS_DOT                         (L'"')

#define COMPRESSION_FORMAT_NONE         (0x0000)
#define COMPRESSION_FORMAT_DEFAULT      (0x0001)
#define COMPRESSION_FORMAT_LZNT1        (0x0002)
#define COMPRESSION_ENGINE_STANDARD     (0x0000)
#define COMPRESSION_ENGINE_MAXIMUM      (0x0100)
#define COMPRESSION_ENGINE_HIBER        (0x0200)

#define FILE_ACTION_ADDED                   0x00000001
#define FILE_ACTION_REMOVED                 0x00000002
#define FILE_ACTION_MODIFIED                0x00000003
#define FILE_ACTION_RENAMED_OLD_NAME        0x00000004
#define FILE_ACTION_RENAMED_NEW_NAME        0x00000005
#define FILE_ACTION_ADDED_STREAM            0x00000006
#define FILE_ACTION_REMOVED_STREAM          0x00000007
#define FILE_ACTION_MODIFIED_STREAM         0x00000008
#define FILE_ACTION_REMOVED_BY_DELETE       0x00000009
#define FILE_ACTION_ID_NOT_TUNNELLED        0x0000000A
#define FILE_ACTION_TUNNELLED_ID_COLLISION  0x0000000B

#define FILE_EA_TYPE_BINARY             0xfffe
#define FILE_EA_TYPE_ASCII              0xfffd
#define FILE_EA_TYPE_BITMAP             0xfffb
#define FILE_EA_TYPE_METAFILE           0xfffa
#define FILE_EA_TYPE_ICON               0xfff9
#define FILE_EA_TYPE_EA                 0xffee
#define FILE_EA_TYPE_MVMT               0xffdf
#define FILE_EA_TYPE_MVST               0xffde
#define FILE_EA_TYPE_ASN1               0xffdd
#define FILE_EA_TYPE_FAMILY_IDS         0xff01

#define FILE_NEED_EA                    0x00000080

#define FILE_NOTIFY_CHANGE_FILE_NAME    0x00000001
#define FILE_NOTIFY_CHANGE_DIR_NAME     0x00000002
#define FILE_NOTIFY_CHANGE_NAME         0x00000003
#define FILE_NOTIFY_CHANGE_ATTRIBUTES   0x00000004
#define FILE_NOTIFY_CHANGE_SIZE         0x00000008
#define FILE_NOTIFY_CHANGE_LAST_WRITE   0x00000010
#define FILE_NOTIFY_CHANGE_LAST_ACCESS  0x00000020
#define FILE_NOTIFY_CHANGE_CREATION     0x00000040
#define FILE_NOTIFY_CHANGE_EA           0x00000080
#define FILE_NOTIFY_CHANGE_SECURITY     0x00000100
#define FILE_NOTIFY_CHANGE_STREAM_NAME  0x00000200
#define FILE_NOTIFY_CHANGE_STREAM_SIZE  0x00000400
#define FILE_NOTIFY_CHANGE_STREAM_WRITE 0x00000800
#define FILE_NOTIFY_VALID_MASK          0x00000fff

#define FILE_OPLOCK_BROKEN_TO_LEVEL_2   0x00000007
#define FILE_OPLOCK_BROKEN_TO_NONE      0x00000008

#define FILE_OPBATCH_BREAK_UNDERWAY     0x00000009

#define FILE_CASE_SENSITIVE_SEARCH      0x00000001
#define FILE_CASE_PRESERVED_NAMES       0x00000002
#define FILE_UNICODE_ON_DISK            0x00000004
#define FILE_PERSISTENT_ACLS            0x00000008
#define FILE_FILE_COMPRESSION           0x00000010
#define FILE_VOLUME_QUOTAS              0x00000020
#define FILE_SUPPORTS_SPARSE_FILES      0x00000040
#define FILE_SUPPORTS_REPARSE_POINTS    0x00000080
#define FILE_SUPPORTS_REMOTE_STORAGE    0x00000100
#define FS_LFN_APIS                     0x00004000
#define FILE_VOLUME_IS_COMPRESSED       0x00008000
#define FILE_SUPPORTS_OBJECT_IDS        0x00010000
#define FILE_SUPPORTS_ENCRYPTION        0x00020000
#define FILE_NAMED_STREAMS              0x00040000
#define FILE_READ_ONLY_VOLUME           0x00080000

#define FILE_PIPE_BYTE_STREAM_TYPE      0x00000000
#define FILE_PIPE_MESSAGE_TYPE          0x00000001

#define FILE_PIPE_BYTE_STREAM_MODE      0x00000000
#define FILE_PIPE_MESSAGE_MODE          0x00000001

#define FILE_PIPE_QUEUE_OPERATION       0x00000000
#define FILE_PIPE_COMPLETE_OPERATION    0x00000001

#define FILE_PIPE_INBOUND               0x00000000
#define FILE_PIPE_OUTBOUND              0x00000001
#define FILE_PIPE_FULL_DUPLEX           0x00000002

#define FILE_PIPE_DISCONNECTED_STATE    0x00000001
#define FILE_PIPE_LISTENING_STATE       0x00000002
#define FILE_PIPE_CONNECTED_STATE       0x00000003
#define FILE_PIPE_CLOSING_STATE         0x00000004

#define FILE_PIPE_CLIENT_END            0x00000000
#define FILE_PIPE_SERVER_END            0x00000001

#define FILE_PIPE_READ_DATA             0x00000000
#define FILE_PIPE_WRITE_SPACE           0x00000001

#define FILE_STORAGE_TYPE_SPECIFIED             0x00000041  // FILE_DIRECTORY_FILE | FILE_NON_DIRECTORY_FILE
#define FILE_STORAGE_TYPE_DEFAULT               (StorageTypeDefault << FILE_STORAGE_TYPE_SHIFT)
#define FILE_STORAGE_TYPE_DIRECTORY             (StorageTypeDirectory << FILE_STORAGE_TYPE_SHIFT)
#define FILE_STORAGE_TYPE_FILE                  (StorageTypeFile << FILE_STORAGE_TYPE_SHIFT)
#define FILE_STORAGE_TYPE_DOCFILE               (StorageTypeDocfile << FILE_STORAGE_TYPE_SHIFT)
#define FILE_STORAGE_TYPE_JUNCTION_POINT        (StorageTypeJunctionPoint << FILE_STORAGE_TYPE_SHIFT)
#define FILE_STORAGE_TYPE_CATALOG               (StorageTypeCatalog << FILE_STORAGE_TYPE_SHIFT)
#define FILE_STORAGE_TYPE_STRUCTURED_STORAGE    (StorageTypeStructuredStorage << FILE_STORAGE_TYPE_SHIFT)
#define FILE_STORAGE_TYPE_EMBEDDING             (StorageTypeEmbedding << FILE_STORAGE_TYPE_SHIFT)
#define FILE_STORAGE_TYPE_STREAM                (StorageTypeStream << FILE_STORAGE_TYPE_SHIFT)
#define FILE_MINIMUM_STORAGE_TYPE               FILE_STORAGE_TYPE_DEFAULT
#define FILE_MAXIMUM_STORAGE_TYPE               FILE_STORAGE_TYPE_STREAM
#define FILE_STORAGE_TYPE_MASK                  0x000f0000
#define FILE_STORAGE_TYPE_SHIFT                 16

#define FILE_VC_QUOTA_NONE              0x00000000
#define FILE_VC_QUOTA_TRACK             0x00000001
#define FILE_VC_QUOTA_ENFORCE           0x00000002
#define FILE_VC_QUOTA_MASK              0x00000003

#define FILE_VC_QUOTAS_LOG_VIOLATIONS   0x00000004
#define FILE_VC_CONTENT_INDEX_DISABLED  0x00000008

#define FILE_VC_LOG_QUOTA_THRESHOLD     0x00000010
#define FILE_VC_LOG_QUOTA_LIMIT         0x00000020
#define FILE_VC_LOG_VOLUME_THRESHOLD    0x00000040
#define FILE_VC_LOG_VOLUME_LIMIT        0x00000080

#define FILE_VC_QUOTAS_INCOMPLETE       0x00000100
#define FILE_VC_QUOTAS_REBUILDING       0x00000200

#define FILE_VC_VALID_MASK              0x000003ff

#define FSRTL_FLAG_FILE_MODIFIED        (0x01)
#define FSRTL_FLAG_FILE_LENGTH_CHANGED  (0x02)
#define FSRTL_FLAG_LIMIT_MODIFIED_PAGES (0x04)
#define FSRTL_FLAG_ACQUIRE_MAIN_RSRC_EX (0x08)
#define FSRTL_FLAG_ACQUIRE_MAIN_RSRC_SH (0x10)
#define FSRTL_FLAG_USER_MAPPED_FILE     (0x20)
#define FSRTL_FLAG_ADVANCED_HEADER      (0x40)
#define FSRTL_FLAG_EOF_ADVANCE_ACTIVE   (0x80)

#define FSRTL_FLAG2_DO_MODIFIED_WRITE           (0x01)
#define FSRTL_FLAG2_SUPPORTS_FILTER_CONTEXTS    (0x02)
#define FSRTL_FLAG2_PURGE_WHEN_MAPPED           (0x04)

#define FSRTL_FSP_TOP_LEVEL_IRP         (0x01)
#define FSRTL_CACHE_TOP_LEVEL_IRP       (0x02)
#define FSRTL_MOD_WRITE_TOP_LEVEL_IRP   (0x03)
#define FSRTL_FAST_IO_TOP_LEVEL_IRP     (0x04)
#define FSRTL_MAX_TOP_LEVEL_IRP_FLAG    (0x04)

#define FSRTL_VOLUME_DISMOUNT           1
#define FSRTL_VOLUME_DISMOUNT_FAILED    2
#define FSRTL_VOLUME_LOCK               3
#define FSRTL_VOLUME_LOCK_FAILED        4
#define FSRTL_VOLUME_UNLOCK             5
#define FSRTL_VOLUME_MOUNT              6

#define FSRTL_WILD_CHARACTER            0x08

#ifdef _X86_
#define HARDWARE_PTE    HARDWARE_PTE_X86
#define PHARDWARE_PTE   PHARDWARE_PTE_X86
#else
#define HARDWARE_PTE    ULONG
#define PHARDWARE_PTE   PULONG
#endif

#define IO_CHECK_CREATE_PARAMETERS      0x0200
#define IO_ATTACH_DEVICE                0x0400

#define IO_ATTACH_DEVICE_API            0x80000000

#define IO_COMPLETION_QUERY_STATE       0x0001
#define IO_COMPLETION_MODIFY_STATE      0x0002
#define IO_COMPLETION_ALL_ACCESS        (STANDARD_RIGHTS_REQUIRED|SYNCHRONIZE|0x3)

#define IO_FILE_OBJECT_NON_PAGED_POOL_CHARGE    64
#define IO_FILE_OBJECT_PAGED_POOL_CHARGE        1024

#define IO_TYPE_APC                     18
#define IO_TYPE_DPC                     19
#define IO_TYPE_DEVICE_QUEUE            20
#define IO_TYPE_EVENT_PAIR              21
#define IO_TYPE_INTERRUPT               22
#define IO_TYPE_PROFILE                 23

#define IRP_BEING_VERIFIED              0x10

#define MAILSLOT_CLASS_FIRSTCLASS       1
#define MAILSLOT_CLASS_SECONDCLASS      2

#define MAILSLOT_SIZE_AUTO              0

#define MAP_PROCESS                     1L
#define MAP_SYSTEM                      2L

#define MEM_DOS_LIM                     0x40000000
#define MEM_IMAGE                       SEC_IMAGE

#define OB_TYPE_TYPE                    1
#define OB_TYPE_DIRECTORY               2
#define OB_TYPE_SYMBOLIC_LINK           3
#define OB_TYPE_TOKEN                   4
#define OB_TYPE_PROCESS                 5
#define OB_TYPE_THREAD                  6
#define OB_TYPE_EVENT                   7
#define OB_TYPE_EVENT_PAIR              8
#define OB_TYPE_MUTANT                  9
#define OB_TYPE_SEMAPHORE               10
#define OB_TYPE_TIMER                   11
#define OB_TYPE_PROFILE                 12
#define OB_TYPE_WINDOW_STATION          13
#define OB_TYPE_DESKTOP                 14
#define OB_TYPE_SECTION                 15
#define OB_TYPE_KEY                     16
#define OB_TYPE_PORT                    17
#define OB_TYPE_ADAPTER                 18
#define OB_TYPE_CONTROLLER              19
#define OB_TYPE_DEVICE                  20
#define OB_TYPE_DRIVER                  21
#define OB_TYPE_IO_COMPLETION           22
#define OB_TYPE_FILE                    23

#define PIN_WAIT                        (1)
#define PIN_EXCLUSIVE                   (2)
#define PIN_NO_READ                     (4)
#define PIN_IF_BCB                      (8)

#define MAP_WAIT                        (1)
#define MAP_NO_READ                     (16)

#define PORT_CONNECT                    0x0001
#define PORT_ALL_ACCESS                 (STANDARD_RIGHTS_ALL |\
                                         PORT_CONNECT)

#define SEC_BASED                       0x00200000
#define SEC_NO_CHANGE                   0x00400000
#define SEC_FILE                        0x00800000
#define SEC_IMAGE                       0x01000000
#define SEC_COMMIT                      0x08000000
#define SEC_NOCACHE                     0x10000000

#define SECURITY_WORLD_SID_AUTHORITY    {0,0,0,0,0,1}
#define SECURITY_WORLD_RID              (0x00000000L)

#define SID_REVISION                    1

#define THREAD_STATE_INITIALIZED        0
#define THREAD_STATE_READY              1
#define THREAD_STATE_RUNNING            2
#define THREAD_STATE_STANDBY            3
#define THREAD_STATE_TERMINATED         4
#define THREAD_STATE_WAIT               5
#define THREAD_STATE_TRANSITION         6
#define THREAD_STATE_UNKNOWN            7

#define TOKEN_ASSIGN_PRIMARY            (0x0001)
#define TOKEN_DUPLICATE                 (0x0002)
#define TOKEN_IMPERSONATE               (0x0004)
#define TOKEN_QUERY                     (0x0008)
#define TOKEN_QUERY_SOURCE              (0x0010)
#define TOKEN_ADJUST_PRIVILEGES         (0x0020)
#define TOKEN_ADJUST_GROUPS             (0x0040)
#define TOKEN_ADJUST_DEFAULT            (0x0080)

#define TOKEN_ALL_ACCESS (STANDARD_RIGHTS_REQUIRED |\
                          TOKEN_ASSIGN_PRIMARY     |\
                          TOKEN_DUPLICATE          |\
                          TOKEN_IMPERSONATE        |\
                          TOKEN_QUERY              |\
                          TOKEN_QUERY_SOURCE       |\
                          TOKEN_ADJUST_PRIVILEGES  |\
                          TOKEN_ADJUST_GROUPS      |\
                          TOKEN_ADJUST_DEFAULT)

#define TOKEN_READ       (STANDARD_RIGHTS_READ     |\
                          TOKEN_QUERY)

#define TOKEN_WRITE      (STANDARD_RIGHTS_WRITE    |\
                          TOKEN_ADJUST_PRIVILEGES  |\
                          TOKEN_ADJUST_GROUPS      |\
                          TOKEN_ADJUST_DEFAULT)

#define TOKEN_EXECUTE    (STANDARD_RIGHTS_EXECUTE)

#define TOKEN_SOURCE_LENGTH 8

#define TOKEN_HAS_TRAVERSE_PRIVILEGE    0x01
#define TOKEN_HAS_BACKUP_PRIVILEGE      0x02
#define TOKEN_HAS_RESTORE_PRIVILEGE     0x04
#define TOKEN_HAS_ADMIN_GROUP           0x08
#define TOKEN_IS_RESTRICTED             0x10
#define TOKEN_SESSION_NOT_REFERENCED    0x20
#define TOKEN_SANDBOX_INERT             0x40
#define TOKEN_HAS_IMPERSONATE_PRIVILEGE 0x80

#define VACB_MAPPING_GRANULARITY        (0x40000)
#define VACB_OFFSET_SHIFT               (18)

#define FSCTL_REQUEST_OPLOCK_LEVEL_1    CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  0, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_REQUEST_OPLOCK_LEVEL_2    CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  1, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_REQUEST_BATCH_OPLOCK      CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  2, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_OPLOCK_BREAK_ACKNOWLEDGE  CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  3, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_OPBATCH_ACK_CLOSE_PENDING CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  4, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_OPLOCK_BREAK_NOTIFY       CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  5, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_LOCK_VOLUME               CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  6, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_UNLOCK_VOLUME             CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  7, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_DISMOUNT_VOLUME           CTL_CODE(FILE_DEVICE_FILE_SYSTEM,  8, METHOD_BUFFERED, FILE_ANY_ACCESS)

#define FSCTL_IS_VOLUME_MOUNTED         CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 10, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_IS_PATHNAME_VALID         CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 11, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_MARK_VOLUME_DIRTY         CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 12, METHOD_BUFFERED, FILE_ANY_ACCESS)

#define FSCTL_QUERY_RETRIEVAL_POINTERS  CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 14,  METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_GET_COMPRESSION           CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 15, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_SET_COMPRESSION           CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 16, METHOD_BUFFERED, FILE_READ_DATA | FILE_WRITE_DATA)


#define FSCTL_MARK_AS_SYSTEM_HIVE       CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 19,  METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_OPLOCK_BREAK_ACK_NO_2     CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 20, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_INVALIDATE_VOLUMES        CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 21, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_QUERY_FAT_BPB             CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 22, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_REQUEST_FILTER_OPLOCK     CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 23, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_FILESYSTEM_GET_STATISTICS CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 24, METHOD_BUFFERED, FILE_ANY_ACCESS)

#if (VER_PRODUCTBUILD >= 1381)

#define FSCTL_GET_NTFS_VOLUME_DATA      CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 25, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_GET_NTFS_FILE_RECORD      CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 26, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_GET_VOLUME_BITMAP         CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 27,  METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_GET_RETRIEVAL_POINTERS    CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 28,  METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_MOVE_FILE                 CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 29, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_IS_VOLUME_DIRTY           CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 30, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_GET_HFS_INFORMATION       CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 31, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_ALLOW_EXTENDED_DASD_IO    CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 32, METHOD_NEITHER,  FILE_ANY_ACCESS)

#endif // (VER_PRODUCTBUILD >= 1381)

#if (VER_PRODUCTBUILD >= 2195)

#define FSCTL_READ_PROPERTY_DATA        CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 33, METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_WRITE_PROPERTY_DATA       CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 34, METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_FIND_FILES_BY_SID         CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 35, METHOD_NEITHER, FILE_ANY_ACCESS)

#define FSCTL_DUMP_PROPERTY_DATA        CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 37,  METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_SET_OBJECT_ID             CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 38, METHOD_BUFFERED, FILE_WRITE_DATA)
#define FSCTL_GET_OBJECT_ID             CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 39, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_DELETE_OBJECT_ID          CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 40, METHOD_BUFFERED, FILE_WRITE_DATA)
#define FSCTL_SET_REPARSE_POINT         CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 41, METHOD_BUFFERED, FILE_WRITE_DATA)
#define FSCTL_GET_REPARSE_POINT         CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 42, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_DELETE_REPARSE_POINT      CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 43, METHOD_BUFFERED, FILE_WRITE_DATA)
#define FSCTL_ENUM_USN_DATA             CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 44,  METHOD_NEITHER, FILE_READ_DATA)
#define FSCTL_SECURITY_ID_CHECK         CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 45,  METHOD_NEITHER, FILE_READ_DATA)
#define FSCTL_READ_USN_JOURNAL          CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 46,  METHOD_NEITHER, FILE_READ_DATA)
#define FSCTL_SET_OBJECT_ID_EXTENDED    CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 47, METHOD_BUFFERED, FILE_WRITE_DATA)
#define FSCTL_CREATE_OR_GET_OBJECT_ID   CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 48, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_SET_SPARSE                CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 49, METHOD_BUFFERED, FILE_WRITE_DATA)
#define FSCTL_SET_ZERO_DATA             CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 50, METHOD_BUFFERED, FILE_WRITE_DATA)
#define FSCTL_QUERY_ALLOCATED_RANGES    CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 51,  METHOD_NEITHER, FILE_READ_DATA)
#define FSCTL_ENABLE_UPGRADE            CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 52, METHOD_BUFFERED, FILE_WRITE_DATA)
#define FSCTL_SET_ENCRYPTION            CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 53, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_ENCRYPTION_FSCTL_IO       CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 54,  METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_WRITE_RAW_ENCRYPTED       CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 55,  METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_READ_RAW_ENCRYPTED        CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 56,  METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_CREATE_USN_JOURNAL        CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 57,  METHOD_NEITHER, FILE_READ_DATA)
#define FSCTL_READ_FILE_USN_DATA        CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 58,  METHOD_NEITHER, FILE_READ_DATA)
#define FSCTL_WRITE_USN_CLOSE_RECORD    CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 59,  METHOD_NEITHER, FILE_READ_DATA)
#define FSCTL_EXTEND_VOLUME             CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 60, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_QUERY_USN_JOURNAL         CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 61, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_DELETE_USN_JOURNAL        CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 62, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_MARK_HANDLE               CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 63, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_SIS_COPYFILE              CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 64, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_SIS_LINK_FILES            CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 65, METHOD_BUFFERED, FILE_READ_DATA | FILE_WRITE_DATA)
#define FSCTL_HSM_MSG                   CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 66, METHOD_BUFFERED, FILE_READ_DATA | FILE_WRITE_DATA)
#define FSCTL_NSS_CONTROL               CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 67, METHOD_BUFFERED, FILE_WRITE_DATA)
#define FSCTL_HSM_DATA                  CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 68, METHOD_NEITHER, FILE_READ_DATA | FILE_WRITE_DATA)
#define FSCTL_RECALL_FILE               CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 69, METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_NSS_RCONTROL              CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 70, METHOD_BUFFERED, FILE_READ_DATA)
#define FSCTL_READ_FROM_PLEX            CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 71, METHOD_OUT_DIRECT, FILE_READ_DATA)
#define FSCTL_FILE_PREFETCH             CTL_CODE(FILE_DEVICE_FILE_SYSTEM, 72, METHOD_BUFFERED, FILE_SPECIAL_ACCESS)

#endif // (VER_PRODUCTBUILD >= 2195)

#define FSCTL_MAILSLOT_PEEK             CTL_CODE(FILE_DEVICE_MAILSLOT, 0, METHOD_NEITHER, FILE_READ_DATA)

#define FSCTL_NETWORK_SET_CONFIGURATION_INFO    CTL_CODE(FILE_DEVICE_NETWORK_FILE_SYSTEM, 102, METHOD_IN_DIRECT, FILE_ANY_ACCESS)
#define FSCTL_NETWORK_GET_CONFIGURATION_INFO    CTL_CODE(FILE_DEVICE_NETWORK_FILE_SYSTEM, 103, METHOD_OUT_DIRECT, FILE_ANY_ACCESS)
#define FSCTL_NETWORK_GET_CONNECTION_INFO       CTL_CODE(FILE_DEVICE_NETWORK_FILE_SYSTEM, 104, METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_NETWORK_ENUMERATE_CONNECTIONS     CTL_CODE(FILE_DEVICE_NETWORK_FILE_SYSTEM, 105, METHOD_NEITHER, FILE_ANY_ACCESS)
#define FSCTL_NETWORK_DELETE_CONNECTION         CTL_CODE(FILE_DEVICE_NETWORK_FILE_SYSTEM, 107, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_NETWORK_GET_STATISTICS            CTL_CODE(FILE_DEVICE_NETWORK_FILE_SYSTEM, 116, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_NETWORK_SET_DOMAIN_NAME           CTL_CODE(FILE_DEVICE_NETWORK_FILE_SYSTEM, 120, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_NETWORK_REMOTE_BOOT_INIT_SCRT     CTL_CODE(FILE_DEVICE_NETWORK_FILE_SYSTEM, 250, METHOD_BUFFERED, FILE_ANY_ACCESS)

#define FSCTL_PIPE_ASSIGN_EVENT         CTL_CODE(FILE_DEVICE_NAMED_PIPE, 0, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_PIPE_DISCONNECT           CTL_CODE(FILE_DEVICE_NAMED_PIPE, 1, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_PIPE_LISTEN               CTL_CODE(FILE_DEVICE_NAMED_PIPE, 2, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_PIPE_PEEK                 CTL_CODE(FILE_DEVICE_NAMED_PIPE, 3, METHOD_BUFFERED, FILE_READ_DATA)
#define FSCTL_PIPE_QUERY_EVENT          CTL_CODE(FILE_DEVICE_NAMED_PIPE, 4, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_PIPE_TRANSCEIVE           CTL_CODE(FILE_DEVICE_NAMED_PIPE, 5, METHOD_NEITHER,  FILE_READ_DATA | FILE_WRITE_DATA)
#define FSCTL_PIPE_WAIT                 CTL_CODE(FILE_DEVICE_NAMED_PIPE, 6, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_PIPE_IMPERSONATE          CTL_CODE(FILE_DEVICE_NAMED_PIPE, 7, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_PIPE_SET_CLIENT_PROCESS   CTL_CODE(FILE_DEVICE_NAMED_PIPE, 8, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_PIPE_QUERY_CLIENT_PROCESS CTL_CODE(FILE_DEVICE_NAMED_PIPE, 9, METHOD_BUFFERED, FILE_ANY_ACCESS)
#define FSCTL_PIPE_INTERNAL_READ        CTL_CODE(FILE_DEVICE_NAMED_PIPE, 2045, METHOD_BUFFERED, FILE_READ_DATA)
#define FSCTL_PIPE_INTERNAL_WRITE       CTL_CODE(FILE_DEVICE_NAMED_PIPE, 2046, METHOD_BUFFERED, FILE_WRITE_DATA)
#define FSCTL_PIPE_INTERNAL_TRANSCEIVE  CTL_CODE(FILE_DEVICE_NAMED_PIPE, 2047, METHOD_NEITHER, FILE_READ_DATA | FILE_WRITE_DATA)
#define FSCTL_PIPE_INTERNAL_READ_OVFLOW CTL_CODE(FILE_DEVICE_NAMED_PIPE, 2048, METHOD_BUFFERED, FILE_READ_DATA)

#define IOCTL_REDIR_QUERY_PATH          CTL_CODE(FILE_DEVICE_NETWORK_FILE_SYSTEM, 99, METHOD_NEITHER, FILE_ANY_ACCESS)

typedef PVOID PEJOB;
typedef PVOID PNOTIFY_SYNC;
typedef PVOID OPLOCK, *POPLOCK;
typedef PVOID PWOW64_PROCESS;

typedef ULONG LBN;
typedef LBN *PLBN;

typedef ULONG VBN;
typedef VBN *PVBN;

typedef struct _CACHE_MANAGER_CALLBACKS         *PCACHE_MANAGER_CALLBACKS;
typedef struct _EPROCESS_QUOTA_BLOCK            *PEPROCESS_QUOTA_BLOCK;
typedef struct _FILE_GET_QUOTA_INFORMATION      *PFILE_GET_QUOTA_INFORMATION;
typedef struct _HANDLE_TABLE                    *PHANDLE_TABLE;
typedef struct _KEVENT_PAIR                     *PKEVENT_PAIR;
typedef struct _KPROCESS                        *PKPROCESS;
typedef struct _KQUEUE                          *PKQUEUE;
typedef struct _KTRAP_FRAME                     *PKTRAP_FRAME;
typedef struct _LPC_MESSAGE                     *PLPC_MESSAGE;
typedef struct _MAILSLOT_CREATE_PARAMETERS      *PMAILSLOT_CREATE_PARAMETERS;
typedef struct _MMWSL                           *PMMWSL;
typedef struct _NAMED_PIPE_CREATE_PARAMETERS    *PNAMED_PIPE_CREATE_PARAMETERS;
typedef struct _OBJECT_DIRECTORY                *POBJECT_DIRECTORY;
typedef struct _PAGEFAULT_HISTORY               *PPAGEFAULT_HISTORY;
typedef struct _PEB                             *PPEB;
typedef struct _PS_IMPERSONATION_INFORMATION    *PPS_IMPERSONATION_INFORMATION;
typedef struct _SECTION_OBJECT                  *PSECTION_OBJECT;
typedef struct _SERVICE_DESCRIPTOR_TABLE        *PSERVICE_DESCRIPTOR_TABLE;
typedef struct _SHARED_CACHE_MAP                *PSHARED_CACHE_MAP;
typedef struct _TERMINATION_PORT                *PTERMINATION_PORT;
typedef struct _VACB                            *PVACB;
typedef struct _VAD_HEADER                      *PVAD_HEADER;

#if (VER_PRODUCTBUILD < 2195)
typedef ULONG SIZE_T, *PSIZE_T;
#endif

typedef enum _FAST_IO_POSSIBLE {
    FastIoIsNotPossible,
    FastIoIsPossible,
    FastIoIsQuestionable
} FAST_IO_POSSIBLE;

typedef enum _FILE_STORAGE_TYPE {
    StorageTypeDefault = 1,
    StorageTypeDirectory,
    StorageTypeFile,
    StorageTypeJunctionPoint,
    StorageTypeCatalog,
    StorageTypeStructuredStorage,
    StorageTypeEmbedding,
    StorageTypeStream
} FILE_STORAGE_TYPE;

typedef enum _IO_COMPLETION_INFORMATION_CLASS {
    IoCompletionBasicInformation
} IO_COMPLETION_INFORMATION_CLASS;

typedef enum _LPC_TYPE {
    LPC_NEW_MESSAGE,
    LPC_REQUEST,
    LPC_REPLY,
    LPC_DATAGRAM,
    LPC_LOST_REPLY,
    LPC_PORT_CLOSED,
    LPC_CLIENT_DIED,
    LPC_EXCEPTION,
    LPC_DEBUG_EVENT,
    LPC_ERROR_EVENT,
    LPC_CONNECTION_REQUEST
} LPC_TYPE;

typedef enum _MMFLUSH_TYPE {
    MmFlushForDelete,
    MmFlushForWrite
} MMFLUSH_TYPE;

typedef enum _OBJECT_INFO_CLASS {
    ObjectBasicInfo,
    ObjectNameInfo,
    ObjectTypeInfo,
    ObjectAllTypesInfo,
    ObjectProtectionInfo
} OBJECT_INFO_CLASS;

typedef enum _PORT_INFORMATION_CLASS {
    PortNoInformation
} PORT_INFORMATION_CLASS;

typedef enum _SECTION_INFORMATION_CLASS {
    SectionBasicInformation,
    SectionImageInformation
} SECTION_INFORMATION_CLASS;

typedef enum _SID_NAME_USE {
    SidTypeUser = 1,
    SidTypeGroup,
    SidTypeDomain,
    SidTypeAlias,
    SidTypeWellKnownGroup,
    SidTypeDeletedAccount,
    SidTypeInvalid,
    SidTypeUnknown
} SID_NAME_USE;

typedef enum _SYSTEM_INFORMATION_CLASS {
    SystemBasicInformation,
    SystemProcessorInformation,
    SystemPerformanceInformation,
    SystemTimeOfDayInformation,
    SystemNotImplemented1,
    SystemProcessesAndThreadsInformation,
    SystemCallCounts,
    SystemConfigurationInformation,
    SystemProcessorTimes,
    SystemGlobalFlag,
    SystemNotImplemented2,
    SystemModuleInformation,
    SystemLockInformation,
    SystemNotImplemented3,
    SystemNotImplemented4,
    SystemNotImplemented5,
    SystemHandleInformation,
    SystemObjectInformation,
    SystemPagefileInformation,
    SystemInstructionEmulationCounts,
    SystemInvalidInfoClass1,
    SystemCacheInformation,
    SystemPoolTagInformation,
    SystemProcessorStatistics,
    SystemDpcInformation,
    SystemNotImplemented6,
    SystemLoadImage,
    SystemUnloadImage,
    SystemTimeAdjustment,
    SystemNotImplemented7,
    SystemNotImplemented8,
    SystemNotImplemented9,
    SystemCrashDumpInformation,
    SystemExceptionInformation,
    SystemCrashDumpStateInformation,
    SystemKernelDebuggerInformation,
    SystemContextSwitchInformation,
    SystemRegistryQuotaInformation,
    SystemLoadAndCallImage,
    SystemPrioritySeparation,
    SystemNotImplemented10,
    SystemNotImplemented11,
    SystemInvalidInfoClass2,
    SystemInvalidInfoClass3,
    SystemTimeZoneInformation,
    SystemLookasideInformation,
    SystemSetTimeSlipEvent,
    SystemCreateSession,
    SystemDeleteSession,
    SystemInvalidInfoClass4,
    SystemRangeStartInformation,
    SystemVerifierInformation,
    SystemAddVerifier,
    SystemSessionProcessesInformation
} SYSTEM_INFORMATION_CLASS;

typedef enum _THREAD_STATE {
    StateInitialized,
    StateReady,
    StateRunning,
    StateStandby,
    StateTerminated,
    StateWait,
    StateTransition,
    StateUnknown
} THREAD_STATE;

typedef enum _TOKEN_INFORMATION_CLASS {
    TokenUser = 1,
    TokenGroups,
    TokenPrivileges,
    TokenOwner,
    TokenPrimaryGroup,
    TokenDefaultDacl,
    TokenSource,
    TokenType,
    TokenImpersonationLevel,
    TokenStatistics,
    TokenRestrictedSids
} TOKEN_INFORMATION_CLASS;

typedef enum _TOKEN_TYPE {
    TokenPrimary = 1,
    TokenImpersonation
} TOKEN_TYPE;

typedef struct _HARDWARE_PTE_X86 {
    ULONG Valid             : 1;
    ULONG Write             : 1;
    ULONG Owner             : 1;
    ULONG WriteThrough      : 1;
    ULONG CacheDisable      : 1;
    ULONG Accessed          : 1;
    ULONG Dirty             : 1;
    ULONG LargePage         : 1;
    ULONG Global            : 1;
    ULONG CopyOnWrite       : 1;
    ULONG Prototype         : 1;
    ULONG reserved          : 1;
    ULONG PageFrameNumber   : 20;
} HARDWARE_PTE_X86, *PHARDWARE_PTE_X86;

typedef struct _KAPC_STATE {
    LIST_ENTRY  ApcListHead[2];
    PKPROCESS   Process;
    BOOLEAN     KernelApcInProgress;
    BOOLEAN     KernelApcPending;
    BOOLEAN     UserApcPending;
} KAPC_STATE, *PKAPC_STATE;

typedef struct _KGDTENTRY {
    USHORT LimitLow;
    USHORT BaseLow;
    union {
        struct {
            UCHAR BaseMid;
            UCHAR Flags1;
            UCHAR Flags2;
            UCHAR BaseHi;
        } Bytes;
        struct {
            ULONG BaseMid       : 8;
            ULONG Type          : 5;
            ULONG Dpl           : 2;
            ULONG Pres          : 1;
            ULONG LimitHi       : 4;
            ULONG Sys           : 1;
            ULONG Reserved_0    : 1;
            ULONG Default_Big   : 1;
            ULONG Granularity   : 1;
            ULONG BaseHi        : 8;
        } Bits;
    } HighWord;
} KGDTENTRY, *PKGDTENTRY;

typedef struct _KIDTENTRY {
    USHORT Offset;
    USHORT Selector;
    USHORT Access;
    USHORT ExtendedOffset;
} KIDTENTRY, *PKIDTENTRY;

#if (VER_PRODUCTBUILD >= 2600)

typedef struct _KPROCESS {
    DISPATCHER_HEADER   Header;
    LIST_ENTRY          ProfileListHead;
    ULONG               DirectoryTableBase[2];
    KGDTENTRY           LdtDescriptor;
    KIDTENTRY           Int21Descriptor;
    USHORT              IopmOffset;
    UCHAR               Iopl;
    UCHAR               Unused;
    ULONG               ActiveProcessors;
    ULONG               KernelTime;
    ULONG               UserTime;
    LIST_ENTRY          ReadyListHead;  
    SINGLE_LIST_ENTRY   SwapListEntry;
    PVOID               VdmTrapcHandler;
    LIST_ENTRY          ThreadListHead;
    KSPIN_LOCK          ProcessLock;
    KAFFINITY           Affinity;
    USHORT              StackCount;
    CHAR                BasePriority;
    CHAR                ThreadQuantum;
    BOOLEAN             AutoAlignment;
    UCHAR               State;
    UCHAR               ThreadSeed;
    BOOLEAN             DisableBoost;
    UCHAR               PowerState;
    BOOLEAN             DisableQuantum;
    UCHAR               IdealNode;
    UCHAR               Spare;
} KPROCESS, *PKPROCESS;

#else

typedef struct _KPROCESS {
    DISPATCHER_HEADER   Header;
    LIST_ENTRY          ProfileListHead;
    ULONG               DirectoryTableBase[2];
    KGDTENTRY           LdtDescriptor;
    KIDTENTRY           Int21Descriptor;
    USHORT              IopmOffset;
    UCHAR               Iopl;
    UCHAR               VdmFlag;
    ULONG               ActiveProcessors;
    ULONG               KernelTime;
    ULONG               UserTime;
    LIST_ENTRY          ReadyListHead;  
    SINGLE_LIST_ENTRY   SwapListEntry;
    PVOID               Reserved1;
    LIST_ENTRY          ThreadListHead;
    KSPIN_LOCK          ProcessLock;
    KAFFINITY           Affinity;
    USHORT              StackCount;
    UCHAR               BasePriority;
    UCHAR               ThreadQuantum;
    BOOLEAN             AutoAlignment;
    UCHAR               State;
    UCHAR               ThreadSeed;
    BOOLEAN             DisableBoost;
#if (VER_PRODUCTBUILD >= 2195)
    UCHAR               PowerState;
    BOOLEAN             DisableQuantum;
    UCHAR               IdealNode;
    UCHAR               Spare;
#endif // (VER_PRODUCTBUILD >= 2195)
} KPROCESS, *PKPROCESS;

#endif

#if (VER_PRODUCTBUILD >= 2600)

typedef struct _KTHREAD {
    DISPATCHER_HEADER           Header;
    LIST_ENTRY                  MutantListHead;
    PVOID                       InitialStack;
    PVOID                       StackLimit;
    struct _TEB                 *Teb;
    PVOID                       TlsArray;
    PVOID                       KernelStack;
    BOOLEAN                     DebugActive;
    UCHAR                       State;
    UCHAR                       Alerted[2];
    UCHAR                       Iopl;
    UCHAR                       NpxState;
    CHAR                        Saturation;
    CHAR                        Priority;
    KAPC_STATE                  ApcState;
    ULONG                       ContextSwitches;
    UCHAR                       IdleSwapBlock;
    UCHAR                       Spare0[3];
    NTSTATUS                    WaitStatus;
    UCHAR                       WaitIrql;
    CHAR                        WaitMode;
    UCHAR                       WaitNext;
    UCHAR                       WaitReason;
    PKWAIT_BLOCK                WaitBlockList;
    union {
        LIST_ENTRY              WaitListEntry;
        SINGLE_LIST_ENTRY       SwapListEntry;
    };
    ULONG                       WaitTime;
    CHAR                        BasePriority;
    UCHAR                       DecrementCount;
    CHAR                        PriorityDecrement;
    CHAR                        Quantum;
    KWAIT_BLOCK                 WaitBlock[4];
    PVOID                       LegoData;
    ULONG                       KernelApcDisable;
    ULONG                       UserAffinity;
    BOOLEAN                     SystemAffinityActive;
    UCHAR                       PowerState;
    UCHAR                       NpxIrql;
    UCHAR                       InitialNode;
    PSERVICE_DESCRIPTOR_TABLE   ServiceTable;
    PKQUEUE                     Queue;
    KSPIN_LOCK                  ApcQueueLock;
    KTIMER                      Timer;
    LIST_ENTRY                  QueueListEntry;
    ULONG                       SoftAffinity;
    ULONG                       Affinity;
    BOOLEAN                     Preempted;
    BOOLEAN                     ProcessReadyQueue;
    BOOLEAN                     KernelStackResident;
    UCHAR                       NextProcessor;
    PVOID                       CallbackStack;
    PVOID                       Win32Thread;
    PKTRAP_FRAME                TrapFrame;
    PKAPC_STATE                 ApcStatePointer[2];
    CHAR                        PreviousMode;
    BOOLEAN                     EnableStackSwap;
    BOOLEAN                     LargeStack;
    UCHAR                       ResourceIndex;
    ULONG                       KernelTime;
    ULONG                       UserTime;
    KAPC_STATE                  SavedApcState;
    BOOLEAN                     Alertable;
    UCHAR                       ApcStateIndex;
    BOOLEAN                     ApcQueueable;
    BOOLEAN                     AutoAlignment;
    PVOID                       StackBase;
    KAPC                        SuspendApc;
    KSEMAPHORE                  SuspendSemaphore;
    LIST_ENTRY                  ThreadListEntry;
    CHAR                        FreezeCount;
    CHAR                        SuspendCount;
    UCHAR                       IdealProcessor;
    BOOLEAN                     DisableBoost;
} KTHREAD, *PKTHREAD;

#else

typedef struct _KTHREAD {
    DISPATCHER_HEADER           Header;
    LIST_ENTRY                  MutantListHead;
    PVOID                       InitialStack;
    PVOID                       StackLimit;
    struct _TEB                 *Teb;
    PVOID                       TlsArray;
    PVOID                       KernelStack;
    BOOLEAN                     DebugActive;
    UCHAR                       State;
    USHORT                      Alerted;
    UCHAR                       Iopl;
    UCHAR                       NpxState;
    UCHAR                       Saturation;
    UCHAR                       Priority;
    KAPC_STATE                  ApcState;
    ULONG                       ContextSwitches;
    NTSTATUS                    WaitStatus;
    UCHAR                       WaitIrql;
    UCHAR                       WaitMode;
    UCHAR                       WaitNext;
    UCHAR                       WaitReason;
    PKWAIT_BLOCK                WaitBlockList;
    LIST_ENTRY                  WaitListEntry;
    ULONG                       WaitTime;
    UCHAR                       BasePriority;
    UCHAR                       DecrementCount;
    UCHAR                       PriorityDecrement;
    UCHAR                       Quantum;
    KWAIT_BLOCK                 WaitBlock[4];
    ULONG                       LegoData;
    ULONG                       KernelApcDisable;
    ULONG                       UserAffinity;
    BOOLEAN                     SystemAffinityActive;
#if (VER_PRODUCTBUILD < 2195)
    UCHAR                       Pad[3];
#else // (VER_PRODUCTBUILD >= 2195)
    UCHAR                       PowerState;
    UCHAR                       NpxIrql;
    UCHAR                       Pad[1];
#endif // (VER_PRODUCTBUILD >= 2195)
    PSERVICE_DESCRIPTOR_TABLE   ServiceDescriptorTable;
    PKQUEUE                     Queue;
    KSPIN_LOCK                  ApcQueueLock;
    KTIMER                      Timer;
    LIST_ENTRY                  QueueListEntry;
    ULONG                       Affinity;
    BOOLEAN                     Preempted;
    BOOLEAN                     ProcessReadyQueue;
    BOOLEAN                     KernelStackResident;
    UCHAR                       NextProcessor;
    PVOID                       CallbackStack;
    PVOID                       Win32Thread;
    PKTRAP_FRAME                TrapFrame;
    PKAPC_STATE                 ApcStatePointer[2];
#if (VER_PRODUCTBUILD >= 2195)
    UCHAR                       PreviousMode;
#endif // (VER_PRODUCTBUILD >= 2195)
    BOOLEAN                     EnableStackSwap;
    BOOLEAN                     LargeStack;
    UCHAR                       ResourceIndex;
#if (VER_PRODUCTBUILD < 2195)
    UCHAR                       PreviousMode;
#endif // (VER_PRODUCTBUILD < 2195)
    ULONG                       KernelTime;
    ULONG                       UserTime;
    KAPC_STATE                  SavedApcState;
    BOOLEAN                     Alertable;
    UCHAR                       ApcStateIndex;
    BOOLEAN                     ApcQueueable;
    BOOLEAN                     AutoAlignment;
    PVOID                       StackBase;
    KAPC                        SuspendApc;
    KSEMAPHORE                  SuspendSemaphore;
    LIST_ENTRY                  ThreadListEntry;
    UCHAR                       FreezeCount;
    UCHAR                       SuspendCount;
    UCHAR                       IdealProcessor;
    BOOLEAN                     DisableBoost;
} KTHREAD, *PKTHREAD;

#endif

#if (VER_PRODUCTBUILD >= 2600)

typedef struct _MMSUPPORT_FLAGS {
    ULONG SessionSpace              : 1;
    ULONG BeingTrimmed              : 1;
    ULONG SessionLeader             : 1;
    ULONG TrimHard                  : 1;
    ULONG WorkingSetHard            : 1;
    ULONG AddressSpaceBeingDeleted  : 1;
    ULONG Available                 : 10;
    ULONG AllowWorkingSetAdjustment : 8;
    ULONG MemoryPriority            : 8;
} MMSUPPORT_FLAGS, *PMMSUPPORT_FLAGS;

#else

typedef struct _MMSUPPORT_FLAGS {
    ULONG SessionSpace      : 1;
    ULONG BeingTrimmed      : 1;
    ULONG ProcessInSession  : 1;
    ULONG SessionLeader     : 1;
    ULONG TrimHard          : 1;
    ULONG WorkingSetHard    : 1;
    ULONG WriteWatch        : 1;
    ULONG Filler            : 25;
} MMSUPPORT_FLAGS, *PMMSUPPORT_FLAGS;

#endif

#if (VER_PRODUCTBUILD >= 2600)

typedef struct _MMSUPPORT {
    LARGE_INTEGER   LastTrimTime;
    MMSUPPORT_FLAGS Flags;
    ULONG           PageFaultCount;
    ULONG           PeakWorkingSetSize;
    ULONG           WorkingSetSize;
    ULONG           MinimumWorkingSetSize;
    ULONG           MaximumWorkingSetSize;
    PMMWSL          VmWorkingSetList;
    LIST_ENTRY      WorkingSetExpansionLinks;
    ULONG           Claim;
    ULONG           NextEstimationSlot;
    ULONG           NextAgingSlot;
    ULONG           EstimatedAvailable;
    ULONG           GrowthSinceLastEstimate;
} MMSUPPORT, *PMMSUPPORT;

#else

typedef struct _MMSUPPORT {
    LARGE_INTEGER   LastTrimTime;
    ULONG           LastTrimFaultCount;
    ULONG           PageFaultCount;
    ULONG           PeakWorkingSetSize;
    ULONG           WorkingSetSize;
    ULONG           MinimumWorkingSetSize;
    ULONG           MaximumWorkingSetSize;
    PMMWSL          VmWorkingSetList;
    LIST_ENTRY      WorkingSetExpansionLinks;
    BOOLEAN         AllowWorkingSetAdjustment;
    BOOLEAN         AddressSpaceBeingDeleted;
    UCHAR           ForegroundSwitchCount;
    UCHAR           MemoryPriority;
#if (VER_PRODUCTBUILD >= 2195)
    union {
        ULONG           LongFlags;
        MMSUPPORT_FLAGS Flags;
    } u;
    ULONG           Claim;
    ULONG           NextEstimationSlot;
    ULONG           NextAgingSlot;
    ULONG           EstimatedAvailable;
    ULONG           GrowthSinceLastEstimate;
#endif // (VER_PRODUCTBUILD >= 2195)
} MMSUPPORT, *PMMSUPPORT;

#endif

typedef struct _SE_AUDIT_PROCESS_CREATION_INFO {
    POBJECT_NAME_INFORMATION ImageFileName;
} SE_AUDIT_PROCESS_CREATION_INFO, *PSE_AUDIT_PROCESS_CREATION_INFO;

typedef struct _SID_IDENTIFIER_AUTHORITY {
    UCHAR Value[6];
} SID_IDENTIFIER_AUTHORITY, *PSID_IDENTIFIER_AUTHORITY;

typedef struct _SID {
    UCHAR                       Revision;
    UCHAR                       SubAuthorityCount;
    SID_IDENTIFIER_AUTHORITY    IdentifierAuthority;
    ULONG                       SubAuthority[1];
} SID, *PREAL_SID;

typedef struct _BITMAP_DESCRIPTOR {
    ULONGLONG   StartLcn;
    ULONGLONG   ClustersToEndOfVol;
    UCHAR       Map[1];
} BITMAP_DESCRIPTOR, *PBITMAP_DESCRIPTOR; 

typedef struct _BITMAP_RANGE {
    LIST_ENTRY      Links;
    LARGE_INTEGER   BasePage;
    ULONG           FirstDirtyPage;
    ULONG           LastDirtyPage;
    ULONG           DirtyPages;
    PULONG          Bitmap;
} BITMAP_RANGE, *PBITMAP_RANGE;

typedef struct _CACHE_UNINITIALIZE_EVENT {
    struct _CACHE_UNINITIALIZE_EVENT    *Next;
    KEVENT                              Event;
} CACHE_UNINITIALIZE_EVENT, *PCACHE_UNINITIALIZE_EVENT;

typedef struct _CC_FILE_SIZES {
    LARGE_INTEGER AllocationSize;
    LARGE_INTEGER FileSize;
    LARGE_INTEGER ValidDataLength;
} CC_FILE_SIZES, *PCC_FILE_SIZES;

typedef struct _COMPRESSED_DATA_INFO {
    USHORT  CompressionFormatAndEngine;
    UCHAR   CompressionUnitShift;
    UCHAR   ChunkShift;
    UCHAR   ClusterShift;
    UCHAR   Reserved;
    USHORT  NumberOfChunks;
    ULONG   CompressedChunkSizes[ANYSIZE_ARRAY];
} COMPRESSED_DATA_INFO, *PCOMPRESSED_DATA_INFO;

typedef struct _DEVICE_MAP {
    POBJECT_DIRECTORY   DosDevicesDirectory;
    POBJECT_DIRECTORY   GlobalDosDevicesDirectory;
    ULONG               ReferenceCount;
    ULONG               DriveMap;
    UCHAR               DriveType[32];
} DEVICE_MAP, *PDEVICE_MAP; 

typedef struct _DIRECTORY_BASIC_INFORMATION {
    UNICODE_STRING ObjectName;
    UNICODE_STRING ObjectTypeName;
} DIRECTORY_BASIC_INFORMATION, *PDIRECTORY_BASIC_INFORMATION;

#if (VER_PRODUCTBUILD >= 2600)

typedef struct _EX_FAST_REF {
    union {
        PVOID Object;
        ULONG RefCnt : 3;
        ULONG Value;
    };
} EX_FAST_REF, *PEX_FAST_REF;

typedef struct _EX_PUSH_LOCK {
    union {
        struct {
            ULONG   Waiting     : 1;
            ULONG   Exclusive   : 1;
            ULONG   Shared      : 30;
        };
        ULONG   Value;
        PVOID   Ptr;
    };
} EX_PUSH_LOCK, *PEX_PUSH_LOCK;


/*
typedef struct _EX_RUNDOWN_REF {
    union {
        ULONG Count;
        PVOID Ptr;
    };
} EX_RUNDOWN_REF, *PEX_RUNDOWN_REF;

*/


typedef struct _EPROCESS {
    KPROCESS                        Pcb;
    EX_PUSH_LOCK                    ProcessLock;
    LARGE_INTEGER                   CreateTime;
    LARGE_INTEGER                   ExitTime;
    EX_RUNDOWN_REF                  RundownProtect;
    PVOID                           UniqueProcessId;
    LIST_ENTRY                      ActiveProcessLinks;
    ULONG                           QuotaUsage[3];
    ULONG                           QuotaPeak[3];
    ULONG                           CommitCharge;
    ULONG                           PeakVirtualSize;
    ULONG                           VirtualSize;
    LIST_ENTRY                      SessionProcessLinks;
    PVOID                           DebugPort;
    PVOID                           ExceptionPort;
    PHANDLE_TABLE                   ObjectTable;
    EX_FAST_REF                     Token;
    FAST_MUTEX                      WorkingSetLock;
    ULONG                           WorkingSetPage;
    FAST_MUTEX                      AddressCreationLock;
    KSPIN_LOCK                      HyperSpaceLock;
    PETHREAD                        ForkInProgress;
    ULONG                           HardwareTrigger;
    PVOID                           VadRoot;
    PVOID                           VadHint;
    PVOID                           CloneRoot;
    ULONG                           NumberOfPrivatePages;
    ULONG                           NumberOfLockedPages;
    PVOID                           Win32Process;
    PEJOB                           Job;
    PSECTION_OBJECT                 SectionObject;
    PVOID                           SectionBaseAddress;
    PEPROCESS_QUOTA_BLOCK           QuotaBlock;
    PPAGEFAULT_HISTORY              WorkingSetWatch;
    PVOID                           Win32WindowStation;
    PVOID                           InheritedFromUniqueProcessId;
    PVOID                           LdtInformation;
    PVOID                           VadFreeHint;
    PVOID                           VdmObjects;
    PDEVICE_MAP                     DeviceMap;
    LIST_ENTRY                      PhysicalVadList;
    union {
        HARDWARE_PTE                PageDirectoryPte;
        ULONGLONG                   Filler;
    };
    PVOID                           Session;
    UCHAR                           ImageFileName[16];
    LIST_ENTRY                      JobLinks;
    PVOID                           LockedPageList;
    LIST_ENTRY                      ThreadListHead;
    PVOID                           SecurityPort;
    PVOID                           PaeTop;
    ULONG                           ActiveThreads;
    ULONG                           GrantedAccess;
    ULONG                           DefaultHardErrorProcessing;
    NTSTATUS                        LastThreadExitStatus;
    PPEB                            Peb;
    EX_FAST_REF                     PrefetchTrace;
    LARGE_INTEGER                   ReadOperationCount;
    LARGE_INTEGER                   WriteOperationCount;
    LARGE_INTEGER                   OtherOperationCount;
    LARGE_INTEGER                   ReadTransferCount;
    LARGE_INTEGER                   WriteTransferCount;
    LARGE_INTEGER                   OtherTransferCount;
    ULONG                           CommitChargeLimit;
    ULONG                           CommitChargePeek;
    PVOID                           AweInfo;
    SE_AUDIT_PROCESS_CREATION_INFO  SeAuditProcessCreationInfo;
    MMSUPPORT                       Vm;
    ULONG                           LastFaultCount;
    ULONG                           ModifiedPageCount;
    ULONG                           NumberOfVads;
    ULONG                           JobStatus;
    union {
        ULONG                       Flags;
        struct {
            ULONG                   CreateReported              : 1;
            ULONG                   NoDebugInherit              : 1;
            ULONG                   ProcessExiting              : 1;
            ULONG                   ProcessDelete               : 1;
            ULONG                   Wow64SplitPages             : 1;
            ULONG                   VmDeleted                   : 1;
            ULONG                   OutswapEnabled              : 1;
            ULONG                   Outswapped                  : 1;
            ULONG                   ForkFailed                  : 1;
            ULONG                   HasPhysicalVad              : 1;
            ULONG                   AddressSpaceInitialized     : 2;
            ULONG                   SetTimerResolution          : 1;
            ULONG                   BreakOnTermination          : 1;
            ULONG                   SessionCreationUnderway     : 1;
            ULONG                   WriteWatch                  : 1;
            ULONG                   ProcessInSession            : 1;
            ULONG                   OverrideAddressSpace        : 1;
            ULONG                   HasAddressSpace             : 1;
            ULONG                   LaunchPrefetched            : 1;
            ULONG                   InjectInpageErrors          : 1;
            ULONG                   Unused                      : 11;
        };
    };
    NTSTATUS                        ExitStatus;
    USHORT                          NextPageColor;
    union {
        struct {
            UCHAR                   SubSystemMinorVersion;
            UCHAR                   SubSystemMajorVersion;
        };
        USHORT                      SubSystemVersion;
    };
    UCHAR                           PriorityClass;
    BOOLEAN                         WorkingSetAcquiredUnsafe;
} EPROCESS, *PEPROCESS;

#else

typedef struct _EPROCESS {
    KPROCESS                        Pcb;
    NTSTATUS                        ExitStatus;
    KEVENT                          LockEvent;
    ULONG                           LockCount;
    LARGE_INTEGER                   CreateTime;
    LARGE_INTEGER                   ExitTime;
    PKTHREAD                        LockOwner;
    ULONG                           UniqueProcessId;
    LIST_ENTRY                      ActiveProcessLinks;
    ULONGLONG                       QuotaPeakPoolUsage;
    ULONGLONG                       QuotaPoolUsage;
    ULONG                           PagefileUsage;
    ULONG                           CommitCharge;
    ULONG                           PeakPagefileUsage;
    ULONG                           PeakVirtualSize;
    ULONGLONG                       VirtualSize;
    MMSUPPORT                       Vm;
#if (VER_PRODUCTBUILD < 2195)
    ULONG                           LastProtoPteFault;
#else // (VER_PRODUCTBUILD >= 2195)
    LIST_ENTRY                      SessionProcessLinks;
#endif // (VER_PRODUCTBUILD >= 2195)
    ULONG                           DebugPort;
    ULONG                           ExceptionPort;
    PHANDLE_TABLE                   ObjectTable;
    PACCESS_TOKEN                   Token;
    FAST_MUTEX                      WorkingSetLock;
    ULONG                           WorkingSetPage;
    BOOLEAN                         ProcessOutswapEnabled;
    BOOLEAN                         ProcessOutswapped;
    BOOLEAN                         AddressSpaceInitialized;
    BOOLEAN                         AddressSpaceDeleted;
    FAST_MUTEX                      AddressCreationLock;
    KSPIN_LOCK                      HyperSpaceLock;
    PETHREAD                        ForkInProgress;
    USHORT                          VmOperation;
    BOOLEAN                         ForkWasSuccessful;
    UCHAR                           MmAgressiveWsTrimMask;
    PKEVENT                         VmOperationEvent;
#if (VER_PRODUCTBUILD < 2195)
    HARDWARE_PTE                    PageDirectoryPte;
#else // (VER_PRODUCTBUILD >= 2195)
    PVOID                           PaeTop;
#endif // (VER_PRODUCTBUILD >= 2195)
    ULONG                           LastFaultCount;
    ULONG                           ModifiedPageCount;
    PVOID                           VadRoot;
    PVOID                           VadHint;
    ULONG                           CloneRoot;
    ULONG                           NumberOfPrivatePages;
    ULONG                           NumberOfLockedPages;
    USHORT                          NextPageColor;
    BOOLEAN                         ExitProcessCalled;
    BOOLEAN                         CreateProcessReported;
    HANDLE                          SectionHandle;
    PPEB                            Peb;
    PVOID                           SectionBaseAddress;
    PEPROCESS_QUOTA_BLOCK           QuotaBlock;
    NTSTATUS                        LastThreadExitStatus;
    PPROCESS_WS_WATCH_INFORMATION   WorkingSetWatch;
    HANDLE                          Win32WindowStation;
    HANDLE                          InheritedFromUniqueProcessId;
    ACCESS_MASK                     GrantedAccess;
    ULONG                           DefaultHardErrorProcessing;
    PVOID                           LdtInformation;
    PVOID                           VadFreeHint;
    PVOID                           VdmObjects;
#if (VER_PRODUCTBUILD < 2195)
    KMUTANT                         ProcessMutant;
#else // (VER_PRODUCTBUILD >= 2195)
    PDEVICE_MAP                     DeviceMap;
    ULONG                           SessionId;
    LIST_ENTRY                      PhysicalVadList;
    HARDWARE_PTE                    PageDirectoryPte;
    ULONG                           Filler;
    ULONG                           PaePageDirectoryPage;
#endif // (VER_PRODUCTBUILD >= 2195)
    UCHAR                           ImageFileName[16];
    ULONG                           VmTrimFaultValue;
    UCHAR                           SetTimerResolution;
    UCHAR                           PriorityClass;
    union {
        struct {
            UCHAR                   SubSystemMinorVersion;
            UCHAR                   SubSystemMajorVersion;
        };
        USHORT                      SubSystemVersion;
    };
    PVOID                           Win32Process;
#if (VER_PRODUCTBUILD >= 2195)
    PEJOB                           Job;
    ULONG                           JobStatus;
    LIST_ENTRY                      JobLinks;
    PVOID                           LockedPageList;
    PVOID                           SecurityPort;
    PWOW64_PROCESS                  Wow64Process;
    LARGE_INTEGER                   ReadOperationCount;
    LARGE_INTEGER                   WriteOperationCount;
    LARGE_INTEGER                   OtherOperationCount;
    LARGE_INTEGER                   ReadTransferCount;
    LARGE_INTEGER                   WriteTransferCount;
    LARGE_INTEGER                   OtherTransferCount;
    ULONG                           CommitChargeLimit;
    ULONG                           CommitChargePeek;
    LIST_ENTRY                      ThreadListHead;
    PRTL_BITMAP                     VadPhysicalPagesBitMap;
    ULONG                           VadPhysicalPages;
    ULONG                           AweLock;
#endif // (VER_PRODUCTBUILD >= 2195)
} EPROCESS, *PEPROCESS;

#endif

#if (VER_PRODUCTBUILD >= 2600)

typedef struct _ETHREAD {
    KTHREAD                         Tcb;
    union {
        LARGE_INTEGER               CreateTime;
        struct {
            ULONG                   NestedFaultCount    : 2;
            ULONG                   ApcNeeded           : 1;
        };
    };
    union {
        LARGE_INTEGER               ExitTime;
        LIST_ENTRY                  LpcReplyChain;
        LIST_ENTRY                  KeyedWaitChain;
    };
    union {
        NTSTATUS                    ExitStatus;
        PVOID                       OfsChain;
    };
    LIST_ENTRY                      PostBlockList;
    union {
        PTERMINATION_PORT           TerminationPort;
        PETHREAD                    ReaperLink;
        PVOID                       KeyedWaitValue;
    };
    KSPIN_LOCK                      ActiveTimerListLock;
    LIST_ENTRY                      ActiveTimerListHead;
    CLIENT_ID                       Cid;
    union {
        KSEMAPHORE                  LpcReplySemaphore;
        KSEMAPHORE                  KeyedWaitSemaphore;
    };
    union {
        PLPC_MESSAGE                LpcReplyMessage;
        PVOID                       LpcWaitingOnPort;
    };
    PPS_IMPERSONATION_INFORMATION   ImpersonationInfo;
    LIST_ENTRY                      IrpList;
    ULONG                           TopLevelIrp;
    PDEVICE_OBJECT                  DeviceToVerify;
    PEPROCESS                       ThreadsProcess;  //xp.sp2=220
    PKSTART_ROUTINE                 StartAddress;    //xp.sp2=224
    union {													//xp.sp2=228
        PVOID                       Win32StartAddress;
        ULONG                       LpcReceivedMessageId;
    };
    LIST_ENTRY                      ThreadListEntry;		//xp.sp2=22c
    EX_RUNDOWN_REF                  RundownProtect;
    EX_PUSH_LOCK                    ThreadLock;
    ULONG                           LpcReplyMessageId;
    ULONG                           ReadClusterSize;
    ACCESS_MASK                     GrantedAccess;
    union {
        ULONG                       CrossThreadFlags;
        struct {
            ULONG                   Terminated              : 1;
            ULONG                   DeadThread              : 1;
            ULONG                   HideFromDebugger        : 1;
            ULONG                   ActiveImpersonationInfo : 1;
            ULONG                   SystemThread            : 1;
            ULONG                   HardErrorsAreDisabled   : 1;
            ULONG                   BreakOnTermination      : 1;
            ULONG                   SkipCreationMsg         : 1;
            ULONG                   SkipTerminationMsg      : 1;
        };
    };
    union {
        ULONG                       SameThreadPassiveFlags;
        struct {
            ULONG                   ActiveExWorker          : 1;
            ULONG                   ExWorkerCanWaitUser     : 1;
            ULONG                   MemoryMaker             : 1;
        };
    };
    union {
        ULONG                       SameThreadApcFlags;
        struct {
            BOOLEAN                 LpcReceivedMsgIdValid   : 1;
            BOOLEAN                 LpcExitThreadCalled     : 1;
            BOOLEAN                 AddressSpaceOwner       : 1;
        };
    };
    BOOLEAN                         ForwardClusterOnly;
    BOOLEAN                         DisablePageFaultClustering;
} ETHREAD, *PETHREAD;

#else

typedef struct _ETHREAD {
    KTHREAD                         Tcb;
    LARGE_INTEGER                   CreateTime;
    union {
        LARGE_INTEGER               ExitTime;
        LIST_ENTRY                  LpcReplyChain;
    };
    union {
        NTSTATUS                    ExitStatus;
        PVOID                       OfsChain;
    };
    LIST_ENTRY                      PostBlockList;
    LIST_ENTRY                      TerminationPortList;
    KSPIN_LOCK                      ActiveTimerListLock;
    LIST_ENTRY                      ActiveTimerListHead;
    CLIENT_ID                       Cid;
    KSEMAPHORE                      LpcReplySemaphore;
    PLPC_MESSAGE                    LpcReplyMessage;
    ULONG                           LpcReplyMessageId;
    ULONG                           PerformanceCountLow;
    PPS_IMPERSONATION_INFORMATION   ImpersonationInfo;
    LIST_ENTRY                      IrpList;
    PVOID                           TopLevelIrp;
    PDEVICE_OBJECT                  DeviceToVerify;
    ULONG                           ReadClusterSize;
    BOOLEAN                         ForwardClusterOnly;
    BOOLEAN                         DisablePageFaultClustering;
    BOOLEAN                         DeadThread;
#if (VER_PRODUCTBUILD >= 2195)
    BOOLEAN                         HideFromDebugger;
#endif // (VER_PRODUCTBUILD >= 2195)
#if (VER_PRODUCTBUILD < 2195)
    BOOLEAN                         HasTerminated;
#else // (VER_PRODUCTBUILD >= 2195)
    ULONG                           HasTerminated;
#endif // (VER_PRODUCTBUILD >= 2195)
#if (VER_PRODUCTBUILD < 2195)
    PKEVENT_PAIR                    EventPair;
#endif // (VER_PRODUCTBUILD < 2195)
    ACCESS_MASK                     GrantedAccess;
    PEPROCESS                       ThreadsProcess;
    PKSTART_ROUTINE                 StartAddress;
    union {
        PVOID                       Win32StartAddress;
        ULONG                       LpcReceivedMessageId;
    };
    BOOLEAN                         LpcExitThreadCalled;
    BOOLEAN                         HardErrorsAreDisabled;
    BOOLEAN                         LpcReceivedMsgIdValid;
    BOOLEAN                         ActiveImpersonationInfo;
    ULONG                           PerformanceCountHigh;
#if (VER_PRODUCTBUILD >= 2195)
    LIST_ENTRY                      ThreadListEntry;
#endif // (VER_PRODUCTBUILD >= 2195)
} ETHREAD, *PETHREAD;

#endif

typedef struct _EPROCESS_QUOTA_ENTRY {
    ULONG Usage;
    ULONG Limit;
    ULONG Peak;
    ULONG Return;
} EPROCESS_QUOTA_ENTRY, *PEPROCESS_QUOTA_ENTRY;

typedef struct _EPROCESS_QUOTA_BLOCK {
    EPROCESS_QUOTA_ENTRY    QuotaEntry[3];
    LIST_ENTRY              QuotaList;
    ULONG                   ReferenceCount;
    ULONG                   ProcessCount;
} EPROCESS_QUOTA_BLOCK, *PEPROCESS_QUOTA_BLOCK;

typedef struct _EXCEPTION_REGISTRATION_RECORD {
   struct _EXCEPTION_REGISTRATION_RECORD    *Next;
   PVOID                                    Handler;
} EXCEPTION_REGISTRATION_RECORD, *PEXCEPTION_REGISTRATION_RECORD;

/*
 * When needing these parameters cast your PIO_STACK_LOCATION to
 * PEXTENDED_IO_STACK_LOCATION
 */
#if !defined(_ALPHA_)
#include <pshpack4.h>
#endif
typedef struct _EXTENDED_IO_STACK_LOCATION {

    /* Included for padding */
    UCHAR MajorFunction;
    UCHAR MinorFunction;
    UCHAR Flags;
    UCHAR Control;

    union {

       struct {
          PIO_SECURITY_CONTEXT              SecurityContext;
          ULONG                             Options;
          USHORT                            Reserved;
          USHORT                            ShareAccess;
          PMAILSLOT_CREATE_PARAMETERS       Parameters;
       } CreateMailslot;

        struct {
            PIO_SECURITY_CONTEXT            SecurityContext;
            ULONG                           Options;
            USHORT                          Reserved;
            USHORT                          ShareAccess;
            PNAMED_PIPE_CREATE_PARAMETERS   Parameters;
        } CreatePipe;

        struct {
            ULONG                           OutputBufferLength;
            ULONG                           InputBufferLength;
            ULONG                           FsControlCode;
            PVOID                           Type3InputBuffer;
        } FileSystemControl;

        struct {
            PLARGE_INTEGER                  Length;
            ULONG                           Key;
            LARGE_INTEGER                   ByteOffset;
        } LockControl;

        struct {
            ULONG                           Length;
            ULONG                           CompletionFilter;
        } NotifyDirectory;

        struct {
            ULONG                           Length;
            PUNICODE_STRING                 FileName;
            FILE_INFORMATION_CLASS          FileInformationClass;
            ULONG                           FileIndex;
        } QueryDirectory;

        struct {
            ULONG                           Length;
            PVOID                           EaList;
            ULONG                           EaListLength;
            ULONG                           EaIndex;
        } QueryEa;

        struct {
            ULONG                           Length;
            PSID                            StartSid;
            PFILE_GET_QUOTA_INFORMATION     SidList;
            ULONG                           SidListLength;
        } QueryQuota;

        struct {
            ULONG                           Length;
        } SetEa;

        struct {
            ULONG                           Length;
        } SetQuota;

        struct {
            ULONG                           Length;
            FS_INFORMATION_CLASS            FsInformationClass;
        } SetVolume;

    } Parameters;

} EXTENDED_IO_STACK_LOCATION, *PEXTENDED_IO_STACK_LOCATION;
#if !defined(_ALPHA_)
#include <poppack.h>
#endif

typedef struct _FILE_ACCESS_INFORMATION {
    ACCESS_MASK AccessFlags;
} FILE_ACCESS_INFORMATION, *PFILE_ACCESS_INFORMATION;

typedef struct _FILE_ALLOCATION_INFORMATION {
    LARGE_INTEGER AllocationSize;
} FILE_ALLOCATION_INFORMATION, *PFILE_ALLOCATION_INFORMATION;

typedef struct _FILE_BOTH_DIR_INFORMATION {
    ULONG           NextEntryOffset;
    ULONG           FileIndex;
    LARGE_INTEGER   CreationTime;
    LARGE_INTEGER   LastAccessTime;
    LARGE_INTEGER   LastWriteTime;
    LARGE_INTEGER   ChangeTime;
    LARGE_INTEGER   EndOfFile;
    LARGE_INTEGER   AllocationSize;
    ULONG           FileAttributes;
    ULONG           FileNameLength;
    ULONG           EaSize;
    CCHAR           ShortNameLength;
    WCHAR           ShortName[12];
    WCHAR           FileName[1];
} FILE_BOTH_DIR_INFORMATION, *PFILE_BOTH_DIR_INFORMATION;

typedef struct _FILE_COMPLETION_INFORMATION {
    HANDLE  Port;
    ULONG   Key;
} FILE_COMPLETION_INFORMATION, *PFILE_COMPLETION_INFORMATION;

typedef struct _FILE_COMPRESSION_INFORMATION {
    LARGE_INTEGER   CompressedFileSize;
    USHORT          CompressionFormat;
    UCHAR           CompressionUnitShift;
    UCHAR           ChunkShift;
    UCHAR           ClusterShift;
    UCHAR           Reserved[3];
} FILE_COMPRESSION_INFORMATION, *PFILE_COMPRESSION_INFORMATION;

typedef struct _FILE_COPY_ON_WRITE_INFORMATION {
    BOOLEAN ReplaceIfExists;
    HANDLE  RootDirectory;
    ULONG   FileNameLength;
    WCHAR   FileName[1];
} FILE_COPY_ON_WRITE_INFORMATION, *PFILE_COPY_ON_WRITE_INFORMATION;

typedef struct _FILE_DIRECTORY_INFORMATION {
    ULONG           NextEntryOffset;
    ULONG           FileIndex;
    LARGE_INTEGER   CreationTime;
    LARGE_INTEGER   LastAccessTime;
    LARGE_INTEGER   LastWriteTime;
    LARGE_INTEGER   ChangeTime;
    LARGE_INTEGER   EndOfFile;
    LARGE_INTEGER   AllocationSize;
    ULONG           FileAttributes;
    ULONG           FileNameLength;
    WCHAR           FileName[1];
} FILE_DIRECTORY_INFORMATION, *PFILE_DIRECTORY_INFORMATION;

typedef struct _FILE_EA_INFORMATION {
    ULONG EaSize;
} FILE_EA_INFORMATION, *PFILE_EA_INFORMATION;

typedef struct _FILE_FS_ATTRIBUTE_INFORMATION {
    ULONG   FileSystemAttributes;
    ULONG   MaximumComponentNameLength;
    ULONG   FileSystemNameLength;
    WCHAR   FileSystemName[1];
} FILE_FS_ATTRIBUTE_INFORMATION, *PFILE_FS_ATTRIBUTE_INFORMATION;

typedef struct _FILE_FS_CONTROL_INFORMATION {
    LARGE_INTEGER   FreeSpaceStartFiltering;
    LARGE_INTEGER   FreeSpaceThreshold;
    LARGE_INTEGER   FreeSpaceStopFiltering;
    LARGE_INTEGER   DefaultQuotaThreshold;
    LARGE_INTEGER   DefaultQuotaLimit;
    ULONG           FileSystemControlFlags;
} FILE_FS_CONTROL_INFORMATION, *PFILE_FS_CONTROL_INFORMATION;

typedef struct _FILE_FS_FULL_SIZE_INFORMATION {
    LARGE_INTEGER   TotalAllocationUnits;
    LARGE_INTEGER   CallerAvailableAllocationUnits;
    LARGE_INTEGER   ActualAvailableAllocationUnits;
    ULONG           SectorsPerAllocationUnit;
    ULONG           BytesPerSector;
} FILE_FS_FULL_SIZE_INFORMATION, *PFILE_FS_FULL_SIZE_INFORMATION;

typedef struct _FILE_FS_LABEL_INFORMATION {
    ULONG VolumeLabelLength;
    WCHAR VolumeLabel[1];
} FILE_FS_LABEL_INFORMATION, *PFILE_FS_LABEL_INFORMATION;

#if (VER_PRODUCTBUILD >= 2195)

typedef struct _FILE_FS_OBJECT_ID_INFORMATION {
    UCHAR ObjectId[16];
    UCHAR ExtendedInfo[48];
} FILE_FS_OBJECT_ID_INFORMATION, *PFILE_FS_OBJECT_ID_INFORMATION;

#endif // (VER_PRODUCTBUILD >= 2195)

typedef struct _FILE_FS_SIZE_INFORMATION {
    LARGE_INTEGER   TotalAllocationUnits;
    LARGE_INTEGER   AvailableAllocationUnits;
    ULONG           SectorsPerAllocationUnit;
    ULONG           BytesPerSector;
} FILE_FS_SIZE_INFORMATION, *PFILE_FS_SIZE_INFORMATION;

typedef struct _FILE_FS_VOLUME_INFORMATION {
    LARGE_INTEGER   VolumeCreationTime;
    ULONG           VolumeSerialNumber;
    ULONG           VolumeLabelLength;
    BOOLEAN         SupportsObjects;
    WCHAR           VolumeLabel[1];
} FILE_FS_VOLUME_INFORMATION, *PFILE_FS_VOLUME_INFORMATION;

typedef struct _FILE_FULL_DIR_INFORMATION {
    ULONG           NextEntryOffset;
    ULONG           FileIndex;
    LARGE_INTEGER   CreationTime;
    LARGE_INTEGER   LastAccessTime;
    LARGE_INTEGER   LastWriteTime;
    LARGE_INTEGER   ChangeTime;
    LARGE_INTEGER   EndOfFile;
    LARGE_INTEGER   AllocationSize;
    ULONG           FileAttributes;
    ULONG           FileNameLength;
    ULONG           EaSize;
    WCHAR           FileName[1];
} FILE_FULL_DIR_INFORMATION, *PFILE_FULL_DIR_INFORMATION;

typedef struct _FILE_GET_EA_INFORMATION {
    ULONG   NextEntryOffset;
    UCHAR   EaNameLength;
    CHAR    EaName[1];
} FILE_GET_EA_INFORMATION, *PFILE_GET_EA_INFORMATION;

typedef struct _FILE_GET_QUOTA_INFORMATION {
    ULONG   NextEntryOffset;
    ULONG   SidLength;
    SID     Sid;
} FILE_GET_QUOTA_INFORMATION, *PFILE_GET_QUOTA_INFORMATION;

typedef struct _FILE_ID_BOTH_DIR_INFORMATION {
    ULONG           NextEntryOffset;
    ULONG           FileIndex;
    LARGE_INTEGER   CreationTime;
    LARGE_INTEGER   LastAccessTime;
    LARGE_INTEGER   LastWriteTime;
    LARGE_INTEGER   ChangeTime;
    LARGE_INTEGER   EndOfFile;
    LARGE_INTEGER   AllocationSize;
    ULONG           FileAttributes;
    ULONG           FileNameLength;
    ULONG           EaSize;
    CCHAR           ShortNameLength;
    WCHAR           ShortName[12];
    LARGE_INTEGER   FileId;
    WCHAR           FileName[1];
} FILE_ID_BOTH_DIR_INFORMATION, *PFILE_ID_BOTH_DIR_INFORMATION;

typedef struct _FILE_ID_FULL_DIR_INFORMATION {
    ULONG           NextEntryOffset;
    ULONG           FileIndex;
    LARGE_INTEGER   CreationTime;
    LARGE_INTEGER   LastAccessTime;
    LARGE_INTEGER   LastWriteTime;
    LARGE_INTEGER   ChangeTime;
    LARGE_INTEGER   EndOfFile;
    LARGE_INTEGER   AllocationSize;
    ULONG           FileAttributes;
    ULONG           FileNameLength;
    ULONG           EaSize;
    LARGE_INTEGER   FileId;
    WCHAR           FileName[1];
} FILE_ID_FULL_DIR_INFORMATION, *PFILE_ID_FULL_DIR_INFORMATION;

typedef struct _FILE_INTERNAL_INFORMATION {
    LARGE_INTEGER IndexNumber;
} FILE_INTERNAL_INFORMATION, *PFILE_INTERNAL_INFORMATION;

typedef struct _FILE_LINK_INFORMATION {
    BOOLEAN ReplaceIfExists;
    HANDLE  RootDirectory;
    ULONG   FileNameLength;
    WCHAR   FileName[1];
} FILE_LINK_INFORMATION, *PFILE_LINK_INFORMATION;

typedef struct _FILE_LOCK_INFO {
    LARGE_INTEGER   StartingByte;
    LARGE_INTEGER   Length;
    BOOLEAN         ExclusiveLock;
    ULONG           Key;
    PFILE_OBJECT    FileObject;
    PEPROCESS       Process;
    LARGE_INTEGER   EndingByte;
} FILE_LOCK_INFO, *PFILE_LOCK_INFO;

// raw internal file lock struct returned from FsRtlGetNextFileLock
typedef struct _FILE_SHARED_LOCK_ENTRY {
    PVOID           Unknown1;
    PVOID           Unknown2;
    FILE_LOCK_INFO  FileLock;
} FILE_SHARED_LOCK_ENTRY, *PFILE_SHARED_LOCK_ENTRY;

// raw internal file lock struct returned from FsRtlGetNextFileLock
typedef struct _FILE_EXCLUSIVE_LOCK_ENTRY {
    LIST_ENTRY      ListEntry;
    PVOID           Unknown1;
    PVOID           Unknown2;
    FILE_LOCK_INFO  FileLock;
} FILE_EXCLUSIVE_LOCK_ENTRY, *PFILE_EXCLUSIVE_LOCK_ENTRY;

typedef NTSTATUS (*PCOMPLETE_LOCK_IRP_ROUTINE) (
    IN PVOID    Context,
    IN PIRP     Irp
);

typedef VOID (*PUNLOCK_ROUTINE) (
    IN PVOID            Context,
    IN PFILE_LOCK_INFO  FileLockInfo
);

typedef struct _FILE_LOCK {
    PCOMPLETE_LOCK_IRP_ROUTINE  CompleteLockIrpRoutine;
    PUNLOCK_ROUTINE             UnlockRoutine;
    BOOLEAN                     FastIoIsQuestionable;
    BOOLEAN                     Pad[3];
    PVOID                       LockInformation;
    FILE_LOCK_INFO              LastReturnedLockInfo;
    PVOID                       LastReturnedLock;
} FILE_LOCK, *PFILE_LOCK;

typedef struct _FILE_MAILSLOT_PEEK_BUFFER {
    ULONG ReadDataAvailable;
    ULONG NumberOfMessages;
    ULONG MessageLength;
} FILE_MAILSLOT_PEEK_BUFFER, *PFILE_MAILSLOT_PEEK_BUFFER;

typedef struct _FILE_MAILSLOT_QUERY_INFORMATION {
    ULONG           MaximumMessageSize;
    ULONG           MailslotQuota;
    ULONG           NextMessageSize;
    ULONG           MessagesAvailable;
    LARGE_INTEGER   ReadTimeout;
} FILE_MAILSLOT_QUERY_INFORMATION, *PFILE_MAILSLOT_QUERY_INFORMATION;

typedef struct _FILE_MAILSLOT_SET_INFORMATION {
    PLARGE_INTEGER ReadTimeout;
} FILE_MAILSLOT_SET_INFORMATION, *PFILE_MAILSLOT_SET_INFORMATION;

typedef struct _FILE_MODE_INFORMATION {
    ULONG Mode;
} FILE_MODE_INFORMATION, *PFILE_MODE_INFORMATION;

// This structure is included in the Windows 2000 DDK but is missing in the
// Windows NT 4.0 DDK
#if (VER_PRODUCTBUILD < 2195)
typedef struct _FILE_NAME_INFORMATION {
    ULONG FileNameLength;
    WCHAR FileName[1];
} FILE_NAME_INFORMATION, *PFILE_NAME_INFORMATION;
#endif // (VER_PRODUCTBUILD < 2195)

typedef struct _FILE_ALL_INFORMATION {
    FILE_BASIC_INFORMATION      BasicInformation;
    FILE_STANDARD_INFORMATION   StandardInformation;
    FILE_INTERNAL_INFORMATION   InternalInformation;
    FILE_EA_INFORMATION         EaInformation;
    FILE_ACCESS_INFORMATION     AccessInformation;
    FILE_POSITION_INFORMATION   PositionInformation;
    FILE_MODE_INFORMATION       ModeInformation;
    FILE_ALIGNMENT_INFORMATION  AlignmentInformation;
    FILE_NAME_INFORMATION       NameInformation;
} FILE_ALL_INFORMATION, *PFILE_ALL_INFORMATION;

typedef struct _FILE_NAMES_INFORMATION {
    ULONG NextEntryOffset;
    ULONG FileIndex;
    ULONG FileNameLength;
    WCHAR FileName[1];
} FILE_NAMES_INFORMATION, *PFILE_NAMES_INFORMATION;

typedef struct _FILE_NOTIFY_INFORMATION {
    ULONG NextEntryOffset;
    ULONG Action;
    ULONG FileNameLength;
    WCHAR FileName[1];
} FILE_NOTIFY_INFORMATION, *PFILE_NOTIFY_INFORMATION;

typedef struct _FILE_OBJECTID_INFORMATION {
    LONGLONG        FileReference;
    UCHAR           ObjectId[16];
    union {
        struct {
            UCHAR   BirthVolumeId[16];
            UCHAR   BirthObjectId[16];
            UCHAR   DomainId[16];
        } ;
        UCHAR       ExtendedInfo[48];
    };
} FILE_OBJECTID_INFORMATION, *PFILE_OBJECTID_INFORMATION;

typedef struct _FILE_OLE_CLASSID_INFORMATION {
    GUID ClassId;
} FILE_OLE_CLASSID_INFORMATION, *PFILE_OLE_CLASSID_INFORMATION;

typedef struct _FILE_OLE_ALL_INFORMATION {
    FILE_BASIC_INFORMATION          BasicInformation;
    FILE_STANDARD_INFORMATION       StandardInformation;
    FILE_INTERNAL_INFORMATION       InternalInformation;
    FILE_EA_INFORMATION             EaInformation;
    FILE_ACCESS_INFORMATION         AccessInformation;
    FILE_POSITION_INFORMATION       PositionInformation;
    FILE_MODE_INFORMATION           ModeInformation;
    FILE_ALIGNMENT_INFORMATION      AlignmentInformation;
    USN                             LastChangeUsn;
    USN                             ReplicationUsn;
    LARGE_INTEGER                   SecurityChangeTime;
    FILE_OLE_CLASSID_INFORMATION    OleClassIdInformation;
    FILE_OBJECTID_INFORMATION       ObjectIdInformation;
    FILE_STORAGE_TYPE               StorageType;
    ULONG                           OleStateBits;
    ULONG                           OleId;
    ULONG                           NumberOfStreamReferences;
    ULONG                           StreamIndex;
    ULONG                           SecurityId;
    BOOLEAN                         ContentIndexDisable;
    BOOLEAN                         InheritContentIndexDisable;
    FILE_NAME_INFORMATION           NameInformation;
} FILE_OLE_ALL_INFORMATION, *PFILE_OLE_ALL_INFORMATION;

typedef struct _FILE_OLE_DIR_INFORMATION {
    ULONG               NextEntryOffset;
    ULONG               FileIndex;
    LARGE_INTEGER       CreationTime;
    LARGE_INTEGER       LastAccessTime;
    LARGE_INTEGER       LastWriteTime;
    LARGE_INTEGER       ChangeTime;
    LARGE_INTEGER       EndOfFile;
    LARGE_INTEGER       AllocationSize;
    ULONG               FileAttributes;
    ULONG               FileNameLength;
    FILE_STORAGE_TYPE   StorageType;
    GUID                OleClassId;
    ULONG               OleStateBits;
    BOOLEAN             ContentIndexDisable;
    BOOLEAN             InheritContentIndexDisable;
    WCHAR               FileName[1];
} FILE_OLE_DIR_INFORMATION, *PFILE_OLE_DIR_INFORMATION;

typedef struct _FILE_OLE_INFORMATION {
    LARGE_INTEGER                   SecurityChangeTime;
    FILE_OLE_CLASSID_INFORMATION    OleClassIdInformation;
    FILE_OBJECTID_INFORMATION       ObjectIdInformation;
    FILE_STORAGE_TYPE               StorageType;
    ULONG                           OleStateBits;
    BOOLEAN                         ContentIndexDisable;
    BOOLEAN                         InheritContentIndexDisable;
} FILE_OLE_INFORMATION, *PFILE_OLE_INFORMATION;

typedef struct _FILE_OLE_STATE_BITS_INFORMATION {
    ULONG StateBits;
    ULONG StateBitsMask;
} FILE_OLE_STATE_BITS_INFORMATION, *PFILE_OLE_STATE_BITS_INFORMATION;

typedef struct _FILE_PIPE_ASSIGN_EVENT_BUFFER {
    HANDLE  EventHandle;
    ULONG   KeyValue;
} FILE_PIPE_ASSIGN_EVENT_BUFFER, *PFILE_PIPE_ASSIGN_EVENT_BUFFER;

typedef struct _FILE_PIPE_CLIENT_PROCESS_BUFFER {
    PVOID ClientSession;
    PVOID ClientProcess;
} FILE_PIPE_CLIENT_PROCESS_BUFFER, *PFILE_PIPE_CLIENT_PROCESS_BUFFER;

typedef struct _FILE_PIPE_EVENT_BUFFER {
    ULONG NamedPipeState;
    ULONG EntryType;
    ULONG ByteCount;
    ULONG KeyValue;
    ULONG NumberRequests;
} FILE_PIPE_EVENT_BUFFER, *PFILE_PIPE_EVENT_BUFFER;

typedef struct _FILE_PIPE_INFORMATION {
    ULONG ReadMode;
    ULONG CompletionMode;
} FILE_PIPE_INFORMATION, *PFILE_PIPE_INFORMATION;

typedef struct _FILE_PIPE_LOCAL_INFORMATION {
    ULONG NamedPipeType;
    ULONG NamedPipeConfiguration;
    ULONG MaximumInstances;
    ULONG CurrentInstances;
    ULONG InboundQuota;
    ULONG ReadDataAvailable;
    ULONG OutboundQuota;
    ULONG WriteQuotaAvailable;
    ULONG NamedPipeState;
    ULONG NamedPipeEnd;
} FILE_PIPE_LOCAL_INFORMATION, *PFILE_PIPE_LOCAL_INFORMATION;

typedef struct _FILE_PIPE_PEEK_BUFFER {
    ULONG   NamedPipeState;
    ULONG   ReadDataAvailable;
    ULONG   NumberOfMessages;
    ULONG   MessageLength;
    CHAR    Data[1];
} FILE_PIPE_PEEK_BUFFER, *PFILE_PIPE_PEEK_BUFFER;

typedef struct _FILE_PIPE_REMOTE_INFORMATION {
    LARGE_INTEGER   CollectDataTime;
    ULONG           MaximumCollectionCount;
} FILE_PIPE_REMOTE_INFORMATION, *PFILE_PIPE_REMOTE_INFORMATION;

typedef struct _FILE_PIPE_WAIT_FOR_BUFFER {
    LARGE_INTEGER   Timeout;
    ULONG           NameLength;
    BOOLEAN         TimeoutSpecified;
    WCHAR           Name[1];
} FILE_PIPE_WAIT_FOR_BUFFER, *PFILE_PIPE_WAIT_FOR_BUFFER;

typedef struct _FILE_QUOTA_INFORMATION {
    ULONG           NextEntryOffset;
    ULONG           SidLength;
    LARGE_INTEGER   ChangeTime;
    LARGE_INTEGER   QuotaUsed;
    LARGE_INTEGER   QuotaThreshold;
    LARGE_INTEGER   QuotaLimit;
    SID             Sid;
} FILE_QUOTA_INFORMATION, *PFILE_QUOTA_INFORMATION;

typedef struct _FILE_RENAME_INFORMATION {
    BOOLEAN ReplaceIfExists;
    HANDLE  RootDirectory;
    ULONG   FileNameLength;
    WCHAR   FileName[1];
} FILE_RENAME_INFORMATION, *PFILE_RENAME_INFORMATION;

typedef struct _FILE_STREAM_INFORMATION {
    ULONG           NextEntryOffset;
    ULONG           StreamNameLength;
    LARGE_INTEGER   StreamSize;
    LARGE_INTEGER   StreamAllocationSize;
    WCHAR           StreamName[1];
} FILE_STREAM_INFORMATION, *PFILE_STREAM_INFORMATION;

typedef struct _FILE_TRACKING_INFORMATION {
    HANDLE  DestinationFile;
    ULONG   ObjectInformationLength;
    CHAR    ObjectInformation[1];
} FILE_TRACKING_INFORMATION, *PFILE_TRACKING_INFORMATION;

typedef struct _FSRTL_COMMON_FCB_HEADER {
    CSHORT          NodeTypeCode;
    CSHORT          NodeByteSize;
    UCHAR           Flags;
    UCHAR           IsFastIoPossible;
#if (VER_PRODUCTBUILD >= 1381)
    UCHAR           Flags2;
    UCHAR           Reserved;
#endif // (VER_PRODUCTBUILD >= 1381)
    PERESOURCE      Resource;
    PERESOURCE      PagingIoResource;
    LARGE_INTEGER   AllocationSize;
    LARGE_INTEGER   FileSize;
    LARGE_INTEGER   ValidDataLength;
} FSRTL_COMMON_FCB_HEADER, *PFSRTL_COMMON_FCB_HEADER;

typedef struct _GENERATE_NAME_CONTEXT {
    USHORT  Checksum;
    BOOLEAN CheckSumInserted;
    UCHAR   NameLength;
    WCHAR   NameBuffer[8];
    ULONG   ExtensionLength;
    WCHAR   ExtensionBuffer[4];
    ULONG   LastIndexValue;
} GENERATE_NAME_CONTEXT, *PGENERATE_NAME_CONTEXT;

typedef struct _HANDLE_TABLE_ENTRY_INFO {
    ULONG AuditMask;
} HANDLE_TABLE_ENTRY_INFO, *PHANDLE_TABLE_ENTRY_INFO;

typedef struct _HANDLE_TABLE_ENTRY {
    union {
        PVOID                       Object;
        ULONG                       ObAttributes;
        PHANDLE_TABLE_ENTRY_INFO    InfoTable;
        ULONG                       Value;
    };
    union {
        ULONG                       GrantedAccess;
        USHORT                      GrantedAccessIndex;
        LONG                        NextFreeTableEntry;
    };
    USHORT                          CreatorBackTraceIndex;
} HANDLE_TABLE_ENTRY, *PHANDLE_TABLE_ENTRY;

typedef struct _MAPPING_PAIR {
    ULONGLONG Vcn;
    ULONGLONG Lcn;
} MAPPING_PAIR, *PMAPPING_PAIR;

typedef struct _GET_RETRIEVAL_DESCRIPTOR {
    ULONG           NumberOfPairs;
    ULONGLONG       StartVcn;
    MAPPING_PAIR    Pair[1];
} GET_RETRIEVAL_DESCRIPTOR, *PGET_RETRIEVAL_DESCRIPTOR;

typedef struct _INITIAL_TEB {
    ULONG Unknown_1;
    ULONG Unknown_2;
    PVOID StackTop;
    PVOID StackBase;
    PVOID Unknown_3;
} INITIAL_TEB, *PINITIAL_TEB;

typedef struct _IO_CLIENT_EXTENSION {
    struct _IO_CLIENT_EXTENSION *NextExtension;
    PVOID                       ClientIdentificationAddress;
} IO_CLIENT_EXTENSION, *PIO_CLIENT_EXTENSION;

typedef struct _IO_COMPLETION_BASIC_INFORMATION {
    LONG Depth;
} IO_COMPLETION_BASIC_INFORMATION, *PIO_COMPLETION_BASIC_INFORMATION;

typedef struct _KEVENT_PAIR {
    USHORT Type;
    USHORT Size;
    KEVENT Event1;
    KEVENT Event2;
} KEVENT_PAIR, *PKEVENT_PAIR;

typedef struct _KINTERRUPT {
    CSHORT              Type;
    CSHORT              Size;
    LIST_ENTRY          InterruptListEntry;
    PKSERVICE_ROUTINE   ServiceRoutine;
    PVOID               ServiceContext;
    KSPIN_LOCK          SpinLock;
    ULONG               TickCount;
    PKSPIN_LOCK         ActualLock;
    PVOID               DispatchAddress;
    ULONG               Vector;
    KIRQL               Irql;
    KIRQL               SynchronizeIrql;
    BOOLEAN             FloatingSave;
    BOOLEAN             Connected;
    CHAR                Number;
    UCHAR               ShareVector;
    KINTERRUPT_MODE     Mode;
    ULONG               ServiceCount;
    ULONG               DispatchCount;
    ULONG               DispatchCode[106];
} KINTERRUPT, *PKINTERRUPT;

typedef struct _KQUEUE {
    DISPATCHER_HEADER   Header;
    LIST_ENTRY          EntryListHead;
    ULONG               CurrentCount;
    ULONG               MaximumCount;
    LIST_ENTRY          ThreadListHead;
} KQUEUE, *PKQUEUE, *RESTRICTED_POINTER PRKQUEUE;

typedef struct _LARGE_MCB {
    PFAST_MUTEX FastMutex;
    ULONG       MaximumPairCount;
    ULONG       PairCount;
    POOL_TYPE   PoolType;
    PVOID       Mapping;
} LARGE_MCB, *PLARGE_MCB;

typedef struct _LPC_MESSAGE {
    USHORT      DataSize;
    USHORT      MessageSize;
    USHORT      MessageType;
    USHORT      VirtualRangesOffset;
    CLIENT_ID   ClientId;
    ULONG       MessageId;
    ULONG       SectionSize;
    UCHAR       Data[1];
} LPC_MESSAGE, *PLPC_MESSAGE;

typedef struct _LPC_SECTION_READ {
    ULONG Length;
    ULONG ViewSize;
    PVOID ViewBase;
} LPC_SECTION_READ, *PLPC_SECTION_READ;

typedef struct _LPC_SECTION_WRITE {
    ULONG   Length;
    HANDLE  SectionHandle;
    ULONG   SectionOffset;
    ULONG   ViewSize;
    PVOID   ViewBase;
    PVOID   TargetViewBase;
} LPC_SECTION_WRITE, *PLPC_SECTION_WRITE;

typedef struct _MAILSLOT_CREATE_PARAMETERS {
    ULONG           MailslotQuota;
    ULONG           MaximumMessageSize;
    LARGE_INTEGER   ReadTimeout;
    BOOLEAN         TimeoutSpecified;
} MAILSLOT_CREATE_PARAMETERS, *PMAILSLOT_CREATE_PARAMETERS;

typedef struct _MBCB {
    CSHORT          NodeTypeCode;
    CSHORT          NodeIsInZone;
    ULONG           PagesToWrite;
    ULONG           DirtyPages;
    ULONG           Reserved;
    LIST_ENTRY      BitmapRanges;
    LONGLONG        ResumeWritePage;
    BITMAP_RANGE    BitmapRange1;
    BITMAP_RANGE    BitmapRange2;
    BITMAP_RANGE    BitmapRange3;
} MBCB, *PMBCB;

typedef struct _MCB {
    LARGE_MCB LargeMcb;
} MCB, *PMCB;

typedef struct _MOVEFILE_DESCRIPTOR {
     HANDLE         FileHandle; 
     ULONG          Reserved;   
     LARGE_INTEGER  StartVcn; 
     LARGE_INTEGER  TargetLcn;
     ULONG          NumVcns; 
     ULONG          Reserved1;  
} MOVEFILE_DESCRIPTOR, *PMOVEFILE_DESCRIPTOR;

typedef struct _NAMED_PIPE_CREATE_PARAMETERS {
    ULONG           NamedPipeType;
    ULONG           ReadMode;
    ULONG           CompletionMode;
    ULONG           MaximumInstances;
    ULONG           InboundQuota;
    ULONG           OutboundQuota;
    LARGE_INTEGER   DefaultTimeout;
    BOOLEAN         TimeoutSpecified;
} NAMED_PIPE_CREATE_PARAMETERS, *PNAMED_PIPE_CREATE_PARAMETERS;

typedef struct _OBJECT_BASIC_INFO {
    ULONG           Attributes;
    ACCESS_MASK     GrantedAccess;
    ULONG           HandleCount;
    ULONG           ReferenceCount;
    ULONG           PagedPoolUsage;
    ULONG           NonPagedPoolUsage;
    ULONG           Reserved[3];
    ULONG           NameInformationLength;
    ULONG           TypeInformationLength;
    ULONG           SecurityDescriptorLength;
    LARGE_INTEGER   CreateTime;
} OBJECT_BASIC_INFO, *POBJECT_BASIC_INFO;

typedef struct _OBJECT_HANDLE_ATTRIBUTE_INFO {
    BOOLEAN Inherit;
    BOOLEAN ProtectFromClose;
} OBJECT_HANDLE_ATTRIBUTE_INFO, *POBJECT_HANDLE_ATTRIBUTE_INFO;

typedef struct _OBJECT_HEADER {
    ULONG           Flags;
    PVOID           Unknown1;
    PVOID           DirectoryObject;
    UNICODE_STRING  ObjectName;
    ULONG           Reserved;
    ULONG           HandleCount;
    ULONG           ReferenceCount;
    PVOID           ObjectMethods;  // (@+0x40 is the unicode type name)
    ULONG           Unknown2;
    ULONG           Unknown3;
    ULONG           Unknown4;
} OBJECT_HEADER, *POBJECT_HEADER;

typedef struct _OBJECT_NAME_INFO {
    UNICODE_STRING  ObjectName;
    WCHAR           ObjectNameBuffer[1];
} OBJECT_NAME_INFO, *POBJECT_NAME_INFO;

typedef struct _OBJECT_PROTECTION_INFO {
    BOOLEAN Inherit;
    BOOLEAN ProtectHandle;
} OBJECT_PROTECTION_INFO, *POBJECT_PROTECTION_INFO;

typedef struct _OBJECT_TYPE {
    ERESOURCE               Mutex;
    LIST_ENTRY              TypeList;
    UNICODE_STRING          Name;
    PVOID                   DefaultObject;
    ULONG                   Index;
    ULONG                   TotalNumberOfObjects;
    ULONG                   TotalNumberOfHandles;
    ULONG                   HighWaterNumberOfObjects;
    ULONG                   HighWaterNumberOfHandles;
    //OBJECT_TYPE_INITIALIZER TypeInfo;
    ULONG                   Key;
    ERESOURCE               ObjectLocks[4];
} OBJECT_TYPE, *POBJECT_TYPE;

typedef struct _OBJECT_TYPE_INFO {
    UNICODE_STRING  ObjectTypeName;
    UCHAR           Unknown[0x58];
    WCHAR           ObjectTypeNameBuffer[1];
} OBJECT_TYPE_INFO, *POBJECT_TYPE_INFO;

typedef struct _OBJECT_ALL_TYPES_INFO {
    ULONG               NumberOfObjectTypes;
    OBJECT_TYPE_INFO    ObjectsTypeInfo[1];
} OBJECT_ALL_TYPES_INFO, *POBJECT_ALL_TYPES_INFO;

typedef struct _PAGEFAULT_HISTORY {
    ULONG                           CurrentIndex;
    ULONG                           MaxIndex;
    KSPIN_LOCK                      SpinLock;
    PVOID                           Reserved;
    PROCESS_WS_WATCH_INFORMATION    WatchInfo[1];
} PAGEFAULT_HISTORY, *PPAGEFAULT_HISTORY;

typedef struct _PATHNAME_BUFFER {
    ULONG PathNameLength;
    WCHAR Name[1];
} PATHNAME_BUFFER, *PPATHNAME_BUFFER;

#if (VER_PRODUCTBUILD >= 2600)

typedef struct _PRIVATE_CACHE_MAP_FLAGS {
    ULONG DontUse           : 16;
    ULONG ReadAheadActive   : 1;
    ULONG ReadAheadEnabled  : 1;
    ULONG Available         : 14;
} PRIVATE_CACHE_MAP_FLAGS, *PPRIVATE_CACHE_MAP_FLAGS;

typedef struct _PRIVATE_CACHE_MAP {
    union {
        CSHORT                  NodeTypeCode;
        PRIVATE_CACHE_MAP_FLAGS Flags;
        ULONG                   UlongFlags;
    };
    ULONG                       ReadAheadMask;
    PFILE_OBJECT                FileObject;
    LARGE_INTEGER               FileOffset1;
    LARGE_INTEGER               BeyondLastByte1;
    LARGE_INTEGER               FileOffset2;
    LARGE_INTEGER               BeyondLastByte2;
    LARGE_INTEGER               ReadAheadOffset[2];
    ULONG                       ReadAheadLength[2];
    KSPIN_LOCK                  ReadAheadSpinLock;
    LIST_ENTRY                  PrivateLinks;
} PRIVATE_CACHE_MAP, *PPRIVATE_CACHE_MAP;

#endif

typedef struct _PROCESS_PRIORITY_CLASS {
    BOOLEAN Foreground;
    UCHAR   PriorityClass;
} PROCESS_PRIORITY_CLASS, *PPROCESS_PRIORITY_CLASS;

typedef struct _PS_IMPERSONATION_INFORMATION {
    PACCESS_TOKEN                   Token;
    BOOLEAN                         CopyOnOpen;
    BOOLEAN                         EffectiveOnly;
    SECURITY_IMPERSONATION_LEVEL    ImpersonationLevel;
} PS_IMPERSONATION_INFORMATION, *PPS_IMPERSONATION_INFORMATION;

typedef struct _PUBLIC_BCB {
    CSHORT          NodeTypeCode;
    CSHORT          NodeByteSize;
    ULONG           MappedLength;
    LARGE_INTEGER   MappedFileOffset;
} PUBLIC_BCB, *PPUBLIC_BCB;

typedef struct _QUERY_PATH_REQUEST {
    ULONG                   PathNameLength;
    PIO_SECURITY_CONTEXT    SecurityContext;
    WCHAR                   FilePathName[1];
} QUERY_PATH_REQUEST, *PQUERY_PATH_REQUEST;

typedef struct _QUERY_PATH_RESPONSE {
    ULONG LengthAccepted;
} QUERY_PATH_RESPONSE, *PQUERY_PATH_RESPONSE;

typedef struct _REPARSE_DATA_BUFFER {

    ULONG  ReparseTag;
    USHORT ReparseDataLength;
    USHORT Reserved;

    union {

        struct {
            USHORT  SubstituteNameOffset;
            USHORT  SubstituteNameLength;
            USHORT  PrintNameOffset;
            USHORT  PrintNameLength;
            WCHAR   PathBuffer[1];
        } SymbolicLinkReparseBuffer;

        struct {
            USHORT  SubstituteNameOffset;
            USHORT  SubstituteNameLength;
            USHORT  PrintNameOffset;
            USHORT  PrintNameLength;
            WCHAR   PathBuffer[1];
        } MountPointReparseBuffer;

        struct {
            UCHAR   DataBuffer[1];
        } GenericReparseBuffer;
    };

} REPARSE_DATA_BUFFER, *PREPARSE_DATA_BUFFER;

typedef struct _RETRIEVAL_POINTERS_BUFFER {
    ULONG               ExtentCount;
    LARGE_INTEGER       StartingVcn;
    struct {
        LARGE_INTEGER   NextVcn;
        LARGE_INTEGER   Lcn;
    } Extents[1];
} RETRIEVAL_POINTERS_BUFFER, *PRETRIEVAL_POINTERS_BUFFER;

typedef struct _RTL_SPLAY_LINKS {
    struct _RTL_SPLAY_LINKS *Parent;
    struct _RTL_SPLAY_LINKS *LeftChild;
    struct _RTL_SPLAY_LINKS *RightChild;
} RTL_SPLAY_LINKS, *PRTL_SPLAY_LINKS;

typedef struct _SE_EXPORTS {

    LUID    SeCreateTokenPrivilege;
    LUID    SeAssignPrimaryTokenPrivilege;
    LUID    SeLockMemoryPrivilege;
    LUID    SeIncreaseQuotaPrivilege;
    LUID    SeUnsolicitedInputPrivilege;
    LUID    SeTcbPrivilege;
    LUID    SeSecurityPrivilege;
    LUID    SeTakeOwnershipPrivilege;
    LUID    SeLoadDriverPrivilege;
    LUID    SeCreatePagefilePrivilege;
    LUID    SeIncreaseBasePriorityPrivilege;
    LUID    SeSystemProfilePrivilege;
    LUID    SeSystemtimePrivilege;
    LUID    SeProfileSingleProcessPrivilege;
    LUID    SeCreatePermanentPrivilege;
    LUID    SeBackupPrivilege;
    LUID    SeRestorePrivilege;
    LUID    SeShutdownPrivilege;
    LUID    SeDebugPrivilege;
    LUID    SeAuditPrivilege;
    LUID    SeSystemEnvironmentPrivilege;
    LUID    SeChangeNotifyPrivilege;
    LUID    SeRemoteShutdownPrivilege;

    PSID    SeNullSid;
    PSID    SeWorldSid;
    PSID    SeLocalSid;
    PSID    SeCreatorOwnerSid;
    PSID    SeCreatorGroupSid;

    PSID    SeNtAuthoritySid;
    PSID    SeDialupSid;
    PSID    SeNetworkSid;
    PSID    SeBatchSid;
    PSID    SeInteractiveSid;
    PSID    SeLocalSystemSid;
    PSID    SeAliasAdminsSid;
    PSID    SeAliasUsersSid;
    PSID    SeAliasGuestsSid;
    PSID    SeAliasPowerUsersSid;
    PSID    SeAliasAccountOpsSid;
    PSID    SeAliasSystemOpsSid;
    PSID    SeAliasPrintOpsSid;
    PSID    SeAliasBackupOpsSid;

    PSID    SeAuthenticatedUsersSid;

    PSID    SeRestrictedSid;
    PSID    SeAnonymousLogonSid;

    LUID    SeUndockPrivilege;
    LUID    SeSyncAgentPrivilege;
    LUID    SeEnableDelegationPrivilege;

} SE_EXPORTS, *PSE_EXPORTS;

typedef struct _SECTION_BASIC_INFORMATION {
    PVOID           BaseAddress;
    ULONG           Attributes;
    LARGE_INTEGER   Size;
} SECTION_BASIC_INFORMATION, *PSECTION_BASIC_INFORMATION;

typedef struct _SECTION_IMAGE_INFORMATION {
    PVOID   EntryPoint;
    ULONG   Unknown1;
    ULONG   StackReserve;
    ULONG   StackCommit;
    ULONG   Subsystem;
    USHORT  MinorSubsystemVersion;
    USHORT  MajorSubsystemVersion;
    ULONG   Unknown2;
    ULONG   Characteristics;
    USHORT  ImageNumber;
    BOOLEAN Executable;
    UCHAR   Unknown3;
    ULONG   Unknown4[3];
} SECTION_IMAGE_INFORMATION, *PSECTION_IMAGE_INFORMATION;

typedef struct _SECTION_OBJECT {
    PVOID                   StartingVa;
    PVOID                   EndingVa;
    struct _SECTION_OBJECT  *Parent;
    struct _SECTION_OBJECT  *LeftChild;
    struct _SECTION_OBJECT  *RightChild;
    PVOID                   Segment;
} SECTION_OBJECT, *PSECTION_OBJECT;

typedef struct _SERVICE_DESCRIPTOR_TABLE {
    /*
     * Table containing cServices elements of pointers to service handler
     * functions, indexed by service ID.
     */
    PVOID   *ServiceTable;
    /*
     * Table that counts how many times each service is used. This table
     * is only updated in checked builds.
     */
    PULONG  CounterTable;
    /*
     * Number of services contained in this table.
     */
    ULONG   TableSize;
    /*
     * Table containing the number of bytes of parameters the handler
     * function takes.
     */
    PUCHAR  ArgumentTable;
} SERVICE_DESCRIPTOR_TABLE, *PSERVICE_DESCRIPTOR_TABLE;

#if (VER_PRODUCTBUILD >= 2600)

typedef struct _SHARED_CACHE_MAP {
    CSHORT                      NodeTypeCode;
    CSHORT                      NodeByteSize;
    ULONG                       OpenCount;
    LARGE_INTEGER               FileSize;
    LIST_ENTRY                  BcbList;
    LARGE_INTEGER               SectionSize;
    LARGE_INTEGER               ValidDataLength;
    LARGE_INTEGER               ValidDataGoal;
    PVACB                       InitialVacbs[4];
    PVACB                       *Vacbs;
    PFILE_OBJECT                FileObject;
    PVACB                       ActiveVacb;
    PVOID                       NeedToZero;
    ULONG                       ActivePage;
    ULONG                       NeedToZeroPage;
    KSPIN_LOCK                  ActiveVacbSpinLock;
    ULONG                       VacbActiveCount;
    ULONG                       DirtyPages;
    LIST_ENTRY                  SharedCacheMapLinks;
    ULONG                       Flags;
    NTSTATUS                    Status;
    PMBCB                       Mbcb;
    PVOID                       Section;
    PKEVENT                     CreateEvent;
    PKEVENT                     WaitOnActiveCount;
    ULONG                       PagesToWrite;
    LONGLONG                    BeyondLastFlush;
    PCACHE_MANAGER_CALLBACKS    Callbacks;
    PVOID                       LazyWriteContext;
    LIST_ENTRY                  PrivateList;
    PVOID                       LogHandle;
    PVOID                       FlushToLsnRoutine;
    ULONG                       DirtyPageThreshold;
    ULONG                       LazyWritePassCount;
    PCACHE_UNINITIALIZE_EVENT   UninitializeEvent;
    PVACB                       NeedToZeroVacb;
    KSPIN_LOCK                  BcbSpinLock;
    PVOID                       Reserved;
    KEVENT                      Event;
    EX_PUSH_LOCK                VacbPushLock;
    PRIVATE_CACHE_MAP           PrivateCacheMap;
} SHARED_CACHE_MAP, *PSHARED_CACHE_MAP;

#endif

typedef struct _SID_AND_ATTRIBUTES {
    PSID    Sid;
    ULONG   Attributes;
} SID_AND_ATTRIBUTES, *PSID_AND_ATTRIBUTES;

typedef struct _STARTING_VCN_INPUT_BUFFER {
    LARGE_INTEGER StartingVcn;
} STARTING_VCN_INPUT_BUFFER, *PSTARTING_VCN_INPUT_BUFFER;

// SystemBasicInformation
typedef struct _SYSTEM_BASIC_INFORMATION {
    ULONG Unknown;
    ULONG MaximumIncrement;
    ULONG PhysicalPageSize;
    ULONG NumberOfPhysicalPages;
    ULONG LowestPhysicalPage;
    ULONG HighestPhysicalPage;
    ULONG AllocationGranularity;
    ULONG LowestUserAddress;
    ULONG HighestUserAddress;
    ULONG ActiveProcessors;
    UCHAR NumberProcessors;
} SYSTEM_BASIC_INFORMATION, *PSYSTEM_BASIC_INFORMATION;

// SystemProcessorInformation
typedef struct _SYSTEM_PROCESSOR_INFORMATION {
    USHORT  ProcessorArchitecture;
    USHORT  ProcessorLevel;
    USHORT  ProcessorRevision;
    USHORT  Unknown;
    ULONG   FeatureBits;
} SYSTEM_PROCESSOR_INFORMATION, *PSYSTEM_PROCESSOR_INFORMATION;

// SystemPerformanceInformation
typedef struct _SYSTEM_PERFORMANCE_INFORMATION {
    LARGE_INTEGER   IdleTime;
    LARGE_INTEGER   ReadTransferCount;
    LARGE_INTEGER   WriteTransferCount;
    LARGE_INTEGER   OtherTransferCount;
    ULONG           ReadOperationCount;
    ULONG           WriteOperationCount;
    ULONG           OtherOperationCount;
    ULONG           AvailablePages;
    ULONG           TotalCommittedPages;
    ULONG           TotalCommitLimit;
    ULONG           PeakCommitment;
    ULONG           PageFaults;
    ULONG           WriteCopyFaults;
    ULONG           TransistionFaults;
    ULONG           Reserved1;
    ULONG           DemandZeroFaults;
    ULONG           PagesRead;
    ULONG           PageReadIos;
    ULONG           Reserved2[2];
    ULONG           PagefilePagesWritten;
    ULONG           PagefilePageWriteIos;
    ULONG           MappedFilePagesWritten;
    ULONG           MappedFilePageWriteIos;
    ULONG           PagedPoolUsage;
    ULONG           NonPagedPoolUsage;
    ULONG           PagedPoolAllocs;
    ULONG           PagedPoolFrees;
    ULONG           NonPagedPoolAllocs;
    ULONG           NonPagedPoolFrees;
    ULONG           TotalFreeSystemPtes;
    ULONG           SystemCodePage;
    ULONG           TotalSystemDriverPages;
    ULONG           TotalSystemCodePages;
    ULONG           SmallNonPagedLookasideListAllocateHits;
    ULONG           SmallPagedLookasideListAllocateHits;
    ULONG           Reserved3;
    ULONG           MmSystemCachePage;
    ULONG           PagedPoolPage;
    ULONG           SystemDriverPage;
    ULONG           FastReadNoWait;
    ULONG           FastReadWait;
    ULONG           FastReadResourceMiss;
    ULONG           FastReadNotPossible;
    ULONG           FastMdlReadNoWait;
    ULONG           FastMdlReadWait;
    ULONG           FastMdlReadResourceMiss;
    ULONG           FastMdlReadNotPossible;
    ULONG           MapDataNoWait;
    ULONG           MapDataWait;
    ULONG           MapDataNoWaitMiss;
    ULONG           MapDataWaitMiss;
    ULONG           PinMappedDataCount;
    ULONG           PinReadNoWait;
    ULONG           PinReadWait;
    ULONG           PinReadNoWaitMiss;
    ULONG           PinReadWaitMiss;
    ULONG           CopyReadNoWait;
    ULONG           CopyReadWait;
    ULONG           CopyReadNoWaitMiss;
    ULONG           CopyReadWaitMiss;
    ULONG           MdlReadNoWait;
    ULONG           MdlReadWait;
    ULONG           MdlReadNoWaitMiss;
    ULONG           MdlReadWaitMiss;
    ULONG           ReadAheadIos;
    ULONG           LazyWriteIos;
    ULONG           LazyWritePages;
    ULONG           DataFlushes;
    ULONG           DataPages;
    ULONG           ContextSwitches;
    ULONG           FirstLevelTbFills;
    ULONG           SecondLevelTbFills;
    ULONG           SystemCalls;
} SYSTEM_PERFORMANCE_INFORMATION, *PSYSTEM_PERFORMANCE_INFORMATION;

// SystemTimeOfDayInformation
typedef struct _SYSTEM_TIME_OF_DAY_INFORMATION {
    LARGE_INTEGER   BootTime;
    LARGE_INTEGER   CurrentTime;
    LARGE_INTEGER   TimeZoneBias;
    ULONG           CurrentTimeZoneId;
} SYSTEM_TIME_OF_DAY_INFORMATION, *PSYSTEM_TIME_OF_DAY_INFORMATION;

typedef struct _SYSTEM_THREADS_INFORMATION {
    LARGE_INTEGER   KernelTime;
    LARGE_INTEGER   UserTime;
    LARGE_INTEGER   CreateTime;
    ULONG           WaitTime;
    PVOID           StartAddress;
    CLIENT_ID       ClientId;
    KPRIORITY       Priority;
    KPRIORITY       BasePriority;
    ULONG           ContextSwitchCount;
    THREAD_STATE    State;
    KWAIT_REASON    WaitReason;
} SYSTEM_THREADS_INFORMATION, *PSYSTEM_THREADS_INFORMATION;

// SystemProcessesAndThreadsInformation
typedef struct _SYSTEM_PROCESSES_INFORMATION {
    ULONG                       NextEntryDelta;
    ULONG                       ThreadCount;
    ULONG                       Reserved1[6];
    LARGE_INTEGER               CreateTime;
    LARGE_INTEGER               UserTime;
    LARGE_INTEGER               KernelTime;
    UNICODE_STRING              ProcessName;
    KPRIORITY                   BasePriority;
    ULONG                       ProcessId;
    ULONG                       InheritedFromProcessId;
    ULONG                       HandleCount;
    ULONG                       Reserved2[2];
    VM_COUNTERS                 VmCounters;
#if (VER_PRODUCTBUILD >= 2195)
    IO_COUNTERS                 IoCounters;
#endif // (VER_PRODUCTBUILD >= 2195)
    SYSTEM_THREADS_INFORMATION  Threads[1];
} SYSTEM_PROCESSES_INFORMATION, *PSYSTEM_PROCESSES_INFORMATION;

// SystemCallCounts
typedef struct _SYSTEM_CALL_COUNTS {
    ULONG Size;
    ULONG NumberOfDescriptorTables;
    ULONG NumberOfRoutinesInTable[1];
    // On checked build this is followed by a ULONG CallCounts[1] variable length array.
} SYSTEM_CALL_COUNTS, *PSYSTEM_CALL_COUNTS;

// SystemConfigurationInformation
typedef struct _SYSTEM_CONFIGURATION_INFORMATION {
    ULONG DiskCount;
    ULONG FloppyCount;
    ULONG CdRomCount;
    ULONG TapeCount;
    ULONG SerialCount;
    ULONG ParallelCount;
} SYSTEM_CONFIGURATION_INFORMATION, *PSYSTEM_CONFIGURATION_INFORMATION;

// SystemProcessorTimes
typedef struct _SYSTEM_PROCESSOR_TIMES {
    LARGE_INTEGER   IdleTime;
    LARGE_INTEGER   KernelTime;
    LARGE_INTEGER   UserTime;
    LARGE_INTEGER   DpcTime;
    LARGE_INTEGER   InterruptTime;
    ULONG           InterruptCount;
} SYSTEM_PROCESSOR_TIMES, *PSYSTEM_PROCESSOR_TIMES;

// SystemGlobalFlag
typedef struct _SYSTEM_GLOBAL_FLAG {
    ULONG GlobalFlag;
} SYSTEM_GLOBAL_FLAG, *PSYSTEM_GLOBAL_FLAG;

// SystemModuleInformation
typedef struct _SYSTEM_MODULE_INFORMATION {
    ULONG   Reserved[2];
    PVOID   Base;
    ULONG   Size;
    ULONG   Flags;
    USHORT  Index;
    USHORT  Unknown;
    USHORT  LoadCount;
    USHORT  ModuleNameOffset;
    CHAR    ImageName[256];
} SYSTEM_MODULE_INFORMATION, *PSYSTEM_MODULE_INFORMATION;

// SystemLockInformation
typedef struct _SYSTEM_LOCK_INFORMATION {
    PVOID   Address;
    USHORT  Type;
    USHORT  Reserved1;
    ULONG   ExclusiveOwnerThreadId;
    ULONG   ActiveCount;
    ULONG   ContentionCount;
    ULONG   Reserved2[2];
    ULONG   NumberOfSharedWaiters;
    ULONG   NumberOfExclusiveWaiters;
} SYSTEM_LOCK_INFORMATION, *PSYSTEM_LOCK_INFORMATION;

// SystemHandleInformation
typedef struct _SYSTEM_HANDLE_INFORMATION {
    ULONG       ProcessId;
    UCHAR       ObjectTypeNumber;
    UCHAR       Flags;
    USHORT      Handle;
    PVOID       Object;
    ACCESS_MASK GrantedAccess;
} SYSTEM_HANDLE_INFORMATION, *PSYSTEM_HANDLE_INFORMATION;

// SystemObjectInformation
typedef struct _SYSTEM_OBJECT_TYPE_INFORMATION {
    ULONG           NextEntryOffset;
    ULONG           ObjectCount;
    ULONG           HandleCount;
    ULONG           TypeNumber;
    ULONG           InvalidAttributes;
    GENERIC_MAPPING GenericMapping;
    ACCESS_MASK     ValidAccessMask;
    POOL_TYPE       PoolType;
    UCHAR           Unknown;
    UNICODE_STRING  Name;
} SYSTEM_OBJECT_TYPE_INFORMATION, *PSYSTEM_OBJECT_TYPE_INFORMATION;

typedef struct _SYSTEM_OBJECT_INFORMATION {
    ULONG                   NextEntryOffset;
    PVOID                   Object;
    ULONG                   CreatorProcessId;
    USHORT                  Unknown;
    USHORT                  Flags;
    ULONG                   PointerCount;
    ULONG                   HandleCount;
    ULONG                   PagedPoolUsage;
    ULONG                   NonPagedPoolUsage;
    ULONG                   ExclusiveProcessId;
    PSECURITY_DESCRIPTOR    SecurityDescriptor;
    UNICODE_STRING          Name;
} SYSTEM_OBJECT_INFORMATION, *PSYSTEM_OBJECT_INFORMATION;

// SystemPagefileInformation
typedef struct _SYSTEM_PAGEFILE_INFORMATION {
    ULONG           NextEntryOffset;
    ULONG           CurrentSize;
    ULONG           TotalUsed;
    ULONG           PeakUsed;
    UNICODE_STRING  FileName;
} SYSTEM_PAGEFILE_INFORMATION, *PSYSTEM_PAGEFILE_INFORMATION;

// SystemInstructionEmulationCounts
typedef struct _SYSTEM_INSTRUCTION_EMULATION_COUNTS {
    ULONG GenericInvalidOpcode;
    ULONG TwoByteOpcode;
    ULONG ESprefix;
    ULONG CSprefix;
    ULONG SSprefix;
    ULONG DSprefix;
    ULONG FSPrefix;
    ULONG GSprefix;
    ULONG OPER32prefix;
    ULONG ADDR32prefix;
    ULONG INSB;
    ULONG INSW;
    ULONG OUTSB;
    ULONG OUTSW;
    ULONG PUSHFD;
    ULONG POPFD;
    ULONG INTnn;
    ULONG INTO;
    ULONG IRETD;
    ULONG FloatingPointOpcode;
    ULONG INBimm;
    ULONG INWimm;
    ULONG OUTBimm;
    ULONG OUTWimm;
    ULONG INB;
    ULONG INW;
    ULONG OUTB;
    ULONG OUTW;
    ULONG LOCKprefix;
    ULONG REPNEprefix;
    ULONG REPprefix;
    ULONG CLI;
    ULONG STI;
    ULONG HLT;
} SYSTEM_INSTRUCTION_EMULATION_COUNTS, *PSYSTEM_INSTRUCTION_EMULATION_COUNTS;

// SystemCacheInformation
typedef struct _SYSTEM_CACHE_INFORMATION {
    ULONG SystemCacheWsSize;
    ULONG SystemCacheWsPeakSize;
    ULONG SystemCacheWsFaults;
    ULONG SystemCacheWsMinimum;
    ULONG SystemCacheWsMaximum;
    ULONG TransitionSharedPages;
    ULONG TransitionSharedPagesPeak;
    ULONG Reserved[2];
} SYSTEM_CACHE_INFORMATION, *PSYSTEM_CACHE_INFORMATION;

// SystemPoolTagInformation
typedef struct _SYSTEM_POOL_TAG_INFORMATION {
    CHAR    Tag[4];
    ULONG   PagedPoolAllocs;
    ULONG   PagedPoolFrees;
    ULONG   PagedPoolUsage;
    ULONG   NonPagedPoolAllocs;
    ULONG   NonPagedPoolFrees;
    ULONG   NonPagedPoolUsage;
} SYSTEM_POOL_TAG_INFORMATION, *PSYSTEM_POOL_TAG_INFORMATION;

// SystemProcessorStatistics
typedef struct _SYSTEM_PROCESSOR_STATISTICS {
    ULONG ContextSwitches;
    ULONG DpcCount;
    ULONG DpcRequestRate;
    ULONG TimeIncrement;
    ULONG DpcBypassCount;
    ULONG ApcBypassCount;
} SYSTEM_PROCESSOR_STATISTICS, *PSYSTEM_PROCESSOR_STATISTICS;

// SystemDpcInformation
typedef struct _SYSTEM_DPC_INFORMATION {
    ULONG Reserved;
    ULONG MaximumDpcQueueDepth;
    ULONG MinimumDpcRate;
    ULONG AdjustDpcThreshold;
    ULONG IdealDpcRate;
} SYSTEM_DPC_INFORMATION, *PSYSTEM_DPC_INFORMATION;

// SystemLoadImage
typedef struct _SYSTEM_LOAD_IMAGE {
    UNICODE_STRING  ModuleName;
    PVOID           ModuleBase;
    PVOID           Unknown;
    PVOID           EntryPoint;
    PVOID           ExportDirectory;
} SYSTEM_LOAD_IMAGE, *PSYSTEM_LOAD_IMAGE;

// SystemUnloadImage
typedef struct _SYSTEM_UNLOAD_IMAGE {
    PVOID ModuleBase;
} SYSTEM_UNLOAD_IMAGE, *PSYSTEM_UNLOAD_IMAGE;

// SystemTimeAdjustment
typedef struct _SYSTEM_QUERY_TIME_ADJUSTMENT {
    ULONG   TimeAdjustment;
    ULONG   MaximumIncrement;
    BOOLEAN TimeSynchronization;
} SYSTEM_QUERY_TIME_ADJUSTMENT, *PSYSTEM_QUERY_TIME_ADJUSTMENT;

// SystemTimeAdjustment
typedef struct _SYSTEM_SET_TIME_ADJUSTMENT {
    ULONG   TimeAdjustment;
    BOOLEAN TimeSynchronization;
} SYSTEM_SET_TIME_ADJUSTMENT, *PSYSTEM_SET_TIME_ADJUSTMENT;

// SystemCrashDumpInformation
typedef struct _SYSTEM_CRASH_DUMP_INFORMATION {
    HANDLE CrashDumpSectionHandle;
#if (VER_PRODUCTBUILD >= 2195)
    HANDLE Unknown;
#endif // (VER_PRODUCTBUILD >= 2195)
} SYSTEM_CRASH_DUMP_INFORMATION, *PSYSTEM_CRASH_DUMP_INFORMATION;

// SystemExceptionInformation
typedef struct _SYSTEM_EXCEPTION_INFORMATION {
    ULONG AlignmentFixupCount;
    ULONG ExceptionDispatchCount;
    ULONG FloatingEmulationCount;
    ULONG Reserved;
} SYSTEM_EXCEPTION_INFORMATION, *PSYSTEM_EXCEPTION_INFORMATION;

// SystemCrashDumpStateInformation
typedef struct _SYSTEM_CRASH_DUMP_STATE_INFORMATION {
    ULONG ValidCrashDump;
#if (VER_PRODUCTBUILD >= 2195)
    ULONG Unknown;
#endif // (VER_PRODUCTBUILD >= 2195)
} SYSTEM_CRASH_DUMP_STATE_INFORMATION, *PSYSTEM_CRASH_DUMP_STATE_INFORMATION;

// SystemKernelDebuggerInformation
typedef struct _SYSTEM_KERNEL_DEBUGGER_INFORMATION {
    BOOLEAN DebuggerEnabled;
    BOOLEAN DebuggerNotPresent;
} SYSTEM_KERNEL_DEBUGGER_INFORMATION, *PSYSTEM_KERNEL_DEBUGGER_INFORMATION;

// SystemContextSwitchInformation
typedef struct _SYSTEM_CONTEXT_SWITCH_INFORMATION {
    ULONG ContextSwitches;
    ULONG ContextSwitchCounters[11];
} SYSTEM_CONTEXT_SWITCH_INFORMATION, *PSYSTEM_CONTEXT_SWITCH_INFORMATION;

// SystemRegistryQuotaInformation
typedef struct _SYSTEM_REGISTRY_QUOTA_INFORMATION {
    ULONG RegistryQuota;
    ULONG RegistryQuotaInUse;
    ULONG PagedPoolSize;
} SYSTEM_REGISTRY_QUOTA_INFORMATION, *PSYSTEM_REGISTRY_QUOTA_INFORMATION;

// SystemLoadAndCallImage
typedef struct _SYSTEM_LOAD_AND_CALL_IMAGE {
    UNICODE_STRING ModuleName;
} SYSTEM_LOAD_AND_CALL_IMAGE, *PSYSTEM_LOAD_AND_CALL_IMAGE;

// SystemPrioritySeparation
typedef struct _SYSTEM_PRIORITY_SEPARATION {
    ULONG PrioritySeparation;
} SYSTEM_PRIORITY_SEPARATION, *PSYSTEM_PRIORITY_SEPARATION;

// SystemTimeZoneInformation
typedef struct _SYSTEM_TIME_ZONE_INFORMATION {
    LONG        Bias;
    WCHAR       StandardName[32];
    TIME_FIELDS StandardDate;
    LONG        StandardBias;
    WCHAR       DaylightName[32];
    TIME_FIELDS DaylightDate;
    LONG        DaylightBias;
} SYSTEM_TIME_ZONE_INFORMATION, *PSYSTEM_TIME_ZONE_INFORMATION;

// SystemLookasideInformation
typedef struct _SYSTEM_LOOKASIDE_INFORMATION {
    USHORT      Depth;
    USHORT      MaximumDepth;
    ULONG       TotalAllocates;
    ULONG       AllocateMisses;
    ULONG       TotalFrees;
    ULONG       FreeMisses;
    POOL_TYPE   Type;
    ULONG       Tag;
    ULONG       Size;
} SYSTEM_LOOKASIDE_INFORMATION, *PSYSTEM_LOOKASIDE_INFORMATION;

// SystemSetTimeSlipEvent
typedef struct _SYSTEM_SET_TIME_SLIP_EVENT {
    HANDLE TimeSlipEvent;
} SYSTEM_SET_TIME_SLIP_EVENT, *PSYSTEM_SET_TIME_SLIP_EVENT;

// SystemCreateSession
typedef struct _SYSTEM_CREATE_SESSION {
    ULONG Session;
} SYSTEM_CREATE_SESSION, *PSYSTEM_CREATE_SESSION;

// SystemDeleteSession
typedef struct _SYSTEM_DELETE_SESSION {
    ULONG Session;
} SYSTEM_DELETE_SESSION, *PSYSTEM_DELETE_SESSION;

// SystemRangeStartInformation
typedef struct _SYSTEM_RANGE_START_INFORMATION {
    PVOID SystemRangeStart;
} SYSTEM_RANGE_START_INFORMATION, *PSYSTEM_RANGE_START_INFORMATION;

// SystemSessionProcessesInformation
typedef struct _SYSTEM_SESSION_PROCESS_INFORMATION {
    ULONG SessionId;
    ULONG BufferSize;
    PVOID Buffer;
} SYSTEM_SESSION_PROCESS_INFORMATION, *PSYSTEM_SESSION_PROCESS_INFORMATION;

typedef struct _TEB {
    NT_TIB      Tib;
    PVOID       EnvironmentPointer;
    CLIENT_ID   ClientId;
    HANDLE      RpcHandle;
    PVOID       *ThreadLocalStorage;
    PVOID       Peb;
    ULONG       LastErrorValue;
} TEB, *PTEB;

typedef struct _TERMINATION_PORT {
    struct _TERMINATION_PORT*   Next;
    PVOID                       Port;
} TERMINATION_PORT, *PTERMINATION_PORT;

typedef struct _TOKEN_SOURCE {
    CCHAR   SourceName[TOKEN_SOURCE_LENGTH];
    LUID    SourceIdentifier;
} TOKEN_SOURCE, *PTOKEN_SOURCE;

typedef struct _TOKEN_CONTROL {
    LUID            TokenId;
    LUID            AuthenticationId;
    LUID            ModifiedId;
    TOKEN_SOURCE    TokenSource;
} TOKEN_CONTROL, *PTOKEN_CONTROL;

typedef struct _TOKEN_DEFAULT_DACL {
    PACL DefaultDacl;
} TOKEN_DEFAULT_DACL, *PTOKEN_DEFAULT_DACL;

typedef struct _TOKEN_GROUPS {
    ULONG               GroupCount;
    SID_AND_ATTRIBUTES  Groups[1];
} TOKEN_GROUPS, *PTOKEN_GROUPS;

#include <pshpack1.h>
typedef union
{
  struct
   {
    TOKEN_SOURCE TokenSource;     /* 0x0: CHAR SourceName[8] = "*SYSTEM*" | "User32  " + LUID SourceIdentifier = 0x10, *SYSTEM* id == 0 */
    LUID TokenId;         /* 0x10: */
    LUID AuthenticationId;    /* 0x18: */
    LARGE_INTEGER ExpirationTime; /* 0x20: -1 no expired. *SYSTEM* has expired? */
    LUID ModifiedId;          /* 0x28: */
    ULONG UserAndGroupCount;      /* 0x30: 3 */
    ULONG PrivilegeCount;     /* 0x34: 14 */
    ULONG VariableLength;     /* 0x38: 0x37C */
    ULONG DynamicCharged;     /* 0x3C: 0x1F4 */
    ULONG DynamicAvailable;   /* 0x40: 0x1A4 */
    ULONG DefaultOwnerIndex;      /* 0x44: 1 */
    PSID_AND_ATTRIBUTES UserAndGroups;/* 0x48: TOKEN_USER Owners [UserAndGroupCount] DefaultOwnerIndex */
    PSID  PrimaryGroup;       /* 0x4C: */
    PLUID_AND_ATTRIBUTES Privileges;/* 0x50: */
    PULONG DynamicPart;       /* 0x54: */
    PACL   DefaultDacl;       /* 0x58: */
    TOKEN_TYPE TokenType;     /* 0x5C: TokenPrimary | TokenImpersonation */
    SECURITY_IMPERSONATION_LEVEL ImpersonationLevel;/* 0x60: 0 */
    UCHAR   TokenFlags;       /* 0x64: 1 */
    BOOLEAN TokenInUse;       /* 0x65: 1 */
    USHORT  Alignment;        /* 0x66: 0 */
    PVOID   ProxyData;        /* 0x68: 0 */
    PVOID   AuditData;        /* 0x6C: 0 */
    ULONG VariablePart;       /* 0x70: */
   } NT;
  struct
   {
    TOKEN_SOURCE TokenSource;     /* 0x0: CHAR SourceName[8] = "*SYSTEM*" | "User32  " + LUID SourceIdentifier = 0x10 */
    LUID TokenId;         /* 0x10: */
    LUID AuthenticationId;    /* 0x18: */
    LUID ParentTokenId;       /* 0x20: 0 */
    LARGE_INTEGER ExpirationTime; /* 0x28: -1 no expired */
    LUID ModifiedId;          /* 0x30: */
    ULONG SessionId;          /* 0x38: 0 */
    ULONG UserAndGroupCount;      /* 0x3C: 9 */
    ULONG RestrictedSidCount;     /*+0x40: 0 */
    ULONG PrivilegeCount;     /* 0x44: 11 */
    ULONG VariableLength;     /* 0x48: 0x1F0 */
    ULONG DynamicCharged;     /* 0x4C: 0x1F4 */
    ULONG DynamicAvailable;   /* 0x50: 0x1A4 */
    ULONG DefaultOwnerIndex;      /* 0x54: 3 */
    PSID_AND_ATTRIBUTES UserAndGroups; /* 0x58: TOKEN_USER Owners [UserAndGroupCount] DefaultOwnerIndex */
    PSID_AND_ATTRIBUTES RestrictedSids;/* 0x5C: 0 */
    PSID  PrimaryGroup;       /* 0x60: */
    PLUID_AND_ATTRIBUTES Privileges;/* 0x64: */
    PULONG DynamicPart;       /* 0x68: */
    PACL   DefaultDacl;       /* 0x6C: */
    TOKEN_TYPE TokenType;     /* 0x70: TokenPrimary | TokenImpersonation */
    SECURITY_IMPERSONATION_LEVEL ImpersonationLevel;/* 0x74: 0 */
    UCHAR   TokenFlags;       /* 0x78: 9 */
    BOOLEAN TokenInUse;       /* 0x79: 1 */
    USHORT  Alignment;        /* 0x7A: 0 */
    PVOID   ProxyData;        /* 0x7C: 0 */
    PVOID   AuditData;        /* 0x80: 0 */
    ULONG VariablePart;           /* 0x84: */
   } K2;
  struct
   {
    TOKEN_SOURCE TokenSource;     /* 0x0: CHAR SourceName[8] = "*SYSTEM*" | "User32  " + LUID SourceIdentifier = 0x10 */
    LUID TokenId;         /* 0x10: 0x6F68 */
    LUID AuthenticationId;    /* 0x18: */
    LUID ParentTokenId;       /* 0x20: 0 */
    LARGE_INTEGER ExpirationTime; /* 0x28: -1 no expired */
    PVOID Xz;             /*+0x30: 0x8xxxxxxxx */
    LUID ModifiedId;          /* 0x34: */
    ULONG SessionId;          /* 0x3C: 0x6F6A */
    ULONG UserAndGroupCount;      /* 0x40: 4 */
    ULONG RestrictedSidCount;     /*+0x44: 0 */
    ULONG VariableLength;     /* 0x48: 0x160 */
    ULONG DynamicCharged;     /* 0x4C: 0x164 */
    ULONG DynamicAvailable;   /* 0x50: 0x1F4 */
    ULONG PrivilegeCount;     /* 0x54: 0 */
    ULONG DefaultOwnerIndex;      /* 0x58: 1 */
    PSID_AND_ATTRIBUTES UserAndGroups; /* 0x5C: TOKEN_USER Owners [UserAndGroupCount] DefaultOwnerIndex */
    PSID_AND_ATTRIBUTES RestrictedSids;/* 0x60: 0 */
    PSID  PrimaryGroup;       /* 0x64: */
    PLUID_AND_ATTRIBUTES Privileges;/* 0x68: */
    PULONG DynamicPart;       /* 0x6C: */
    PACL   DefaultDacl;       /* 0x70: */
    TOKEN_TYPE TokenType;     /* 0x74: TokenPrimary | TokenImpersonation */
    SECURITY_IMPERSONATION_LEVEL ImpersonationLevel;/* 0x78: 0 */
    UCHAR   TokenFlags;       /* 0x7C: 9 */
    BOOLEAN TokenInUse;       /* 0x7D: 1 */
    USHORT  Alignment;        /* 0x7E: 4BB4 */
    PVOID   ProxyData;        /* 0x80: 0 */
    PVOID   AuditData;        /* 0x84: 0 */
    ULONG VariablePart;       /* 0x88: */
   } XP;
  struct
   {
    TOKEN_SOURCE TokenSource;     /* 0x0: CHAR SourceName[8] = "*SYSTEM*" | "User32  " + LUID SourceIdentifier = 0x10 */
    LUID TokenId;         /* 0x10: 0x6F68 */
    LUID AuthenticationId;    /* 0x18: */
    LUID ParentTokenId;       /* 0x20: 0 */
    LARGE_INTEGER ExpirationTime; /* 0x28: -1 no expired */
    PVOID Xz1;            /*+0x30: 0x8xxxxxxxx */
    PVOID Xz2;            /*+0x34: 0xXxxxxxxxx */
    LUID ModifiedId;          /* 0x38: */
    ULONG SessionId;          /* 0x40: 0x6F6A */
    LUID  Xz3;            /*+0x44: 0 */
    ULONG UserAndGroupCount;      /* 0x4C: 4 */
    ULONG RestrictedSidCount;     /*+0x50: 0 */
    ULONG VariableLength;     /* 0x54: 0x18 */
    ULONG DynamicCharged;     /* 0x58: 0x17C */
    ULONG DynamicAvailable;   /* 0x5C: 0x1F4 */
    ULONG PrivilegeCount;     /* 0x60: 0 */
    ULONG DefaultOwnerIndex;      /* 0x64: 1 */
    PSID_AND_ATTRIBUTES UserAndGroups; /* 0x68: TOKEN_USER Owners [UserAndGroupCount] DefaultOwnerIndex */
    PSID_AND_ATTRIBUTES RestrictedSids;/* 0x6C: 0 */
    PSID  PrimaryGroup;       /* 0x70: */
    PLUID_AND_ATTRIBUTES Privileges;/* 0x74: */
    PULONG DynamicPart;       /* 0x78: */
    PACL   DefaultDacl;       /* 0x7C: */
    TOKEN_TYPE TokenType;     /* 0x80: TokenPrimary | TokenImpersonation */
    SECURITY_IMPERSONATION_LEVEL ImpersonationLevel;/* 0x84: 0 */
    UCHAR   TokenFlags;       /* 0x88: 9 */
    BOOLEAN TokenInUse;       /* 0x89: 1 */
    USHORT  Alignment;        /* 0x8A: 4BB4 */
    PVOID   ProxyData;        /* 0x8C: 0x8xxxxxxxx */
    PVOID   AuditData;        /* 0x90: 0 */
    ULONG VariablePart;       /* 0x94: */
   } K23;
  /* VariablePart */
} TOKEN_OBJECT, *PTOKEN_OBJECT;
#include <poppack.h>

typedef struct _TOKEN_OWNER {
    PSID Owner;
} TOKEN_OWNER, *PTOKEN_OWNER;

typedef struct _TOKEN_PRIMARY_GROUP {
    PSID PrimaryGroup;
} TOKEN_PRIMARY_GROUP, *PTOKEN_PRIMARY_GROUP;

typedef struct _TOKEN_PRIVILEGES {
    ULONG               PrivilegeCount;
    LUID_AND_ATTRIBUTES Privileges[1];
} TOKEN_PRIVILEGES, *PTOKEN_PRIVILEGES;

typedef struct _TOKEN_STATISTICS {
    LUID                            TokenId;
    LUID                            AuthenticationId;
    LARGE_INTEGER                   ExpirationTime;
    TOKEN_TYPE                      TokenType;
    SECURITY_IMPERSONATION_LEVEL    ImpersonationLevel;
    ULONG                           DynamicCharged;
    ULONG                           DynamicAvailable;
    ULONG                           GroupCount;
    ULONG                           PrivilegeCount;
    LUID                            ModifiedId;
} TOKEN_STATISTICS, *PTOKEN_STATISTICS;

typedef struct _TOKEN_USER {
    SID_AND_ATTRIBUTES User;
} TOKEN_USER, *PTOKEN_USER;

typedef struct _SECURITY_CLIENT_CONTEXT {
    SECURITY_QUALITY_OF_SERVICE SecurityQos;
    PACCESS_TOKEN               ClientToken;
    BOOLEAN                     DirectlyAccessClientToken;
    BOOLEAN                     DirectAccessEffectiveOnly;
    BOOLEAN                     ServerIsRemote;
    TOKEN_CONTROL               ClientTokenControl;
} SECURITY_CLIENT_CONTEXT, *PSECURITY_CLIENT_CONTEXT;

typedef struct _TUNNEL {
    FAST_MUTEX          Mutex;
    PRTL_SPLAY_LINKS    Cache;
    LIST_ENTRY          TimerQueue;
    USHORT              NumEntries;
} TUNNEL, *PTUNNEL;

typedef struct _VACB {
    PVOID               BaseAddress;
    PSHARED_CACHE_MAP   SharedCacheMap;
    union {
        LARGE_INTEGER   FileOffset;
        USHORT          ActiveCount;
    } Overlay;
    LIST_ENTRY          LruList;
} VACB, *PVACB;

typedef struct _VAD_HEADER {
    PVOID       StartVPN;
    PVOID       EndVPN;
    PVAD_HEADER ParentLink;
    PVAD_HEADER LeftLink;
    PVAD_HEADER RightLink;
    ULONG       Flags;          // LSB = CommitCharge
    PVOID       ControlArea;
    PVOID       FirstProtoPte;
    PVOID       LastPTE;
    ULONG       Unknown;
    LIST_ENTRY  Secured;
} VAD_HEADER, *PVAD_HEADER;

NTKERNELAPI
BOOLEAN
CcCanIWrite (
    IN PFILE_OBJECT FileObject,
    IN ULONG        BytesToWrite,
    IN BOOLEAN      Wait,
    IN BOOLEAN      Retrying
);

NTKERNELAPI
BOOLEAN
CcCopyRead (
    IN PFILE_OBJECT         FileObject,
    IN PLARGE_INTEGER       FileOffset,
    IN ULONG                Length,
    IN BOOLEAN              Wait,
    OUT PVOID               Buffer,
    OUT PIO_STATUS_BLOCK    IoStatus
);

NTKERNELAPI
BOOLEAN
CcCopyWrite (
    IN PFILE_OBJECT     FileObject,
    IN PLARGE_INTEGER   FileOffset,
    IN ULONG            Length,
    IN BOOLEAN          Wait,
    IN PVOID            Buffer
);

#define CcCopyWriteWontFlush(FO, FOFF, LEN) ((LEN) <= 0x10000)

typedef VOID (*PCC_POST_DEFERRED_WRITE) (
    IN PVOID Context1,
    IN PVOID Context2
);

NTKERNELAPI
VOID
CcDeferWrite (
    IN PFILE_OBJECT             FileObject,
    IN PCC_POST_DEFERRED_WRITE  PostRoutine,
    IN PVOID                    Context1,
    IN PVOID                    Context2,
    IN ULONG                    BytesToWrite,
    IN BOOLEAN                  Retrying
);

NTKERNELAPI
VOID
CcFastCopyRead (
    IN PFILE_OBJECT         FileObject,
    IN ULONG                FileOffset,
    IN ULONG                Length,
    IN ULONG                PageCount,
    OUT PVOID               Buffer,
    OUT PIO_STATUS_BLOCK    IoStatus
);

NTKERNELAPI
VOID
CcFastCopyWrite (
    IN PFILE_OBJECT FileObject,
    IN ULONG        FileOffset,
    IN ULONG        Length,
    IN PVOID        Buffer
);

NTKERNELAPI
VOID
CcFlushCache (
    IN PSECTION_OBJECT_POINTERS SectionObjectPointer,
    IN PLARGE_INTEGER           FileOffset OPTIONAL,
    IN ULONG                    Length,
    OUT PIO_STATUS_BLOCK        IoStatus OPTIONAL
);

typedef VOID (*PDIRTY_PAGE_ROUTINE) (
    IN PFILE_OBJECT     FileObject,
    IN PLARGE_INTEGER   FileOffset,
    IN ULONG            Length,
    IN PLARGE_INTEGER   OldestLsn,
    IN PLARGE_INTEGER   NewestLsn,
    IN PVOID            Context1,
    IN PVOID            Context2
);

NTKERNELAPI
LARGE_INTEGER
CcGetDirtyPages (
    IN PVOID                LogHandle,
    IN PDIRTY_PAGE_ROUTINE  DirtyPageRoutine,
    IN PVOID                Context1,
    IN PVOID                Context2
);

NTKERNELAPI
PFILE_OBJECT
CcGetFileObjectFromBcb (
    IN PVOID Bcb
);

NTKERNELAPI
PFILE_OBJECT
CcGetFileObjectFromSectionPtrs (
    IN PSECTION_OBJECT_POINTERS SectionObjectPointer
);

#define CcGetFileSizePointer(FO) (                                     \
    ((PLARGE_INTEGER)((FO)->SectionObjectPointer->SharedCacheMap) + 1) \
)

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
LARGE_INTEGER
CcGetFlushedValidData (
    IN PSECTION_OBJECT_POINTERS SectionObjectPointer,
    IN BOOLEAN                  BcbListHeld
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
LARGE_INTEGER
CcGetLsnForFileObject (
    IN PFILE_OBJECT     FileObject,
    OUT PLARGE_INTEGER  OldestLsn OPTIONAL
);

typedef BOOLEAN (*PACQUIRE_FOR_LAZY_WRITE) (
    IN PVOID    Context,
    IN BOOLEAN  Wait
);

typedef VOID (*PRELEASE_FROM_LAZY_WRITE) (
    IN PVOID Context
);

typedef BOOLEAN (*PACQUIRE_FOR_READ_AHEAD) (
    IN PVOID    Context,
    IN BOOLEAN  Wait
);

typedef VOID (*PRELEASE_FROM_READ_AHEAD) (
    IN PVOID Context
);

typedef struct _CACHE_MANAGER_CALLBACKS {
    PACQUIRE_FOR_LAZY_WRITE     AcquireForLazyWrite;
    PRELEASE_FROM_LAZY_WRITE    ReleaseFromLazyWrite;
    PACQUIRE_FOR_READ_AHEAD     AcquireForReadAhead;
    PRELEASE_FROM_READ_AHEAD    ReleaseFromReadAhead;
} CACHE_MANAGER_CALLBACKS, *PCACHE_MANAGER_CALLBACKS;

NTKERNELAPI
VOID
CcInitializeCacheMap (
    IN PFILE_OBJECT             FileObject,
    IN PCC_FILE_SIZES           FileSizes,
    IN BOOLEAN                  PinAccess,
    IN PCACHE_MANAGER_CALLBACKS Callbacks,
    IN PVOID                    LazyWriteContext
);

#define CcIsFileCached(FO) (                                                         \
    ((FO)->SectionObjectPointer != NULL) &&                                          \
    (((PSECTION_OBJECT_POINTERS)(FO)->SectionObjectPointer)->SharedCacheMap != NULL) \
)

NTKERNELAPI
BOOLEAN
CcIsThereDirtyData (
    IN PVPB Vpb
);

NTKERNELAPI
BOOLEAN
CcMapData (
    IN PFILE_OBJECT     FileObject,
    IN PLARGE_INTEGER   FileOffset,
    IN ULONG            Length,
#if (VER_PRODUCTBUILD >= 2600)
    IN ULONG            Flags,
#else
    IN BOOLEAN          Wait,
#endif
    OUT PVOID           *Bcb,
    OUT PVOID           *Buffer
);

NTKERNELAPI
VOID
CcMdlRead (
    IN PFILE_OBJECT         FileObject,
    IN PLARGE_INTEGER       FileOffset,
    IN ULONG                Length,
    OUT PMDL                *MdlChain,
    OUT PIO_STATUS_BLOCK    IoStatus
);

NTKERNELAPI
VOID
CcMdlReadComplete (
    IN PFILE_OBJECT FileObject,
    IN PMDL         MdlChain
);

#if (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
VOID
CcMdlWriteAbort (
    IN PFILE_OBJECT FileObject,
    IN PMDL         MdlChain
);

#endif

NTKERNELAPI
VOID
CcMdlWriteComplete (
    IN PFILE_OBJECT     FileObject,
    IN PLARGE_INTEGER   FileOffset,
    IN PMDL             MdlChain
);

NTKERNELAPI
BOOLEAN
CcPinMappedData (
    IN PFILE_OBJECT     FileObject,
    IN PLARGE_INTEGER   FileOffset,
    IN ULONG            Length,
#if (VER_PRODUCTBUILD >= 2195)
    IN ULONG            Flags,
#else
    IN BOOLEAN          Wait,
#endif
    IN OUT PVOID        *Bcb
);

NTKERNELAPI
BOOLEAN
CcPinRead (
    IN PFILE_OBJECT     FileObject,
    IN PLARGE_INTEGER   FileOffset,
    IN ULONG            Length,
#if (VER_PRODUCTBUILD >= 2195)
    IN ULONG            Flags,
#else
    IN BOOLEAN          Wait,
#endif
    OUT PVOID           *Bcb,
    OUT PVOID           *Buffer
);

NTKERNELAPI
VOID
CcPrepareMdlWrite (
    IN PFILE_OBJECT         FileObject,
    IN PLARGE_INTEGER       FileOffset,
    IN ULONG                Length,
    OUT PMDL                *MdlChain,
    OUT PIO_STATUS_BLOCK    IoStatus
);

NTKERNELAPI
BOOLEAN
CcPreparePinWrite (
    IN PFILE_OBJECT     FileObject,
    IN PLARGE_INTEGER   FileOffset,
    IN ULONG            Length,
    IN BOOLEAN          Zero,
#if (VER_PRODUCTBUILD >= 2195)
    IN ULONG            Flags,
#else
    IN BOOLEAN          Wait,
#endif
    OUT PVOID           *Bcb,
    OUT PVOID           *Buffer
);

NTKERNELAPI
BOOLEAN
CcPurgeCacheSection (
    IN PSECTION_OBJECT_POINTERS SectionObjectPointer,
    IN PLARGE_INTEGER           FileOffset OPTIONAL,
    IN ULONG                    Length,
    IN BOOLEAN                  UninitializeCacheMaps
);

#define CcReadAhead(FO, FOFF, LEN) (                \
    if ((LEN) >= 256) {                             \
        CcScheduleReadAhead((FO), (FOFF), (LEN));   \
    }                                               \
)

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
PVOID
CcRemapBcb (
    IN PVOID Bcb
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
VOID
CcRepinBcb (
    IN PVOID Bcb
);

NTKERNELAPI
VOID
CcScheduleReadAhead (
    IN PFILE_OBJECT     FileObject,
    IN PLARGE_INTEGER   FileOffset,
    IN ULONG            Length
);

NTKERNELAPI
VOID
CcSetAdditionalCacheAttributes (
    IN PFILE_OBJECT FileObject,
    IN BOOLEAN      DisableReadAhead,
    IN BOOLEAN      DisableWriteBehind
);

NTKERNELAPI
VOID
CcSetBcbOwnerPointer (
    IN PVOID Bcb,
    IN PVOID OwnerPointer
);

NTKERNELAPI
VOID
CcSetDirtyPageThreshold (
    IN PFILE_OBJECT FileObject,
    IN ULONG        DirtyPageThreshold
);

NTKERNELAPI
VOID
CcSetDirtyPinnedData (
    IN PVOID            BcbVoid,
    IN PLARGE_INTEGER   Lsn OPTIONAL
);

NTKERNELAPI
VOID
CcSetFileSizes (
    IN PFILE_OBJECT     FileObject,
    IN PCC_FILE_SIZES   FileSizes
);

typedef VOID (*PFLUSH_TO_LSN) (
    IN PVOID            LogHandle,
    IN PLARGE_INTEGER   Lsn
);

NTKERNELAPI
VOID
CcSetLogHandleForFile (
    IN PFILE_OBJECT     FileObject,
    IN PVOID            LogHandle,
    IN PFLUSH_TO_LSN    FlushToLsnRoutine
);

NTKERNELAPI
VOID
CcSetReadAheadGranularity (
    IN PFILE_OBJECT FileObject,
    IN ULONG        Granularity     // default: PAGE_SIZE
                                    // allowed: 2^n * PAGE_SIZE
);

NTKERNELAPI
BOOLEAN
CcUninitializeCacheMap (
    IN PFILE_OBJECT                 FileObject,
    IN PLARGE_INTEGER               TruncateSize OPTIONAL,
    IN PCACHE_UNINITIALIZE_EVENT    UninitializeCompleteEvent OPTIONAL
);

NTKERNELAPI
VOID
CcUnpinData (
    IN PVOID Bcb
);

NTKERNELAPI
VOID
CcUnpinDataForThread (
    IN PVOID            Bcb,
    IN ERESOURCE_THREAD ResourceThreadId
);

NTKERNELAPI
VOID
CcUnpinRepinnedBcb (
    IN PVOID                Bcb,
    IN BOOLEAN              WriteThrough,
    OUT PIO_STATUS_BLOCK    IoStatus
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
CcWaitForCurrentLazyWriterActivity (
    VOID
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
BOOLEAN
CcZeroData (
    IN PFILE_OBJECT     FileObject,
    IN PLARGE_INTEGER   StartOffset,
    IN PLARGE_INTEGER   EndOffset,
    IN BOOLEAN          Wait
);

NTKERNELAPI
VOID
ExDisableResourceBoostLite (
    IN PERESOURCE Resource
);

NTKERNELAPI
ULONG
ExQueryPoolBlockSize (
    IN PVOID        PoolBlock,
    OUT PBOOLEAN    QuotaCharged
);

#define FlagOn(x, f) ((x) & (f))

NTKERNELAPI
BOOLEAN
FsRtlAddLargeMcbEntry (
    IN PLARGE_MCB   Mcb,
    IN LONGLONG     Vbn,
    IN LONGLONG     Lbn,
    IN LONGLONG     SectorCount
);

NTKERNELAPI
BOOLEAN
FsRtlAddMcbEntry (
    IN PMCB     Mcb,
    IN VBN      Vbn,
    IN LBN      Lbn,
    IN ULONG    SectorCount
);

NTKERNELAPI
VOID
FsRtlAddToTunnelCache (
    IN PTUNNEL          Cache,
    IN ULONGLONG        DirectoryKey,
    IN PUNICODE_STRING  ShortName,
    IN PUNICODE_STRING  LongName,
    IN BOOLEAN          KeyByShortName,
    IN ULONG            DataLength,
    IN PVOID            Data
);

#if (VER_PRODUCTBUILD >= 2195)

PFILE_LOCK
FsRtlAllocateFileLock (
    IN PCOMPLETE_LOCK_IRP_ROUTINE   CompleteLockIrpRoutine OPTIONAL,
    IN PUNLOCK_ROUTINE              UnlockRoutine OPTIONAL
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
PVOID
FsRtlAllocatePool (
    IN POOL_TYPE    PoolType,
    IN ULONG        NumberOfBytes
);

NTKERNELAPI
PVOID
FsRtlAllocatePoolWithQuota (
    IN POOL_TYPE    PoolType,
    IN ULONG        NumberOfBytes
);

NTKERNELAPI
PVOID
FsRtlAllocatePoolWithQuotaTag (
    IN POOL_TYPE    PoolType,
    IN ULONG        NumberOfBytes,
    IN ULONG        Tag
);

NTKERNELAPI
PVOID
FsRtlAllocatePoolWithTag (
    IN POOL_TYPE    PoolType,
    IN ULONG        NumberOfBytes,
    IN ULONG        Tag
);

NTKERNELAPI
PVOID
FsRtlAllocateResource (
    VOID
);

NTKERNELAPI
BOOLEAN
FsRtlAreNamesEqual (
    IN PUNICODE_STRING  Name1,
    IN PUNICODE_STRING  Name2,
    IN BOOLEAN          IgnoreCase,
    IN PWCHAR           UpcaseTable OPTIONAL
);

#define FsRtlAreThereCurrentFileLocks(FL) ( \
    ((FL)->FastIoIsQuestionable)            \
)

/*
  FsRtlCheckLockForReadAccess:

  All this really does is pick out the lock parameters from the irp (io stack
  location?), get IoGetRequestorProcess, and pass values on to
  FsRtlFastCheckLockForRead.
*/
NTKERNELAPI
BOOLEAN
FsRtlCheckLockForReadAccess (
    IN PFILE_LOCK   FileLock,
    IN PIRP         Irp
);

/*
  FsRtlCheckLockForWriteAccess:

  All this really does is pick out the lock parameters from the irp (io stack
  location?), get IoGetRequestorProcess, and pass values on to
  FsRtlFastCheckLockForWrite.
*/
NTKERNELAPI
BOOLEAN
FsRtlCheckLockForWriteAccess (
    IN PFILE_LOCK   FileLock,
    IN PIRP         Irp
);

typedef
VOID
(*POPLOCK_WAIT_COMPLETE_ROUTINE) (
    IN PVOID    Context,
    IN PIRP     Irp
);

typedef
VOID
(*POPLOCK_FS_PREPOST_IRP) (
    IN PVOID    Context,
    IN PIRP     Irp
);

NTKERNELAPI
NTSTATUS
FsRtlCheckOplock (
    IN POPLOCK                          Oplock,
    IN PIRP                             Irp,
    IN PVOID                            Context,
    IN POPLOCK_WAIT_COMPLETE_ROUTINE    CompletionRoutine OPTIONAL,
    IN POPLOCK_FS_PREPOST_IRP           PostIrpRoutine OPTIONAL
);

NTKERNELAPI
BOOLEAN
FsRtlCopyRead (
    IN PFILE_OBJECT         FileObject,
    IN PLARGE_INTEGER       FileOffset,
    IN ULONG                Length,
    IN BOOLEAN              Wait,
    IN ULONG                LockKey,
    OUT PVOID               Buffer,
    OUT PIO_STATUS_BLOCK    IoStatus,
    IN PDEVICE_OBJECT       DeviceObject
);

NTKERNELAPI
BOOLEAN
FsRtlCopyWrite (
    IN PFILE_OBJECT         FileObject,
    IN PLARGE_INTEGER       FileOffset,
    IN ULONG                Length,
    IN BOOLEAN              Wait,
    IN ULONG                LockKey,
    IN PVOID                Buffer,
    OUT PIO_STATUS_BLOCK    IoStatus,
    IN PDEVICE_OBJECT       DeviceObject
);

NTKERNELAPI
BOOLEAN
FsRtlCurrentBatchOplock (
    IN POPLOCK Oplock
);

NTKERNELAPI
VOID
FsRtlDeleteKeyFromTunnelCache (
    IN PTUNNEL      Cache,
    IN ULONGLONG    DirectoryKey
);

NTKERNELAPI
VOID
FsRtlDeleteTunnelCache (
    IN PTUNNEL Cache
);

NTKERNELAPI
VOID
FsRtlDeregisterUncProvider (
    IN HANDLE Handle
);

NTKERNELAPI
VOID
FsRtlDissectName (
    IN UNICODE_STRING   Path,
    OUT PUNICODE_STRING FirstName,
    OUT PUNICODE_STRING RemainingName
);

NTKERNELAPI
BOOLEAN
FsRtlDoesNameContainWildCards (
    IN PUNICODE_STRING Name
);

#define FsRtlEnterFileSystem    KeEnterCriticalRegion

#define FsRtlExitFileSystem     KeLeaveCriticalRegion

NTKERNELAPI
BOOLEAN
FsRtlFastCheckLockForRead (
    IN PFILE_LOCK           FileLock,
    IN PLARGE_INTEGER       FileOffset,
    IN PLARGE_INTEGER       Length,
    IN ULONG                Key,
    IN PFILE_OBJECT         FileObject,
    IN PEPROCESS            Process
);

NTKERNELAPI
BOOLEAN
FsRtlFastCheckLockForWrite (
    IN PFILE_LOCK           FileLock,
    IN PLARGE_INTEGER       FileOffset,
    IN PLARGE_INTEGER       Length,
    IN ULONG                Key,
    IN PFILE_OBJECT         FileObject,
    IN PEPROCESS            Process
);

#define FsRtlFastLock(A1, A2, A3, A4, A5, A6, A7, A8, A9, A10, A11) (       \
     FsRtlPrivateLock(A1, A2, A3, A4, A5, A6, A7, A8, A9, NULL, A10, A11)   \
)

NTKERNELAPI
NTSTATUS
FsRtlFastUnlockAll (
    IN PFILE_LOCK           FileLock,
    IN PFILE_OBJECT         FileObject,
    IN PEPROCESS            Process,
    IN PVOID                Context OPTIONAL
);
//ret: STATUS_RANGE_NOT_LOCKED

NTKERNELAPI
NTSTATUS
FsRtlFastUnlockAllByKey (
    IN PFILE_LOCK           FileLock,
    IN PFILE_OBJECT         FileObject,
    IN PEPROCESS            Process,
    IN ULONG                Key,
    IN PVOID                Context OPTIONAL
);  
//ret: STATUS_RANGE_NOT_LOCKED

NTKERNELAPI
NTSTATUS
FsRtlFastUnlockSingle (
    IN PFILE_LOCK           FileLock,
    IN PFILE_OBJECT         FileObject,
    IN PLARGE_INTEGER       FileOffset,
    IN PLARGE_INTEGER       Length,
    IN PEPROCESS            Process,
    IN ULONG                Key,
    IN PVOID                Context OPTIONAL,
    IN BOOLEAN              AlreadySynchronized
);                      
//ret:  STATUS_RANGE_NOT_LOCKED

NTKERNELAPI
BOOLEAN
FsRtlFindInTunnelCache (
    IN PTUNNEL          Cache,
    IN ULONGLONG        DirectoryKey,
    IN PUNICODE_STRING  Name,
    OUT PUNICODE_STRING ShortName,
    OUT PUNICODE_STRING LongName,
    IN OUT PULONG       DataLength,
    OUT PVOID           Data
);

#if (VER_PRODUCTBUILD >= 2195)

VOID
FsRtlFreeFileLock (
    IN PFILE_LOCK FileLock
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
FsRtlGetFileSize (
    IN PFILE_OBJECT         FileObject,
    IN OUT PLARGE_INTEGER   FileSize
);

/*
  FsRtlGetNextFileLock:

  ret: NULL if no more locks

  Internals:
    FsRtlGetNextFileLock uses FileLock->LastReturnedLockInfo and
    FileLock->LastReturnedLock as storage.
    LastReturnedLock is a pointer to the 'raw' lock inkl. double linked
    list, and FsRtlGetNextFileLock needs this to get next lock on subsequent
    calls with Restart = FALSE.
*/
NTKERNELAPI
PFILE_LOCK_INFO
FsRtlGetNextFileLock (
    IN PFILE_LOCK   FileLock,
    IN BOOLEAN      Restart
);

NTKERNELAPI
BOOLEAN
FsRtlGetNextLargeMcbEntry (
    IN PLARGE_MCB   Mcb,
    IN ULONG        RunIndex,
    OUT PLONGLONG   Vbn,
    OUT PLONGLONG   Lbn,
    OUT PLONGLONG   SectorCount
);

NTKERNELAPI
BOOLEAN
FsRtlGetNextMcbEntry (
    IN PMCB     Mcb,
    IN ULONG    RunIndex,
    OUT PVBN    Vbn,
    OUT PLBN    Lbn,
    OUT PULONG  SectorCount
);

#if (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
VOID
FsRtlIncrementCcFastReadNotPossible (
    VOID
);

NTKERNELAPI
VOID
FsRtlIncrementCcFastReadNoWait (
    VOID
);

NTKERNELAPI
VOID
FsRtlIncrementCcFastReadResourceMiss (
    VOID
);

NTKERNELAPI
VOID
FsRtlIncrementCcFastReadWait (
    VOID
);

#endif // (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
VOID
FsRtlInitializeFileLock (
    IN PFILE_LOCK                   FileLock,
    IN PCOMPLETE_LOCK_IRP_ROUTINE   CompleteLockIrpRoutine OPTIONAL,
    IN PUNLOCK_ROUTINE              UnlockRoutine OPTIONAL
);

NTKERNELAPI
VOID
FsRtlInitializeLargeMcb (
    IN PLARGE_MCB   Mcb,
    IN POOL_TYPE    PoolType
);

NTKERNELAPI
VOID
FsRtlInitializeMcb (
    IN PMCB         Mcb,
    IN POOL_TYPE    PoolType
);

NTKERNELAPI
VOID
FsRtlInitializeOplock (
    IN OUT POPLOCK Oplock
);

NTKERNELAPI
VOID
FsRtlInitializeTunnelCache (
    IN PTUNNEL Cache
);

NTKERNELAPI
BOOLEAN
FsRtlIsNameInExpression (
    IN PUNICODE_STRING  Expression,
    IN PUNICODE_STRING  Name,
    IN BOOLEAN          IgnoreCase,
    IN PWCHAR           UpcaseTable OPTIONAL
);

NTKERNELAPI
BOOLEAN
FsRtlIsNtstatusExpected (
    IN NTSTATUS Ntstatus
);

#define FsRtlIsUnicodeCharacterWild(C) (                                    \
    (((C) >= 0x40) ?                                                        \
    FALSE :                                                                 \
    FlagOn((*FsRtlLegalAnsiCharacterArray)[(C)], FSRTL_WILD_CHARACTER ))    \
)

NTKERNELAPI
BOOLEAN
FsRtlLookupLargeMcbEntry (
    IN PLARGE_MCB   Mcb,
    IN LONGLONG     Vbn,
    OUT PLONGLONG   Lbn OPTIONAL,
    OUT PLONGLONG   SectorCountFromLbn OPTIONAL,
    OUT PLONGLONG   StartingLbn OPTIONAL,
    OUT PLONGLONG   SectorCountFromStartingLbn OPTIONAL,
    OUT PULONG      Index OPTIONAL
);

NTKERNELAPI
BOOLEAN
FsRtlLookupLastLargeMcbEntry (
    IN PLARGE_MCB Mcb,
    OUT PLONGLONG Vbn,
    OUT PLONGLONG Lbn
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
BOOLEAN
FsRtlLookupLastLargeMcbEntryAndIndex (
    IN PLARGE_MCB   OpaqueMcb,
    OUT PLONGLONG   LargeVbn,
    OUT PLONGLONG   LargeLbn,
    OUT PULONG      Index
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
BOOLEAN
FsRtlLookupLastMcbEntry (
    IN PMCB     Mcb,
    OUT PVBN    Vbn,
    OUT PLBN    Lbn
);

NTKERNELAPI
BOOLEAN
FsRtlLookupMcbEntry (
    IN PMCB     Mcb,
    IN VBN      Vbn,
    OUT PLBN    Lbn,
    OUT PULONG  SectorCount OPTIONAL,
    OUT PULONG  Index
);

NTKERNELAPI
BOOLEAN
FsRtlMdlReadComplete (
    IN PFILE_OBJECT     FileObject,
    IN PMDL             MdlChain
);

NTKERNELAPI
BOOLEAN
FsRtlMdlReadCompleteDev (
    IN PFILE_OBJECT     FileObject,
    IN PMDL             MdlChain,
    IN PDEVICE_OBJECT   DeviceObject
);

NTKERNELAPI
BOOLEAN
FsRtlMdlWriteComplete (
    IN PFILE_OBJECT     FileObject,
    IN PLARGE_INTEGER   FileOffset,
    IN PMDL             MdlChain
);

NTKERNELAPI
BOOLEAN
FsRtlMdlWriteCompleteDev (
    IN PFILE_OBJECT     FileObject,
    IN PLARGE_INTEGER   FileOffset,
    IN PMDL             MdlChain,
    IN PDEVICE_OBJECT   DeviceObject
);

NTKERNELAPI
NTSTATUS
FsRtlNormalizeNtstatus (
    IN NTSTATUS Exception,
    IN NTSTATUS GenericException
);

NTKERNELAPI
VOID
FsRtlNotifyChangeDirectory (
    IN PNOTIFY_SYNC NotifySync,
    IN PVOID        FsContext,
    IN PSTRING      FullDirectoryName,
    IN PLIST_ENTRY  NotifyList,
    IN BOOLEAN      WatchTree,
    IN ULONG        CompletionFilter,
    IN PIRP         NotifyIrp
);

NTKERNELAPI
VOID
FsRtlNotifyCleanup (
    IN PNOTIFY_SYNC NotifySync,
    IN PLIST_ENTRY  NotifyList,
    IN PVOID        FsContext
);

typedef BOOLEAN (*PCHECK_FOR_TRAVERSE_ACCESS) (
    IN PVOID                        NotifyContext,
    IN PVOID                        TargetContext,
    IN PSECURITY_SUBJECT_CONTEXT    SubjectContext
);

#if (VER_PRODUCTBUILD >= 2600)

typedef BOOLEAN (*PFILTER_REPORT_CHANGE) (
    IN PVOID NotifyContext,
    IN PVOID FilterContext
);

NTKERNELAPI
VOID
FsRtlNotifyFilterChangeDirectory (
    IN PNOTIFY_SYNC                 NotifySync,
    IN PLIST_ENTRY                  NotifyList,
    IN PVOID                        FsContext,
    IN PSTRING                      FullDirectoryName,
    IN BOOLEAN                      WatchTree,
    IN BOOLEAN                      IgnoreBuffer,
    IN ULONG                        CompletionFilter,
    IN PIRP                         NotifyIrp,
    IN PCHECK_FOR_TRAVERSE_ACCESS   TraverseCallback OPTIONAL,
    IN PSECURITY_SUBJECT_CONTEXT    SubjectContext OPTIONAL,
    IN PFILTER_REPORT_CHANGE        FilterCallback OPTIONAL
);

NTKERNELAPI
VOID
FsRtlNotifyFilterReportChange (
    IN PNOTIFY_SYNC NotifySync,
    IN PLIST_ENTRY  NotifyList,
    IN PSTRING      FullTargetName,
    IN USHORT       TargetNameOffset,
    IN PSTRING      StreamName OPTIONAL,
    IN PSTRING      NormalizedParentName OPTIONAL,
    IN ULONG        FilterMatch,
    IN ULONG        Action,
    IN PVOID        TargetContext,
    IN PVOID        FilterContext
);

#endif // (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
VOID
FsRtlNotifyFullChangeDirectory (
    IN PNOTIFY_SYNC                 NotifySync,
    IN PLIST_ENTRY                  NotifyList,
    IN PVOID                        FsContext,
    IN PSTRING                      FullDirectoryName,
    IN BOOLEAN                      WatchTree,
    IN BOOLEAN                      IgnoreBuffer,
    IN ULONG                        CompletionFilter,
    IN PIRP                         NotifyIrp,
    IN PCHECK_FOR_TRAVERSE_ACCESS   TraverseCallback OPTIONAL,
    IN PSECURITY_SUBJECT_CONTEXT    SubjectContext OPTIONAL
);

NTKERNELAPI
VOID
FsRtlNotifyFullReportChange (
    IN PNOTIFY_SYNC NotifySync,
    IN PLIST_ENTRY  NotifyList,
    IN PSTRING      FullTargetName,
    IN USHORT       TargetNameOffset,
    IN PSTRING      StreamName OPTIONAL,
    IN PSTRING      NormalizedParentName OPTIONAL,
    IN ULONG        FilterMatch,
    IN ULONG        Action,
    IN PVOID        TargetContext
);

NTKERNELAPI
VOID
FsRtlNotifyInitializeSync (
    IN PNOTIFY_SYNC *NotifySync
);

NTKERNELAPI
VOID
FsRtlNotifyReportChange (
    IN PNOTIFY_SYNC NotifySync,
    IN PLIST_ENTRY  NotifyList,
    IN PSTRING      FullTargetName,
    IN PUSHORT      FileNamePartLength,
    IN ULONG        FilterMatch
);

NTKERNELAPI
VOID
FsRtlNotifyUninitializeSync (
    IN PNOTIFY_SYNC *NotifySync
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
FsRtlNotifyVolumeEvent (
    IN PFILE_OBJECT FileObject,
    IN ULONG        EventCode
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
ULONG
FsRtlNumberOfRunsInLargeMcb (
    IN PLARGE_MCB Mcb
);

NTKERNELAPI
ULONG
FsRtlNumberOfRunsInMcb (
    IN PMCB Mcb
);

NTKERNELAPI
NTSTATUS
FsRtlOplockFsctrl (
    IN POPLOCK  Oplock,
    IN PIRP     Irp,
    IN ULONG    OpenCount
);

NTKERNELAPI
BOOLEAN
FsRtlOplockIsFastIoPossible (
    IN POPLOCK Oplock
);

/*
  FsRtlPrivateLock:

  ret: IoStatus->Status: STATUS_PENDING, STATUS_LOCK_NOT_GRANTED

  Internals: 
    -Calls IoCompleteRequest if Irp
    -Uses exception handling / ExRaiseStatus with STATUS_INSUFFICIENT_RESOURCES
*/
NTKERNELAPI
BOOLEAN
FsRtlPrivateLock (
    IN PFILE_LOCK           FileLock,
    IN PFILE_OBJECT         FileObject,
    IN PLARGE_INTEGER       FileOffset,
    IN PLARGE_INTEGER       Length,
    IN PEPROCESS            Process,
    IN ULONG                Key,
    IN BOOLEAN              FailImmediately, 
    IN BOOLEAN              ExclusiveLock,
    OUT PIO_STATUS_BLOCK    IoStatus, 
    IN PIRP                 Irp OPTIONAL,
    IN PVOID                Context,
    IN BOOLEAN              AlreadySynchronized
);

/*
  FsRtlProcessFileLock:

  ret:
    -STATUS_INVALID_DEVICE_REQUEST
    -STATUS_RANGE_NOT_LOCKED from unlock routines.
    -STATUS_PENDING, STATUS_LOCK_NOT_GRANTED from FsRtlPrivateLock
    (redirected IoStatus->Status).

  Internals: 
    -switch ( Irp->CurrentStackLocation->MinorFunction )
        lock: return FsRtlPrivateLock;
        unlocksingle: return FsRtlFastUnlockSingle;
        unlockall: return FsRtlFastUnlockAll;
        unlockallbykey: return FsRtlFastUnlockAllByKey;
        default: IofCompleteRequest with STATUS_INVALID_DEVICE_REQUEST;
                 return STATUS_INVALID_DEVICE_REQUEST;

    -'AllwaysZero' is passed thru as 'AllwaysZero' to lock / unlock routines.
    -'Irp' is passet thru as 'Irp' to FsRtlPrivateLock.
*/
NTKERNELAPI
NTSTATUS
FsRtlProcessFileLock (
    IN PFILE_LOCK   FileLock,
    IN PIRP         Irp,
    IN PVOID        Context OPTIONAL
);

NTKERNELAPI
NTSTATUS
FsRtlRegisterUncProvider (
    IN OUT PHANDLE      MupHandle,
    IN PUNICODE_STRING  RedirectorDeviceName,
    IN BOOLEAN          MailslotsSupported
);

NTKERNELAPI
VOID
FsRtlRemoveLargeMcbEntry (
    IN PLARGE_MCB   Mcb,
    IN LONGLONG     Vbn,
    IN LONGLONG     SectorCount
);

NTKERNELAPI
VOID
FsRtlRemoveMcbEntry (
    IN PMCB     Mcb,
    IN VBN      Vbn,
    IN ULONG    SectorCount
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
VOID
FsRtlResetLargeMcb (
    IN PLARGE_MCB   Mcb,
    IN BOOLEAN      SelfSynchronized
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
BOOLEAN
FsRtlSplitLargeMcb (
    IN PLARGE_MCB   Mcb,
    IN LONGLONG     Vbn,
    IN LONGLONG     Amount
);

NTKERNELAPI
VOID
FsRtlTruncateLargeMcb (
    IN PLARGE_MCB   Mcb,
    IN LONGLONG     Vbn
);

NTKERNELAPI
VOID
FsRtlTruncateMcb (
    IN PMCB Mcb,
    IN VBN  Vbn
);

NTKERNELAPI
VOID
FsRtlUninitializeFileLock (
    IN PFILE_LOCK FileLock
);

NTKERNELAPI
VOID
FsRtlUninitializeLargeMcb (
    IN PLARGE_MCB Mcb
);

NTKERNELAPI
VOID
FsRtlUninitializeMcb (
    IN PMCB Mcb
);

NTKERNELAPI
VOID
FsRtlUninitializeOplock (
    IN OUT POPLOCK Oplock
);

//
// If using HalDisplayString during boot on Windows 2000 or later you must
// first call InbvEnableDisplayString.
//
NTSYSAPI
VOID
NTAPI
HalDisplayString (
    IN PCHAR String
);

NTSYSAPI
VOID
NTAPI
HalQueryRealTimeClock (
    IN OUT PTIME_FIELDS TimeFields
);

NTSYSAPI
VOID
NTAPI
HalSetRealTimeClock (
    IN PTIME_FIELDS TimeFields
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
VOID
InbvAcquireDisplayOwnership (
    VOID
);

NTKERNELAPI
BOOLEAN
InbvCheckDisplayOwnership (
    VOID
);

NTKERNELAPI
BOOLEAN
InbvDisplayString (
    IN PCHAR String
);

NTKERNELAPI
VOID
InbvEnableBootDriver (
    IN BOOLEAN Enable
);

NTKERNELAPI
BOOLEAN
InbvEnableDisplayString (
    IN BOOLEAN Enable
);

NTKERNELAPI
VOID
InbvInstallDisplayStringFilter (
    IN PVOID Unknown
);

NTKERNELAPI
BOOLEAN
InbvIsBootDriverInstalled (
    VOID
);

NTKERNELAPI
VOID
InbvNotifyDisplayOwnershipLost (
    IN PVOID Callback
);

NTKERNELAPI
BOOLEAN
InbvResetDisplay (
    VOID
);

NTKERNELAPI
VOID
InbvSetScrollRegion (
    IN ULONG Left,
    IN ULONG Top,
    IN ULONG Width,
    IN ULONG Height
);

NTKERNELAPI
VOID
InbvSetTextColor (
    IN ULONG Color
);

NTKERNELAPI
VOID
InbvSolidColorFill (
    IN ULONG Left,
    IN ULONG Top,
    IN ULONG Width,
    IN ULONG Height,
    IN ULONG Color
);

#endif // (VER_PRODUCTBUILD >= 2195)

#define InitializeMessageHeader(m, l, t) {                  \
    (m)->Length = (USHORT)(l);                              \
    (m)->DataLength = (USHORT)(l - sizeof( LPC_MESSAGE ));  \
    (m)->MessageType = (USHORT)(t);                         \
    (m)->DataInfoOffset = 0;                                \
}

NTKERNELAPI
VOID
IoAcquireVpbSpinLock (
    OUT PKIRQL Irql
);

NTKERNELAPI
NTSTATUS
IoCheckDesiredAccess (
    IN OUT PACCESS_MASK DesiredAccess,
    IN ACCESS_MASK      GrantedAccess
);

NTKERNELAPI
NTSTATUS
IoCheckEaBufferValidity (
    IN PFILE_FULL_EA_INFORMATION    EaBuffer,
    IN ULONG                        EaLength,
    OUT PULONG                      ErrorOffset
);

NTKERNELAPI
NTSTATUS
IoCheckFunctionAccess (
    IN ACCESS_MASK              GrantedAccess,
    IN UCHAR                    MajorFunction,
    IN UCHAR                    MinorFunction,
    IN ULONG                    IoControlCode,
    IN PFILE_INFORMATION_CLASS  FileInformationClass OPTIONAL,
    IN PFS_INFORMATION_CLASS    FsInformationClass OPTIONAL
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
IoCheckQuotaBufferValidity (
    IN PFILE_QUOTA_INFORMATION  QuotaBuffer,
    IN ULONG                    QuotaLength,
    OUT PULONG                  ErrorOffset
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
PFILE_OBJECT
IoCreateStreamFileObject (
    IN PFILE_OBJECT     FileObject OPTIONAL,
    IN PDEVICE_OBJECT   DeviceObject OPTIONAL
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
PFILE_OBJECT
IoCreateStreamFileObjectLite (
    IN PFILE_OBJECT     FileObject OPTIONAL,
    IN PDEVICE_OBJECT   DeviceObject OPTIONAL
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
BOOLEAN
IoFastQueryNetworkAttributes (
    IN POBJECT_ATTRIBUTES               ObjectAttributes,
    IN ACCESS_MASK                      DesiredAccess,
    IN ULONG                            OpenOptions,
    OUT PIO_STATUS_BLOCK                IoStatus,
    OUT PFILE_NETWORK_OPEN_INFORMATION  Buffer
);

NTKERNELAPI
PDEVICE_OBJECT
IoGetAttachedDevice (
    IN PDEVICE_OBJECT DeviceObject
);

NTKERNELAPI
PDEVICE_OBJECT
IoGetBaseFileSystemDeviceObject (
    IN PFILE_OBJECT FileObject
);

NTKERNELAPI
PEPROCESS
IoGetRequestorProcess (
    IN PIRP Irp
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
ULONG
IoGetRequestorProcessId (
    IN PIRP Irp
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
PIRP
IoGetTopLevelIrp (
    VOID
);

#define IoIsFileOpenedExclusively(FileObject) ( \
    (BOOLEAN) !(                                \
    (FileObject)->SharedRead ||                 \
    (FileObject)->SharedWrite ||                \
    (FileObject)->SharedDelete                  \
    )                                           \
)

NTKERNELAPI
BOOLEAN
IoIsOperationSynchronous (
    IN PIRP Irp
);

NTKERNELAPI
BOOLEAN
IoIsSystemThread (
    IN PETHREAD Thread
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
BOOLEAN
IoIsValidNameGraftingBuffer (
    IN PIRP                 Irp,
    IN PREPARSE_DATA_BUFFER ReparseBuffer
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
IoPageRead (
    IN PFILE_OBJECT         FileObject,
    IN PMDL                 Mdl,
    IN PLARGE_INTEGER       Offset,
    IN PKEVENT              Event,
    OUT PIO_STATUS_BLOCK    IoStatusBlock
);

NTKERNELAPI
NTSTATUS
IoQueryFileInformation (
    IN PFILE_OBJECT             FileObject,
    IN FILE_INFORMATION_CLASS   FileInformationClass,
    IN ULONG                    Length,
    OUT PVOID                   FileInformation,
    OUT PULONG                  ReturnedLength
);

NTKERNELAPI
NTSTATUS
IoQueryVolumeInformation (
    IN PFILE_OBJECT         FileObject,
    IN FS_INFORMATION_CLASS FsInformationClass,
    IN ULONG                Length,
    OUT PVOID               FsInformation,
    OUT PULONG              ReturnedLength
);

NTKERNELAPI
VOID
IoRegisterFileSystem (
    IN OUT PDEVICE_OBJECT DeviceObject
);

#if (VER_PRODUCTBUILD >= 1381)

typedef VOID (*PDRIVER_FS_NOTIFICATION) (
    IN PDEVICE_OBJECT DeviceObject,
    IN BOOLEAN        DriverActive
);

NTKERNELAPI
NTSTATUS
IoRegisterFsRegistrationChange (
    IN PDRIVER_OBJECT           DriverObject,
    IN PDRIVER_FS_NOTIFICATION  DriverNotificationRoutine
);

#endif // (VER_PRODUCTBUILD >= 1381)

NTKERNELAPI
VOID
IoReleaseVpbSpinLock (
    IN KIRQL Irql
);

NTKERNELAPI
VOID
IoSetDeviceToVerify (
    IN PETHREAD         Thread,
    IN PDEVICE_OBJECT   DeviceObject
);

NTKERNELAPI
NTSTATUS
IoSetInformation (
    IN PFILE_OBJECT             FileObject,
    IN FILE_INFORMATION_CLASS   FileInformationClass,
    IN ULONG                    Length,
    IN PVOID                    FileInformation
);

NTKERNELAPI
VOID
IoSetTopLevelIrp (
    IN PIRP Irp
);

NTKERNELAPI
NTSTATUS
IoSynchronousPageWrite (
    IN PFILE_OBJECT         FileObject,
    IN PMDL                 Mdl,
    IN PLARGE_INTEGER       FileOffset,
    IN PKEVENT              Event,
    OUT PIO_STATUS_BLOCK    IoStatusBlock
);

NTKERNELAPI
PEPROCESS
IoThreadToProcess (
    IN PETHREAD Thread
);

NTKERNELAPI
VOID
IoUnregisterFileSystem (
    IN OUT PDEVICE_OBJECT DeviceObject
);

#if (VER_PRODUCTBUILD >= 1381)

NTKERNELAPI
NTSTATUS
IoUnregisterFsRegistrationChange (
    IN PDRIVER_OBJECT           DriverObject,
    IN PDRIVER_FS_NOTIFICATION  DriverNotificationRoutine
);

#endif // (VER_PRODUCTBUILD >= 1381)

NTKERNELAPI
NTSTATUS
IoVerifyVolume (
    IN PDEVICE_OBJECT   DeviceObject,
    IN BOOLEAN          AllowRawMount
);

NTKERNELAPI
VOID
KeAttachProcess (
    IN PEPROCESS Process
);

NTKERNELAPI
VOID
KeDetachProcess (
    VOID
);

NTKERNELAPI
VOID
KeInitializeApc (
    PKAPC               Apc,
    PKTHREAD            Thread,
    UCHAR               StateIndex,
    PKKERNEL_ROUTINE    KernelRoutine,
    PKRUNDOWN_ROUTINE   RundownRoutine,
    PKNORMAL_ROUTINE    NormalRoutine,
    KPROCESSOR_MODE     ApcMode,
    PVOID               NormalContext
);

NTKERNELAPI
VOID
KeInitializeQueue (
    IN PRKQUEUE Queue,
    IN ULONG    Count OPTIONAL
);

NTKERNELAPI
LONG
KeInsertHeadQueue (
    IN PRKQUEUE     Queue,
    IN PLIST_ENTRY  Entry
);

NTKERNELAPI
LONG
KeInsertQueue (
    IN PRKQUEUE     Queue,
    IN PLIST_ENTRY  Entry
);

NTKERNELAPI
VOID
KeInsertQueueApc (
    IN PKAPC    Apc,
    IN PVOID    SystemArgument1,
    IN PVOID    SystemArgument2,
    UCHAR       Unknown
);

#if (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
BOOLEAN
KeIsAttachedProcess (
    VOID
);

#endif // (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
BOOLEAN
KeIsExecutingDpc (
    VOID
);

NTKERNELAPI
LONG
KeReadStateQueue (
    IN PRKQUEUE Queue
);

NTKERNELAPI
PLIST_ENTRY
KeRemoveQueue (
    IN PRKQUEUE         Queue,
    IN KPROCESSOR_MODE  WaitMode,
    IN PLARGE_INTEGER   Timeout OPTIONAL
);

#if (VER_PRODUCTBUILD >= 2195)
/*
NTKERNELAPI
NTSTATUS
KeRevertToUserAffinityThread (
    VOID
);
*/

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
PLIST_ENTRY
KeRundownQueue (
    IN PRKQUEUE Queue
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
VOID
KeStackAttachProcess (
    IN PKPROCESS    Process,
    OUT PKAPC_STATE ApcState
);

NTKERNELAPI
VOID
KeUnstackDetachProcess (
    IN PKAPC_STATE ApcState
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
KeUpdateSystemTime (
    VOID
);

NTKERNELAPI
BOOLEAN
MmCanFileBeTruncated (
    IN PSECTION_OBJECT_POINTERS     SectionObjectPointer,
    IN PLARGE_INTEGER               NewFileSize
);

NTKERNELAPI
NTSTATUS
MmCreateSection (
    OUT PVOID               *SectionObject,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes OPTIONAL,
    IN PLARGE_INTEGER       MaximumSize,
    IN ULONG                SectionPageProtection,
    IN ULONG                AllocationAttributes,
    IN HANDLE               FileHandle OPTIONAL,
    IN PFILE_OBJECT         FileObject OPTIONAL
);

NTKERNELAPI
BOOLEAN
MmFlushImageSection (
    IN PSECTION_OBJECT_POINTERS     SectionObjectPointer,
    IN MMFLUSH_TYPE                 FlushType
);

NTKERNELAPI
BOOLEAN
MmForceSectionClosed (
    IN PSECTION_OBJECT_POINTERS SectionObjectPointer,
    IN BOOLEAN                  DelayClose
);

#if (VER_PRODUCTBUILD >= 1381)

NTKERNELAPI
BOOLEAN
MmIsRecursiveIoFault (
    VOID
);

#else

#define MmIsRecursiveIoFault() (                            \
    (PsGetCurrentThread()->DisablePageFaultClustering) |    \
    (PsGetCurrentThread()->ForwardClusterOnly)              \
)

#endif

NTKERNELAPI
NTSTATUS
MmMapViewOfSection (
    IN PVOID                SectionObject,
    IN PEPROCESS            Process,
    IN OUT PVOID            *BaseAddress,
    IN ULONG                ZeroBits,
    IN ULONG                CommitSize,
    IN OUT PLARGE_INTEGER   SectionOffset OPTIONAL,
    IN OUT PULONG           ViewSize,
    IN SECTION_INHERIT      InheritDisposition,
    IN ULONG                AllocationType,
    IN ULONG                Protect
);

NTKERNELAPI
BOOLEAN
MmSetAddressRangeModified (
    IN PVOID    Address,
    IN SIZE_T   Length
);

NTKERNELAPI
NTSTATUS
ObCreateObject (
    IN KPROCESSOR_MODE      ObjectAttributesAccessMode OPTIONAL,
    IN POBJECT_TYPE         ObjectType,
    IN POBJECT_ATTRIBUTES   ObjectAttributes OPTIONAL,
    IN KPROCESSOR_MODE      AccessMode,
    IN OUT PVOID            ParseContext OPTIONAL,
    IN ULONG                ObjectSize,
    IN ULONG                PagedPoolCharge OPTIONAL,
    IN ULONG                NonPagedPoolCharge OPTIONAL,
    OUT PVOID               *Object
);

NTKERNELAPI
ULONG
ObGetObjectPointerCount (
    IN PVOID Object
);

NTKERNELAPI
NTSTATUS
ObInsertObject (
    IN PVOID            Object,
    IN PACCESS_STATE    PassedAccessState OPTIONAL,
    IN ACCESS_MASK      DesiredAccess,
    IN ULONG            AdditionalReferences,
    OUT PVOID           *ReferencedObject OPTIONAL,
    OUT PHANDLE         Handle
);

NTKERNELAPI
VOID
ObMakeTemporaryObject (
    IN PVOID Object
);

NTKERNELAPI
NTSTATUS
ObOpenObjectByPointer (
    IN PVOID            Object,
    IN ULONG            HandleAttributes,
    IN PACCESS_STATE    PassedAccessState OPTIONAL,
    IN ACCESS_MASK      DesiredAccess OPTIONAL,
    IN POBJECT_TYPE     ObjectType OPTIONAL,
    IN KPROCESSOR_MODE  AccessMode,
    OUT PHANDLE         Handle
);

NTKERNELAPI
NTSTATUS
ObQueryNameString (
    IN PVOID                        Object,
    OUT POBJECT_NAME_INFORMATION    ObjectNameInfo,
    IN ULONG                        Length,
    OUT PULONG                      ReturnLength
);

NTKERNELAPI
NTSTATUS
ObQueryObjectAuditingByHandle (
    IN HANDLE       Handle,
    OUT PBOOLEAN    GenerateOnClose
);

NTKERNELAPI
NTSTATUS
ObReferenceObjectByName (
    IN PUNICODE_STRING  ObjectName,
    IN ULONG            Attributes,
    IN PACCESS_STATE    PassedAccessState OPTIONAL,
    IN ACCESS_MASK      DesiredAccess OPTIONAL,
    IN POBJECT_TYPE     ObjectType,
    IN KPROCESSOR_MODE  AccessMode,
    IN OUT PVOID        ParseContext OPTIONAL,
    OUT PVOID           *Object
);

NTKERNELAPI
NTSTATUS
PsAssignImpersonationToken (
    IN PETHREAD Thread,
    IN HANDLE   Token
);

NTKERNELAPI
VOID
PsChargePoolQuota (
    IN PEPROCESS    Process,
    IN POOL_TYPE    PoolType,
    IN ULONG        Amount
);

#if (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
NTSTATUS
PsChargeProcessNonPagedPoolQuota (
    IN PEPROCESS Process,
    IN ULONG_PTR Amount
);

NTKERNELAPI
NTSTATUS
PsChargeProcessPagedPoolQuota (
    IN PEPROCESS Process,
    IN ULONG_PTR Amount
);

NTKERNELAPI
NTSTATUS
PsChargeProcessPoolQuota (
    IN PEPROCESS Process,
    IN POOL_TYPE PoolType,
    IN ULONG_PTR Amount
);

#endif // (VER_PRODUCTBUILD >= 2600)

#if (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
VOID
PsDereferenceImpersonationToken (
    IN PACCESS_TOKEN ImpersonationToken
);

NTKERNELAPI
VOID
PsDereferencePrimaryToken (
    IN PACCESS_TOKEN PrimaryToken
);

#else

#define PsDereferenceImpersonationToken(T)  \
            {if (ARGUMENT_PRESENT(T)) {     \
                (ObDereferenceObject((T))); \
            } else {                        \
                ;                           \
            }                               \
}

#define PsDereferencePrimaryToken(T) (ObDereferenceObject((T)))

#endif

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
BOOLEAN
PsDisableImpersonation (
    IN PETHREAD                 Thread,
    IN PSE_IMPERSONATION_STATE  ImpersonationState
);

#endif // (VER_PRODUCTBUILD >= 2195)

#if (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
ULONG
PsGetCurrentProcessSessionId (
    VOID
);

NTKERNELAPI
KPROCESSOR_MODE
PsGetCurrentThreadPreviousMode (
    VOID
);

NTKERNELAPI
PVOID
PsGetCurrentThreadStackBase (
    VOID
);

NTKERNELAPI
PVOID
PsGetCurrentThreadStackLimit (
    VOID
);

#endif // (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
LARGE_INTEGER
PsGetProcessExitTime (
    VOID
);

NTKERNELAPI
NTSTATUS
PsImpersonateClient (
    IN PETHREAD                     Thread,
    IN PACCESS_TOKEN                Token,
    IN BOOLEAN                      CopyOnOpen,
    IN BOOLEAN                      EffectiveOnly,
    IN SECURITY_IMPERSONATION_LEVEL ImpersonationLevel
);

#if (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
BOOLEAN
PsIsSystemThread (
    IN PETHREAD Thread
);

#endif // (VER_PRODUCTBUILD >= 2600)

NTKERNELAPI
BOOLEAN
PsIsThreadTerminating (
    IN PETHREAD Thread
);

//
// PsLookupProcessByProcessId returns a referenced pointer to the process
// that should be dereferenced after use with a call to ObDereferenceObject.
//
NTKERNELAPI
NTSTATUS
PsLookupProcessByProcessId (
    IN PVOID        ProcessId,
    OUT PEPROCESS   *Process
);

NTKERNELAPI
NTSTATUS
PsLookupProcessThreadByCid (
    IN PCLIENT_ID   Cid,
    OUT PEPROCESS   *Process OPTIONAL,
    OUT PETHREAD    *Thread
);

NTKERNELAPI
NTSTATUS
PsLookupThreadByThreadId (
    IN PVOID        UniqueThreadId,
    OUT PETHREAD    *Thread
);

NTKERNELAPI
PACCESS_TOKEN
PsReferenceImpersonationToken (
    IN PETHREAD                         Thread,
    OUT PBOOLEAN                        CopyOnOpen,
    OUT PBOOLEAN                        EffectiveOnly,
    OUT PSECURITY_IMPERSONATION_LEVEL   ImpersonationLevel
);

NTKERNELAPI
PACCESS_TOKEN
PsReferencePrimaryToken (
    IN PEPROCESS Process
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
VOID
PsRestoreImpersonation (
    IN PETHREAD                 Thread,
    IN PSE_IMPERSONATION_STATE  ImpersonationState
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
VOID
PsReturnPoolQuota (
    IN PEPROCESS    Process,
    IN POOL_TYPE    PoolType,
    IN ULONG        Amount
);

NTKERNELAPI
VOID
PsRevertToSelf (
    VOID
);

NTSYSAPI
NTSTATUS
NTAPI
RtlAbsoluteToSelfRelativeSD (
    IN PSECURITY_DESCRIPTOR     AbsoluteSecurityDescriptor,
    IN OUT PSECURITY_DESCRIPTOR SelfRelativeSecurityDescriptor,
    IN PULONG                   BufferLength
);

NTSYSAPI
PVOID
NTAPI
RtlAllocateHeap (
    IN HANDLE  HeapHandle,
    IN ULONG   Flags,
    IN ULONG   Size
);

NTSYSAPI
NTSTATUS
NTAPI
RtlCompressBuffer (
    IN USHORT   CompressionFormatAndEngine,
    IN PUCHAR   UncompressedBuffer,
    IN ULONG    UncompressedBufferSize,
    OUT PUCHAR  CompressedBuffer,
    IN ULONG    CompressedBufferSize,
    IN ULONG    UncompressedChunkSize,
    OUT PULONG  FinalCompressedSize,
    IN PVOID    WorkSpace
);

NTSYSAPI
NTSTATUS
NTAPI
RtlCompressChunks (
    IN PUCHAR                       UncompressedBuffer,
    IN ULONG                        UncompressedBufferSize,
    OUT PUCHAR                      CompressedBuffer,
    IN ULONG                        CompressedBufferSize,
    IN OUT PCOMPRESSED_DATA_INFO    CompressedDataInfo,
    IN ULONG                        CompressedDataInfoLength,
    IN PVOID                        WorkSpace
);

NTSYSAPI
NTSTATUS
NTAPI
RtlConvertSidToUnicodeString (
    OUT PUNICODE_STRING DestinationString,
    IN PSID             Sid,
    IN BOOLEAN          AllocateDestinationString
);

NTSYSAPI
NTSTATUS
NTAPI
RtlCopySid (
    IN ULONG   Length,
    IN PSID    Destination,
    IN PSID    Source
);

NTSYSAPI
HANDLE
NTAPI
RtlCreateHeap (
    IN ULONG Flags,
    IN PVOID Base,
    IN ULONG Reserve, 
    IN ULONG Commit, 
    IN ULONG Lock, 
    IN PVOID RtlHeapParams
);

NTSYSAPI
NTSTATUS
NTAPI
RtlDecompressBuffer (
    IN USHORT   CompressionFormat,
    OUT PUCHAR  UncompressedBuffer,
    IN ULONG    UncompressedBufferSize,
    IN PUCHAR   CompressedBuffer,
    IN ULONG    CompressedBufferSize,
    OUT PULONG  FinalUncompressedSize
);

NTSYSAPI
NTSTATUS
NTAPI
RtlDecompressChunks (
    OUT PUCHAR                  UncompressedBuffer,
    IN ULONG                    UncompressedBufferSize,
    IN PUCHAR                   CompressedBuffer,
    IN ULONG                    CompressedBufferSize,
    IN PUCHAR                   CompressedTail,
    IN ULONG                    CompressedTailSize,
    IN PCOMPRESSED_DATA_INFO    CompressedDataInfo
);

NTSYSAPI
NTSTATUS
NTAPI
RtlDecompressFragment (
    IN USHORT   CompressionFormat,
    OUT PUCHAR  UncompressedFragment,
    IN ULONG    UncompressedFragmentSize,
    IN PUCHAR   CompressedBuffer,
    IN ULONG    CompressedBufferSize,
    IN ULONG    FragmentOffset,
    OUT PULONG  FinalUncompressedSize,
    IN PVOID    WorkSpace
);

NTSYSAPI
NTSTATUS
NTAPI
RtlDescribeChunk (
    IN USHORT       CompressionFormat,
    IN OUT PUCHAR   *CompressedBuffer,
    IN PUCHAR       EndOfCompressedBufferPlus1,
    OUT PUCHAR      *ChunkBuffer,
    OUT PULONG      ChunkSize
);

NTSYSAPI
NTSTATUS
NTAPI
RtlDestroyHeap (
    IN HANDLE  HeapHandle
);

NTSYSAPI
BOOLEAN
NTAPI
RtlEqualSid (
    IN PSID Sid1,
    IN PSID Sid2
);

NTSYSAPI
VOID
NTAPI
RtlFillMemoryUlong (
    IN PVOID    Destination,
    IN ULONG    Length,
    IN ULONG    Fill
);

NTSYSAPI
BOOLEAN
NTAPI
RtlFreeHeap (
    IN HANDLE  HeapHandle,
    IN ULONG   Flags,
    IN PVOID   P
);

NTSYSAPI
VOID
NTAPI
RtlGenerate8dot3Name (
    IN PUNICODE_STRING              Name,
    IN BOOLEAN                      AllowExtendedCharacters,
    IN OUT PGENERATE_NAME_CONTEXT   Context,
    OUT PUNICODE_STRING             Name8dot3
);

NTSYSAPI
NTSTATUS
NTAPI
RtlGetCompressionWorkSpaceSize (
    IN USHORT   CompressionFormatAndEngine,
    OUT PULONG  CompressBufferWorkSpaceSize,
    OUT PULONG  CompressFragmentWorkSpaceSize
);

NTSYSAPI
NTSTATUS
NTAPI
RtlGetDaclSecurityDescriptor (
    IN PSECURITY_DESCRIPTOR SecurityDescriptor,
    OUT PBOOLEAN            DaclPresent,
    OUT PACL                *Dacl,
    OUT PBOOLEAN            DaclDefaulted
);

NTSYSAPI
NTSTATUS
NTAPI
RtlGetGroupSecurityDescriptor (
    IN PSECURITY_DESCRIPTOR SecurityDescriptor,
    OUT PSID                *Group,
    OUT PBOOLEAN            GroupDefaulted
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
ULONG
NTAPI
RtlGetNtGlobalFlags (
    VOID
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
RtlGetOwnerSecurityDescriptor (
    IN PSECURITY_DESCRIPTOR SecurityDescriptor,
    OUT PSID                *Owner,
    OUT PBOOLEAN            OwnerDefaulted
);

//
// This function returns a PIMAGE_NT_HEADERS,
// see the standard include file winnt.h
//
NTSYSAPI
PVOID
NTAPI
RtlImageNtHeader (
    IN PVOID BaseAddress
);

NTSYSAPI
NTSTATUS
NTAPI
RtlInitializeSid (
    IN OUT PSID                     Sid,
    IN PSID_IDENTIFIER_AUTHORITY    IdentifierAuthority,
    IN UCHAR                        SubAuthorityCount
);

NTSYSAPI
BOOLEAN
NTAPI
RtlIsNameLegalDOS8Dot3 (
    IN PUNICODE_STRING UnicodeName,
    IN PANSI_STRING    AnsiName,
    PBOOLEAN           Unknown
);

NTSYSAPI
ULONG
NTAPI
RtlLengthRequiredSid (
    IN UCHAR SubAuthorityCount
);

NTSYSAPI
ULONG
NTAPI
RtlLengthSid (
    IN PSID Sid
);

NTSYSAPI
ULONG
NTAPI
RtlNtStatusToDosError (
    IN NTSTATUS Status
);

#define RtlOemStringToCountedUnicodeSize(STRING) (                    \
    (ULONG)(RtlOemStringToUnicodeSize(STRING) - sizeof(UNICODE_NULL)) \
)

#define RtlOemStringToUnicodeSize(STRING) (                \
    NLS_MB_OEM_CODE_PAGE_TAG ?                             \
    RtlxOemStringToUnicodeSize(STRING) :                   \
    ((STRING)->Length + sizeof(ANSI_NULL)) * sizeof(WCHAR) \
)

NTSYSAPI
NTSTATUS
NTAPI
RtlOemStringToUnicodeString (
    OUT PUNICODE_STRING DestinationString,
    IN POEM_STRING      SourceString,
    IN BOOLEAN          AllocateDestinationString
);

NTSYSAPI
NTSTATUS
NTAPI
RtlReserveChunk (
    IN USHORT       CompressionFormat,
    IN OUT PUCHAR   *CompressedBuffer,
    IN PUCHAR       EndOfCompressedBufferPlus1,
    OUT PUCHAR      *ChunkBuffer,
    IN ULONG        ChunkSize
);

NTSYSAPI
VOID
NTAPI
RtlSecondsSince1970ToTime (
    IN ULONG            SecondsSince1970,
    OUT PLARGE_INTEGER  Time
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
RtlSelfRelativeToAbsoluteSD (
    IN PSECURITY_DESCRIPTOR     SelfRelativeSD,
    OUT PSECURITY_DESCRIPTOR    AbsoluteSD,
    IN PULONG                   AbsoluteSDSize,
    IN PACL                     Dacl,
    IN PULONG                   DaclSize,
    IN PACL                     Sacl,
    IN PULONG                   SaclSize,
    IN PSID                     Owner,
    IN PULONG                   OwnerSize,
    IN PSID                     PrimaryGroup,
    IN PULONG                   PrimaryGroupSize
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
RtlSetGroupSecurityDescriptor (
    IN OUT PSECURITY_DESCRIPTOR SecurityDescriptor,
    IN PSID                     Group,
    IN BOOLEAN                  GroupDefaulted
);

NTSYSAPI
NTSTATUS
NTAPI
RtlSetOwnerSecurityDescriptor (
    IN OUT PSECURITY_DESCRIPTOR SecurityDescriptor,
    IN PSID                     Owner,
    IN BOOLEAN                  OwnerDefaulted
);

NTSYSAPI
NTSTATUS
NTAPI
RtlSetSaclSecurityDescriptor (
    IN OUT PSECURITY_DESCRIPTOR SecurityDescriptor,
    IN BOOLEAN                  SaclPresent,
    IN PACL                     Sacl,
    IN BOOLEAN                  SaclDefaulted
);

NTSYSAPI
PUCHAR
NTAPI
RtlSubAuthorityCountSid (
    IN PSID Sid
);

NTSYSAPI
PULONG
NTAPI
RtlSubAuthoritySid (
    IN PSID    Sid,
    IN ULONG   SubAuthority
);

NTSYSAPI
VOID
NTAPI
RtlTimeToSecondsSince1970 (
    IN PLARGE_INTEGER   Time,
    OUT PULONG          SecondsSince1970
);

#define RtlUnicodeStringToOemSize(STRING) (                   \
    NLS_MB_OEM_CODE_PAGE_TAG ?                                \
    RtlxUnicodeStringToOemSize(STRING) :                      \
    ((STRING)->Length + sizeof(UNICODE_NULL)) / sizeof(WCHAR) \
)

NTSYSAPI
NTSTATUS
NTAPI
RtlUnicodeStringToOemString (
    OUT POEM_STRING     DestinationString,
    IN PUNICODE_STRING  SourceString,
    IN BOOLEAN          AllocateDestinationString
);

NTSYSAPI
BOOLEAN
NTAPI
RtlValidSid (
    IN PSID Sid
);

NTSYSAPI
ULONG
NTAPI
RtlxOemStringToUnicodeSize (
    IN POEM_STRING OemString
);

NTSYSAPI
ULONG
NTAPI
RtlxUnicodeStringToOemSize (
    IN PUNICODE_STRING UnicodeString
);

NTKERNELAPI
NTSTATUS
SeAppendPrivileges (
    PACCESS_STATE   AccessState,
    PPRIVILEGE_SET  Privileges
);

NTKERNELAPI
BOOLEAN
SeAuditingFileEvents (
    IN BOOLEAN              AccessGranted,
    IN PSECURITY_DESCRIPTOR SecurityDescriptor
);

NTKERNELAPI
BOOLEAN
SeAuditingFileOrGlobalEvents (
    IN BOOLEAN                      AccessGranted,
    IN PSECURITY_DESCRIPTOR         SecurityDescriptor,
    IN PSECURITY_SUBJECT_CONTEXT    SubjectContext
);

NTKERNELAPI
VOID
SeCaptureSubjectContext (
    OUT PSECURITY_SUBJECT_CONTEXT SubjectContext
);

NTKERNELAPI
NTSTATUS
SeCreateAccessState (
    OUT PACCESS_STATE   AccessState,
    IN PVOID            AuxData,
    IN ACCESS_MASK      AccessMask,
    IN PGENERIC_MAPPING Mapping
);

NTKERNELAPI
NTSTATUS
SeCreateClientSecurity (
    IN PETHREAD                     Thread,
    IN PSECURITY_QUALITY_OF_SERVICE QualityOfService,
    IN BOOLEAN                      RemoteClient,
    OUT PSECURITY_CLIENT_CONTEXT    ClientContext
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
SeCreateClientSecurityFromSubjectContext (
    IN PSECURITY_SUBJECT_CONTEXT    SubjectContext,
    IN PSECURITY_QUALITY_OF_SERVICE QualityOfService,
    IN BOOLEAN                      ServerIsRemote,
    OUT PSECURITY_CLIENT_CONTEXT    ClientContext
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
VOID
SeDeleteAccessState (
    IN PACCESS_STATE AccessState
);

#define SeDeleteClientSecurity(C)  {                                           \
            if (SeTokenType((C)->ClientToken) == TokenPrimary) {               \
                PsDereferencePrimaryToken( (C)->ClientToken );                 \
            } else {                                                           \
                PsDereferenceImpersonationToken( (C)->ClientToken );           \
            }                                                                  \
}

VOID
SeDeleteObjectAuditAlarm (
    IN PVOID    Object,
    IN HANDLE   Handle
);

#define SeEnableAccessToExports() SeExports = *(PSE_EXPORTS *)SeExports;

NTKERNELAPI
VOID
SeFreePrivileges (
    IN PPRIVILEGE_SET Privileges
);

NTKERNELAPI
VOID
SeImpersonateClient (
    IN PSECURITY_CLIENT_CONTEXT ClientContext,
    IN PETHREAD                 ServerThread OPTIONAL
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
SeImpersonateClientEx (
    IN PSECURITY_CLIENT_CONTEXT ClientContext,
    IN PETHREAD                 ServerThread OPTIONAL
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
VOID
SeLockSubjectContext (
    IN PSECURITY_SUBJECT_CONTEXT SubjectContext
);

NTKERNELAPI
NTSTATUS
SeMarkLogonSessionForTerminationNotification (
    IN PLUID LogonId
);

NTKERNELAPI
VOID
SeOpenObjectAuditAlarm (
    IN PUNICODE_STRING      ObjectTypeName,
    IN PVOID                Object OPTIONAL,
    IN PUNICODE_STRING      AbsoluteObjectName OPTIONAL,
    IN PSECURITY_DESCRIPTOR SecurityDescriptor,
    IN PACCESS_STATE        AccessState,
    IN BOOLEAN              ObjectCreated,
    IN BOOLEAN              AccessGranted,
    IN KPROCESSOR_MODE      AccessMode,
    OUT PBOOLEAN            GenerateOnClose
);

NTKERNELAPI
VOID
SeOpenObjectForDeleteAuditAlarm (
    IN PUNICODE_STRING      ObjectTypeName,
    IN PVOID                Object OPTIONAL,
    IN PUNICODE_STRING      AbsoluteObjectName OPTIONAL,
    IN PSECURITY_DESCRIPTOR SecurityDescriptor,
    IN PACCESS_STATE        AccessState,
    IN BOOLEAN              ObjectCreated,
    IN BOOLEAN              AccessGranted,
    IN KPROCESSOR_MODE      AccessMode,
    OUT PBOOLEAN            GenerateOnClose
);

NTKERNELAPI
BOOLEAN
SePrivilegeCheck (
    IN OUT PPRIVILEGE_SET           RequiredPrivileges,
    IN PSECURITY_SUBJECT_CONTEXT    SubjectContext,
    IN KPROCESSOR_MODE              AccessMode
);

NTKERNELAPI
NTSTATUS
SeQueryAuthenticationIdToken (
    IN PACCESS_TOKEN    Token,
    OUT PLUID           LogonId
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
SeQueryInformationToken (
    IN PACCESS_TOKEN           Token,
    IN TOKEN_INFORMATION_CLASS TokenInformationClass,
    OUT PVOID                  *TokenInformation
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
SeQuerySecurityDescriptorInfo (
    IN PSECURITY_INFORMATION    SecurityInformation,
    OUT PSECURITY_DESCRIPTOR    SecurityDescriptor,
    IN OUT PULONG               Length,
    IN PSECURITY_DESCRIPTOR     *ObjectsSecurityDescriptor
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
SeQuerySessionIdToken (
    IN PACCESS_TOKEN    Token,
    IN PULONG           SessionId
);

#endif // (VER_PRODUCTBUILD >= 2195)

#define SeQuerySubjectContextToken( SubjectContext )                \
    ( ARGUMENT_PRESENT(                                             \
        ((PSECURITY_SUBJECT_CONTEXT) SubjectContext)->ClientToken   \
        ) ?                                                         \
    ((PSECURITY_SUBJECT_CONTEXT) SubjectContext)->ClientToken :     \
    ((PSECURITY_SUBJECT_CONTEXT) SubjectContext)->PrimaryToken )

typedef NTSTATUS (*PSE_LOGON_SESSION_TERMINATED_ROUTINE) (
    IN PLUID LogonId
);

NTKERNELAPI
NTSTATUS
SeRegisterLogonSessionTerminatedRoutine (
    IN PSE_LOGON_SESSION_TERMINATED_ROUTINE CallbackRoutine
);

NTKERNELAPI
VOID
SeReleaseSubjectContext (
    IN PSECURITY_SUBJECT_CONTEXT SubjectContext
);

NTKERNELAPI
VOID
SeSetAccessStateGenericMapping (
    PACCESS_STATE       AccessState,
    PGENERIC_MAPPING    GenericMapping
);

NTKERNELAPI
NTSTATUS
SeSetSecurityDescriptorInfo (
    IN PVOID                    Object OPTIONAL,
    IN PSECURITY_INFORMATION    SecurityInformation,
    IN PSECURITY_DESCRIPTOR     SecurityDescriptor,
    IN OUT PSECURITY_DESCRIPTOR *ObjectsSecurityDescriptor,
    IN POOL_TYPE                PoolType,
    IN PGENERIC_MAPPING         GenericMapping
);

#if (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
NTSTATUS
SeSetSecurityDescriptorInfoEx (
    IN PVOID                    Object OPTIONAL,
    IN PSECURITY_INFORMATION    SecurityInformation,
    IN PSECURITY_DESCRIPTOR     ModificationDescriptor,
    IN OUT PSECURITY_DESCRIPTOR *ObjectsSecurityDescriptor,
    IN ULONG                    AutoInheritFlags,
    IN POOL_TYPE                PoolType,
    IN PGENERIC_MAPPING         GenericMapping
);

NTKERNELAPI
BOOLEAN
SeTokenIsAdmin (
    IN PACCESS_TOKEN Token
);

NTKERNELAPI
BOOLEAN
SeTokenIsRestricted (
    IN PACCESS_TOKEN Token
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTKERNELAPI
TOKEN_TYPE
SeTokenType (
    IN PACCESS_TOKEN Token
);

NTKERNELAPI
VOID
SeUnlockSubjectContext (
    IN PSECURITY_SUBJECT_CONTEXT SubjectContext
);

NTKERNELAPI
NTSTATUS
SeUnregisterLogonSessionTerminatedRoutine (
    IN PSE_LOGON_SESSION_TERMINATED_ROUTINE CallbackRoutine
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwAdjustPrivilegesToken (
    IN HANDLE               TokenHandle,
    IN BOOLEAN              DisableAllPrivileges,
    IN PTOKEN_PRIVILEGES    NewState,
    IN ULONG                BufferLength,
    OUT PTOKEN_PRIVILEGES   PreviousState OPTIONAL,
    OUT PULONG              ReturnLength
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwAlertThread (
    IN HANDLE ThreadHandle
);

NTSYSAPI
NTSTATUS
NTAPI
ZwAllocateVirtualMemory (
    IN HANDLE       ProcessHandle,
    IN OUT PVOID    *BaseAddress,
    IN ULONG        ZeroBits,
    IN OUT PSIZE_T  RegionSize,
    IN ULONG        AllocationType,
    IN ULONG        Protect
);

NTSYSAPI
NTSTATUS
NTAPI
ZwAccessCheckAndAuditAlarm (
    IN PUNICODE_STRING      SubsystemName,
    IN PVOID                HandleId,
    IN PUNICODE_STRING      ObjectTypeName,
    IN PUNICODE_STRING      ObjectName,
    IN PSECURITY_DESCRIPTOR SecurityDescriptor,
    IN ACCESS_MASK          DesiredAccess,
    IN PGENERIC_MAPPING     GenericMapping,
    IN BOOLEAN              ObjectCreation,
    OUT PACCESS_MASK        GrantedAccess,
    OUT PBOOLEAN            AccessStatus,
    OUT PBOOLEAN            GenerateOnClose
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwCancelIoFile (
    IN HANDLE               FileHandle,
    OUT PIO_STATUS_BLOCK    IoStatusBlock
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwClearEvent (
    IN HANDLE EventHandle
);

NTSYSAPI
NTSTATUS
NTAPI
ZwConnectPort (
    OUT PHANDLE                     ClientPortHandle,
    IN PUNICODE_STRING              ServerPortName,
    IN PSECURITY_QUALITY_OF_SERVICE SecurityQos,
    IN OUT PLPC_SECTION_WRITE       ClientSharedMemory OPTIONAL,
    IN OUT PLPC_SECTION_READ        ServerSharedMemory OPTIONAL,
    OUT PULONG                      MaximumMessageLength OPTIONAL,
    IN OUT PVOID                    ConnectionInfo OPTIONAL,
    IN OUT PULONG                   ConnectionInfoLength OPTIONAL
);

NTSYSAPI
NTSTATUS
NTAPI
ZwCloseObjectAuditAlarm (
    IN PUNICODE_STRING  SubsystemName,
    IN PVOID            HandleId,
    IN BOOLEAN          GenerateOnClose
);

NTSYSAPI
NTSTATUS
NTAPI
ZwCreateEvent (
    OUT PHANDLE             EventHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes OPTIONAL,
    IN EVENT_TYPE           EventType,
    IN BOOLEAN              InitialState
);

NTSYSAPI
NTSTATUS
NTAPI
ZwCreateSection (
    OUT PHANDLE             SectionHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes OPTIONAL,
    IN PLARGE_INTEGER       MaximumSize OPTIONAL,
    IN ULONG                SectionPageProtection,
    IN ULONG                AllocationAttributes,
    IN HANDLE               FileHandle OPTIONAL
);

NTSYSAPI
NTSTATUS
NTAPI
ZwCreateSymbolicLinkObject (
    OUT PHANDLE             SymbolicLinkHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes,
    IN PUNICODE_STRING      TargetName
);

NTSYSAPI
NTSTATUS
NTAPI
ZwDeleteFile (
    IN POBJECT_ATTRIBUTES ObjectAttributes
);

NTSYSAPI
NTSTATUS
NTAPI
ZwDeleteValueKey (
    IN HANDLE           Handle,
    IN PUNICODE_STRING  Name
);

NTSYSAPI
NTSTATUS
NTAPI
ZwDeviceIoControlFile (
    IN HANDLE               FileHandle,
    IN HANDLE               Event OPTIONAL,
    IN PIO_APC_ROUTINE      ApcRoutine OPTIONAL,
    IN PVOID                ApcContext OPTIONAL,
    OUT PIO_STATUS_BLOCK    IoStatusBlock,
    IN ULONG                IoControlCode,
    IN PVOID                InputBuffer OPTIONAL,
    IN ULONG                InputBufferLength,
    OUT PVOID               OutputBuffer OPTIONAL,
    IN ULONG                OutputBufferLength
);

//
// If using ZwDisplayString during boot on Windows 2000 or later you must
// first call InbvEnableDisplayString.
//
NTSYSAPI
NTSTATUS
NTAPI
ZwDisplayString (
    IN PUNICODE_STRING String
);

NTSYSAPI
NTSTATUS
NTAPI
ZwDuplicateObject (
    IN HANDLE       SourceProcessHandle,
    IN HANDLE       SourceHandle,
    IN HANDLE       TargetProcessHandle OPTIONAL,
    OUT PHANDLE     TargetHandle OPTIONAL,
    IN ACCESS_MASK  DesiredAccess,
    IN ULONG        HandleAttributes,
    IN ULONG        Options
);

NTSYSAPI
NTSTATUS
NTAPI
ZwDuplicateToken (
    IN HANDLE               ExistingTokenHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes,
    IN BOOLEAN              EffectiveOnly,
    IN TOKEN_TYPE           TokenType,
    OUT PHANDLE             NewTokenHandle
);

NTSYSAPI
NTSTATUS
NTAPI
ZwFlushInstructionCache (
    IN HANDLE   ProcessHandle,
    IN PVOID    BaseAddress OPTIONAL,
    IN ULONG    FlushSize
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwFlushVirtualMemory (
    IN HANDLE               ProcessHandle,
    IN OUT PVOID            *BaseAddress,
    IN OUT PSIZE_T          RegionSize,
    OUT PIO_STATUS_BLOCK    IoStatusBlock
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwFreeVirtualMemory (
    IN HANDLE       ProcessHandle,
    IN OUT PVOID    *BaseAddress,
    IN OUT PSIZE_T  RegionSize,
    IN ULONG        FreeType
);

NTSYSAPI
NTSTATUS
NTAPI
ZwFsControlFile (
    IN HANDLE               FileHandle,
    IN HANDLE               Event OPTIONAL,
    IN PIO_APC_ROUTINE      ApcRoutine OPTIONAL,
    IN PVOID                ApcContext OPTIONAL,
    OUT PIO_STATUS_BLOCK    IoStatusBlock,
    IN ULONG                FsControlCode,
    IN PVOID                InputBuffer OPTIONAL,
    IN ULONG                InputBufferLength,
    OUT PVOID               OutputBuffer OPTIONAL,
    IN ULONG                OutputBufferLength
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwInitiatePowerAction (
    IN POWER_ACTION         SystemAction,
    IN SYSTEM_POWER_STATE   MinSystemState,
    IN ULONG                Flags,
    IN BOOLEAN              Asynchronous
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwLoadDriver (
    // "\\Registry\\Machine\\System\\CurrentControlSet\\Services\\<DriverName>"
    IN PUNICODE_STRING RegistryPath
);

NTSYSAPI
NTSTATUS
NTAPI
ZwLoadKey (
    IN POBJECT_ATTRIBUTES KeyObjectAttributes,
    IN POBJECT_ATTRIBUTES FileObjectAttributes
);

NTSYSAPI
NTSTATUS
NTAPI
ZwNotifyChangeKey (
    IN HANDLE               KeyHandle,
    IN HANDLE               EventHandle OPTIONAL,
    IN PIO_APC_ROUTINE      ApcRoutine OPTIONAL,
    IN PVOID                ApcContext OPTIONAL,
    OUT PIO_STATUS_BLOCK    IoStatusBlock,
    IN ULONG                NotifyFilter,
    IN BOOLEAN              WatchSubtree,
    IN PVOID                Buffer,
    IN ULONG                BufferLength,
    IN BOOLEAN              Asynchronous
);

NTSYSAPI
NTSTATUS
NTAPI
ZwOpenDirectoryObject (
    OUT PHANDLE             DirectoryHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes
);

NTSYSAPI
NTSTATUS
NTAPI
ZwOpenEvent (
    OUT PHANDLE             EventHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes
);

NTSYSAPI
NTSTATUS
NTAPI
ZwOpenProcess (
    OUT PHANDLE             ProcessHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes,
    IN PCLIENT_ID           ClientId OPTIONAL
);

NTSYSAPI
NTSTATUS
NTAPI
ZwOpenProcessToken (
    IN HANDLE       ProcessHandle,
    IN ACCESS_MASK  DesiredAccess,
    OUT PHANDLE     TokenHandle
);

#if (VER_PRODUCTBUILD >= 2600)

NTSYSAPI
NTSTATUS
NTAPI
ZwOpenProcessTokenEx (
    IN HANDLE       ProcessHandle,
    IN ACCESS_MASK  DesiredAccess,
    IN ULONG        HandleAttributes,
    OUT PHANDLE     TokenHandle
);

#endif // (VER_PRODUCTBUILD >= 2600)

NTSYSAPI
NTSTATUS
NTAPI
ZwOpenThread (
    OUT PHANDLE             ThreadHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes,
    IN PCLIENT_ID           ClientId
);

NTSYSAPI
NTSTATUS
NTAPI
ZwOpenThreadToken (
    IN HANDLE       ThreadHandle,
    IN ACCESS_MASK  DesiredAccess,
    IN BOOLEAN      OpenAsSelf,
    OUT PHANDLE     TokenHandle
);

#if (VER_PRODUCTBUILD >= 2600)

NTSYSAPI
NTSTATUS
NTAPI
ZwOpenThreadTokenEx (
    IN HANDLE       ThreadHandle,
    IN ACCESS_MASK  DesiredAccess,
    IN BOOLEAN      OpenAsSelf,
    IN ULONG        HandleAttributes,
    OUT PHANDLE     TokenHandle
);

#endif // (VER_PRODUCTBUILD >= 2600)

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwPowerInformation (
    IN POWER_INFORMATION_LEVEL  PowerInformationLevel,
    IN PVOID                    InputBuffer OPTIONAL,
    IN ULONG                    InputBufferLength,
    OUT PVOID                   OutputBuffer OPTIONAL,
    IN ULONG                    OutputBufferLength
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwPulseEvent (
    IN HANDLE   EventHandle,
    OUT PULONG  PreviousState OPTIONAL
);

NTSYSAPI
NTSTATUS
NTAPI
ZwQueryDefaultLocale (
    IN BOOLEAN  ThreadOrSystem,
    OUT PLCID   Locale
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwQueryDefaultUILanguage (
    OUT LANGID *LanguageId
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwQueryDirectoryFile (
    IN HANDLE                   FileHandle,
    IN HANDLE                   Event OPTIONAL,
    IN PIO_APC_ROUTINE          ApcRoutine OPTIONAL,
    IN PVOID                    ApcContext OPTIONAL,
    OUT PIO_STATUS_BLOCK        IoStatusBlock,
    OUT PVOID                   FileInformation,
    IN ULONG                    Length,
    IN FILE_INFORMATION_CLASS   FileInformationClass,
    IN BOOLEAN                  ReturnSingleEntry,
    IN PUNICODE_STRING          FileName OPTIONAL,
    IN BOOLEAN                  RestartScan
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwQueryDirectoryObject (
    IN HANDLE       DirectoryHandle,
    OUT PVOID       Buffer,
    IN ULONG        Length,
    IN BOOLEAN      ReturnSingleEntry,
    IN BOOLEAN      RestartScan,
    IN OUT PULONG   Context,
    OUT PULONG      ReturnLength OPTIONAL
);

NTSYSAPI
NTSTATUS
NTAPI
ZwQueryEaFile (
    IN HANDLE               FileHandle,
    OUT PIO_STATUS_BLOCK    IoStatusBlock,
    OUT PVOID               Buffer,
    IN ULONG                Length,
    IN BOOLEAN              ReturnSingleEntry,
    IN PVOID                EaList OPTIONAL,
    IN ULONG                EaListLength,
    IN PULONG               EaIndex OPTIONAL,
    IN BOOLEAN              RestartScan
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwQueryInformationProcess (
    IN HANDLE           ProcessHandle,
    IN PROCESSINFOCLASS ProcessInformationClass,
    OUT PVOID           ProcessInformation,
    IN ULONG            ProcessInformationLength,
    OUT PULONG          ReturnLength OPTIONAL
);

NTSYSAPI
NTSTATUS
NTAPI
ZwQueryInformationToken (
    IN HANDLE                   TokenHandle,
    IN TOKEN_INFORMATION_CLASS  TokenInformationClass,
    OUT PVOID                   TokenInformation,
    IN ULONG                    TokenInformationLength,
    OUT PULONG                  ReturnLength
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwQueryInstallUILanguage (
    OUT LANGID *LanguageId
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwQueryObject (
    IN HANDLE               ObjectHandle,
    IN OBJECT_INFO_CLASS    ObjectInformationClass,
    OUT PVOID               ObjectInformation,
    IN ULONG                Length,
    OUT PULONG              ResultLength
);

NTSYSAPI
NTSTATUS
NTAPI
ZwQuerySection (
    IN HANDLE                       SectionHandle,
    IN SECTION_INFORMATION_CLASS    SectionInformationClass,
    OUT PVOID                       SectionInformation,
    IN ULONG                        SectionInformationLength,
    OUT PULONG                      ResultLength OPTIONAL
);

NTSYSAPI
NTSTATUS
NTAPI
ZwQuerySecurityObject (
    IN HANDLE                   FileHandle,
    IN SECURITY_INFORMATION     SecurityInformation,
    OUT PSECURITY_DESCRIPTOR    SecurityDescriptor,
    IN ULONG                    Length,
    OUT PULONG                  ResultLength
);

NTSYSAPI
NTSTATUS
NTAPI
ZwQuerySystemInformation (
    IN SYSTEM_INFORMATION_CLASS SystemInformationClass,
    OUT PVOID                   SystemInformation,
    IN ULONG                    Length,
    OUT PULONG                  ReturnLength
);

NTSYSAPI
NTSTATUS
NTAPI
ZwQueryVolumeInformationFile (
    IN HANDLE               FileHandle,
    OUT PIO_STATUS_BLOCK    IoStatusBlock,
    OUT PVOID               FsInformation,
    IN ULONG                Length,
    IN FS_INFORMATION_CLASS FsInformationClass
);

NTSYSAPI
NTSTATUS
NTAPI
ZwReplaceKey (
    IN POBJECT_ATTRIBUTES   NewFileObjectAttributes,
    IN HANDLE               KeyHandle,
    IN POBJECT_ATTRIBUTES   OldFileObjectAttributes
);

NTSYSAPI
NTSTATUS
NTAPI
ZwRequestWaitReplyPort (
    IN HANDLE           PortHandle,
    IN PLPC_MESSAGE     Request,
    OUT PLPC_MESSAGE    Reply
);

NTSYSAPI
NTSTATUS
NTAPI
ZwResetEvent (
    IN HANDLE   EventHandle,
    OUT PULONG  PreviousState OPTIONAL
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwRestoreKey (
    IN HANDLE   KeyHandle,
    IN HANDLE   FileHandle,
    IN ULONG    Flags
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwSaveKey (
    IN HANDLE KeyHandle,
    IN HANDLE FileHandle
);

NTSYSAPI
NTSTATUS
NTAPI
ZwSetDefaultLocale (
    IN BOOLEAN  ThreadOrSystem,
    IN LCID     Locale
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwSetDefaultUILanguage (
    IN LANGID LanguageId
);

NTSYSAPI
NTSTATUS
NTAPI
ZwSetEaFile (
    IN HANDLE               FileHandle,
    OUT PIO_STATUS_BLOCK    IoStatusBlock,
    OUT PVOID               Buffer,
    IN ULONG                Length
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwSetEvent (
    IN HANDLE   EventHandle,
    OUT PULONG  PreviousState OPTIONAL
);

NTSYSAPI
NTSTATUS
NTAPI
ZwSetInformationObject (
    IN HANDLE               ObjectHandle,
    IN OBJECT_INFO_CLASS    ObjectInformationClass,
    IN PVOID                ObjectInformation,
    IN ULONG                ObjectInformationLength
);

NTSYSAPI
NTSTATUS
NTAPI
ZwSetInformationProcess (
    IN HANDLE           ProcessHandle,
    IN PROCESSINFOCLASS ProcessInformationClass,
    IN PVOID            ProcessInformation,
    IN ULONG            ProcessInformationLength
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwSetSecurityObject (
    IN HANDLE               Handle,
    IN SECURITY_INFORMATION SecurityInformation,
    IN PSECURITY_DESCRIPTOR SecurityDescriptor
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwSetSystemInformation (
    IN SYSTEM_INFORMATION_CLASS SystemInformationClass,
    IN PVOID                    SystemInformation,
    IN ULONG                    Length
);

NTSYSAPI
NTSTATUS
NTAPI
ZwSetSystemTime (
    IN PLARGE_INTEGER   NewTime,
    OUT PLARGE_INTEGER  OldTime OPTIONAL
);

#if (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwSetVolumeInformationFile (
    IN HANDLE               FileHandle,
    OUT PIO_STATUS_BLOCK    IoStatusBlock,
    IN PVOID                FsInformation,
    IN ULONG                Length,
    IN FS_INFORMATION_CLASS FsInformationClass
);

#endif // (VER_PRODUCTBUILD >= 2195)

NTSYSAPI
NTSTATUS
NTAPI
ZwTerminateProcess (
    IN HANDLE   ProcessHandle OPTIONAL,
    IN NTSTATUS ExitStatus
);

NTSYSAPI
NTSTATUS
NTAPI
ZwUnloadDriver (
    // "\\Registry\\Machine\\System\\CurrentControlSet\\Services\\<DriverName>"
    IN PUNICODE_STRING RegistryPath
);

NTSYSAPI
NTSTATUS
NTAPI
ZwUnloadKey (
    IN POBJECT_ATTRIBUTES KeyObjectAttributes
);

NTSYSAPI
NTSTATUS
NTAPI
ZwWaitForSingleObject (
    IN HANDLE           Handle,
    IN BOOLEAN          Alertable,
    IN PLARGE_INTEGER   Timeout OPTIONAL
);

NTSYSAPI
NTSTATUS
NTAPI
ZwWaitForMultipleObjects (
    IN ULONG            HandleCount,
    IN PHANDLE          Handles,
    IN WAIT_TYPE        WaitType,
    IN BOOLEAN          Alertable,
    IN PLARGE_INTEGER   Timeout OPTIONAL
);

NTSYSAPI
NTSTATUS
NTAPI
ZwYieldExecution (
    VOID
);

//
// Below is stuff that is included in the Windows 2000 DDK but is missing in
// the Windows NT 4.0 DDK
//

#if (VER_PRODUCTBUILD < 2195)

NTSYSAPI
VOID
NTAPI
HalMakeBeep (
    IN ULONG Frequency
);

#ifndef IoCopyCurrentIrpStackLocationToNext
#define IoCopyCurrentIrpStackLocationToNext( Irp ) { \
    PIO_STACK_LOCATION irpSp; \
    PIO_STACK_LOCATION nextIrpSp; \
    irpSp = IoGetCurrentIrpStackLocation( (Irp) ); \
    nextIrpSp = IoGetNextIrpStackLocation( (Irp) ); \
    RtlCopyMemory( \
        nextIrpSp, \
        irpSp, \
        FIELD_OFFSET(IO_STACK_LOCATION, CompletionRoutine) \
        ); \
    nextIrpSp->Control = 0; }
#endif

NTKERNELAPI
NTSTATUS
IoCreateFile (
    OUT PHANDLE             FileHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes,
    OUT PIO_STATUS_BLOCK    IoStatusBlock,
    IN PLARGE_INTEGER       AllocationSize OPTIONAL,
    IN ULONG                FileAttributes,
    IN ULONG                ShareAccess,
    IN ULONG                CreateDisposition,
    IN ULONG                CreateOptions,
    IN PVOID                EaBuffer OPTIONAL,
    IN ULONG                EaLength,
    IN CREATE_FILE_TYPE     CreateFileType,
    IN PVOID                ExtraCreateParameters,
    IN ULONG                Options
);

#ifndef IoSkipCurrentIrpStackLocation
#define IoSkipCurrentIrpStackLocation( Irp ) \
    (Irp)->CurrentLocation++; \
    (Irp)->Tail.Overlay.CurrentStackLocation++;
#endif

NTSYSAPI
VOID
NTAPI
ProbeForWrite (
    IN PVOID Address,
    IN ULONG Length,
    IN ULONG Alignment
);

NTSYSAPI
NTSTATUS
NTAPI
ZwOpenFile (
    OUT PHANDLE             FileHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes,
    OUT PIO_STATUS_BLOCK    IoStatusBlock,
    IN ULONG                ShareAccess,
    IN ULONG                OpenOptions
);

NTSYSAPI
NTSTATUS
NTAPI
ZwOpenSymbolicLinkObject (
    OUT PHANDLE             SymbolicLinkHandle,
    IN ACCESS_MASK          DesiredAccess,
    IN POBJECT_ATTRIBUTES   ObjectAttributes
);

NTSYSAPI
NTSTATUS
NTAPI
ZwQuerySymbolicLinkObject (
    IN HANDLE               LinkHandle,
    IN OUT PUNICODE_STRING  LinkTarget,
    OUT PULONG              ReturnedLength OPTIONAL
);

#endif // (VER_PRODUCTBUILD < 2195)

#ifdef __cplusplus
}
#endif

#endif // _NTIFS_

